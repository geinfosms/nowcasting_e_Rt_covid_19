# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(forecast)
library(EpiEstim)
library(EpiModel)
library(foreign)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(zoo)
library(nowfcts)
library(reshape2)
library(RcppRoll)

# Nowcasting --------------------------------------------------------------------
#Utiliza-se os dados de casos confirmados do estado de SC 
#Disponível em: http://dados.sc.gov.br/dataset/casos-19-dados-anonimizados-de-casos-confirmados/resource/76d6dfe8-7fe9-45c1-95f4-cab971803d49
base <- read_delim("base/boavista_covid_dados_abertos.csv", 
					   ";", escape_double = FALSE, trim_ws = TRUE)

casos <- base

casos <- data.frame(INICIO_SINTOMAS = casos$data_inicio_sintomas, 
		    CASOS = rep.int(1,nrow(casos)))

casos <- casos %>%
	group_by(INICIO_SINTOMAS) %>%
	summarise(CASOS = sum(CASOS, na.rm = T))
casos$INICIO_SINTOMAS <- as.Date(casos$INICIO_SINTOMAS, format = "%d/%m/%Y")

#A contaminação ocorre, em média, 5 dias antes do início dos sintomas, trabalhar-se-á a data provável de contaminação
casos$DT_CONTAMINACAO <- casos$INICIO_SINTOMAS - 5 
casos$INICIO_SINTOMAS <- NULL

ggplot(casos, aes(DT_CONTAMINACAO, CASOS, group = 1))+
	geom_smooth()




#Recortando datas em que o paciente foi contaminado, mas que ainda não tem resultado de exame
contaminacao_ao_inicio_sintomas <- 5
inicio_sintomas_notificacao <- 14 #dados Florianópolis
notificacao_resultado_exames <- 7


recorte <- (contaminacao_ao_inicio_sintomas + 
	    	inicio_sintomas_notificacao +
	       notificacao_resultado_exames
	    )

casos <- casos[order(casos$DT_CONTAMINACAO),]
casos_trunc <- subset(casos, casos$DT_CONTAMINACAO <
			       	(tail(casos$DT_CONTAMINACAO,1) - recorte))


#realizando as projeções do período excluído, para ajustar os artefatos
casos_proj <- forecast(auto.arima(casos_trunc$CASOS),
		       h=(nrow(casos)-nrow(casos_trunc)+6))$mean[1:(nrow(casos)-nrow(casos_trunc)+5)] %>% #5 pois a data foi defazada para chegar à data de contaminação + 1 para avançar 1 dia
	as.data.frame()

casos_proj <- data.frame(CASOS = casos_proj,
			 DT_CONTAMINACAO = c(max(as.Date(casos_trunc$DT_CONTAMINACAO)+1):(max(as.Date(casos_trunc$DT_CONTAMINACAO))+(nrow(casos)-nrow(casos_trunc))+5))) 

names(casos_proj) <- c("CASOS", "DT_CONTAMINACAO")
casos_proj$DT_CONTAMINACAO <- as.Date(casos_proj$DT_CONTAMINACAO, origin = "1970-01-01")
casos_arima <- rbind(casos_trunc, casos_proj) %>% as.data.frame()
names(casos_arima) <- c("CASOS", "DATA")
ggplot(casos_arima, aes(DATA, CASOS, group = 1))+
	geom_line()
ggplot(casos_arima, aes(DATA, CASOS, group = 1))+
	geom_smooth()




#Modelo COVID-BR
# casos_zoo <- zoo(casos_trunc$CASOS,casos_trunc$DT_CONTAMINACAO)
# casos_proj_covid_br <- forecast.exponential(zoo.obj = casos_zoo, start = min(casos$DT_CONTAMINACAO), end = length(casos_zoo), days.forecast = nrow(casos)-nrow(casos_trunc)+5)
# casos_proj_covid_br <- data_frame(CASOS = casos_proj_covid_br$predito,
# 				  DT_CONTAMINACAO = casos_proj$DT_CONTAMINACAO)
# 
# casos_exp_poisson <- rbind(casos_trunc, casos_proj_covid_br) %>% as.data.frame()
# ggplot(casos_exp_poisson, aes(DT_CONTAMINACAO, CASOS, group = 1))+
# 	geom_line()
# ggplot(casos_exp_poisson, aes(DT_CONTAMINACAO, CASOS, group = 1))+
# 	geom_smooth()
# 




# Definição dos parâmentro iniciais para o modelo  ------------------------------------------------

#################################################
#Ajsute para a truncagem à direita
#################################################

# A estimativa em tempo quase real requer não apenas inferir os tempos de infecção a partir dos dados observados,
# mas também ajustar as observações ausentes de infecções recentes.
# A ausência de infecções recentes nos dados analisados é conhecida como truncamento.
# Sem ajuste para o truncamento correto, o número de infecções recentes parecerá artificialmente baixo
# porque ainda não foram relatadas. (Gostic KM et al, 2020)
#Para corrigir isso, os casos classificados em um período menor que o do tempo do contágio ao tempo de notificação
#serão truncados e utilizar-se-á a função auto.arima, que seleciona entre modelos de arima e de
#suavização esponencial, para o nowcasting desse período.
#O perído de incubação é definido como tempo do contágio ao início dos sintomas. (Prem K et al, 2020; He X et al, 2020).
#A média do período de incubação da COVID-19 foi estimada em 5.0 dias (IC95% 4.2, 6.0). (Prem K et al, 2020)
#Assim, utilizar-se-á 5 dias como tempo de contato com o vírus ao início dos sintomas. O tempo do início dos sintomas
#à notificação pode ser inferido dos dados da SMS.

#################################################
#Estimativa do número de óbitos
#################################################
obitos <- base

obitos <- data.frame(DATA = base$data_obito, 
		    OBITOS = ifelse(obitos$obito == 'SIM', 1, 0))

obitos <- obitos %>% 
	  group_by(DATA) %>%
	  summarise(OBITOS = sum(OBITOS, na.rm = T))

obitos$DATA <- as.Date(obitos$DATA, format = "%d/%m/%Y")
obitos <- na.omit(obitos)

#Recortando datas em que o paciente foi contaminado ao óbito
contaminacao_ao_obito <- 17

obitos <- obitos[order(obitos$DATA),]
obitos_trunc <- subset(obitos, obitos$DATA <
		      	(tail(obitos$DATA,1) - contaminacao_ao_obito))


#realizando as projeções do período excluído, para ajustar os artefatos
obitos_proj <- forecast(auto.arima(obitos_trunc$OBITOS),
		       h=(nrow(obitos)-nrow(obitos_trunc)+1))$mean[1:(nrow(obitos)-nrow(obitos_trunc))] %>% 
	as.data.frame()

obitos_proj <- data.frame(obitos = obitos_proj,
			 DATA = c(max(as.Date(obitos_trunc$DATA)+1):(max(as.Date(obitos_trunc$DATA))+(nrow(obitos)-nrow(obitos_trunc))))) 

names(obitos_proj) <- c("OBITOS", "DATA")
obitos_proj$DATA <- as.Date(obitos_proj$DATA, origin = "1970-01-01")
obitos_arima <- rbind(obitos_trunc, obitos_proj) %>% as.data.frame()
names(obitos_arima) <- c("OBITOS", "DATA")


#################################################
#Estimativa do número de expostos
#################################################
#O perído de exposição pode ser definido como tempo entre o contato com o vírus (início da incubação) ao início da infectividade.
#Os pacientes expostos iniciam o contágio, em média, 2 a 3 dias antes do início dos sintomas. (Wölfel R et all, 2020).
#Adotou-se, então, como período de exposição aquele entrea 5 dias e 3 dias antes do início dos sintomas.
#Para se analisar a quantidade de pacientes expostos por dia, subtraiu-se 5 da data de início de sintomas e utilizou-se
#soma móvel de 2 dias (5 dia ao 3 dia antes do início dos sintomas).Para corrigir o truncados à direita,
#utilizou-se o modelo de suavização esponencial ou ARIMA, com o menor erro quadrado.
expostos <- casos_arima
expostos$EXPOSTOS <- roll_sum(expostos$CASOS,2, fill = 0, align = "right") #menos de 3 dias do início dos sintomas
names(obitos_arima) <- c("OBITOS", "DATA")
expostos$CASOS <- NULL

#################################################
#Estimativa do número de infectantes
#################################################
#De acordo com estudos recentes(Zou L et al, 2020; To KKW et al, 2020), a carga viral diminui monotonicamente
#após o início dos sintomas. Outro estudo de Wuhan detectou o vírums em pacientes 20 dias (mediana)
#após o início dos sintomas (Zhou F et al, 2020). Contudo, após 8 didas do início dos sintomas,
#o vírus vivo não pode mais ser cultivado, o que pode indicar o fim do perído de infectividade. (Wölfel R et al, 2020)
#Esta pesquisa adotou, então, como período infectante aquele entre dois dias antes e 8 dias após o início dos sintomas.
#Para a estimativa dos casos truncados, utilizou-se o modelo de suavização esponencial
#ou ARIMA, com o menor erro quadrado
infectantes <- casos_arima
infectantes$INFECTANTES <- roll_sum(infectantes$CASOS,11, fill = 0, align = "right") 
infectantes$CASOS 


#################################################
#Estimativa do número de recuperados
#################################################
#Considerou-se recuperado o indivíduo com mais de 14 dias após a contaminação e que não foi a óbito. Óbitos e recuperados
#são cumulativos
recuperados <- casos_arima
recuperados$DATA <- (recuperados$DATA+14) %>% as.character()
names(recuperados)[1] <- "RECUPERADOS"
recuperados$DATA <- as.Date(recuperados$DATA)

recuperados <- merge(recuperados, obitos, by = "DATA", all = T)
recuperados[is.na(recuperados)] <- 0
recuperados$RECUPERADOS <- recuperados$RECUPERADOS - recuperados$OBITOS
#Base SEIRD
expostos$DATA <- as.Date(expostos$DATA)
infectantes$DATA <- as.Date(infectantes$DATA)

base <- merge(recuperados, expostos, by = "DATA", all = T)
base <- merge(base, infectantes, by = "DATA", all = T)
##Cumulativos e suceptíveis
base$DATA <- as.Date(base$DATA, "%Y-%m-%d")
base <- base[order(base$DATA),]
base[is.na(base$RECUPERADOS), names(base) == "RECUPERADOS"] <- 0
base$CUM_RECUPERADOS <- cumsum(base$RECUPERADOS)
base[is.na(base$OBITOS), names(base) == "OBITOS"] <- 0
base$CUM_OBITOS <- cumsum(base$OBITOS)

#################################################
#Estimativa do número de suscetíveis
#################################################
POP <- 7164788
base$SUSCETIVEIS <- POP - base$CUM_RECUPERADOS - base$CUM_OBITOS - base$EXPOSTOS - base$INFECTANTES

#################################################
#merge
#################################################
#merge dos dados de ocupação de leitos e número de intenação com os outros dados
base$DATA <- as.Date(base$DATA)
base <- unique(base)
base <- na.omit(base)

# Estimando o Rt ----------------------------------------------------------
source("apeEstim.R")
source("apePredPost.R")

incidencia <- base
incidencia <- incidencia %>% dplyr::select('CASOS', 'DATA')
incidencia <- subset(incidencia, incidencia$DATA > c(Sys.Date()-92)) #Utilizando dados dos últimos trës meses

#Left trunc
trunc <- 0

Icovid <- incidencia$CASOS #Incidência
gencovid <- EpiEstim::discr_si(c(0:max(incidencia$CASOS)), mu = 4.8, sigma = 2.3) #distribuição gama
Lcovid <- overall_infectivity(Icovid, gencovid)

#Priors and settings
Rprior = c(1, 5); a = 0.025 #Confidence interval level

#Clean Lam vectors of NAs
Lcovid[is.na(Lcovid)] = 0# <------ important

#Best estimates and prediction
Rmodcovid <- apeEstim(Icovid, gencovid, Lcovid, Rprior, a, trunc, "covid")
Rcovid <- Rmodcovid[[2]][[4]]
RcovidCI_025 <- Rmodcovid[[2]][[5]][1,]
RcovidCI_975 <- Rmodcovid[[2]][[5]][2,]
DATA <- tail(incidencia$DATA,-1)
res_base <- data.frame(DATA = DATA, MEDIA = Rcovid, IC025 = RcovidCI_025, IC975 = RcovidCI_975)
res_base <- subset(res_base, res_base$DATA >= (Sys.Date() -61))

res_melt <- melt(res_base, id.vars = "DATA")
res_melt$DATA <- as.Date(res_melt$DATA, origin = "1970-01-01")
write.csv(res_melt, "base/rt.csv",row.names = F)
res_base$DATA <- as.Date(res_base$DATA, origin = "1970-01-01")
res_base_14dias <- subset(res_base, res_base$DATA > Sys.Date() -15)
write.csv(res_base_14dias, "base/res_base_14dias.csv", row.names = F)


ggplot(res_melt, aes(DATA, value, group = variable, color = variable))+
	geom_line()+
	geom_hline(yintercept = 1) +
	theme_bw()



# Forecast do número de casos e dos óbitos --------------------------------
#Estados Iniciais
S <- tail(base$SUSCETIVEIS,1)[1]
E <- tail(base$EXPOSTOS,1)[1]
I <- tail(base$INFECTANTES,1)[1]
R <- tail(base$CUM_RECUPERADOS,1)[1]
D <- tail(base$CUM_OBITOS,1)[1]
N <- S + E + I +  R + D
id.dur <- 12
ei.dur <- 2
etha <- 1/ir.dur
betha <- 1/ei.dur
delta <- 1/id.dur

#Estimativa das probabilidade para o modelo
base$TOTAL <- base$EXPOSTOS + base$INFECTANTES + base$CUM_RECUPERADOS + base$CUM_OBITOS
base$CUM_CASOS <- cumsum(base$CASOS)
prob1 <- tail(base$CUM_OBITOS,1)/tail(base$TOTAL,1) #Taxa de hospitalizados recuperados de UTI - denominador usando a defasagem

init <- init.dcm(S = S,
		 E = E,
		 I = I,
		 R = R,
		 D = D,
		 se.flow = 0,
		 ei.flow = 0,
		 ir.flow = 0,
		 id.flow = 0
)



param <- param.dcm(Rt = c(tail(res_base$IC025,1),
			  tail(res_base$MEDIA,1),
			  tail(res_base$IC975,1)),
		   etha = 1/ir.dur,
		   betha = 1/ei.dur,
		   delta = 1/id.dur,
		   prob1 = prob1
)


#Função SEIRD
SEIRD <- function(t, t0, parms) {
	with(as.list(c(t0, parms)), {
		
		N <- S + E + I +  R + D
		
		alpha <- etha * Rt * I/N
		
		#Equações diferenciais
		dS <- -alpha*S
		dE <- alpha*S - betha*E
		dI <- betha*E - prob1*delta*I - (1-prob1)*etha*I
		dR <- (1 - prob1)*etha*I 
		dD <- prob1*delta*I
		
		#Outputs
		list(c(dS, dE, dI, dR, dD,
		       se.flow = alpha * S,
		       ei.flow = betha * E,
		       ir.flow = (1 - prob1)*etha*I,
		       id.flow = prob1*delta*I),
		     num = N,
		     s.prev = S / N,
		     e.prev = E / N,
		     i.prev = I / N,
		     ei.prev = (E + I)/N,
		     r.prev = R / N,
		     d.prev = D / N)
	})
}


#Resolvendo as equações diferenciais
projecao <- 21
control <- control.dcm(nsteps = projecao, new.mod = SEIRD)
mod <- dcm(param, init, control)



######################################
#Cenário Rt 1 - IC2.5
######################################
resultados_cenario_1 <- data.frame(SUSCETIVEIS = mod$epi$S$run1,
				   EXPOSTOS = mod$epi$E$run1,
				   INFECTANTES = mod$epi$I$run1,
				   CUM_RECUPERADOS = mod$epi$R$run1,
				   CUM_OBITOS = mod$epi$D$run1
)

resultados_cenario_1 <- resultados_cenario_1[-1,]
resultados_cenario_1$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_1$DATA  <- as.Date(resultados_cenario_1$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
resultados_cenario_1 <- rbind(base_select, resultados_cenario_1)
names(resultados_cenario_1) <-c("DATA", "SUSCETIVEIS_CENARIO_1", "CUM_RECUPERADOS_CENARIO_1", "EXPOSTOS_CENARIO_1", "INFECTANTES_CENARIO_1", "CUM_OBITOS_CENARIO_1")

######################################
#Cenário 2 - Rt Mediana
######################################
resultados_cenario_2 <- data.frame(SUSCETIVEIS = mod$epi$S$run2,
				   EXPOSTOS = mod$epi$E$run2,
				   INFECTANTES = mod$epi$I$run2,
				   CUM_RECUPERADOS = mod$epi$R$run2,
				   CUM_OBITOS = mod$epi$D$run2)

resultados_cenario_2 <- resultados_cenario_2[-1,]
resultados_cenario_2$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_2$DATA  <- as.Date(resultados_cenario_2$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
resultados_cenario_2 <- rbind(base_select, resultados_cenario_2)
names(resultados_cenario_2) <-c("DATA", "SUSCETIVEIS_CENARIO_2", "CUM_RECUPERADOS_CENARIO_2", "EXPOSTOS_CENARIO_2", "INFECTANTES_CENARIO_2", "CUM_OBITOS_CENARIO_2")

######################################
#Cenário 3 - Rt IC975
######################################
resultados_cenario_3 <- data.frame(SUSCETIVEIS = mod$epi$S$run3,
				   EXPOSTOS = mod$epi$E$run3,
				   INFECTANTES = mod$epi$I$run3,
				   CUM_RECUPERADOS = mod$epi$R$run3,
				   CUM_OBITOS = mod$epi$D$run3)

resultados_cenario_3 <- resultados_cenario_3[-1,]
resultados_cenario_3$DATA <- c((Sys.Date()):(Sys.Date()+projecao-2))
resultados_cenario_3$DATA  <- as.Date(resultados_cenario_3$DATA , origin = "1970-01-01")
base_select <- base %>% dplyr::select(DATA,SUSCETIVEIS, CUM_RECUPERADOS, EXPOSTOS, INFECTANTES, CUM_OBITOS)
resultados_cenario_3 <- rbind(base_select, resultados_cenario_3)
names(resultados_cenario_3) <-c("DATA", "SUSCETIVEIS_CENARIO_3", "CUM_RECUPERADOS_CENARIO_3", "EXPOSTOS_CENARIO_3", "INFECTANTES_CENARIO_3", "CUM_OBITOS_CENARIO_3")

#Unindo as bases de resultados
resultados <- merge(resultados_cenario_1, resultados_cenario_2, by= "DATA", all = T)
resultados <- merge(resultados_cenario_3, resultados, by= "DATA", all = T)


write.csv(resultados, "dados/resultados.csv", row.names = F)
#resultados$DATA <- as.Date(resultados$DATA, origin = "1970-01-01")

#Análise dos resultados
resultados_melt <- resultados %>% dplyr::select(DATA, CUM_OBITOS_CENARIO_3)
resultados_melt <- melt(resultados_melt, id.vars = "DATA")

ggplot(resultados_melt, aes(DATA, value, color = variable))+
	geom_line()+
	theme_bw()





