# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(googlesheets4)

# Bases --------------------------------------------------------------------
#Mobilidade
mobilidade <- read_csv("dados/mobilidade_santa_catarina.csv")

mobilidade <- mobilidade %>% dplyr::select(date, 
					   retail_and_recreation_percent_change_from_baseline,
					   grocery_and_pharmacy_percent_change_from_baseline,
					   parks_percent_change_from_baseline,
					   transit_stations_percent_change_from_baseline,
					   workplaces_percent_change_from_baseline,
					   residential_percent_change_from_baseline)

names(mobilidade) <- c("DATA", 
		       "VAREJO_RECREACAO",
		       "MERCADOS_FARMACIAS",
		       "PARQUES",
		       "ESTACOES_TRANSITO",
		       "LOCAIS_TRABALHO",
		       "RESIDENCIAS")


mobilidade_melt <- melt(mobilidade,id.vars = "DATA")
mobilidade_melt$DATA <- as.Date(mobilidade_melt$DATA, format = "%m/%d/%Y")
ggplot(mobilidade_melt, aes(DATA, value, group = as.factor(variable), color = as.factor(variable)))+
	geom_line(se = F)+
	theme(axis.text.x = element_text(angle = 90))+
	theme_bw()
	


#Utiliza-se os dados de casos confirmados do estado de SC 
#Disponível em: http://dados.sc.gov.br/dataset/casos-19-dados-anonimizados-de-casos-confirmados/resource/76d6dfe8-7fe9-45c1-95f4-cab971803d49
casos <- read_delim("dados/boavista_covid_dados_abertos.csv", 
					   ";", escape_double = FALSE, trim_ws = TRUE)

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
	geom_line()

#Recocasos_truncando datas em que o paciente foi contaminado, mas que ainda não tem resultado de exame
contaminacao_ao_inicio_sintomas <- 5
inicio_sintomas_notificacao <- 14 #dados Florianópolis
notificacao_resultado_exames <- 7


recocasos_trunce <- (contaminacao_ao_inicio_sintomas + 
	    	inicio_sintomas_notificacao +
	       notificacao_resultado_exames
	    )

casos <- casos[order(casos$DT_CONTAMINACAO),]
casos_trunc <- subset(casos, casos$DT_CONTAMINACAO <
			       	(tail(casos$DT_CONTAMINACAO,1) - recocasos_trunce))

ggplot(casos_trunc, aes(DT_CONTAMINACAO, CASOS, group = 1))+
	geom_line()

names(casos_trunc)[2] <- "DATA"
# merge -------------------------------------------------------------------
mobilidade$DATA <- as.Date(mobilidade$DATA, format = "%m/%d/%Y")
mobilidade$DATA <- mobilidade$DATA 

loop_base <- mobilidade
#Analisando melhor lag entre mobilidade e o número de contaminados
lags <- list()
for(i in 1:14){
	loop_base$DATA <- as.Date(mobilidade$DATA) - i
	casos_trunc_mob <- merge(casos_trunc, loop_base, by = "DATA", all.x = T)
	casos_trunc_mob <- na.omit(casos_trunc_mob)
	external <- casos_trunc_mob %>% dplyr::select(VAREJO_RECREACAO,
			       MERCADOS_FARMACIAS,
			       PARQUES,
			       ESTACOES_TRANSITO,
			       LOCAIS_TRABALHO,
			       RESIDENCIAS) %>% as.matrix()


	x<-ts(casos_trunc_mob$CASOS, frequency=7)
	fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"),allowdrift=TRUE)
	lags[[i]] <- fit$aicc
}


melhor_lag <- do.call(rbind, lags) %>% as.data.frame()
melhor_lag$LAG <- c(1:14)
names(melhor_lag)[1] <- "AICc"

casos_trunc$DATA <- as.Date(casos_trunc$DATA) - melhor_lag[melhor_lag$AIC == min(melhor_lag$AICc), names(melhor_lag) == "LAG"]
casos_trunc_mob <- merge(casos_trunc, mobilidade, by = "DATA", all.x = T)
casos_trunc_mob <- na.omit(casos_trunc_mob)

external <- casos_trunc_mob %>% dplyr::select(VAREJO_RECREACAO,
		       MERCADOS_FARMACIAS,
		       PARQUES,
		       ESTACOES_TRANSITO,
		       LOCAIS_TRABALHO,
		       RESIDENCIAS) %>% as.matrix()


x<-ts(casos_trunc_mob$CASOS, frequency=7)
fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"), allowdrift=TRUE)
fit %>% summary()


acf(casos_trunc_mob$CASOS)
diff(casos_trunc_mob$CASOS,lag = 50)
library(urca)
ur <-ur.df(y=casos_trunc_mob$CASOS,lags=4,type="trend",selectlags="BIC")
ur@testreg
plot(ur)
