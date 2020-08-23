# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(googlesheets4)

# Bases --------------------------------------------------------------------

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


names(casos)[2] <- "DATA"


#decretos
id_decretos <-  "https://docs.google.com/spreadsheets/d/1MFlRMSpfkdPVUPMYjtpXrOR4LjrxjUdLamV3-geErV0/edit#gid=114661743"
decretos <- read_sheet(id_decretos,"decretos_estaduais",skip = 0,col_names = T) %>% as.data.frame()
#Considerou-se que os estabelecimentos estavam abertos do contágio do primeiro paciente até 14 dias antes do primeiro decreto
decretos_complem <- decretos[integer(),]
decretos_complem[1:14,3:20] <- "aberto"
decretos_complem$PUBLICACAO <-  c((min(casos$DATA)-14):(min(casos$DATA)-1))
decretos_complem$PUBLICACAO <- as.Date(decretos_complem$PUBLICACAO, origin = "1970-01-01")
decretos <- rbind(decretos_complem, decretos) %>% as.data.frame()
decretos$DECRETO <- NULL
library(anchors)
decretos <- replace.value(decretos, names = names(decretos[,-1]), from = "aberto_restricao", to= "aberto", verbose = FALSE)

#Agrupando intervenções que iniciaram e finalizarma no mesmo período
decretos <- decretos %>% rename(ACADEMIA_SHOPPING_AREAS_COMUNS_ESP_PRIVADO = ACADEMIA)
decretos$SHOPPING <- NULL
decretos$AREAS_COMUNS_ESP_PRIVADO <- NULL
decretos <- decretos %>% rename(TRANSPORTE_COLETIVO_RESTAURANTES = RESTAURANTES)
decretos$TRANSPORTE_COLETIVO <- NULL
decretos <- decretos %>% rename(HOTEL_PROF_LIBERAL_CONST_CIVIL_COMERCIO_RUA = PROF_LIBERAL)
decretos$HOTEL <- NULL
decretos$CONST_CIVIL <- NULL
decretos$COMERCIO_RUA <- NULL
decretos <- decretos %>% rename(ESCOLA_TRANSPORTE_TURISTICO_ESTAB_GRANDE_CIRCUL_DESPORT_COMPETICOES = ESTAB_GRANDE_CIRCUL)
decretos$TRANSPORTE_TURISTICO <- NULL
decretos$ESCOLA <- NULL
decretos$DESPORT_COMPETICOES <- NULL
decretos$DELIVERY_ALIMENTOS <- NULL # SEMPRE ABERTO
decretos$PUBLICO_ESSEN <- NULL # SEMPRE ABERTO
decretos$SERV_ESSEN <- NULL # SEMPRE ABERTO



# Analisando melhor lag entre decretos e o número de contaminados ---------
loop_base <- decretos
lags <- list()
for(i in 1:14){
	loop_base$PUBLICACAO <- as.Date(decretos$PUBLICACAO) - i
	#merge
	base <- merge(casos, loop_base, by.x = "DATA", by.y = "PUBLICACAO", all = T)

	
	#Recocasos_truncando datas em que o paciente foi contaminado, mas que ainda não tem resultado de exame
	contaminacao_ao_inicio_sintomas <- 5
	inicio_sintomas_notificacao <- 14 #dados Florianópolis
	notificacao_resultado_exames <- 7
	
	recorte <- (contaminacao_ao_inicio_sintomas + 
		    	inicio_sintomas_notificacao +
		       notificacao_resultado_exames
		    )
	
	base <- base[order(base$DATA),]
	base <- subset(base, base$DATA < (tail(base$DATA,1) - recorte))
	base <- replace.value(base, names = names(decretos[,-1]), from = NA, to= "aberto", verbose = FALSE)

	
	base[,-c(1:2)] <- lapply(base[,-c(1:2)], function(x)as.factor(x))

	
	lags[[i]] <- lm(log(CASOS) ~ ACADEMIA_SHOPPING_AREAS_COMUNS_ESP_PRIVADO+
	   	TRANSPORTE_COLETIVO_RESTAURANTES +
	   	ESCOLA_TRANSPORTE_TURISTICO_ESTAB_GRANDE_CIRCUL_DESPORT_COMPETICOES +
	   	PUBLICO_NAO_ESSEN+
	   	HOTEL_PROF_LIBERAL_CONST_CIVIL_COMERCIO_RUA+
	   	PRAIAS_ESP_PUBLIC
	   	, base[,-1]) %>% AIC()
		 
}


melhor_lag <- do.call(rbind, lags) %>% as.data.frame()
melhor_lag$LAG <- c(1:14)
names(melhor_lag)[1] <- "AIC"



# Regressão utilizando o melhor lag ---------------------------------------

decretos$PUBLICACAO <- as.Date(decretos$PUBLICACAO) - melhor_lag[melhor_lag$AIC == min(melhor_lag$AIC), names(melhor_lag) == "LAG"]
#merge
base <- merge(casos, decretos, by.x = "DATA", by.y = "PUBLICACAO", all = T)


#Recocasos_truncando datas em que o paciente foi contaminado, mas que ainda não tem resultado de exame
contaminacao_ao_inicio_sintomas <- 5
inicio_sintomas_notificacao <- 14 #dados Florianópolis
notificacao_resultado_exames <- 7

recorte <- (contaminacao_ao_inicio_sintomas + 
	    	inicio_sintomas_notificacao +
	       notificacao_resultado_exames
	    )

base <- base[order(base$DATA),]
base <- subset(base, base$DATA < (tail(base$DATA,1) - recorte))
base <- replace.value(base, names = names(decretos[,-1]), from = NA, to= "aberto", verbose = FALSE)


base[,-c(1:2)] <- lapply(base[,-c(1:2)], function(x)as.factor(x))


mod1 <- lm(log(CASOS) ~ ACADEMIA_SHOPPING_AREAS_COMUNS_ESP_PRIVADO+
   	TRANSPORTE_COLETIVO_RESTAURANTES +
   	ESCOLA_TRANSPORTE_TURISTICO_ESTAB_GRANDE_CIRCUL_DESPORT_COMPETICOES +
   	PUBLICO_NAO_ESSEN+
   	HOTEL_PROF_LIBERAL_CONST_CIVIL_COMERCIO_RUA+
   	PRAIAS_ESP_PUBLIC
   	, base[,-1])

summary(mod1)
plot(mod1)
