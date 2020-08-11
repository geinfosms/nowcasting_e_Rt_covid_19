# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(forecast)


# Nowcasting --------------------------------------------------------------------
#Utiliza-se os dados de casos confirmados do estado de SC 
#Disponível em: http://dados.sc.gov.br/dataset/casos-19-dados-anonimizados-de-casos-confirmados/resource/76d6dfe8-7fe9-45c1-95f4-cab971803d49
casos <- read_delim("base/boavista_covid_dados_abertos.csv", 
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
casos <- subset(casos, casos$DT_CONTAMINACAO <
			       	(tail(casos$DT_CONTAMINACAO,1) - recorte))


#realizando as projeções do período excluído, para ajustar os artefatos
casos_proj <- forecast(auto.arima(casos$CASOS),
		       h=((Sys.Date()-1)-max(as.Date(casos$DT_CONTAMINACAO))))$mean[1:((Sys.Date()-1)-max(as.Date(casos$DT_CONTAMINACAO)))] %>%
	as.data.frame()

casos_proj <- data.frame(MEDIANA_CASOS = casos_proj,
			 DT_CONTAMINACAO = c(max(as.Date(casos$DT_CONTAMINACAO)+1):(Sys.Date()-1)))

names(casos_proj) <- c("CASOS", "DT_CONTAMINACAO")
casos_proj$DT_CONTAMINACAO <- as.Date(casos_proj$DT_CONTAMINACAO, origin = "1970-01-01")

casos <- rbind(casos, casos_proj) %>% as.data.frame()
ggplot(casos, aes(DT_CONTAMINACAO, CASOS, group = 1))+
	geom_line()
ggplot(casos, aes(DT_CONTAMINACAO, CASOS, group = 1))+
	geom_smooth()



