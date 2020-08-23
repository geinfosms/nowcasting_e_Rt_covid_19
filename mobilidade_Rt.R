# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)




# Bases --------------------------------------------------------------------
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
	geom_smooth(se = F)+
	theme(axis.text.x = element_text(angle = 90))+
	theme_bw()
	


rt <- read_csv("dados/rt.csv")
names(rt) <- c("DATA", "IC_RT", "VALOR_RT")
rt <- subset(rt, rt$IC_RT == "IC975")

# merge -------------------------------------------------------------------
mobilidade$DATA <- as.Date(mobilidade$DATA, format = "%m/%d/%Y")
mobilidade$DATA <- mobilidade$DATA 

loop_base <- mobilidade
#Analisando melhor lag entre mobilidade e Rt
lags <- list()
for(i in 1:14){
	loop_base$DATA <- as.Date(mobilidade$DATA) - i
	rt_mob <- merge(rt, loop_base, by = "DATA", all.x = T)
	rt_mob <- na.omit(rt_mob)

	external <- rt_mob %>% dplyr::select(VAREJO_RECREACAO,
			       MERCADOS_FARMACIAS,
			       PARQUES,
			       ESTACOES_TRANSITO,
			       LOCAIS_TRABALHO,
			       RESIDENCIAS) %>% as.matrix()


	x<-ts(rt_mob$VALOR_RT, frequency=7)
	fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"),allowdrift=TRUE)
	lags[[i]] <- fit$aicc
}


melhor_lag <- do.call(rbind, lags) %>% as.data.frame()
melhor_lag$LAG <- c(1:14)
names(melhor_lag)[1] <- "AICc"

rt$DATA <- as.Date(rt$DATA) - melhor_lag[melhor_lag$AIC == min(melhor_lag$AICc), names(melhor_lag) == "LAG"]
rt_mob <- merge(rt, mobilidade, by = "DATA", all.x = T)
rt_mob <- subset(rt_mob, IC_RT == "IC975")
rt_mob <- na.omit(rt_mob)

external <- rt_mob %>% dplyr::select(VAREJO_RECREACAO,
		       MERCADOS_FARMACIAS,
		       PARQUES,
		       ESTACOES_TRANSITO,
		       LOCAIS_TRABALHO,
		       RESIDENCIAS) %>% as.matrix()


x<-ts(rt_mob$VALOR_RT, frequency=7)
fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"), allowdrift=TRUE)
fit %>% summary()


#suavizando

VAREJO_RECREACAO <- loess(mobilidade$VAREJO_RECREACAO ~ as.numeric(mobilidade$DATA))$fitted
#VAREJO_RECREACAO <- (VAREJO_RECREACAO-min(VAREJO_RECREACAO))/(max(VAREJO_RECREACAO)-min(VAREJO_RECREACAO))

MERCADOS_FARMACIAS <- loess(mobilidade$MERCADOS_FARMACIAS ~ as.numeric(mobilidade$DATA))$fitted
#MERCADOS_FARMACIAS <- (MERCADOS_FARMACIAS-min(MERCADOS_FARMACIAS))/(max(MERCADOS_FARMACIAS)-min(MERCADOS_FARMACIAS))

PARQUES <- loess(mobilidade$PARQUES ~ as.numeric(mobilidade$DATA))$fitted
#PARQUES <- (PARQUES-min(PARQUES))/(max(PARQUES)-min(PARQUES))

ESTACOES_TRANSITO <- loess(mobilidade$ESTACOES_TRANSITO ~ as.numeric(mobilidade$DATA))$fitted
#ESTACOES_TRANSITO <- (ESTACOES_TRANSITO-min(ESTACOES_TRANSITO))/(max(ESTACOES_TRANSITO)-min(ESTACOES_TRANSITO))

LOCAIS_TRABALHO <- loess(mobilidade$LOCAIS_TRABALHO ~ as.numeric(mobilidade$DATA))$fitted
#LOCAIS_TRABALHO <- (LOCAIS_TRABALHO-min(LOCAIS_TRABALHO))/(max(LOCAIS_TRABALHO)-min(LOCAIS_TRABALHO))

RESIDENCIAS <- loess(mobilidade$RESIDENCIAS ~ as.numeric(mobilidade$DATA))$fitted
#RESIDENCIAS <- (RESIDENCIAS-min(RESIDENCIAS))/(max(RESIDENCIAS)-min(RESIDENCIAS))


DATA <- mobilidade$DATA

mobilidade_smooth <- cbind(DATA,
		       VAREJO_RECREACAO,
		       MERCADOS_FARMACIAS,
		       PARQUES,
		       ESTACOES_TRANSITO,
		       LOCAIS_TRABALHO,
		       RESIDENCIAS) %>% as.data.frame()
mobilidade_smooth$DATA <- as.Date(mobilidade_smooth$DATA, origin = "1970-01-01")

VALOR_RT <- loess(rt$VALOR_RT ~ as.numeric(rt$DATA))$fitted
#VALOR_RT <- (VALOR_RT-min(VALOR_RT))/(max(VALOR_RT)-min(VALOR_RT))

rt_smooth <- data.frame(VALOR_RT = VALOR_RT,
		DATA = rt$DATA)

loop_base_smooth <- mobilidade_smooth
#Analisando melhor lag entre mobilidade_smooth e Rt

lags <- list()
for(i in 1:14){
	loop_base_smooth$DATA <- as.Date(mobilidade_smooth$DATA) - i
	rt_mob_smooth <- merge(rt_smooth, loop_base_smooth,  by = "DATA", all.x = T)
	rt_mob_smooth <- na.omit(rt_mob_smooth)
	
	external <- rt_mob_smooth %>% dplyr::select(VAREJO_RECREACAO,
			       MERCADOS_FARMACIAS,
			       PARQUES,
			       ESTACOES_TRANSITO,
			       LOCAIS_TRABALHO,
			       RESIDENCIAS) %>% as.matrix()
	
	
	x<-ts(rt_mob_smooth$VALOR_RT, frequency=7)
	fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"),allowdrift=TRUE)
	lags[[i]] <- fit$aicc 	
}


melhor_lag <- do.call(rbind, lags) %>% as.data.frame()
melhor_lag$LAG <- c(1:14)
names(melhor_lag)[1] <- "AICc"

rt_smooth$DATA <- as.Date(rt_smooth$DATA) - melhor_lag[melhor_lag$AIC == min(melhor_lag$AICc), names(melhor_lag) == "LAG"]
rt_mob_smooth <- merge(rt_smooth, mobilidade_smooth, by = "DATA", all.x = T)
rt_mob_smooth <- na.omit(rt_mob_smooth)

external <- rt_mob_smooth %>% dplyr::select(VAREJO_RECREACAO,
		       MERCADOS_FARMACIAS,
		       PARQUES,
		       ESTACOES_TRANSITO,
		       LOCAIS_TRABALHO,
		       RESIDENCIAS) %>% as.matrix()


x<-ts(rt_mob_smooth$VALOR_RT, frequency=7)
fit<-auto.arima(x,xreg=external, test=c("kpss","adf","pp"), allowdrift=TRUE)
fit %>% summary()


rt_mob_smooth_melt <- melt(rt_mob_smooth, id.vars = "DATA")


	
ggplot(rt_mob_smooth_melt, aes(DATA, value, group = as.factor(variable), color = as.factor(variable)))+
	geom_smooth(se = F)
	
	

mod_null <- lm(VALOR_RT ~ 1, 
   rt_mob_smooth) %>% 
	summary()

mod_1 <- lm(VALOR_RT ~ VAREJO_RECREACAO, 
   rt_mob_smooth) %>% 
	summary()

mod_2 <- lm(VALOR_RT ~ MERCADOS_FARMACIAS, 
   rt_mob_smooth) %>% 
	summary()

mod_3 <- lm(VALOR_RT ~ PARQUES, 
   rt_mob_smooth) %>% 
	summary()

mod_4 <- lm(VALOR_RT ~ ESTACOES_TRANSITO, 
   rt_mob_smooth) %>% 
	summary()

mod_5 <- lm(VALOR_RT ~ LOCAIS_TRABALHO, 
   rt_mob_smooth) %>% 
	summary()

mod_6 <- lm(VALOR_RT ~ RESIDENCIAS, 
   rt_mob_smooth) %>% 
	summary()


mod_full <- lm(VALOR_RT ~ VAREJO_RECREACAO + MERCADOS_FARMACIAS + PARQUES + ESTACOES_TRANSITO + LOCAIS_TRABALHO + RESIDENCIAS, 
   rt_mob_smooth) %>% 
	summary()
