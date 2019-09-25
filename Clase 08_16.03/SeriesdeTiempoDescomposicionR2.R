rm(list=ls())

#########################################################################
### -- Programa de Especialización Nivel 1-- ## 
#########################################################################
### Autores: Jose Cardenas - Andre Chavez ## 

#########################################################################

library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)
#library(arima)

# Carga de datos
Yt<-read.delim("datatesisaereo.txt",header=T)
Yt<-ts(Yt,start=c(2000,1),freq=12)  ##le ponemos TS y le damos formato de SeriedeTimepo

# Grafico de la serie
plot(Yt) 
#en este caso no se puede observar CICLOS, porque para eso se necesita mucho mas data, muchos años, al menos 20 años

# Agrupacion de meses
boxplot(Yt ~ cycle(Yt))
cycle(Yt)

## descomposicion
Yt.ts.desc = decompose(Yt,type="multiplicative")
plot(Yt.ts.desc, xlab='Año')

Yt.ts.desc 
Yt.ts.desc$seasonal # ESTACIONALIDAD 
Yt.ts.desc$trend #SOLO TENDENCIA
Yt.ts.desc$random #LA ALEATORIEDAD

#convierto la tendencia en Dataframe para trabajarla
tendencia=data.frame(Yt.ts.desc$trend)

#le quito los valores perdidos (debido al filtro que se aplica para suavizar los datos, por lo genreal promedios moviles)
tendencia$Yt.ts.desc.trend[is.na(tendencia$Yt.ts.desc.trend)]=0

tendencia$x=seq(1:nrow(tendencia)) #creo un vector correlativo
modelo=lm(Yt.ts.desc.trend~x,data=tendencia) #uso un modelo de regresoion para estimar la tendencia
tendencia_estimada=modelo$fitted.values #estimo los valores de la tendencia

#convierto en dataframe la estacionalidad
estacional=data.frame(Yt.ts.desc$seasonal)

# estimacion de la serie

dataf=data.frame(tendencia_estimada,estacional);colnames(dataf)=c('tend_est','estacionalidad')
dataf$Yt_est=dataf$tend_est*dataf$estacionalidad

## Grafico del modelo final

plot(Yt,col="red")
points(dataf$Yt_est,type='l',col="blue")

