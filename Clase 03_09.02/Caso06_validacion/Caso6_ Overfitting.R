rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel I -- ########
##########################################################################
######## Tema : Modelos de Regresion Avanzados ###########################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

# OVERFITTING : CASO PRACTICO #        

df_file1<-read.table("df_overfitting.txt",sep=",",head=T)

unique(df_file1$muestra)

# ELECCION DEL MODELO

#ELIGIENDO LA MUESTRA= 1 PARA DESARROLLAR EL MODELO:
df_temp <- df_file1[df_file1$muestra==1,]

#RELACION ENTRE X E Y (GRAFICAMENTE)
windows()
plot(df_temp$x,df_temp$y)

#PROPUESTA MODELO 1: MODELO LINEAL
model1<-lm(y ~ x,df_temp)
summary(model1)
#model1$coefficients

#Y ESTIMADO DEL MODELO 1:
df_temp$Pred1<-fitted(model1)

#Y ESTIMADO DEL MODELO 1 EN LA GRAFICA:
lines(df_temp$x,df_temp$Pred1, col="red")

#PROPUESTA PARA MEJORAR EL MODELO 1: MODELO POLINOMIAL GRADO 5
model2<-lm(y ~ x + I(x^2)+ I(x^3)
           + I(x^4)+ I(x^5),df_temp)

#Y ESTIMADO DEL MODELO 2:
df_temp$Pred2<- fitted(model2)

summary(model2) ## el R cuadrado ajustado es superior al modelo 1

#Y ESTIMADO DEL MODELO 2 EN LA GRAFICA:
windows()
plot(df_temp$x,df_temp$y)
lines(df_temp$x,df_temp$Pred2, col="green")

#QUE MODELO ES MEJOR?
ECM1<-sum((df_temp$Pred1-df_temp$y)^2)/nrow(df_temp)
ECM2<-sum((df_temp$Pred2-df_temp$y)^2)/nrow(df_temp)

ECM1
ECM2 # es menor, modelo 2 es mejor

# ELEGIMOS EL MODELO2 : VALIDEMOSLO EN LA MUESTRA 0

#ELIGIENDO LA MUESTRA= 0 PARA VALIDAR EL MODELO:
df_temp <- df_file1[df_file1$muestra==0,]

#CALCULANDO EL PREDICTOR CON LOS COEFICIENTES DEL MODELO ELEGIDO (MODEL 2)
df_temp$Pred3<- predict.lm(model1,df_temp,type = "response")
df_temp$Pred4<- predict.lm(model2,df_temp,type = "response")

#VALIDANDO EL MARGEN DE ERROR
ECM3<-sum((df_temp$Pred3-df_temp$y)^2)/nrow(df_temp)
ECM4<-sum((df_temp$Pred4-df_temp$y)^2)/nrow(df_temp)

ECM3
ECM4
#CONCLUSIONES
# el modelo 2 estaba sobre ajustado... por eso al cambiar la data en el test, se cayo. el modelo habia memorizado la data.