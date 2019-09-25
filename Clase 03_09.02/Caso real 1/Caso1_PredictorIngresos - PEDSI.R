rm(list=ls())
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel I -- ########
##########################################################################
######## Tema : Modelos de Regresion Avanzados ###########################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

## EJERCICIO DE APLICACION : PREDICTOR DE INGRESOS ##

library(ggplot2)
library(reshape2)
library(partykit)
library(randomForest)

# Leemos la informacion de clientes #
data<-read.table("df_Ingresos.csv",header=T,
                      sep=",",fill=T)

# Generalmente partimos en train y test (podria ser 70/30 segun lo hablado en clase), 
#y debemos considerar filtros o criterios e exclusion #
sample=sample.int(nrow(data),round(0.3*nrow(data)))
df_training<-data[-sample,] #el 70% entrena
df_testing<-data[sample,]   #el 30% valida

#TECNICA DM1: REGRESION LINEAL #ajuste del modelo
lm_1 <- lm(IngresoReal~MontoPrestamo+Flg_Bancarizado,
           data=df_training)

summary(lm_1)
# Prediccion sobre la datatest del modelo entrenado
df_testing$IngresoPred_lm<-predict(lm_1,df_testing,type="response")

#TECNICA DM2: ARBOL DE DECISION
n=nrow(df_training)
tree_1<-partykit::ctree(IngresoReal~MontoPrestamo+Flg_Bancarizado,
              data=df_training,
              control=ctree_control(mincriterion = .95,minsplit = .1*n,
              minbucket = .05))

df_testing$IngresoPred_tree<-predict(tree_1,df_testing,type="response")

#TECNICA DM3: RANDOM FORESTS 
rf_1 <- randomForest(IngresoReal~MontoPrestamo+Flg_Bancarizado,
                    data=df_training, ntree=50, mtry = 3, importance = TRUE)

df_testing$IngresoPred_rf<-predict(rf_1,df_testing,type="response")


#COMPARACION GRAFICA DE LOS PREDICTORES:

df_testing$X=seq(1:nrow(df_testing))

#rm(df_temp)
df_temp<-melt(df_testing[,c("X","IngresoReal",
                            "IngresoPred_tree","IngresoPred_lm",
                            "IngresoPred_rf"
                            )],
              id="X")
ggplot()+
  geom_density(data=df_temp, aes(value,color=variable, alpha = 0.2))+
  scale_x_continuous(limits = c(500, 15000))

#Error Cuadratico Medio de los Predictores (ECM)
ECM1<-sum((df_testing$IngresoPred_tree-df_testing$IngresoReal)^2)/nrow(df_testing)
ECM2<-sum((df_testing$IngresoPred_lm-df_testing$IngresoReal)^2)/nrow(df_testing)
ECM3<-sum((df_testing$IngresoPred_rf-df_testing$IngresoReal)^2)/nrow(df_testing)

# Comparamos los predictores, el de menor ECM es el más preciso.
ECM1
ECM2
ECM3



  