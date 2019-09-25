######################  EJERCICIO DIABETES, APLICAR ARBOLES Y REGRESION  #######################
rm(list=ls())

################### 1. EJERCICIO DONDE APLICAMOS LOS 3 ARBOLES ESTUDIADOS (CART, C50, CHAID) ##################

#### -- 1) Librerias a usar ####

library("readxl")
library("ggplot2")
library(gee)
library(readxl)
library(leaps)
library(dplyr)
library(caret)
library(pROC)
library(party)
library(C50)
library(rpart)
library(e1071)

#### -- 2) Modelo de Arboles de Decision

## Cargar la data

Train=read.csv("PimaIndiansDiabetes.csv")
Train$diabetes = ifelse(Train$diabetes == "pos", 1,0) #convertimos a numeros el target o variable dependiente
Train$diabetes <- as.factor(Train$diabetes)
str(Train)
## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## supuestos

cor(Train[,1:8]) 

## Particion Muestral

set.seed(123)
training.samples <- Train$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]


## Modelos de Arboles 


########################################### modelo 1.- Arbol CHAID

modelo1<- ctree(diabetes~.,data = train.data, 
               controls=ctree_control(mincriterion=0.95))

##probabilidades
proba1=sapply(predict(modelo1, newdata=test.data,type="prob"),'[[',1)

# curva ROC	
AUC1 <- roc(test.data$diabetes, proba1) 
auc_modelo1=AUC1$auc

# Gini
gini1 <- 2*(AUC1$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba1 <= 0.5 ,0,1)
PRED <- as.factor(PRED)

# str(PRED)  para vreificar los niveles de ambos para la matriz de confusion:
# str(test.data$diabetes)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")

# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])
# Especificidad
Specificity1=as.numeric(tabla$byClass[2])

# Precision
Accuracy1=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error1=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1
Specificity1

###########################################  modelo 2.- Arbol CART 

arbol.completo <- rpart(diabetes~.,data = train.data,method="class",cp=0, minbucket=0)
xerr <- arbol.completo$cptable[,"xerror"] ## error de la validacion cruzada
minxerr <- which.min(xerr)
mincp <- arbol.completo$cptable[minxerr, "CP"]

modelo2 <- prune(arbol.completo,cp=mincp)

##probabilidades
proba2=predict(modelo2, newdata=test.data,type="prob")[,2]

# curva ROC
AUC2 <- roc(test.data$diabetes, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo2, newdata=test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")

# sensibilidad
Sensitivity2=as.numeric(tabla$byClass[1])
# Especificidad
Specificity2=as.numeric(tabla$byClass[2])

# Precision
Accuracy2=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error2=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo2
gini2
Accuracy2
error2
Sensitivity2
Specificity2

########################################### modelo 3.- Arbol c5.0

train.data$diabetes= as.factor(train.data$diabetes)
modelo3 <- C5.0(diabetes~.,data = train.data,trials = 55,rules= TRUE,tree=FALSE,winnow=FALSE)

##probabilidades
proba3=predict(modelo3, newdata=test.data,type="prob")[,2]

# curva ROC
AUC3 <- roc(test.data$diabetes, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 2*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo3, newdata=test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")

# sensibilidad
Sensitivity3=as.numeric(tabla$byClass[1])
# Especificidad
Specificity3=as.numeric(tabla$byClass[2])

# Precision
Accuracy3=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error3=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo3
gini3
Accuracy3
error3
Sensitivity3
Specificity3


############## --Tabla De Resultados ###############

AUC=rbind(auc_modelo1,
          auc_modelo2,
          auc_modelo3
)
GINI=rbind(gini1,
           gini2,
           gini3
)
Accuracy=rbind(Accuracy1,
               Accuracy2,
               Accuracy3
)

ERROR= rbind(error1,
             error2,
             error3
)
SENSIBILIDAD=rbind(Sensitivity1,
                   Sensitivity2,
                   Sensitivity3
)
ESPECIFICIDAD=rbind(Specificity1,
                    Specificity2,
                    Specificity3
)

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD,ESPECIFICIDAD)
rownames(resultado)=c('CHAID',
                      'CART',
                      'C50'
)
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado


##################################### ##################################### ##################################### 
##################################### 2. EJERCICIO DONDE APLICAMOS REGRESION  ################################
rm(list=ls())

library("readxl")
library("ggplot2")
library(glmnet)
library(gee)
library(readxl)
library(leaps)
library(dplyr)
library(caret)
library(pROC)
library(DMwR)
library(sqldf)
library(ggvis)
library(party)
library(Boruta)
library(pROC)
library(randomForest)
library(e1071)
library(glmnet)
library(mboost)
library(adabag)
library(xgboost)
library(ROCR)
library(C50)
library(mlr)
library(gmodels)
library(gplots)
library(DMwR)
library(rminer)
library(class)


Train=read.csv("PimaIndiansDiabetes.csv")
Train$diabetes = ifelse(Train$diabetes == "pos", 1,0) #convertimos a numeros el target o variable dependiente
Train$diabetes <- as.factor(Train$diabetes)
str(Train)




sample_n(Train, 3)

## supuestos
correlacion <- cor(Train[,1:ncol(Train)-1],method = "spearman") 
# calculando la correlacion de spearman para las x
write.csv(correlacion,"correlacionEjercicioDiabetes.csv")

# Es importante el analisis de colinealidad, entre las variables independientes (x)
# si hay 2 variables correlacionadas, entonces se elimina la que tenga menos asociacion con Y (target)
# siempre se busca eliminar la correlacion entre las variables independientes (X) y buscar o mantener 
#   las relaciones fuertes entre las X y el target (Y).

#colnames(Train)
Train <- Train %>% select(
        pregnant,
        glucose,
        pressure,
        triceps,
        #insulin, ##para remover segun analisis (XLS)
        #mass, ##para remover segun analisis (XLS)
        pedigree,
        #age, ##para remover segun analisis (XLS)
        diabetes
        )
#falta analizar como se relaciona cada una de esas X correlacionadas con Y, para ver cual se elimina del modelo.

summary(correlacion)

## categorizando las variables categoricas mediante factor
# Train$diabetes <- as.factor(Train$diabetes)
# Train$Gender <- as.factor(Train$Gender)
# Train$Dependents <- as.factor(Train$Dependents)
# Train$Credit_History <- as.factor(Train$Credit_History)
# Train$Self_Employed <- as.factor(Train$Self_Employed)
# Train$Property_Area <- as.factor(Train$Property_Area)
# Train$Edu_Ma <- as.factor(Train$Edu_Ma)

## Particion Muestral

set.seed(123)
training.samples <- Train$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]

## Modelado

modelo_logistica=glm(diabetes~.,data=train.data,family="binomial" )
summary(modelo_logistica)


## indicadores

proba1=predict(modelo_logistica, newdata=test.data,type="response")
AUC1 <- roc(test.data$diabetes, proba1)
## calcular el AUC
auc_modelo1=AUC1$auc
## calcular el GINI
gini1 <- 2*(AUC1$auc) -1
# Calcular los valores predichos
PRED <-predict(modelo_logistica,test.data,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")
# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])
#Specificity
Specificity1=as.numeric(tabla$byClass[2])
# Precision
Accuracy1=tabla$overall[1]
# Calcular el error de mala clasificaci?n
error1=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1
Specificity1
