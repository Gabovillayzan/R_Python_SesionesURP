
rm(list=ls())
################################################################################
##### -- Programa de Especialización en Data Science - Nivel I -- ##############
################################################################################
######## Tema : Regresión Logística,Logística Penalizada,Naive Bayes  - KNN ####
######## Autores: Jose Cardenas - Andre Chavez  ################################ 
################################################################################

#### -- 1) Librerias a usar ####

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
library(caret)
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

#### -- 2) Modelo de Regresion Logistica

## Cargar la data


Train=read.csv("data_loan_status_limpia_forR.csv")
head(Train)
str(Train)
Train$Loan_Status<-as.factor(Train$Loan_Status)
## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## supuestos

correlacion <- cor(Train[,2:ncol(Train)], method = "spearman") #seleccionamos menos el target (columna 1)
#calculamos correlacionspearman  y exportamos
write.csv(correlacion,"correla.csv")
#filtramos columnas para quitar las correlacionadas, nos quedamos con las que tienen (0.6)
Train <- Train %>% select(
        # ApplicantIncome,   #se retira
        CoapplicantIncome,
        # LoanAmount,
        Loan_Amount_Term,
        Gender,
        Dependents,
        Self_Employed,
        Credit_History,
        Total_income,
        # log_LoanAmount,
        Amauntxterm,
        Property_Area,
        Edu_Ma,
        Loan_Status
      )

## Particion Muestral

set.seed(123)
training.samples <- Train$Loan_Status %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]

## Modelado

modelo_logistica=glm(Loan_Status~.,data=train.data,family="binomial" )
summary(modelo_logistica)


## indicadores

proba1=predict(modelo_logistica, newdata=test.data,type="response")
AUC1 <- roc(test.data$Loan_Status, proba1)
## calcular el AUC
auc_modelo1=AUC1$auc
## calcular el GINI
gini1 <- 2*(AUC1$auc) -1
# Calcular los valores predichos
PRED <-predict(modelo_logistica,test.data,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")
# sensibilidad para ver que tan bueno es prediciendo los 1
Sensitivity1=as.numeric(tabla$byClass[1])
# Specificity para ver que tan bueno es prediciendo los 0
Specificity1=as.numeric(tabla$byClass[2])
# Precision
Accuracy1=tabla$overall[1]
# Calcular el error de mala clasificaci?n
error1=mean(PRED!=test.data$Loan_Status)


# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1
Specificity1


##otra manera de calcular los indicadores ##########
PRED <-predict(modelo_logistica,test.data,type="response")
PRED=ifelse(PRED<=mean(PRED),0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusion
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")
# sensibilidad para ver que tan bueno es prediciendo los 1
Sensitivity11=as.numeric(tabla$byClass[1])
# Specificity para ver que tan bueno es prediciendo los 0
Specificity11=as.numeric(tabla$byClass[2])
# Precision
Accuracy11=tabla$overall[1]
# Calcular el error de mala clasificaci?n
error11=mean(PRED!=test.data$Loan_Status)


# indicadores
Accuracy11
error11
Sensitivity11
Specificity11
# comparando se obtiene que este segundo metodo es mejor respecto a especificidad, tienes mas 0 que 1 en el target
# por eso se busca mejorar la especificidad

Sensitivity <- data.frame(Sensitivity1,Sensitivity11)
colnames(Sensitivity)<-c('corte_0.5','corte_prom')
Specificity <- data.frame(Specificity1,Specificity11)
colnames(Specificity)<-c('corte_0.5','corte_prom')

#### -- 3) Modelo de Regresion Penalizada

# modelo 2.- Naive Bayes ## Bueno para data desbalaceada

modelo2=naiveBayes(Loan_Status~.,data = train.data)

##probabilidades
proba2<-predict(modelo2, newdata=test.data,type="raw")
proba2=proba2[,2]

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba2>=0.5,1,0)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity2=as.numeric(tabla$byClass[1])
# Specificity para ver que tan bueno es prediciendo los 0
Specificity2=as.numeric(tabla$byClass[2])

# Precision
Accuracy2=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error2=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo2
gini2
Accuracy2
error2
Sensitivity2
Specificity2

# modelo 3.- KNN

# Utilizar libreria para ML, tratamiento de la data
library(mlr)
n=ncol(train.data)
x<-train.data[,1:(n-1)]
y<-train.data$Loan_Status
x[,1:(n-1)]=lapply(x[,1:(n-1)],as.numeric)
x <- scale(x) # standarizando las xi
data.train.2 <- data.frame(x,y)

# Para data Test
x<-test.data[,1:(n-1)]
y<-test.data$Loan_Status
x[,1:(n-1)]=lapply(x[,1:(n-1)],as.numeric)
x <- scale(x) # standarizando las xi
data.test.2 <- data.frame(x,y)

#create a task para la libreria MLR
trainTask <- makeClassifTask(data = data.train.2,target = "y", positive = "1")
testTask <- makeClassifTask(data = data.test.2, target = "y")


# Modelado KNN

set.seed(1234)
knn <- makeLearner("classif.knn",prob = TRUE,k = 10)
detach("package:caret", unload=TRUE)
library(mlr)

qmodel <- train(knn, trainTask)
qpredict <- predict(qmodel, testTask)

response=as.numeric(qpredict$data$response[1:nrow(data.test.2)])
response=ifelse(response==2,1,0)
proba3=response

library(caret)

# curva ROC
AUC3 <- roc(test.data$Loan_Status, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 3*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-response
PRED <- as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity3=as.numeric(tabla$byClass[1])

# Specificity
Specificity3=as.numeric(tabla$byClass[2])

# Precision
Accuracy3=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error3=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo3
gini3
Accuracy3
error3
Sensitivity3
Specificity3



## --Tabla De Resultados ####

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

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD)
rownames(resultado)=c('Logistico',
                      'Naive_Bayes',
                      'KNN'
)
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado
