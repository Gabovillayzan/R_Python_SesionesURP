
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
library(corrplot)


#### -- 2) Modelo de Regresion Logistica

## Cargar la data

Train=read.csv("data_loan_status_limpia.csv")

## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## supuestos
 
M = cor(Train[,c(2,3,4,5,10,11,12)], method = "spearman") #variables continuas, solo algunas columnas
corrplot.mixed(M, lower = "number", upper = "pie") # se busca las variables correlacionadas y se sacan :

Train <- Train %>% select(CoapplicantIncome,Loan_Amount_Term,Total_income,Amauntxterm, Gender,Dependents,Self_Employed, Credit_History, Property_Area,
                    Edu_Ma, Loan_Status) #se retiraron las variables correlacionadas "ApplicantIncome", "LoanAmount" y "log_LoanAmount"

M = cor(Train, method = "spearman")
corrplot.mixed(M, lower = "number", upper = "pie") # volvemos validar la correlacion


#categorizar las variables
Train[,5:ncol(Train)] <- lapply(Train[,5:ncol(Train)], as.factor) # categorizo las variables de la 5 hasta la final como CATEGORICAS para que no sean reconocidas como discretas
str(Train)

## Particion Muestral
set.seed(123)
training.samples <- Train$Loan_Status %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]


## Modelado

modelo_logistica=glm(Loan_Status~.,data=train.data,family="binomial" ) # se coloca la y~. que significa contra todas las demas.
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
PRED=ifelse(PRED<=0.5,0,1) #analizar punto de corte
PRED=as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")
# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])
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

# modelo 2.- Naive Bayes

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


# modelo 3.- KNN

# Utilizar libreria para ML, tratamiento de la data
library(mlr)
n=ncol(train.data)
numericos = train.data[,1:4] # solo las variables cuantitativas
data.train.2 <- numericos
 #numeo de columas 
data.train.2[,2:n]=lapply(data.train.2[,2:n],scale)

data.test.2=test.data
data.test.2[,2:n]=lapply(data.test.2[,2:n],scale)

#create a task
trainTask <- makeClassifTask(data = data.train.2,target = "Loan_Status", positive = "1")
testTask <- makeClassifTask(data = data.test.2, target = "Loan_Status")

# Modelado KNN

set.seed(1234)
knn <- makeLearner("classif.knn",prob = TRUE,k = 10)

qmodel <- train(knn, trainTask)
qpredict <- predict(qmodel, testTask)

response=as.numeric(qpredict$data$response[1:nrow(data.test.2)])
response=ifelse(response==2,1,0)
proba3=response

# curva ROC
AUC3 <- roc(test.data$Loan_Status, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 3*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-response

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity3=as.numeric(tabla$byClass[1])

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
