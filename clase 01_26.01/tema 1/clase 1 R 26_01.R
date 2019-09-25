setwd("D:/GVILLAYZAN/Clase 1 Intro R")

x=3 #test
rm(list = ls())  #borra lo anteriormente guardado en moemoria

library(neuralnet)

data <- read.table("preciovivienda.txt", header = T)

data2 = read.csv("Preciovivienda.txt", header = T, sep = '\t')

library(data.table) # libreria para cargar grandes volumenes de datos
library(sqldf) ## importar libreria para consultar usando SQL

data <- fread() # se usa para grandes volumenes de datos 


head(data,5) # primeros 5 registros de mi data
summary(data) # TABLA RESUMEN DE VARIABLES Y SU COMPORTAMIENTOs

### MANIPULACION DE VARIABLES

data$casa=1  # adicionar o llamar variable
data$Barrio  #llamar al vector barrio

data[1:4,2] ## consultar
data[1:4,2] ## consultar

datossf <- sqldf("select Barrio, count(*) Operaciones from data group by Barrio")  ##usar comandos SQL
View(datossf)
write.csv(datossf,"datossf.csv", row.names=F) #exporta




######################################### EJERCICIOS ##############################################
#se usa bancomark.xlsx, luego todos los graficos de la clase:

rm(list=ls())

##GRAFICOS BEANPLOT
library(beanplot)
library(readxl)
bancomark <- read_excel("bancomark.xlsx")
attach(bancomark)  #deja pegado la variable, para poder invocar directamente las variables de 'datos'
beanplot(edad~estciv,col = "red")

library(vioplot)
vioplot(balance[trabajo=='student'],balance[trabajo=='management'],balance[trabajo=='retired'],
        col="tomato")

##MULTIVARIADOS 
library(lattice)
library(MASS)
histogram(~edad | vivienda, data = bancomark) 

bwplot(~edad | mora, data = bancomark, layout = c(1, 2))  #mas mora en los jovenes 

bwplot(~edad | presultado, data = bancomark, layout = c(1, 4)) 
bwplot(~edad | vivienda, data = bancomark, layout = c(1, 2)) 
bwplot(edad ~ estciv | mora, data = bancomark, layout = c(3, 1)) 

#relacion entre las variables
xyplot(edad ~ balance | vivienda* y, data = bancomark)

#matriz de correlaciones
library(beanplot)
library(readxl)
bancomark <- read_excel("bancomark.xlsx")
attach(bancomark)
M = cor(bancomark[0:5], method="spearman")  #usa correlacion de pearson por defecto, pero se puede especificar: method="spearman"|pearson
corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "ellipse")
corrplot(M, method = "number")
corrplot(M, method = "shade")
corrplot(M, method = "color")
corrplot(M, method = "pie")
corrplot(M, type = "upper")
corrplot(M, type = "lower")
corrplot.mixed(M)
corrplot.mixed(M, lower = "ellipse", upper = "circle")






