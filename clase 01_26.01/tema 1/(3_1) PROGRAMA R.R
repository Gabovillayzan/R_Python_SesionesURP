#DIAGRAMAS BEANPLOT
rm(list=ls())
library(beanplot)
datos=read.table("PrecioVivienda.txt",header=T)
attach(datos)  #deja pegado la variable, para poder invocar directamente las variables de 'datos'
beanplot(Precio~Barrio,col = "green")

library(vioplot)
vioplot(Precio[Barrio=='Este'],Precio[Barrio=='Norte'],Precio[Barrio=='Oeste'],
col="tomato")

### multivariada

# El paquete lattice es muy ´util para describir gr´aficamente datos multivariantes.
# La idea consiste en que el gr´afico est´a formado por un cierto n´umero de paneles. Normalmente cada
# uno de ellos corresponde a alguno de los valores de una variable que condiciona. Es decir, un gr´afico
# diferente para cada nivel del factor utilizado como condici´on. Las funciones se escriben con la notaci´on
# de la f´ormula del modelo. En los gr´aficos univariantes como los histogramas, la variable respuesta, a la
# izquierda, se deja vac´ia.

library(lattice)
library(MASS)
histogram(~CW | sp, data = crabs) # carga la data de cangrejos (usar ?crabs para consultar la ayuda). #histograma que cruza tamaño de caparazon x color o tipo de cangrejo
bwplot(~CW | sp, data = crabs, layout = c(1, 2)) #grafico de cajas. tamaño de caparazon x color de cangrejo

bwplot(sex ~ CW | sp, data = crabs, layout = c(1, 2)) ## Gr´aficos de caja para el sexo seg´un especie

## Tambi´en disponemos de diagramas de dispersi´on. Con la funci´on xyplot en lugar de plot. En este caso
##se necesitan dos variables.

# Como se puede ver en la figura 5, el resultado de esta instrucci´on es un gr´afico con cuatro paneles donde
# podemos estudiar la relaci´on entre dos variables seg´un dos factores.
xyplot(CL ~ CW | sp * sex, data = crabs)

# En algunos casos es preciso modificar la estructura de los datos para poder realizar algunos gr´aficos.
matriz <- data.matrix(crabs[, -c(1, 2, 3)]) #le resta la columna 1,2 y 3  #objeto matriz es mas simple, no tiene nombre de columnas
vect <- as.vector(t(matriz))
nombres <- factor(rep(names(crabs[, -c(1, 2, 3)]), 200))
especie <- rep(crabs$sp, each = 5)  #cada 5 observaciones repite la especie
cangrejos <- data.frame(caract = nombres, val = vect, esp = especie)
cangrejos[1:10, ]
matriz[1:2, ]

##Ahora, la siguiente instrucci´on dibuja el gr´afico de la figura 6.
#diseño divide el dispositivo en tantos filas y columnas como los hay en la estera de la matriz, 
#con las columnas-anchos y los fila-alturas especificadas en los respectivos argumentos.

#Gr´aficos de caja de cada variables, seg´un especie
bwplot(esp ~ val | caract, data = cangrejos, layout = c(1, 5))

#MATRICES DE CORRELACIÓN 
library(corrplot)
datos=read.table("PrecioVivienda.txt",header=T)
attach(datos)
M = cor(datos[,2:6], method="spearman")  #usa correlacion de pearson por defecto, pero se puede especificar: method="spearman"|pearson
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

#GRÁFICOS SPLOM # es muy pesado y no trabaja con grandes volumenes de datos
library(cwhmisc)
datos=read.table("PrecioVivienda.txt",header=T)
attach(datos)
SplomT(datos[,2:6])
SplomT(datos[,2:6],mainL="",hist="d",cex.diag=0.6,hist.col="green")

#GRÁFICOS PARA GRANDES VOLÚMENES DE DATOS
library(IDPmisc)
iplot(Precio,Piescuad)
iplot(Precio,Piescuad,pixs=4)

ipairs(datos[,2:6],pixs=2)

# library(vcd)
# data("HairEyeColor")
# # Agregado sobre sexo:
# x = margin.table(HairEyeColor, c(1, 2))
# ## Ordinario assocplot:
# windows()
# assoc(x)
# #sombreo basado en los residuales (de independencia)

require(stats)
mosaicplot(Titanic, main = "Sobrevivencia en el Titanic", color = TRUE)
#Fórmula de interface para datos tabulados:
mosaicplot(~ Sex + Age + Survived, data = Titanic, color = TRUE)

#GRÁFICOS DE ESTRELLAS
require(grDevices)
stars(mtcars[, 1:7], key.loc = c(14, 2),
      main = "Tendencia en Motores de Autos : stars(*, full = F)", full = FALSE)
#Diagrama de segmentos
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Tendencia en Motores de Autos", draw.segments = TRUE)

# #COORDENADAS PARALELAS
# library(MASS)
# parcoord(state.x77[, c(7, 4, 6, 2, 5, 3)])
# 
# ir = rbind(iris3[,,1], iris3[,,2], iris3[,,3])
# parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)

#CARAS DE CHERNOFF
library(aplpack)
data(longley)
faces(longley[1:9,],face.type=0)
faces(longley[1:9,],face.type=1)

plot(longley[1:16,2:3],bty="n")
a=faces(longley[1:16,],plot=FALSE)
plot.faces(a,longley[1:16,2],longley[1:16,3],width=35,height=30)

a=faces(rbind(1:3,5:3,3:5,5:7),plot.faces=FALSE)
plot(0:5,0:5,type="n")
plot(a,x.pos=1:4,y.pos=1:4,1.5,0.7)
#durante la temporada de navidad
faces(face.type=2)

#GRÁFICO SUMMARY
library(aplpack)
plotsummary(cars)
plotsummary(cars, types=c("ecdf", "density", "boxplot"),
y.sizes = c(1,1,1), design ="stripes")


## Not run:
daten<-iris[,2:3]
slider.bootstrap.lm.plot(daten)

## Not run:
slider.hist(log(islands))

## Not run:
slider.lowess.plot(cars)

## Not run:
slider.split.plot.ts(as.vector(sunspots)[1:100])

## Not run:
slider.stem.leaf(islands)

## Not run:
slider.zoom.plot.ts(co2,2)

# xyz<-matrix(rnorm(300),100,3)
# # now start: spin3R(xyz)
# 
# stem.leaf(co2)
# stem.leaf.backback(co2[1:120],co2[121:240])
# stem.leaf.backback(co2[1:120],co2[121:240], back.to.back = FALSE)




