# LAB 4 - PARTE CALIFICADA:
### MARKO ANDRE TALLEDO HERRERA (20162271)

# IMPORTAMOS E INSTALAMOS LAS LIBRERIAS NECESARIAS PARA EL ESTUDIO
library(datasets)
library(stats)
library(ggplot2)
library(tidyverse)
library(e1071)

# Leemos los datos
card_data = read.table("creditcard.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
head(card_data,10)

# SVM - LINEAL
set.seed(20162271)
# Quitamos la columna Time ya que parece comportarse como categorica 
credit_data = card_data[,2:31]
reduccion = sample(1:nrow(credit_data), size=round(nrow(credit_data)*0.01))
credit_data = credit_data[muestra,]

# Factorizamos el Class y nos quedamos con 2 variables para el modelo lineal
credit_data$Class = as.factor(credit_data$Class)
credit_data = credit_data[,c("V1","V2","Class")]

# Creamos una funcion para normalizar
nor = function(x) { (x - min(x))/(max(x)-min(x))}

# Corremos la normalizacion de las 6 primeras columnas del dataset por que son nuestras variables predictoras
credit = as.data.frame(lapply(credit_data[,c(1,2)],nor))
credit_data = cbind (credit, credit_data$Class)
colnames(credit_data) <- c("V1","V2","Class")



# Separamos en train y test el dataset
muestra = sample(1:nrow(credit_data), size=round(nrow(credit_data)*0.75))
credit_train = credit_data[muestra,]
credit_test = credit_data[-muestra,]

dim(credit_train)
dim(credit_test)



# Escalamos el modelo debido a que Amount presenta valores con mayor magnitud en comparación a las demás variables
svm_lineal <- svm(Class~., data=credit_train, kernel="linear", cost = 0.01, scale = FALSE)

prediccion <- predict(svm_lineal,credit_test)
plot(svm_lineal,credit_train)



# Declaramos la funcion
make.grid = function(x, n=75){grange = apply(x,2,range)  
V1 = seq(from=grange[1,1], to = grange[2,1], length=n)   
V2 = seq(from=grange[1,2], to = grange[2,2], length=n)   
expand.grid(V1=V1, V2=V2)}
# Aplico la funcion
xgrid = make.grid(credit_train)
xgrid[1:20,]

# Puntos de soporte
ygrid = predict(svm_lineal, xgrid) 
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2) 
points(credit_train[,1], col = credit_train[,2] + 3, pch = 19) 
points(credit_train[svm_lineal$index,], pch = 5, cex = 2)

beta = drop(t(svm_lineal$coefs)%*%credit_train[,1:2][svm_lineal$index,])
beta0 = svm_lineal$rho


plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(credit_train, col = y + 3, pch = 19)
points(credit_train[svm_lineal$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)




# 2

# Leemos los datos
card_data = read.table("creditcard.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
head(card_data,10)

credit_data=card_data[,2:31]

# Escalamos los datos ya que la columna Amount presenta datos muy altos en comparacion a la otras
fit = svm(Class ~ ., data = credit_data, scale = TRUE, kernel = "radial", cost = 1)

# Usamos el xgrid utilizado en la pregunta 1
xgrid
ygrid = predict(fit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(dat, col = 4, pch = 19)

func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(dat, col = 4, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)









