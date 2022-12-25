# LAB 3 - PARTE CALIFICADA:
### MARKO ANDRE TALLEDO HERRERA (20162271)

# IMPORTAMOS E INSTALAMOS LAS LIBRERIAS NECESARIAS PARA EL ESTUDIO
library(datasets)
library(stats)
library(ggplot2)
library(tidyverse)

#Leemos los datos
bank_data <- read.csv(file = 'bank-additional-full.csv',sep = ';')
head(bank_data)

# Vemos que variables son con las que tenemos que quedarnos
str(bank_data)

# Nos quedamos con las variables que nos ayudan a realizar el estudio
bank_data$target = ifelse(bank_data$y == 'no', 0,1)
bank_data$nr.employed = as.character(bank_data$nr.employed)
bank_data$campaign = as.character(bank_data$campaign)
bank_data = bank_data[, !sapply(bank_data, is.character)]

# Quitamos las variables (por que esta muy relacionada con previous) y emp.var.rate (por que es una variable que esta relacionada con el empleado no con el cliente)
bank_data <- bank_data[, -which(names(bank_data) %in% c("pdays", "emp.var.rate"))]

# Convertimos nuestra variable target en factor
bank_data$target = as.factor(bank_data$target)


# Vemos la estructura de nuestro dataset final
str(bank_data)

# Comprobamos si existen datos nulos para imputar
sum(is.na(bank_data))

# Generamos un sample del 90% del total de filas de nuestro dataset
set.seed(20162271)
ran = sample(1:nrow(bank_data),0.9*nrow(bank_data))

# Creamos una funcion para normalizar
nor = function(x) { (x - min(x))/(max(x)-min(x))}

# Corremos la normalizacion de las 6 primeras columnas del dataset por que son nuestras variables predictoras
bank_data_norm = as.data.frame(lapply(bank_data[,c(1,2,3,4,5,6)],nor))
summary(bank_data_norm)


# Extraemos el Training Set
bank_train = bank_data_norm[ran,]

# Extraemos test set
bank_test = bank_data_norm[-ran,]

# extraemos la 5ta columna que usaremos como argumento 'cl'
bank_target_category = bank_data[ran,7]

# Extraemos la 5ta columna para medir el accuracy
bank_test_category = bank_data[-ran,7]



####################### KNN = 5 #######################
library(class)
set.seed(20162271)
# Corremos la funcion KNN para 5
pr = knn(bank_train,bank_test,cl = bank_target_category, k = 5)
# knn(x predictoras, nuevas a predecir, y train, knn)

# Creamos la matriz de confusion
tab = table(pr, bank_test_category)
tab

# Creamos una funcion de accuracy
accuracy = function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)


## ACC = 90.6045156591406



####################### KNN = 5 #######################

library(class)
set.seed(20162271)
# Corremos la funcion KNN para 5
pr = knn(bank_train,bank_test,cl = bank_target_category, k = 9)
# knn(x predictoras, nuevas a predecir, y train, knn)


# Creamos la matriz de confusion
tab = table(pr, bank_test_category)
tab

# Creamos la matriz de confusion
tab = table(pr, bank_test_category)
tab


## ACC = 90.9686817188638



####################### ENCONTRAMOS EL K OPTIMO #######################

# Definimos semilla aleatoria
set.seed(20162271)

# Creamos lista para almacenar resultados de cada iteraci�n
accuracy_list <- 0
# Generar bucle
for (i in 1:20){ 
  knn.pr <-  knn(bank_train, bank_test, cl=bank_target_category, k=i) # Corremos knn
  conf_matrix <- table(knn.pr, bank_test_category)
  accuracy_list[i] = accuracy(conf_matrix) 
  cat(i,'=', accuracy_list[i], '\n') # para mostrar los resultados de cada iteraci�n 
}

# Graficar resultados: Accuracy vs n�mero de vecinos
plot(accuracy_list, xlab="Valor de K", ylab="Accuracy", col=4, type="b") 
points(which.max(accuracy_list), max(accuracy_list), col=2, pch=8, cex=3)


# Mostrar valor de K que arroja la mayor precision (accuracy)
cat('K optimo =', which.max(accuracy_list),'; max accuracy =', max(accuracy_list))


### K optimo = 14 ; max accuracy = 91.13863