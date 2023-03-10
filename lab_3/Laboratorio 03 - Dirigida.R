########################################
### INSTALACI?N Y CARGA DE LIBRER?AS ###
########################################

# Instalamos y cargamos las librer?as requeridas
#install.packages("class")
#install.packages("ggplot2")
library(class)
library(ggplot2)



################################################
### EJEMPLO 1: KNN CON TABLA DE DATOS "IRIS" ###
################################################

# Cargamos la tabla "Iris" y mostramos un resumen de su contenido
data(iris)
summary(iris)
str(iris)

# Visualizamos los primeros registros de la tabla "Iris"
head(iris)

# Previo a la partici?n de nuestra tabla de datos en 2 partes
# Definimos una semilla aleatoria para asegurar la reproducibilidad de los resultados
set.seed(20)

# Definimos el 90% de filas como muestra de entrenamiento 
ran <- sample(1:nrow(iris), 0.9*nrow(iris))
ran

# Definimos la funci?n de normalizaci?n
nor <- function(x) {(x-min(x))/(max(x)-min(x))}

# Aplicamos la funci?n de normalizaci?n en las variables predictoras
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
iris_norm
summary(iris_norm)

# Extraemos la data de entrenamiento
iris_train <- iris_norm[ran,]

# Extraemos la data de prueba
iris_test <- iris_norm[-ran,]

# Extraemos la variable target de la data de entrenamiento
# Se usar? al correr la funci?n KNN
iris_target_category <- iris[ran, 5]

# Extraemos la variable target de la data de prueba
# Se usar? para medir la precisi?n del modelo
iris_test_category <- iris[-ran, 5]

# Corremos el algoritmo KNN
pr <- knn(iris_train, iris_test, cl=iris_target_category, k=13)

# Creamos la matriz de confusi?n
tab <- table(pr, iris_test_category)
tab

# Definimos funci?n "Accuracy" para determinar qu? tan preciso es el modelo con la data de prueba
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))*100}

# Aplicamos la funci?n "Accuracy" en nuestra matriz de confusi?n
accuracy(tab)




####################################################
### EJEMPLO 2: KNN CON TABLA DE DATOS "DIAMONDS" ###
####################################################

# Cargamos la tabla "Diamonds" y la convertimos en una data frame
data(diamonds)
dia <- as.data.frame(diamonds)

# Visualizamos la estructura y los primeros registros
str(dia)
?diamonds

# Previo a la partici?n de nuestra tabla de datos en 2 partes
# Definimos una semilla aleatoria para asegurar la reproducibilidad de los resultados
set.seed(50)

# Definimos el 90% de filas como muestra de entrenamiento 
ran2 <- sample(1:nrow(dia), 0.9*nrow(dia))
ran2

# Aplicamos la funci?n de normalizaci?n en las variables predictoras
dia_norm <- as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))
summary(dia_norm)

# Extraemos la data de entrenamiento
dia_train <- dia_norm[ran2,]

# Extraemos la data de prueba
dia_test <- dia_norm[-ran2,]

# Extraemos la variable target de la data de entrenamiento
# Se usar? al correr la funci?n KNN
dia_target_category <- as.factor(dia[ran2, 2])

# Extraemos la variable target de la data de prueba
# Se usar? para medir la precisi?n del modelo
dia_test_category <- as.factor(dia[-ran2, 2])

# Corremos el algoritmo KNN
pr2 <- knn(dia_train, dia_test, cl=dia_target_category, k=20)

# Creamos la matriz de confusi?n
tab2 <- table(pr2, dia_test_category)
tab2

# Aplicamos la funci?n "Accuracy" en nuestra matriz de confusi?n
accuracy(tab2)



###################################
### ADICIONAL: DEFINIR K ?PTIMO ###
###################################

# Bucle para iterar sobre diferente valores de k (vecinos cercanos)

# Definimos semilla aleatoria
set.seed(50)

# Creamos lista para almacenar resultados de cada iteraci?n
accuracy_list <- 0

# Generar bucle
for (i in 1:20){ 
  knn.pr <-  knn(dia_train, dia_test, cl=dia_target_category, k=i) # Corremos knn
  conf_matrix <- table(knn.pr, dia_test_category)
  accuracy_list[i] = accuracy(conf_matrix) 
  cat(i,'=', accuracy_list[i], '\n') # para mostrar los resultados de cada iteraci?n 
}

# Graficar resultados: Accuracy vs n?mero de vecinos
plot(accuracy_list, xlab="Valor de K", ylab="Accuracy", col=4, type="b") 
points(which.max(accuracy_list), max(accuracy_list), col=2, pch=8, cex=3)

# Mostrar valor de K que arroja la mayor precisi?n (accuracy)
cat('K ?ptimo =', which.max(accuracy_list),'; m?x accuracy =', max(accuracy_list))


