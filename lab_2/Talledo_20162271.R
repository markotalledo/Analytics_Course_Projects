# LAB 2 - PARTE CALIFICADA:
### MARKO ANDRE TALLEDO HERRERA (20162271)

# IMPORTAMOS E INSTALAMOS LAS LIBRERIAS NECESARIAS PARA EL ESTUDIO
install.packages("fpc")
library(datasets)
library("stats")
library(stats)
library(fpc)
library(cluster)

#Leemos los datos
wine_data <- read.csv(file = 'wine-clustering.csv')
wine_data2 = wine_data

# Ojeamos el dataset
str(wine_data)

# Todas las variables son numericas
# Las variables pueden servir para realizar una reduccion PCA para quedarnos con componentes principales
# Como sabemos de anterioridad que existen 3 viñedos distintos, trataremos de aproximar la clusterización para identificar a los 3 segmentos de vino
# nro de clusters = 3
head(wine_data2)


# K means
# Definimos una semilla
set.seed(20162271)

# Corremos kmeans para segmentar a los datos de los viñedos
kmeans.result <- kmeans(wine_data2, 3)
kmeans.result


### Vemos las combinaciones de las variables que tienen las variables mas distintas entre los clusters resultantes.
### Elegimos las variables: "Proline", "Color_Intensity" ya que se visualiza mejor la diferencia entre clusters
### between_SS / total_SS =  85.1 % -- BUEN INDICADOR DE CLUSTERING (ACCURACY)

# Ploteamos los clusters en un diagrama de dispersi�n
plot(wine_data2[c("Proline", "Ash_Alcanity")], col=kmeans.result$cluster)
# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Proline", "Ash_Alcanity")], col=1:3, pch=8, cex=3)


# Ploteamos los clusters en un diagrama de dispersi�n
plot(wine_data2[c("Proline", "Color_Intensity")], col=kmeans.result$cluster)
# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Proline", "Color_Intensity")], col=1:3, pch=8, cex=3)


# Ploteamos los clusters en un diagrama de dispersi�n
plot(wine_data2[c("Proline", "Color_Intensity")], col=kmeans.result$cluster)
# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Proline", "Color_Intensity")], col=1:3, pch=8, cex=3)


# Ploteamos los clusters en un diagrama de dispersi�n
plot(wine_data2[c("Proline", "Flavanoids")], col=kmeans.result$cluster)
# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Proline", "Flavanoids")], col=1:3, pch=8, cex=3)

# Comprobamos nuevamente que el average width se aproxima a 1: Correcta eleccion de clusters
D <- daisy(wine_data)
plot(silhouette(kmeans.result$cluster, D), col=1:3)


# K mediods

# Ahora empleamos el algoritmo k medoides 
pamk.result <- pamk(wine_data2)

# Encontramos el nro de clusters optimo: Segun PAMK, tenemos 2 clusters como optimos
pamk.result$nc

# Ploteamos los clusters y el grafico de silueta
layout(matrix(c(1,2), 1, 2)) # vemos los graficos
plot(pamk.result$pamobject)

# Si bien tenemos un buen width de silueta: 0.65, visualmente no tenemos considerable distancia entre clusters segun el grafico de sus componentes.

# Corremos el algoritmo k medoides (forzado con 3 clusters, debido a que realmente tenemos 3 tipos de viñedo)
pam.result <- pam(wine_data2, 3) 
# Ploteamos los clusters y grafico de silueta
layout(matrix(c(1,2), 1, 2))
plot(pam.result)

# Ahora vemos que el width disminuye y existe un cluster visualmente más diferenciado, sin embargo se debe a la estructura de los componentes a que los clusters se traslapen unos a otros y no exista una distancia considerable. Ello, no quiere decir que sea una mala clusterizacion, pues el indicador width de cada cluster se aproxima a 1. 

# K means se queda como mejor tecnica de clusterizacion


# Jerarquico

# Para tener una variable categorica
kmeans.result <- kmeans(wine_data2, 3)
kmeans.result

wine_data2$cluster = kmeans.result$cluster

# Elegimos un tamaño de muestra aproximadamente representativo para la data
dim(wine_data2)[1]/3

# Definimos una semilla para los n�meros aleatorios
set.seed(20162271)

# Seleccionamos la tercera parte de la data
idx <- sample(1:dim(wine_data2)[1],60 )
idx

# Definimos nueva tabla con solo los 60 registros
winesample <- wine_data2[idx, ]
str(winesample)

# Aplicamos clustering jer�rquico
hc <- hclust(dist(winesample), method = "ave")

# Ploteamos el dendograma
plot(hc, hang = -1, labels = winesample$cluster[idx])



# Ploteamos el dendograma
plot(hc, hang = -1, labels = winesample$cluster[idx])

# Cortamos el dendograma en 3 clusters
rect.hclust(hc, k = 3)

# Por dendograma tambien nos quedan 2 big clusters