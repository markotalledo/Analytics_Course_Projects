########################################
### INSTALACIÓN Y CARGA DE LIBRERÍAS ###
########################################

# Instalamos y cargamos las librerías requeridas
#install.packages("fpc")
#install.packages("cluster")
library(stats)
library(fpc)
library(cluster)



#############################
### ALGORITMO DE K MEDIAS ###
#############################

# Cargamos la tabla "Iris" y mostramos un resumen de su contenido
data(iris)
summary(iris)
str(iris)

# Nombramos a la tabla como "iris2"
iris2 <- iris

# Eliminamos la variable target "Species"
iris2$Species <- NULL
summary(iris2)

# Definimos una semilla para los números aleatorios
set.seed(20)

# Corremos el algoritmo k medias (o k-means)
kmeans.result <- kmeans(iris2, 3)
kmeans.result

# Ploteamos los clusters en un diagrama de dispersión
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans.result$cluster)

# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=3)

# Comparamos los resultados de k-medias versus las categorías reales
table(iris$Species, kmeans.result$cluster)



###############################
### ALGORITMO DE K MEDOIDES ###
###############################

# Corremos el algoritmo k medoides 
pamk.result <- pamk(iris2)

# Mostramos el número de clusters óptimo
pamk.result$nc

# Ploteamos los clusters y el gráfico de silueta
layout(matrix(c(1,2), 1, 2)) # 2 gráficos por página
plot(pamk.result$pamobject)

# Un gráfico por página
layout(matrix(1))

# Comparamos los resultados de k-medoides versus las categorías reales
table(iris$Species, pamk.result$pamobject$clustering)


## 3 clusters

# Corremos el algoritmo k medoides (forzado con 3 clusters)
pam.result <- pam(iris2, 3) 

# Ploteamos los clusters y el gráfico de silueta
layout(matrix(c(1,2), 1, 2)) # 2 gráficos por página
plot(pam.result)

# Comparamos los resultados de k-medoides versus las categorías reales
table(iris$Species, pam.result$clustering)



#################################
### CLUSTERIZACIÓN JERÁRQUICA ###
#################################

# Definimos una semilla para los números aleatorios
set.seed(20)

# Seleccionamos 40 registros de iris para que el gráfico no esté sobrepoblado
idx <- sample(1:dim(iris)[1], 40)
idx

# Definimos nueva tabla con solo los 40 registros
irisSample <- iris[idx, ]
str(irisSample)

# Eliminamos la variable target ("Species")
irisSample$Species <- NULL
str(irisSample)

# Aplicamos clustering jerárquico
hc <- hclust(dist(irisSample), method = "ave")
?hclust

# Un gráfico por página
layout(matrix(1))

# Ploteamos el dendograma
plot(hc, hang = -1, labels = iris$Species[idx])

# Cortamos el dendograma en 3 clusters
rect.hclust(hc, k = 3)

# Identificamos qué registros pertenecen a qué cluster
groups <- cutree(hc, k=3)
groups



### ADICIONAL

# ¿Cómo hacer gráfico de la silueta con k-medias?
D <- daisy(iris2)
plot(silhouette(kmeans.result$cluster, D), col=1:3)


