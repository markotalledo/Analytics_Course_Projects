########################################
### INSTALACI?N Y CARGA DE LIBRER?AS ###
########################################

# Instalamos y cargamos las librer?as requeridas
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

# Definimos una semilla para los n?meros aleatorios
set.seed(20)

# Corremos el algoritmo k medias (o k-means)
kmeans.result <- kmeans(iris2, 3)
kmeans.result

# Ploteamos los clusters en un diagrama de dispersi?n
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans.result$cluster)

# Agregamos los centroides de cada cluster
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=3)

# Comparamos los resultados de k-medias versus las categor?as reales
table(iris$Species, kmeans.result$cluster)



###############################
### ALGORITMO DE K MEDOIDES ###
###############################

# Corremos el algoritmo k medoides 
pamk.result <- pamk(iris2)

# Mostramos el n?mero de clusters ?ptimo
pamk.result$nc

# Ploteamos los clusters y el gr?fico de silueta
layout(matrix(c(1,2), 1, 2)) # 2 gr?ficos por p?gina
plot(pamk.result$pamobject)

# Un gr?fico por p?gina
layout(matrix(1))

# Comparamos los resultados de k-medoides versus las categor?as reales
table(iris$Species, pamk.result$pamobject$clustering)


## 3 clusters

# Corremos el algoritmo k medoides (forzado con 3 clusters)
pam.result <- pam(iris2, 3) 

# Ploteamos los clusters y el gr?fico de silueta
layout(matrix(c(1,2), 1, 2)) # 2 gr?ficos por p?gina
plot(pam.result)

# Comparamos los resultados de k-medoides versus las categor?as reales
table(iris$Species, pam.result$clustering)



#################################
### CLUSTERIZACI?N JER?RQUICA ###
#################################

# Definimos una semilla para los n?meros aleatorios
set.seed(20)

# Seleccionamos 40 registros de iris para que el gr?fico no est? sobrepoblado
idx <- sample(1:dim(iris)[1], 40)
idx

# Definimos nueva tabla con solo los 40 registros
irisSample <- iris[idx, ]
str(irisSample)

# Eliminamos la variable target ("Species")
irisSample$Species <- NULL
str(irisSample)

# Aplicamos clustering jer?rquico
hc <- hclust(dist(irisSample), method = "ave")
?hclust

# Un gr?fico por p?gina
layout(matrix(1))

# Ploteamos el dendograma
plot(hc, hang = -1, labels = iris$Species[idx])

# Cortamos el dendograma en 3 clusters
rect.hclust(hc, k = 3)

# Identificamos qu? registros pertenecen a qu? cluster
groups <- cutree(hc, k=3)
groups



### ADICIONAL

# ?C?mo hacer gr?fico de la silueta con k-medias?
D <- daisy(iris2)
plot(silhouette(kmeans.result$cluster, D), col=1:3)


