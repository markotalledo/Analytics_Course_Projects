# Instalamos y llamamos a las librerias necesarias
# IMPORTAMOS E INSTALAMOS LAS LIBRERIAS NECESARIAS PARA EL ESTUDIO
 install.packages('twitteR')
 install.packages('ROAuth')
 install.packages('magrittr')
 install.packages('tm')
 install.packages('SnowballC')
 install.packages('wordcloud')

library(datasets)
library(stats)
library(ggplot2)
library(tidyverse)
library(e1071)
library(twitteR)
library(ROAuth)
library(magrittr)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# LEEMOS LA BASE DE DATOS HEART
wcommerce <- read.csv(file = 'Womens Clothing E-Commerce Reviews.csv',sep = ',')
head(wcommerce,1)


str(wcommerce)
# Tenemos 23486 observaciones


# Filtramos para 3000 filas y solo nos quedamos con la columna de review text para generar la matriz de terminos de reseñas
wcommerce_sample = wcommerce %>% sample_frac(300/nrow(wcommerce))

############################################################## LIMPIEZA DEL TEXTO

## FUNCION PARA ELIMINAR DIRECCIONES URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

# FUNCION PARA REMOVER COSAS DISTINTAS DE PALABRAS - todo excepto alfabetos
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

# PERSONALIZANDO LAS PALABRAS CONECTORAS
myStopwords <- c(setdiff(stopwords('english'), c("buy", "store",'retail','retailer')),"use", "see", "got", "im", "wear",'just','like','look','ordered','looked')



## CONSTRUCCION DE CORPUS
corpus.raw <- wcommerce_sample$Review.Text %>% VectorSource() %>% Corpus()

## LIMPIEZA DEL CORPUS
corpus.cleaned <- corpus.raw %>%
  # Convertir todo a minúsculas
  tm_map(content_transformer(tolower)) %>%
  # Retirar los URL
  tm_map(content_transformer(removeURL)) %>%
  # Remover numeros y puntuación
  tm_map(content_transformer(removeNumPunct)) %>%
  # Retirar a las palabras conectoras
  tm_map(removeWords, myStopwords) %>%
  # Remover los espacios en blanco extras
  tm_map(stripWhitespace)



## COMPLETADO/CORTADO DE PALABRAS A PALABRAS RAIZ
corpus.stemmed <- corpus.cleaned %>% tm_map(stemDocument)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  stripWhitespace(x)
}

corpus.completed <- corpus.stemmed %>%
  lapply(stemCompletion2, dictionary=corpus.cleaned) %>% VectorSource() %>% Corpus()



# Vemos cuales son las Classes de productos con mayor review para poder crear una cloudword a nuestro top 5 productos
wcommerce_sample %>% count(Class.Name, sort = TRUE)



##############################################################  REEMPLAZO DE PALABRAS DOMINADAS

## Conteo/frecuencia de palabras
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) grep(as.character(x), pattern=paste0("\\<",word)) )
  sum(unlist(results))
}
                    
                    
n.dresses <- corpus.cleaned %>% wordFreq("dresses")
n.dress <- corpus.cleaned %>% wordFreq("dress")
n.knits <- corpus.cleaned %>% wordFreq("knits")
n.knit <- corpus.cleaned %>% wordFreq("knit")
n.blouses <- corpus.cleaned %>% wordFreq("blouses")
n.blouse <- corpus.cleaned %>% wordFreq("blouse")
n.sweaters <- corpus.cleaned %>% wordFreq("sweaters")
n.sweater <- corpus.cleaned %>% wordFreq("sweater")
n.pants <- corpus.cleaned %>% wordFreq("pants")
n.pant <- corpus.cleaned %>% wordFreq("pant")
cat(n.dresses, n.dress,n.knits,n.knit,n.blouses,n.blouse,n.sweaters,n.sweater,n.pants,n.pant)



## Reemplazar palabra antigua por nueva
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
corpus.completed <- corpus.completed %>%
  replaceWord("dresses", "dress") %>%
  replaceWord("knits", "knit") %>%
  replaceWord("blouses", "blouse")  %>%
replaceWord("sweaters", "sweater") %>%
replaceWord("pants", "pant")



############################################################## CONSTRUCCION DE LA MATRIZ DE TERMINOS
tdm <- corpus.completed %>%
  TermDocumentMatrix(control = list(wordLengths = c(1, Inf))) %>%
  print




############################################################## ANALISIS EXPLORATORIO DE LA MATRIZ DE TERMINOS
## Identificar dentro de la matriz de terminos los tweets relacionados a "r" "data" "mining"
idx <- which(dimnames(tdm)$Terms %in% c("dress", "knit", "blouse", 'sweater', 'pant'))
tdm[idx, 21:30] %>% as.matrix()




## Visualizar terminos que cumplen con una frecuencia minima
freq.terms <- tdm %>% findFreqTerms(lowfreq = 30) %>% print

##  Construir una tabla de frecuencia de terminos por encima de una frecuencia minima
term.freq <- tdm %>% as.matrix() %>% rowSums()
term.freq <- term.freq %>% subset(term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)
df

## Grafico de frecuencia de terminos
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Palabras") + ylab("Cantidad Reviews") + coord_flip() +
  theme(axis.text=element_text(size=7))


############################################################## WORDCLOUDS
m <- tdm %>% as.matrix
word.freq <- m %>% rowSums() %>% sort(decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)



############################################################## MEDICION DE NIVEL DE RELACION ENTRE TERMINOS
tdm %>% findAssocs("dress", 0.2)
tdm %>% findAssocs("knit", 0.2)
tdm %>% findAssocs("blouse", 0.2)
tdm %>% findAssocs("sweater", 0.2)
tdm %>% findAssocs("pant", 0.2)


############################################################## CLUSTERING

## Clustering Jerárquico
m2 <- tdm %>% removeSparseTerms(sparse = 0.95) %>% as.matrix()
dist.matrix <- m2 %>% scale() %>% dist()
fit <- dist.matrix %>% hclust(method = "ward.D")
plot(fit)
fit %>% rect.hclust(k = 3)
groups <- fit %>% cutree(k = 3)



