# IMPORTAMOS E INSTALAMOS LAS LIBRERIAS NECESARIAS PARA EL ESTUDIO
library(datasets)
library(stats)
library(ggplot2)
library(tidyverse)
library(e1071)
library(discretization)


# LEEMOS LA BASE DE DATOS HEART
heart_data <- read.csv(file = 'heart.csv',sep = ',')
head(heart_data)

# Generamos variables dummies 
# Eliminar Sex y Sex_M
# Eliminar RestingECG y RestingECG_Normal
# Eliminar ExerciseAngina y ExerciseAngina_Y
# Eliminar ST_Slope y ST_Slope_Up
library('fastDummies')
heart_data <- dummy_cols(heart_data, select_columns = 'Sex')
heart_data <- dummy_cols(heart_data, select_columns = 'ChestPainType')
heart_data <- dummy_cols(heart_data, select_columns = 'RestingECG')
heart_data <- dummy_cols(heart_data, select_columns = 'ExerciseAngina') 
heart_data <- dummy_cols(heart_data, select_columns = 'ST_Slope') 
heart_data =  heart_data[, -which(names(heart_data) %in% c("Sex", "Sex_M", "RestingECG", "ChestPainType",
                                                          "RestingECG_Normal","ExerciseAngina",
                                                          "ExerciseAngina_Y", "ST_Slope","ST_Slope_Up"))]


                                                          



# Sin discretizar aplicamos el algoritmo NB
a = naiveBayes(HeartDisease~.,data = heart_data)
pred = predict(a,heart_data[,-7],type = "class")

# Vemos la CM
table(pred,heart_data[,7])

# Observamos el error
error = mean(heart_data[,7] != pred1)
error


# Discretizando
# Metodo de entropia

# Discretizando con el metodo de la entropia 

dheart = mdlp(heart_data)$Disc.data
for (i in 1:17)
    dheart[,i] = as.factor(dheart[,i])
b = naiveBayes(HeartDisease~.,data = dheart)


predb = predict(b,dheart[,7],,type = "class")

table(predb,dheart[,7])


error = mean(predb!=dheart[,7])
error


## Metodo Chi Merge

chidata = chiM(heart_data, alpha = 0.05)$Disc.data
for (i in 1:17)
    chidata[,i] = as.factor(chidata[,i])
b = naiveBayes(HeartDisease~.,data = chidata)


predchi = predict(b,chidata[,-7])


table(predchi,chidata[,7])


error = mean(predchi!=chidata[,7])
error
# Discretizacion con Chi tienen menor error -- mejor modelo - nos quedamos con este a comparacion de los errores de 0.446 de sin discretizaar y entropia



### PREGUNTAA 2


train = read.csv(file = 'train.csv',sep = ',')
test = read.csv(file = 'test.csv',sep = ',')
test_label = read.csv(file = 'test_label.csv',sep = ',')





# Parece un dataset de propension de credito de un banco, asi que nos quedamos con las variables mas determinantes para esta casuistica

# dummy Job: remove job
# dummy marital
# ordinal education
# dummy default de pago
# dummy housing
# dummy loan
# remove day
# remove month
# remove poutcome
# reclasificar y

train =  dummy_cols(train, select_columns = 'job')
train =  dummy_cols(train, select_columns = 'marital')
train =  dummy_cols(train, select_columns = 'default')
train =  dummy_cols(train, select_columns = 'housing')
train =  dummy_cols(train, select_columns = 'loan')
train$education = ifelse(train$education == "unknown", 0, ifelse(train$education == "primary",1,
                                                                ifelse(train$education == "secondary",
                                                                      2,3)))
train$y = ifelse(train$y == "no", 0, 1)

train =  train[, -which(names(train) %in% c("ID", "job", "marital", "default",
                                                          "housing","loan",
                                                          "contact", "day","month",
                                           "campaign", "poutcome","default_no", "housing_no", "loan_no" ))]



# Parece un dataset de propension de credito de un banco, asi que nos quedamos con las variables mas determinantes para esta casuistica

# dummy Job: remove job
# dummy marital
# ordinal education
# dummy default de pago
# dummy housing
# dummy loan
# remove day
# remove month
# remove poutcome
# reclasificar y

train =  dummy_cols(train, select_columns = 'job')
train =  dummy_cols(train, select_columns = 'marital')
train =  dummy_cols(train, select_columns = 'default')
train =  dummy_cols(train, select_columns = 'housing')
train =  dummy_cols(train, select_columns = 'loan')
train$education = ifelse(train$education == "unknown", 0, ifelse(train$education == "primary",1,
                                                                ifelse(train$education == "secondary",
                                                                      2,3)))
train$y = ifelse(train$y == "no", 0, 1)

train =  train[, -which(names(train) %in% c("ID", "job", "marital", "default",
                                                          "housing","loan",
                                                          "contact", "day","month",
                                           "campaign", "poutcome","default_no", "housing_no", "loan_no" ))]


test =  dummy_cols(test, select_columns = 'job')
test =  dummy_cols(test, select_columns = 'marital')
test =  dummy_cols(test, select_columns = 'default')
test =  dummy_cols(test, select_columns = 'housing')
test =  dummy_cols(test, select_columns = 'loan')
test$education = ifelse(test$education == "unknown", 0, ifelse(test$education == "primary",1,
                                                                ifelse(test$education == "secondary",
                                                                      2,3)))

test =  test[, -which(names(test) %in% c("ID", "job", "marital", "default",
                                                          "housing","loan",
                                                          "contact", "day","month",
                                           "campaign", "poutcome","default_no", "housing_no", "loan_no" ))]







# Sin discretizar aplicamos el algoritmo NB
a = naiveBayes(y~.,data = train)

pred = predict(a,test,type = "class")



# Vemos la CM
table(pred,test_label$y)

error = mean(pred!=test_label$y)
error



















































































