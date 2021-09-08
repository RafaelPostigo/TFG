#Abrir matriz_percent (matriz 0,25%)
matriz_percent <- readRDS("matriz_percent.rds")

source("functions.R")

#Cargar librerías necesarias.
library(randomForest)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(modelr)

#Dividir matriz en dataset-test(30%) y dataset-train(70%)
set.seed(2021)
splitData <- resample_partition(matriz_percent, c(test = 0.3, train = 0.7))
matriz_percent_test     <- as_tibble(splitData$test)
matriz_percent_train    <- as_tibble(splitData$train)

# Pre-procesado :Random Forest (classification) requiere que la etiqueta sea un factor.
matriz_percent_train <- matriz_percent_train %>% 
  mutate(emergencia = factor(emergencia))

matriz_percent_test <- matriz_percent_test %>% 
  mutate(emergencia = factor(emergencia))

#Crear modelo Random Forest.
emergencia_model <- randomForest(emergencia ~ . , data = matriz_percent_train)

#Calcular la precisión del Random Forest.
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class" )
mean(matriz_percent_test$pred == matriz_percent_test$emergencia)



#Calcular el área bajo la curva.
library(pROC)

make_predictors_numeric <- function(dataset) {
  # esta funcion covierte la etiqueta : charcater -> factor 
  # esta funcion solo sirve para olive_test 
  # si tengo otro dataset, tengo que cambiar los case_when
  dataset %>% mutate(emergencia = case_when(emergencia == "yes" ~ 1,
                                            emergencia == "no" ~ 2),
                     pred = case_when(pred == "yes" ~ 1,
                                      pred == "no" ~ 2)) 
}

matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE) 

#Abrir librerías necesarias.
library(rpart)
library(rpart.plot)

#Configuración predeterminada del modelo.
rpart.plot(emergencia_model)

#Hacer prediciones en el dataset "test".
matriz_percent_test$pred <- predict(emergencia_model,matriz_percent_test, type ="class")

#Examinar la matriz confusión.
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)
conf_mat(matriz_percent_test, truth = 'emergencia', estimate = 'pred')

#Calcular precisión, sensibilidad y especificidad.
accuracy(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
sensitivity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
especificity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')




# creating a forest with lower number of trees, does it result in greater performance on the test dataset ? 
emergencia_model$ntree #by default

emergencia_model <- randomForest(emergencia ~. , data = matriz_percent_train, ntree = 10, importance = TRUE)
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class" )
mean(matriz_percent_test$pred == matriz_percent_test $emergencia)





#Crear modelo Random Forest, fijando parámetro máximo ntree 500 (de 0 a 500, calculando de 50 en 50.)
ntrees = seq(1,500, by = 50)
acc <- sapply(ntrees, function(nt) {
  emergencia_model <- randomForest(emergencia ~. , data = matriz_percent_train , ntree = nt)
  matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class" )
  mean(matriz_percent_test$pred == matriz_percent_test$emergencia)
  
} )

plot(ntrees, acc, type = "o", pch = 19, lty=2, 
     xlab = "number of trees in the forest", 
     ylab = "accuracy", 
     col = "midnightblue")

# si lo repito otra vez (463-469 + 477)el resultado es diferente porque la seleccion de los trees es diferente 
lines(ntrees, acc, type = "o", pch = 19, lty=2, col = 5)


# Monte Carlo simulations/ Lo que hace es repetir paso anterior x veces y hacer la media de todas.
rep = 50
mc_sim = replicate(rep, 
                   acc <- sapply(ntrees, function(nt) {
                     emergencia_model <- randomForest(emergencia ~. , data = matriz_percent_train, ntree = nt)
                     matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class" )
                     mean(matriz_percent_test$pred == matriz_percent_test$emergencia)
                   } )
)

saveRDS(mc_sim, file = "mc_sim.rds")
rownames(mc_sim) = ntrees
head(mc_sim)

rowMeans(mc_sim)
which.max(rowMeans(mc_sim))

plot(names(rowMeans(mc_sim)), as.numeric(rowMeans(mc_sim)), type = "o", pch = 19, lty=2, 
     xlab = "number of trees in the forest", 
     ylab = "accuracy", 
     col = "red")     
#Se elije la mejor opción y procedemos a:


#Calcular la precisión.
emergencia_model <- randomForest(emergencia ~. , data = matriz_percent_train, ntree = 300)
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class" )
mean(matriz_percent_test$pred == matriz_percent_test$emergencia)

#Calcular el área bajo la curva.
matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE) 

#Abrir librerías necesarias.
library(rpart)
library(rpart.plot)

#Configuración predeterminada del modelo.
rpart.plot(emergencia_model)

#Hacer prediciones en el dataset "test".
matriz_percent_test$pred <- predict(emergencia_model,matriz_percent_test, type ="class")

#Examinar la matriz confusión.
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)
conf_mat(matriz_percent_test, truth = 'emergencia', estimate = 'pred')

#Calcular precisión, sensibilidad y especificidad.
accuracy(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
sensitivity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
especificity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')


