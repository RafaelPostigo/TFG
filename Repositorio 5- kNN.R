#Abrir matriz_percent (matriz 0,25%)
matriz_percent <- readRDS("matriz_percent.rds")


source("functions.R")
#Abrir las siguientes librerías
library(dplyr)
library(ggplot2)
library(class)
library(modelr)

#Dividir matriz en test(30%) y train(70%)
set.seed(1234)
splitData = resample_partition(matriz_percent, c(test = 0.3, train = 0.7))

lapply(splitData, dim)

#Organizamos dataset test y training
matriz_percent_test = as_tibble(splitData$test)
matriz_percent_train = as_tibble(splitData$train)


# testeamos la K 
ks <- c(3,5,7,9,11,13,15)
acc = sapply(ks, function(k) {
  pred <- knn(train = matriz_percent_train[-c(1)], 
              test = matriz_percent_test[-c(1)], 
              cl = matriz_percent_train$emergencia, 
              k = k)
  actual <- matriz_percent_test$emergencia
  mean(pred == actual)
})

plot(ks, acc, type = "o", pch = 19, lty=2, xlab = "k value", ylab = "accuracy", col = "midnightblue")


#seleccionamos la k= 5
set.seed(1234)
pred <- knn(train = matriz_percent_train[-c(1)], 
            test = matriz_percent_test[-c(1)], 
            cl = matriz_percent_train$emergencia, 
            k = 5)

#Calcular el área bajo la curva
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


#Fijar las prediciones.
matriz_percent_test$pred=pred

#Examinar la matriz confusión.
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)
conf_mat(matriz_percent_test, truth = 'emergencia', estimate = 'pred')

#Calcular precisión, sensibilidad y especificidad.
accuracy(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
sensitivity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
especificity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
