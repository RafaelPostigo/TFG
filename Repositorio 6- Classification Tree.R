#Abrir matriz_percent (matriz 0,25%)
matriz_percent <- readRDS("matriz_percent.rds")

source("functions.R")

#Abrir las siguientes librerías necesarias
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(modelr)



##Dividir matriz en dataset-test(30%) y dataset-train(70%)
set.seed(2021)
splitData <- resample_partition(matriz_percent, c(test = 0.3, train = 0.7))
matriz_percent_test     <- as_tibble(splitData$test)
matriz_percent_train    <- as_tibble(splitData$train)

#Pre-procesado: transformar la etiqueta en factor pues es necesario para hacer Classification Tree
matriz_percent_train <- matriz_percent_train %>% 
  mutate(emergencia = factor(emergencia))

matriz_percent_test <- matriz_percent_test %>% 
  mutate(emergencia = factor(emergencia))


#Crear el árbol usando los valores de la matriz
emergencia_model <- rpart(emergencia ~ ., data = matriz_percent_train , method = "class", control = rpart.control(cp = 0))


#Hacer predicciones sobre los datos "test"
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test , type = "class")

#Examinar la matriz de confusión
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)

#Calcular la precisión de dataset test
mean(matriz_percent_test$pred == matriz_percent_test$emergencia)



#Calcular el área bajo la curva 
library(pROC)

# 'multiclass.roc' or 'roc' requiere que la etiqueta del "predictor" sea numérica.

make_predictors_numeric <- function(dataset) {
  # Convertirovierte la etiqueta : charcater -> factor 
  dataset %>% mutate(emergencia = case_when(emergencia == "yes" ~ 1,
                                            emergencia == "no" ~ 2),
                     pred = case_when(pred == "yes" ~ 1,
                                      pred == "no" ~ 2)) 
}

matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE) 
#auc(multiclass.roc(kk$Region, kk$pred, percent = FALSE))



library(rpart)
library(rpart.plot)

#Configuración predeterminada del modelo.
rpart.plot(emergencia_model)

#Hacer prediciones en el dataset "test". 
matriz_percent_test$pred <- predict(emergencia_model,matriz_percent_test, type ="class")

##Examinar la matriz confusión.
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)
conf_mat(matriz_percent_test, truth = 'emergencia', estimate = 'pred')

##Calcular precisión, sensibilidad y especificidad.
accuracy(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
sensitivity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
especificity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')


#Modelo Pre-pruning (pre-poda):
#Buscar el "mejor" nivel (usando valores proxs(por arriba/abajo). a la depth que tiene el arbol)
depth = c(2:10)
acc <- sapply(depth, function(d) {
  emergencia_model <- rpart(emergencia ~ . , matriz_percent_train, cp = 0, maxdepth = d)
  matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class")
  mean(matriz_percent_test$pred  == matriz_percent_test$emergencia)
  
} )

plot(depth, acc, type = "o", pch = 19, lty=2, xlab = "depth", ylab = "accuracy", col = "midnightblue")

#Calcular la precisión para obtener el mejor número de niveles del árbol.
emergencia_model <- rpart(emergencia ~ . , matriz_percent_train, cp = 0, maxdepth = 5)
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class")
mean(matriz_percent_test$pred  == matriz_percent_test$emergencia)

#Calcular el área bajo la curva para el "mejor" nivel.
matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE)

#Configuración predeterminada del modelo.
rpart.plot(emergencia_model)

#Hacer prediciones en el dataset "test".
matriz_percent_test$pred <- predict(emergencia_model,matriz_percent_test, type ="class")

#Examinar la matriz de confusión.
table(predicted = matriz_percent_test$pred, actual = matriz_percent_test$emergencia)
conf_mat(matriz_percent_test, truth = 'emergencia', estimate = 'pred')

#Calcular precisión, sensibilidad y especificidad.
accuracy(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
sensitivity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')
especificity(matriz_percent_test, truth = 'emergencia', estimate = 'pred')


# 2.Cambiar el maxdepth por una división mínima de 150 

emergencia_model <- rpart(emergencia ~ ., data = matriz_percent_train, method = "class", control = rpart.control(cp = 0, minsplit = 150 ))

#Hacer una predicción de clase en el dataset test
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test , type = "class")

# Compute the accuracy of the larger tree
mean(matriz_percent_test$pred  == matriz_percent_test$emergencia)


#Buscar el "mejor" numeros de observaciones por nodo.
msplit = seq(20,100, by = 5)
acc <- sapply(msplit, function(ms) {
  emergencia_model <- rpart(emergencia ~ ., data = matriz_percent_train, method = "class", control = rpart.control(cp = 0, minsplit = ms))
  matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class")
  mean(matriz_percent_test$pred  == matriz_percent_test$emergencia)
  
} )

plot(msplit, acc, type = "o", pch = 19, lty=2, xlab = "observations in each node", ylab = "accuracy", col = "midnightblue")



#Mejor observacion por nodo: precisión.
emergencia_model <- rpart(emergencia ~ ., data = matriz_percent_train, method = "class", control = rpart.control(cp = 0, minsplit = 20))
matriz_percent_test$pred <- predict(emergencia_model, matriz_percent_test, type = "class")
mean(matriz_percent_test$pred  == matriz_percent_test$emergencia)

#Mejor observacion por nodo : AUC
matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE)

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

# POST-PRUNING METHOD: How does the accuracy change?
#grow a large and complex tree then prune it to be smaller and more efficient later on.
#construct a visualization of the tree's performance versus complexity, and use this information to prune the tree to an appropriate level.

#Hacer creccer el árbol completo
# Establecer cp = 0 en rpart.control() para evitar la poda previa.
set.seed(1234)
emergencia_model <- rpart(emergencia ~ . , matriz_percent_train, method = "class", control = rpart.control(cp = 0))

# Examinar la complejidad 
printcp(emergencia_model)
plotcp(emergencia_model)

#Podar el árbol
emergencia_model_pruned <- prune(emergencia_model, cp = 0.022 )


#Calcular la precisión del árbol podado.
matriz_percent_test$pred <- predict(emergencia_model , matriz_percent_test, type = "class")
mean(matriz_percent_test$pred == matriz_percent_test$emergencia)

matriz_percent_test$pred <- predict(emergencia_model_pruned , matriz_percent_test, type = "class")
mean(matriz_percent_test$pred == matriz_percent_test$emergencia)


#Calcular AUC (Área bajo la curva)
matriz_percent_test4roc = make_predictors_numeric(matriz_percent_test)
roc(matriz_percent_test4roc$emergencia, matriz_percent_test4roc$pred, percent = TRUE) 

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


