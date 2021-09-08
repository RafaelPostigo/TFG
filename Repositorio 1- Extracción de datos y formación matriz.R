library(jpeg)
library(tidyr)
library(tibble)
library(dplyr)
library(readr)

dim(img)
img <- readJPEG("P1100763.jpg")

#Asegurar todas las fotografías tienen mismas dimensiones
img_r = img[1:nrow(img),1:ncol(img),1]
img_g = img[1:nrow(img),1:ncol(img),2]
img_b = img[1:nrow(img),1:ncol(img),3]

#Hacer dataframe vacio
mydf = data.frame()

# Extraer los pixel y ponerlos en fila
#Rojo
foo= append(t(img_r),0) #añadimos un cero que en siguiente paso quitaremos
foo = head(foo, -1)     # quitamos el 0
tmp = data.frame(canal = paste0("r",1:length(foo)), val = foo)
mydf = rbind(mydf, tmp) #Unimos datos en el dataframe

#Verde (= que en el canal rojo)
foo = append(t(img_g),0)
foo = head(foo, -1) 
tmp = data.frame(canal = paste0("g",1:length(foo)), val = foo)
mydf = rbind(mydf, tmp) #unimos datos con el R

#Azul (= que en el canal rojo)
foo = append(t(img_b),0)
foo = head(foo, -1) 
tmp = data.frame(canal = paste0("b",1:length(foo)), val = foo)
mydf = rbind(mydf, tmp)

#Hacer dataframe final y se añade etiquetas: fecha, RIL, emergencia (YES O NO dependiendo de si hay emergencia)
#CUANDO ACABEMOS CON LA IMAGEN HAY QUE CAMBIAR DE NOMBRE A ESTE DATAFRAME.
rafael1 = cbind(fecha = "16SEP", RIL = "54", emergencia = "no", spread(mydf, canal, val))


dim(rafael1)

#Guardamos la primera fotografía
saveRDS(rafaelNO, file = "rafaelNO.rds")


#Unimos supervector de imagen a las del resto 
rafaelNO = rbind(rafaelNO, rafael1)
saveRDS(rafaelNO, file = "rafaelNO.rds")
dim(rafaelNO)
#Habrá que repetir este script tantas veces como fotografías haya para agregarlas a la matriz-emergencia
#En este caso se hizo 2 matrices, una con las fotografías con "emergencia" y otra con "no emergencia" porque se relentizaba por la cantidad de datos.
readRDS("rafael.rds")
