getwd()
setwd("C:/Users/Rafael Postigo Luque/Desktop/TFG/Cosas importantes/Matrices pre")
library(stringr)
library(dplyr)
library(tibble)


#sacamos los valres verde.Quitamos la columna 3 porue hay una g
columnverde = str_detect(names(superPostigo[-3]),"g")

columnverde[1:5]

#columnas verdaderas
names(superPostigo[-3])[columnverde]
rafa4=names(superPostigo[-3])[columnverde]


#superPostigo %>% select(rafa4)
rafat= superPostigo %>% select(rafa4)


#sumar todas las columnas de la matriz Verdeeee
sumsG <- colSums(rafat)
saveRDS(sumsG, file = "sumsG.rds")

#dibujamos la gráfica (tarda su tiempo)
plot(sumsG)

#probamos con 55 y vemos que tal
#y nos dará si la suma es mayor o menor que 55 (de las columnas)/which nos indica la posición de los verdaderos
which(sumsG < 55 & sumsG > 40)
extraer= which(sumsG < 55 & sumsG > 40)
length(extraer)

#todas las lineas y columnas menos la que tienen menos de 55
rafat[,-extraer]

#38 pintar de manera grafica columnas vamos a dejar fuera/ 39 transofrmar data-frame en matriz
image(as.matrix(rafat[,extraer]))
as.matrix(rafat[,extraer])

#selección final de las columnas, sería matiz final
matrizverdeok= rafat %>% select(-extraer)
dim(matrizverdeok)
x= superPostigo[1]
y= superPostigo[2]
z= superPostigo[3]
matrizverdeok= matrizverdeok %>% add_column(x,y,z,.before= 1)
saveRDS(matrizverdeok, file = "matrizverdeok.rds")



