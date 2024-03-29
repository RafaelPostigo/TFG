getwd()
setwd("")

#cargamos las dos matrices y vemos si coinciden en orden (sabiendo que los valores de los p�xeles no est�n ordenador num�ricamente)
names(rafael)
names(rafaelNO)
identical(names(rafael), names(rafaelNO))

#Unimos las dos supermatrices en una ultramatriz
superPostigo= rbind(rafael,rafaelNO)
dim(superPostigo)

library(stringr)

#Ordenamos desde la variable 4 (dejando fuera las variables no num�ricas, es decir, fecha, RIL y emergencia)
fotos_ord <- str_sort(names(superPostigo)[4:ncol(superPostigo)], numeric = TRUE)

#Ahora ordenamos los valores de los p�xeles num�ricamente
idx <- match(fotos_ord, names(superPostigo)[4:ncol(superPostigo)]) 

#sumamos las 3 variables no num�ricas extraidas en pasos anteriores.
idx <- idx +3

superPostigo <- superPostigo[,c(1:3,idx)]
superPostigo[1:3,1:9]

#Guardamos la ultramatriz ya ordenada.
saveRDS(superPostigo, file = "superPostigo.rds")
