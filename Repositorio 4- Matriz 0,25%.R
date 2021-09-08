#Crgar matriz rafat (matriz con los valores del canal green)
# rafat <- readRDS("rafat.rds")

#suma de las columnas matriz "emergencia" == yes
m1 = rafat[1:40, ]
sum1 = colSums(m1[,-1])

#suma de las columnas matriz no_emergencia == no
m2 = rafat[41:80, ]
sum2 = colSums(m2[,-1])

#Realizar la diferencias entre "emergencia"/no-emergencia"
diffs <- sum1 - sum2

# Coger los valores 0,25% mas extremos de la diferencia entre los valores del canal green de emergencia y no emergencia 
# #extremos 0.25% (0.9975 + 0.0025) : 9 mil columnas 
hist(diffs, main = "emergencia - no_emergencia", xlab = "")
abline(v = c(quantile(diffs, .0025), quantile(diffs, 0.9975)), col="blue", lwd=3, lty=2)


# Seleccion de columnas para matriz final 
mycols <- names(c(diffs[diffs < quantile(diffs, .0025)], diffs[diffs > quantile(diffs, .9975)]))
matriz <- rafat %>% select(emergencia, mycols)
