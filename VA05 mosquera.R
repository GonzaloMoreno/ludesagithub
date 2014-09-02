direcccion<-setwd("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Diagramas de Flujo/")

archivo <- read.csv("VA05 Mosquera.csv",header=TRUE, sep=";", row.names=1)
archivo
media<-apply(archivo,2, mean)
## calcula la media para cada hora
media
vector<-c(archivo$X8,archivo$X9,archivo$X10,archivo$X11,archivo$X12,archivo$X13,archivo$X14,archivo$X15,archivo$X16,archivo$X17,archivo$X18,archivo$X19,archivo$X20)
hist(vector, col="blue")
##hist(archivo$X18)
## cuando se grafica se demuestra que el compostamiento se ajusta a una distribución
## exponencial
media2<-mean(vector)
media2
# esta es la media de la llegada de los pedidos al sistema

