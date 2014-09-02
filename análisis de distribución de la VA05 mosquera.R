direcccion<-setwd("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Diagramas de Flujo/")

archivo <- read.csv("VA05 Mosquera.csv",header=TRUE, sep=";", rownames=1)
media<-apply(archivo,2, mean)
media
vector<-c(archivo$X8,archivo$X9,archivo$X10,archivo$X11,archivo$X12,archivo$X13,archivo$X14,archivo$X15,archivo$X16,archivo$X17,archivo$X18,archivo$X19,archivo$X20)
hist(vector)