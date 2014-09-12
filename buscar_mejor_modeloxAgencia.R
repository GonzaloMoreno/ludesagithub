## calculo de pronóstic por Agencia
## Estamos usando la función Fpronóstico
## y calibrar Winters para encontrar el mejor modelo de pronóstico para cada agencia

install.packages("gdata")
library(gdata)
library(XLConnect)
library(xlsx)


specdata<-setwd ("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Pronosticos R/Total Galones x Agencia")
Ventas_agencia<-read.csv("Galones x Agencia.csv", header=TRUE, sep=",", row.names=NULL)
Ventas_agencia<-Ventas_agencia[,c(2:6,8:13)]
resultadosxagencia<-NULL
resultadosxagencia2<-NULL

filas<-nrow(Ventas_agencia)
fila_inicial<-filas- 7 ## revisar si es 7 u 8
fila_inicial<-fila_inicial
filas_resultados<- fila_inicial:filas
ProperDates <- as.POSIXct(Ventas_agencia[filas_resultados,1], format="%Y-%m-%d")
year<-as.numeric(format(ProperDates[1], "%Y"))
month<-as.numeric(format(ProperDates, "%m"))
Ventas_agencia<-Ventas_agencia[,-c(1,3,4,9)]


for (count in 1:ncol(Ventas_agencia)){
  


  serie.total2<-Ventas_agencia[,count]
  
  source('~/LUDESA DE COLOMBIA/Pronosticos R/Fpronostico.R')
  resultadosxagencia<-fpronostico(serie.total2, colnames(Ventas_agencia)[count], year, month)
  resultadosxagencia2<-rbind(resultadosxagencia,resultadosxagencia2)
 
}

resultadosxagencia2
