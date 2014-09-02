
install.packages(c("xlsx","rJava","xlsxjars"))
library(xlsx)                 
library(stringr)
library(sqldf)
library(plyr)
library(zoo)

##sudo apt-get install openjdk-7-*

specdata<-setwd ("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Pronosticos R")
## Readlines, leee todas las filas del archivo

Todas_las_filas = readLines("ZSD_VENTAS_COMPLETO.xls")
## Elimino las filas q no necesito
filtrar_filas = Todas_las_filas[-c(1:8,10)]
## 

Ventas_filtradas = read.table(textConnection(filtrar_filas), sep="\t", header = TRUE, dec=",", stringsAsFactors = FALSE)

##classes <- sapply(Ventas_filtradas, class)
## Aqui no tengo en cuenta los aditivos. Solo lo hago para lubricantes, pero esto lo puedo replicar despues
## Ventas_filtradas = read.table(textConnection(filtrar_filas), sep="\t", header = TRUE, dec=",", colClasses=classes, stringsAsFactors = FALSE)
totales<-Ventas_filtradas[,56]
totales2<- totales
## quito los puntos
totales2<-gsub("\\.", "_", totales2)
## quito las comas y las reemplazo por puntos"."
totales3<-gsub("\\,", ".", totales2)
totales2<-gsub("\\_", "", totales3)
totales3<-as.numeric(totales2)
sum(totales3)


Fechas<-Ventas_filtradas[,3]
Fecha1<-Fechas[1]
Fechas<-gsub("\\.", "", Fechas)
Fechas<-as.character(as.Date(Fechas, "%d%m%Y"))
Fechas<-gsub("\\-", "/", Fechas)

Ventas_filtradas[,3]<- Fechas
Ventas_filtradas[,56]<- totales3



 

##classes[names(classes) %in% c("Total")] <- "numeric"


ZSD_Ventas <- Ventas_filtradas[,-c(1)] 
Galones_ludesa<- as.integer(sum(ZSD_Ventas[,55]))
Galonesxagencia <- sapply(split(ZSD_Ventas[,55],ZSD_Ventas[,11]), sum)
Carpeta_total.agencia<-setwd ("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Pronosticos R/Total Galones Ludesa/")
historico_total<-read.csv("Total Galones Ludesa.csv",  sep=";", stringsAsFactors=FALSE)
## aqui ya tnego el archivo actualizado ocn las ventas del ultimo mes totalizado

Ventas_actualizado<-rbind(historico_total, c(Fechas[1], Galones_ludesa))
Ventas_actualizado[,1]<-as.Date(Ventas_actualizado[,1])



## Aqui actualizo el archivo por agencia


Carpeta_gal_agencia<-setwd ("C:/Users/GMORENO/Documents/LUDESA DE COLOMBIA/Pronosticos R/Total Galones x Agencia/")
total_agencia<-read.csv("Galones x Agencia.csv",  sep=";", stringsAsFactors=FALSE)
Ventas_agencia<-rbind(total_agencia, c(Fechas[1], Galonesxagencia))
Ventas_agencia[,1]<-as.Date(Ventas_agencia[,1])

