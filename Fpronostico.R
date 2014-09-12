
## Función para calcular el mejor modelo predictivo comparando diversos modelos
## y escogiendo el que tiene el menor RMSE


fpronostico <- function(serie.total2, Agencia, year, month){

      ## instalando el paquete de Robert Hyndman en versión estable
      ## install.packages('forecast', dependencies = TRUE)
      ## install.packages(c("tseries", "astsa", "TSA", "itsmr", "urca"), dependencies=TRUE)
         library(forecast)
         library(tseries)
         library(stats)
         library(astsa)
         library(TSA)
         library(itsmr)
         library(urca)


         nombre.Agencia<-as.character(Agencia)
         serie.test<-ts(as.numeric(serie.total2[filas_resultados]), start= c(2014, 1),frequency=12)
         ##Ventasxagencia<-ts(as.numeric(Ventas_agencia[,i]),start=c(2011,1), frequency=12)
         serie.total<-ts(as.numeric(serie.total2), start= c(2011, 1),frequency=12)


         if (month[length(month)]==11){
              mes.pronostico1<-1
  
           }

         

         if (month[length(month)]==12){
        
              mes.pronostico2<-2
  
           }


        ## creando la matriz de resultados

        nombre.col.resultados<-c("mes", "Valor Real", "Valor Estimado X+2", "RMSE", "Modelo Escogido", "Nombre Agencia")
        datos.total<-matrix(ncol=6)
        colnames(datos.total)<-nombre.col.resultados


        ## 1)guardar como data.frame los datos de los ultimos 8 meses

        for (i in 1:length(filas_resultados))

            {
  
                    RMSE.min<- Inf

                     ## 2) Seleccionar datos desde la fila 1 a la fila i
     
                     datos_recortados<-serie.total[1:filas_resultados[i]-1]
                     serie.aprendizaje<-ts(as.numeric(datos_recortados), start=c(2011, 1), frequency=12)
        
        
                     ##  ANALISIS PRELIMINAR DE LA SERIE DE TIEMPO
        
                     ## DESCOMPOSICION DE LA SERIE
        
                     desc.serie<- stl(serie.aprendizaje, s.window="periodic")
                     ## plot de la serie aprendizaje
                     plot(stl(serie.aprendizaje, s.window="periodic"),col="blue", lwd=2)
                     ##plot de la serie completa
                     plot(stl(ts(serie.total, start=c(year, month),frequency=12), s.window="periodic"), col="blue", lwd=2)
        
                     ## GRAFICO DE LA SERIE
                     ## ploteo de la serie y de  sus diferencias
                     plot(serie.aprendizaje, type="l", col="purple")
                     plot(diff(serie.aprendizaje), type="l", col="magenta")
                     ## histograma de las difernecias de orden 1
                     ## para verificar normalidad de las diferencias
                     hist(diff(serie.aprendizaje), prob=T, col="blue", xlim=c(-100000, 100000), )
                     lines(density(diff(serie.aprendizaje)), lwd=2)
                     media.diff<-mean(diff(serie.aprendizaje))
                     desv.diff<-sd(diff(serie.aprendizaje))
        
                 ## 3) aplicar técnicas  de pronóstico 
        
                 ## Media
            
                     f.media<-meanf(serie.aprendizaje, h=2)
                     media.accuracy<-accuracy(f.media$mean[2], serie.test[[i]])
         
                 if (RMSE.min > media.accuracy[2]){
            
                         RMSE.min <- media.accuracy[2]
                         model<-"meanf"
                         pronostico<-meanf(serie.aprendizaje, h=2)
                    
                }
          
  
                 ## Naive
  
                 f.naive<-naive(serie.aprendizaje, h=2)
                 naive.accuracy<-accuracy(f.naive$mean[2], serie.test[[i]])
        
                 if (RMSE.min > naive.accuracy[2]){
          
                         RMSE.min <- naive.accuracy[2]
                         model<-"naive"
                         pronostico<-naive(serie.aprendizaje, h=2)
          
                  }
        
                 ## snaive
  
                 f.snaive<-snaive(serie.aprendizaje, h=2)
                 snaive.accuracy<-accuracy(f.snaive$mean[2], serie.test[[i]])
        
                 if (RMSE.min > snaive.accuracy[2]){
          
                        RMSE.min <- snaive.accuracy[2]
                        model<-"snaive"
                        pronostico<-snaive(serie.aprendizaje, h=2)
          
                  }
        
                 ## drift es el pronóstico basado en el último valor mas el cambio promedio
        
                 f.drift<- rwf(serie.aprendizaje, drift=TRUE, h=2)
                 drift.accuracy<-accuracy(f.drift$mean[2], serie.test[[i]])
        
        
                 if (RMSE.min > drift.accuracy[2]){
          
                      RMSE.min <- drift.accuracy[2]
                      model<-"drift"
                      pronostico<-rwf(serie.aprendizaje, drift=TRUE,  h=2)
          
                 }
        
        
        
                 ## Croston
        
                 f.croston<- croston(serie.aprendizaje, h=2)
                 croston.accuracy<-accuracy(f.croston$mean[2], serie.test[[i]])
        
        
                 if (RMSE.min > croston.accuracy[2]){
          
                       RMSE.min <- croston.accuracy[2]
                       model<-"croston"
                      pronostico<-croston(serie.aprendizaje, h=2)
           
                 }
        
        
        
          ## stlf
        
                 f.stlf<-stlf(serie.aprendizaje, h=2)
                 stlf.accuracy<-accuracy(f.stlf$mean[2], serie.test[[i]])
        
        
                 if (RMSE.min > stlf.accuracy[2]){
          
                     RMSE.min <- stlf.accuracy[2]
                     model<-"stlf"
                     pronostico<-stlf(serie.aprendizaje, h=2)
          
                 }
        
        
                 plot(f.stlf)
                ## SES Simple Exponential Suavization
        
                f.ses<-ses(serie.aprendizaje, h=2)
                ses.accuracy<-accuracy(f.ses$mean[2], serie.test[[i]])
        
               if (RMSE.min > ses.accuracy[2]){
          
                     RMSE.min <- ses.accuracy[2]
                     model<-"ses"
                     pronostico<-ses(serie.aprendizaje, h=2)
          
                }
        
               plot(f.ses)
        
               ## holt en tendencia lineal
        
                    f.holt<-holt(serie.aprendizaje, h=2)
                    holt.accuracy<-accuracy(f.holt$mean[2], serie.test[[i]])
      
                if (RMSE.min > holt.accuracy[2]){
          
                    RMSE.min <- holt.accuracy[2]
                    model<-"holt"
                    pronostico<-holt(serie.aprendizaje, h=2)
          
                 }
        
               ## holt exponential trend
        
                   f.holt.exp<-holt(serie.aprendizaje, exponential=TRUE, h=2)
                   holt.exp.accuracy<-accuracy(f.holt.exp$mean[2], serie.test[[i]])
        
              if (RMSE.min > holt.exp.accuracy[2]){
          
                   RMSE.min <- holt.accuracy[2]
                   model<-"Holt Exponential"
                   pronostico<-holt(serie.aprendizaje, exponential=TRUE, h=2)
          
               }
        
        
       
          ## holt Damped trend method
          
               f.holt.damped<-holt(serie.aprendizaje, damped=TRUE, h=2)
               holt.damped.accuracy<-accuracy(f.holt.damped$mean[2], serie.test[[i]])
        
           if (RMSE.min > holt.damped.accuracy[2]){
          
               RMSE.min <- holt.accuracy[2]
               model<-"Holt Damped Trend"
               pronostico<-holt(serie.aprendizaje,damped=TRUE, h=2)
          
        
            }
        
        
          
            ## holt en Damped Exponential Trend method
        
            f.holt.exp.damped<-holt(serie.aprendizaje, damped=TRUE, exponential=TRUE, h=2)
            holt.exp.damped.accuracy<-accuracy(f.holt.exp.damped$mean[2], serie.test[[i]])
        
            if (RMSE.min > holt.exp.damped.accuracy[2]){
          
                  RMSE.min <- holt.accuracy[2]
                  model<-"Holt Damped exponential Trend"
                  pronostico<-holt(serie.aprendizaje, damped=TRUE, exponential=TRUE,  h=2)
          
          
              }
        
          ## Holt- Winters
         
    ##        source('~/LUDESA DE COLOMBIA/Pronosticos R/calibrar.winters.R')
    ##        f.HW<- calibrar.winters(serie.aprendizaje, serie.test[i+1])
    ##        hw.accuracy<-accuracy(f.HW$fitted[2], serie.test[[i+1]])
    ##        plot(f.HW)
        
          ## f.splinef
        
             f.splinef<-splinef(serie.aprendizaje, h=2)
             splinef.accuracy<-accuracy(f.splinef$mean[2], serie.test[[i]])
             plot(f.splinef)
        
        
             if (RMSE.min > splinef.accuracy[2]){
          
                 RMSE.min <- holt.accuracy[2]
                 model<-"splinef"
                 pronostico<-splinef(serie.aprendizaje,  h=2)
          
          
              }
        
        
        
          ## thetaf
        
              f.thetaf<-thetaf(serie.aprendizaje, h=2)
              thetaf.accuracy<-accuracy(f.thetaf$mean[2], serie.test[[i]])
              if (RMSE.min > splinef.accuracy[2]){
          
                  RMSE.min <- thetaf.accuracy[2]
                  model<-"thetaf"
                  pronostico<-thetaf(serie.aprendizaje,  h=2)
          
          
               }
        

          ## automatic forecast
        
          ## f.forecast<- forecast(serie.aprendizaje, h=2)
        
        
          ## ARima
        
   
          
          ## encontrar.arima<-auto.arima(serie.aprendizaje)
               order.arima<-arimaorder(auto.arima(serie.aprendizaje))
               f.arima<- Arima(serie.aprendizaje, order=c(order.arima[1],order.arima[2],order.arima[3]))
               f.arima1<-forecast.Arima(f.arima, h=2)
               plot(f.arima1)
               spec.pgram(serie.aprendizaje, log="no")
               arima.accuracy<-accuracy(f.arima1[[4]][2], serie.test[[i]])
        
           if (RMSE.min > arima.accuracy[2]){
          
                   RMSE.min <- arima.accuracy[2]
                   model<-"ARIMA"
                   f.arima2<- Arima(serie.aprendizaje, order=c(order.arima[1],order.arima[2],order.arima[3]))
                   pronostico<-forecast.Arima(f.arima2, h=2)
               
               
          
          
             }
        
        
        
             ## Métodos de suavización exponencial
             ## A=Aditive
             ## Ad= Aditivo amortiguado
             ## N= None
             ## Md=  Multiplicativo amortiguado
             ## ETF(Error, Trend,Seasonal)
        
             ## ETF (A,N,N)= Suavización Exponencial Simple
        
                se.ETS<-ets(serie.aprendizaje, model="ANN")
                fets.SES<-forecast.ets(se.ETS, h=2)
                ets.SES.accuracy<-accuracy(fets.SES$mean[2], serie.test[[i]])
        
             if (RMSE.min > ets.SES.accuracy[2]){
          
                     RMSE.min <- ets.SES.accuracy[2]
                     model<-"ETS.SES"
                     pronostico<-fets.SES
          
          
               }
        
        
              ## ETF (A,A,N)= Holt´s linear method with additive errors
        
               holt.ETS<-ets(serie.aprendizaje, model="AAN")
               fholt.SES<-forecast.ets(se.ETS, h=2)
               ets.holt.accuracy<-accuracy(fets.SES$mean[2], serie.test[[i]])
        
              if (RMSE.min > ets.holt.accuracy[2]){
        
                     RMSE.min <- ets.holt.accuracy[2]
                     model<-"ETS.holt.AAN"
                     pronostico<-fholt.SES
                }
        
        
              ## ETF (A,A,A)=   Additive Holt-Winters´method with additive errors
        
              HW.ETS<-ets(serie.aprendizaje, model="AAA")
              FETS.HW<-forecast.ets(HW.ETS, h=2)
              ets.HW.accuracy<-accuracy(FETS.HW$mean[2], serie.test[[i]])
  ## aqui hay error a revisar en la instrucción pronóstico
  
             if (RMSE.min > ets.HW.accuracy[2]){
          
                     RMSE.min <- ets.HW.accuracy[2]
                     model<-"ETS.holt.AAA"
                     ## aqui se genera error
                     pronostico<-FETS.HW
              }
        
        
        
               ## ETF (M,A,M)=   Multiplicative Holt-Winters´method with multiplicative errors
        
              MHW.ETS<-ets(serie.aprendizaje, model="MAM")
              FETS.MHW<-forecast.ets(MHW.ETS, h=2)
              ets.MHW.accuracy<-accuracy(FETS.MHW$mean[2], serie.test[[i]])
        
        
              if (RMSE.min > ets.MHW.accuracy[2]){
          
                       RMSE.min <- ets.MHW.accuracy[2]
                       model<-"ETS.holt.MHW"
                       pronostico<-FETS.MHW
               }
        
        
          ## ETF (A,Ad,N)=   Damped trend method with additive errors
        
              AAdN.ETS<-ets(serie.aprendizaje, model="AAN", damped=TRUE)
              FETS.AAdN<-forecast.ets(AAdN.ETS, h=2)
              ets.AAdN.accuracy<-accuracy(FETS.AAdN$mean[2], serie.test[[i]])
        
            if (RMSE.min > ets.AAdN.accuracy[2]){
          
                RMSE.min <- ets.AAdN.accuracy[2]
                model<-"ETS.holt.AAdN"
                pronostico<-FETS.AAdN
             }
        
        
           ## ETS UNSTABLE MODELS
  
    
  
           ## ETF (Z, Z, Z) Automatic model 
        
                ZZZ.ETS<-ets(serie.aprendizaje, model="ZZZ")
                FETS.ZZZ<-forecast.ets(ZZZ.ETS, h=2)
                ets.ZZZ.accuracy<-accuracy(FETS.ZZZ$mean[2], serie.test[[i]])
        
           if (RMSE.min > ets.ZZZ.accuracy[2]){
          
                RMSE.min <- ets.ZZZ.accuracy[2]
                model<-"ETS.ZZZ"
                pronostico<-FETS.ZZZ
           }
       
    
  ## copiar los datos a una matriz o data frame.
  
  
## actualizamos datos
    
          dato.i<- c(month[i],as.character(serie.test[i]),as.character(pronostico$mean[2]),as.character(RMSE.min),model, as.character(nombre.Agencia))
          datos.total<-rbind(datos.total, dato.i)
  

        
         if (i == (length(filas_resultados)))
       
           {
        
                pronostico.final<-pronostico
                pronostico.final
                ##dato.i=NA
        
           }
  

  i
  }
  
datos.total<-datos.total[-1,]
##write.csv(datos.total, file = "Busqueda mejor modelo global.csv")
##return (datos.total)
}
