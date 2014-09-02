
## url_dir<-"http://cran.ms.unimelb.edu.au/src/contrib/forecast_5.5.tar.gz"
## download.file(arch_url, destfile="./dataset.zip")

## instalando el paquete de Robert Hyndman en versión estable
install.packages('forecast', dependencies = TRUE)
install.packages(c("tseries", "astsa", "TSA", "itsmr", "urca"), dependencies=TRUE)
library(forecast)
library(tseries)
library(stats)
library(astsa)
library(TSA)
library(itsmr)
library(urca)

filas<-nrow(Ventas_actualizado)
fila_inicial<-filas- 7 ## revisar si es 7 u 8
fila_inicial<-fila_inicial+1
filas_resultados<- fila_inicial:filas
year<-as.numeric(format(Ventas_actualizado[filas_resultados[1],1], "%Y"))
month<-as.numeric(format(Ventas_actualizado[filas_resultados[1],1], "%m"))
serie.test<-ts(as.numeric(Ventas_actualizado[filas_resultados,2]), start= c(year, month),frequency=12)



## 1)guardar como data.frame los datos de los ultimos 8 meses

for (i in length(filas_resultados)){
  
        RMSE.min<- Inf

        ## 2) Seleccionar datos desde la fila 1 a la fila i
     
        datos_recortados<-Ventas_actualizado[1:filas_resultados[i],2]
        serie.aprendizaje<-ts(as.numeric(datos_recortados), start=c(2011,1), frequency=12)
        
        
        ##  ANALISIS PRELIMINAR DE LA SERIE DE TIEMPO
        
        ## DESCOMPOSICION DE LA SERIE
        
        desc.serie<- stl(serie.aprendizaje, s.window="periodic")
        ## plot de la serie aprendizaje
        plot(stl(serie.aprendizaje, s.window="periodic"),col="blue", lwd=2)
        ##plot de la serie completa
        plot(stl(ts(Ventas_actualizado[,2], start=c(2011,1),frequency=12), s.window="periodic"), col="blue", lwd=2)
        
        ## GRAFICO DE LA SERIE
        ## ploteo de la serie y de  sus diferencias
        plot(serie.aprendizaje, type="l", col="purple")
        plot(diff(serie.aprendizaje), type="l")
        ## histograma de las difernecias de orden 1
        ## para verificar normalidad de las diferencias
        hist(diff(serie.aprendizaje), prob=T, col="blue", xlim=c(-100000, 100000), )
        lines(density(diff(serie.aprendizaje)), lwd=2)
        media.diff<-mean(diff(serie.aprendizaje))
        desv.diff<-sd(diff(serie.aprendizaje))
        
        ## 3) aplicar técnicas  de pronóstico 
        
          ## Media
          f.media<-meanf(serie.aprendizaje, h=2)
          media.accuracy<-accuracy(f.media$mean[2], serie.test[[i+1]])
  
          ## Naive
  
          f.naive<-naive(serie.aprendizaje, h=2)
          naive.accuracy<-accuracy(f.naive$mean[2], serie.test[[i+1]])
          ## snaive
  
          f.snaive<-snaive(serie.aprendizaje, h=2)
          snaive.accuracy<-accuracy(f.snaive$mean[2], serie.test[[i+1]])
        
          ## drift es el pronóstico basado en el último valor mas el cambiio promedio
        
          f.drift<- rwf(serie.aprendizaje, drift=TRUE, h=2)
          drif.accuracy<-accuracy(f.drift$mean[2], serie.test[[i+1]])
        
          ## Croston
        
          f.croston<- croston(serie.aprendizaje, h=2)
          croston.accuracy<-accuracy(f.croston$mean[2], serie.test[[i+1]])
          ## stlf
        
          f.stlf<-stlf(serie.aprendizaje, h=2)
          stlf.accuracy<-accuracy(f.stlf$mean[2], serie.test[[i+1]])
           plot(f.stlf)
          ## SES Simple Exponential Suavization
        
          f.ses<-ses(serie.aprendizaje, h=2)
          ses.accuracy<-accuracy(f.ses$mean[2], serie.test[[i+1]])
        
          ## holt en tendencia lineal
        
          f.holt<-holt(serie.aprendizaje, h=2)
          holt.accuracy<-accuracy(f.holt$mean[2], serie.test[[i+1]])
        
          ## holt exponential trend
        
          f.holt.exp<-holt(serie.aprendizaje, exponential=TRUE, h=2)
          holt.exp.accuracy<-accuracy(f.holt.exp$mean[2], serie.test[[i+1]])
       
          ## holt Damped trend method
        
          f.holt.damped<-holt(serie.aprendizaje, damped=TRUE, h=2)
          holt.damped.accuracy<-accuracy(f.holt.damped$mean[2], serie.test[[i+1]])
        
          ## holt en Damped Exponential Trend method
        
          f.holt.exp.damped<-holt(serie.aprendizaje, damped=TRUE, exponential=TRUE, h=2)
          holt.exp.damped.accuracy<-accuracy(f.holt.exp.damped$mean[2], serie.test[[i+1]])
        
        
          ## Holt- Winters
         
          source('~/LUDESA DE COLOMBIA/Pronosticos R/calibrar.winters.R')
          f.HW<- calibrar.winters(serie.aprendizaje, serie.test[i+1])
          hw.accuracy<-accuracy(f.HW$fitted[2], serie.test[[i+1]])
          plot(f.HW)
        
          ## f.splinef
        
          f.splinef<-splinef(serie.aprendizaje, h=2)
          splinef.accuracy<-accuracy(f.splinef$mean[2], serie.test[[i+1]])
          plot(f.splinef)
        
          ## thetaf
        
          f.thetaf<-thetaf(serie.aprendizaje, h=2)
          thetaf.accuracy<-accuracy(f.thetaf$mean[2], serie.test[[i+1]])
        
          ## automatic forecast
        
         ## f.forecast<- forecast(serie.aprendizaje, h=2)
        
        
          ## ARima
        
         ## encontrar.arima<-auto.arima(serie.aprendizaje)
          order.arima<-arimaorder(auto.arima(serie.aprendizaje))
          f.arima<- Arima(serie.aprendizaje, order=c(order.arima[1],order.arima[2],order.arima[3]))
          f.arima1<-forecast.Arima(f.arima, h=2)
          plot(f.arima1)
          spec.pgram(serie.aprendizaje, log="no")
          arima.accuracy<-accuracy(f.arima1[[4]][2], serie.test[[i+1]])
        
        
        ## Métodos de suavización exponencial
        ## A=Aditive
        ## Ad= Aditivo amortiguado
        ## N= None
        ## Md=  Multiplicativo amortiguado
        ## ETF(Error, Trend,Seasonal)
        
        ## ETF (A,N,N)= Suavización Exponencial Simple
        
           se.ETS<-ets(serie.aprendizaje, model="ANN")
           fets.SES<-forecast.ets(se.ETS, h=2)
           ets.SES.accuracy<-accuracy(fets.SES$mean[2], serie.test[[i+1]])
        
        
        ## ETF (A,A,N)= Holt´s linear method with additive errors
        
        holt.ETS<-ets(serie.aprendizaje, model="AAN")
        fholt.SES<-forecast.ets(se.ETS, h=2)
        ets.holt.accuracy<-accuracy(fets.SES$mean[2], serie.test[[i+1]])
        
        ## ETF (A,A,A)=   Additive Holt-Winters´method with additive errors
        
        HW.ETS<-ets(serie.aprendizaje, model="AAA")
        FETS.HW<-forecast.ets(HW.ETS, h=2)
        ets.HW.accuracy<-accuracy(FETS.HW$mean[2], serie.test[[i+1]])
        
        
        ## ETF (M,A,M)=   Multiplicative Holt-Winters´method with multiplicative errors
        
        MHW.ETS<-ets(serie.aprendizaje, model="MAM")
        FETS.MHW<-forecast.ets(MHW.ETS, h=2)
        ets.MHW.accuracy<-accuracy(FETS.MHW$mean[2], serie.test[[i+1]])
        
        ## ETF (A,Ad,N)=   Damped trend method with additive errors
        
        AAdN.ETS<-ets(serie.aprendizaje, model="AAN", damped=TRUE)
        FETS.AAdN<-forecast.ets(AAdN.ETS, h=2)
        ets.AAdN.accuracy<-accuracy(FETS.AAdN$mean[2], serie.test[[i+1]])
        
        ## ETS UNSTABLE MODELS
  
        ## ETF (M, M, A)
        
        MMA.ETS<-ets(serie.aprendizaje, model="MMA")
        FETS.MMA<-forecast.ets(MMA.ETS, h=2)
        ets.MMA.accuracy<-accuracy(FETS.MMA$mean[2], serie.test[[i+1]])
        
  
        ## ETF (M, MD, A)
        
        MMdA.ETS<-ets(serie.aprendizaje, model="MMA", damped=TRUE)
        FETS.MMdA<-forecast.ets(MMdA.ETS, h=2)
        ets.MMdA.accuracy<-accuracy(FETS.MMdA$mean[2], serie.test[[i+1]])
        
        ## ETF (Z, Z, Z) Automatic model 
        
        ZZZ.ETS<-ets(serie.aprendizaje, model="ZZZ")
        FETS.ZZZ<-forecast.ets(ZZZ.ETS, h=2)
        ets.ZZZ.accuracy<-accuracy(FETS.ZZZ$mean[2], serie.test[[i+1]])
        
        
  
  ## 4) aplicar medida de precisión y comparar
  ## 5) guardar el modelo, el mejor pronostico de ese modelo y la medida de precisión
  ## 6) crear un datafram que guarde el valor real, el mejor valor pronosticado, la técnica usada y el indicador de precisión
  
  
  
  
  
  
  
}

