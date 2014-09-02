## función para optimizar los parametro de Holt Winters


calibrar.winters<-function(aprendizaje, testing){
 ## install.packages("urca", dependencies=TRUE)
  library(urca)
  
  inc<- 0.1
  
  error.c<-Inf
  
  alpha.i<- 0.1 ## alpha no puede ser 0. Siempre es mayor q cero
  
  while (alpha.i<=1){
    
    beta.i<- 0
    
    while (beta.i<=1){
      
      gamma.i<-0
      
      while(gamma.i<=1){
        
       ## mod.i<-hw(serie.aprendizaje, alpha=alpha.i, beta= beta.i, gamma=gamma.i)
        mod.i<-HoltWinters(aprendizaje, alpha=alpha.i, beta= beta.i, gamma=gamma.i)
        res.i<-predict(mod.i,n.ahead=2)
        error.i<-abs(res.i[2]-testing)
        
        if(error.i < error.c){
          
          error.c<-error.i
          mod.c<-mod.i
          
        }
        gamma.i<-gamma.i+ inc    
      }
      
      beta.i<- beta.i+ inc
    }
    
    alpha.i<-alpha.i + inc
    
    
  }
  
  
  
  return (mod.c)
  
}
