library(MASS)

fish_pred.ci_zip<-function(model, newdata,n = 1000, prov = NULL){
  
  ## computing confidence interval for prediction for a gam object with zip distribution
  ## of the response variable
  
  sigma<-vcov(model) ##covariance matrix of the model
  beta<-coef(model)  ##coefficients of the model
  
  draw<-mvrnorm(n, beta, sigma) ##simulations from a multivariate normal
  
  ones.vec<-vector()
  count.vec<-vector()
  
  if(exists("prov")){
    newdata<-left_join(newdata, prov)
  }
  
  
  
  for (i in 1:n){
    
    model$coefficients<-draw[i,]
    
    lam<-exp(predict.gam(model,newdata = newdata)[,1])
    pi<-1-exp(-exp(predict.gam(model,newdata = newdata)[,2]))
    
    ones<- pi * newdata$popres_class
    count<-lam*pi*newdata$popres_class
    

    if(exists("prov")){
      ones<-ones[newdata$coastal_region == 1]
      count<-count[newdata$coastal_region == 1]
    }
    
    
    ones.vec[i]<-sum(ones)
    count.vec[i]<-sum(count)

  }

  out <-cbind(ones.vec,count.vec)
  return(out)
}




