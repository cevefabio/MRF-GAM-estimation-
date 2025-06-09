

#### MODELS PREDICTION ###

## this file is meant to be called by other R files

rm (list=ls())

library(openxlsx)
library(mgcv)
library (ggplot2)
library(gridExtra)

setwd("~/GitHub/MRF-GAM-estimation-")

newdata<-read.xlsx("data/distanze_ageclass.xlsx")

load("data/modelli")

##compute tr cost for predictions 
newdata$VTT<-0.75*(newdata$Reddito.comune/2000)
newdata$tr_cost<- ((newdata$VTT * (newdata$time_gmaps*2/3600)) + (newdata$dist_gmaps*2/1000)*(1.5*0.057))

## set male presence 
newdata$male<-newdata$male/newdata$pop_res

## predicted fishermen: linear model
pred_m1<-1-exp(-exp(predict.gam(trc_m4,newdata = newdata)[,2]))
newdata$fisher_m1<-round(pred_m1*newdata$popres_class)

prov_sum1 = data.frame("region" = sort(unique(newdata$prov)),
                      "fishers"= tapply(newdata$fisher_m1,newdata$prov,sum),
                      "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)


## predicted fishermen: log-linear model
newdata$pred_m2<-1-exp(-exp(predict.gam(trc_m5,newdata = newdata)[,2]))
newdata$fisher_m2<-round(newdata$pred_m2*newdata$popres_class)

prov_sum2 = data.frame("region" = sort(unique(newdata$prov)),
                       "fishers"= tapply(newdata$fisher_m2,newdata$prov,sum),
                       "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)

## predicted fishermen: GAM
newdata$pred_m3<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))
newdata$fisher_m3<-round(newdata$pred_m3*newdata$popres_class)

prov_sum3 = data.frame("region" = sort(unique(newdata$prov)),
                       "fishers"= tapply(newdata$fisher_m3,newdata$prov,sum),
                       "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)



## predicted trips: linear model 
lam_m4<-exp(predict.gam(trc_m4,newdata = newdata)[,1])
pi_m4<-1-exp(-exp(predict.gam(trc_m4,newdata = newdata)[,2]))

newdata$pred_m4 <- lam_m4*pi_m4
newdata$fisher_m4<-round(newdata$pred_m4*newdata$popres_class)

prov_sum4= data.frame("region" = sort(unique(newdata$prov)),
                       "fishers"= tapply(newdata$fisher_m4,newdata$prov,sum),
                       "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)

## predicted trips: log-linear model 
lam_m5<-exp(predict.gam(trc_m5,newdata = newdata)[,1])
pi_m5<-1-exp(-exp(predict.gam(trc_m5,newdata = newdata)[,2]))

newdata$pred_m5 <- lam_m5*pi_m5
newdata$fisher_m5<-round(newdata$pred_m5*newdata$popres_class)

prov_sum5= data.frame("region" = sort(unique(newdata$prov)),
                      "fishers"= tapply(newdata$fisher_m5,newdata$prov,sum),
                      "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)

## predicted trips: GAM
lam_m6<-exp(predict.gam(trc_m6,newdata = newdata)[,1])
pi_m6<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))

newdata$pred_m6 <- lam_m6*pi_m6
newdata$fisher_m6<-round(newdata$pred_m6*newdata$popres_class)

prov_sum6= data.frame("region" = sort(unique(newdata$prov)),
                      "fishers"= tapply(newdata$fisher_m6,newdata$prov,sum),
                      "prov_pop" = tapply(newdata$pop_res,newdata$prov,sum)/4)


lista<-list(prov_sum1,prov_sum2,prov_sum3,prov_sum4,prov_sum5,prov_sum6)














