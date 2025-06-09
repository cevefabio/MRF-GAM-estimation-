
#### MODELS PREDICTION ONLY FOR COASTAL REGIONS ###
rm (list=ls())

library(openxlsx)
library(mgcv)
library (ggplot2)
library(gridExtra)

setwd("~/GitHub/MRF-GAM-estimation-")

newdata<-read.xlsx("data/distanze_ageclass.xlsx")

load("data/modelli")

source("rfiles/functions/fish_pred.ci_zip.R")

##compute tr cost for predictions 
newdata$VTT<-0.75*(newdata$Reddito.comune/2000)
newdata$tr_cost<- ((newdata$VTT * (newdata$time_gmaps*2/3600)) + (newdata$dist_gmaps*2/1000)*(1.5*0.057))

## set male presence 
newdata$male<-newdata$male/newdata$pop_res

prov<-read.xlsx("data/province_ita.xlsx")

linear<-fish_pred.ci_zip(trc_m4,newdata,1000, prov)
log_li<-fish_pred.ci_zip(trc_m5,newdata,1000,prov)
gam<-fish_pred.ci_zip(trc_m6,newdata,1000,prov)



out<-cbind(linear,log_li,gam)

res_mean<-apply(out,2,mean)
res_ci<-apply(out,2,quantile, probs = c(0.025,0.975) )

res<-rbind(res_mean,res_ci)

#write.xlsx(res, "results_onlycoastal.xlsx")


