

rm(list = ls())

library(mgcv)
library(openxlsx)
library (ggplot2)
library(gridExtra)

setwd("~/GitHub/MRF-GAM-estimation-")

#import the dataset
data<-read.xlsx("data/gam_fish_data_v3.xlsx")



##solo primi rispondenti
data<- data[!duplicated(data$fam_id),] 

##VTT defined as in Fezzi et al. 2014
data$VTT<-0.75*(data$average_inc/2000)


##compute travel cost with 1.5 euro per liter and fuel efficiency of 0.057 l/km
##2019 data by ministero per la transizione ecologica e IEA (round trip calculation)

data$tr_cost<- (data$VTT * (data$time_maps*2/3600)) + ((data$dist_maps*2/1000)*(1.5*0.057))

##subsetting for age 
data<-data[data$age>=14,]

a<-data[data$uscite>250,]
### here the truncation 

cutoff<-150 ## change the cutoff accordingly to replicate the results 



data<-data[data$uscite<cutoff,]


## USCITE ZERO INFLATED POISSON GAM

# linear 
trc_m4<-gam( list(uscite ~ tr_cost + age + male,
                  ~ tr_cost + age  + male),
             data = data, family=ziplss())

# log 

trc_m5<-gam( list(uscite ~ log(tr_cost) + log(age) + male,
                  ~ log(tr_cost) + log(age) + male),
             data = data, family=ziplss())


# gam 

trc_m6<-gam( list(uscite ~ s(tr_cost, bs="cr", k=5) +
                    s(age, bs = "cr",k = 5) + male,
                  
                  ~ s(tr_cost, bs="cr", k=5) +
                    s(age, bs = "cr",k = 5)+ male),
             knots = list(0,2,3,4,max(data$tr_cost)),
             data = data, family=ziplss(), gamma = 1)

plot.gam(trc_m6)


#############  PREDICTIONS #######################

source("rfiles/functions/fish_pred.ci_zip.R")
newdata<-read.xlsx("data/distanze_ageclass.xlsx")

##compute tr cost for predictions 
newdata$VTT<-0.75*(newdata$Reddito.comune/2000)
newdata$tr_cost<- ((newdata$VTT * (newdata$time_gmaps*2/3600)) + (newdata$dist_gmaps*2/1000)*(1.5*0.057))

## set male presence 
newdata$male<-newdata$male/newdata$pop_res

linear<-fish_pred.ci_zip(model = trc_m4, newdata = newdata, n = 1000,prov = NULL)
loglin<-fish_pred.ci_zip(model = trc_m5, newdata = newdata,n = 1000, prov = NULL)
gam<-fish_pred.ci_zip(model = trc_m6, newdata = newdata,n = 1000, prov = NULL)


### manipulate and export results 
out<-cbind(linear,loglin,gam)\

res_mean <- apply(out,2,mean)
res_ci<-apply(out,2,quantile, probs = c(0.025,0.975) )

res<-rbind(res_mean,res_ci)

#write.xlsx(res, "results_trunc150.xlsx")



