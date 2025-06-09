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

## clean age typos
data<-data[data$age>=14,]

######################################################
###### DISTANZE IN TERM OF travel cost ###############
######################################################

## USCITE ZERO INFLATED POISSON GAM

#modello lineare - lineare 

trc_m4<-gam( list(uscite ~ tr_cost + age + male,
                  ~ tr_cost + age  + male),
             data = data, family=ziplss())

#modello log - log 

trc_m5<-gam( list(uscite ~ log(tr_cost) + log(age) + male,
                  ~ log(tr_cost) + log(age) + male),
             data = data, family=ziplss())


#modello gam - gam 

trc_m6<-gam( list(uscite ~ s(tr_cost, bs="cr", k=5) +
                     s(age, bs = "cr",k = 5) + male,
                   
                   ~ s(tr_cost, bs="cr", k=5) +
                     s(age, bs = "cr",k = 5)+ male),
             knots = list(0,2,3,4,max(data$tr_cost)),
              data = data, family=ziplss(), gamma = 1)
             
plot.gam(trc_m6)


#####################################################
save("trc_m4","trc_m5","trc_m6", file = "modelli")


