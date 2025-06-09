


rm(list = ls())

library(mgcv)
library(openxlsx)
library (ggplot2)
library(gridExtra)

setwd("~/GitHub/MRF-GAM-estimation-")


 
subsample_simulation<- function(data, model,newdata, R = 1000, n.vec = c(1000,2000,4000,6000,8000), 
                                 true_total ){
   
   ## initialize some empty vectors 
   MAPE.fish<-vector()
   MAPE.trips<-vector()
   MAPE.fish_inf90<- vector()
   MAPE.fish_sup90<- vector()
   MAPE.fish_inf95<- vector()
   MAPE.fish_sup95<- vector()
   MAPE.trips_inf90<- vector()
   MAPE.trips_sup90<- vector()
   MAPE.trips_inf95<- vector()
   MAPE.trips_sup95<- vector()
   
   T.fish<-true_total[1]   ## linear
   T.trips<-true_total[2]
   
   
   a<-Sys.time()
   
   for (j in 1:length(n.vec)){  
     
     n<-n.vec[j]
     
     ## initialize two empty vectors to store results 
     
     fishers.vec<-vector()
     trips.vec<-vector()
     
     
     for (i in 1:R){
       
       df<-data[sample(nrow(data),n, replace = F),] ## extract a random sample
       
       if(model == 3){
         trc_m6<-gam( list(uscite ~ s(tr_cost, bs="cr", k=5) +
                               s(age, bs = "cr",k = 5) + male,
                             
                             ~ s(tr_cost, bs="cr", k=5) +
                               s(age, bs = "cr",k = 5)+ male),
                        knots = list(0,2,3,4,max(data$tr_cost)),
                        data = df, family=ziplss(), gamma = 1)}
       
       if(model == 2){
         trc_m6<-gam( list(uscite ~ log(tr_cost) + log(age) + male,
                           ~  log(tr_cost) + log(age) + male),
                      data = df, family=ziplss())}

       if(model == 1){
         trc_m6<-gam( list(uscite ~ tr_cost + age + male,
                           ~  tr_cost + age + male),
                      data = df, family=ziplss())}
       

       
       ## compute the predictions
       
       ## predicted fishermen: GAM
       pred_m3<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))
       fishers<-round(pred_m3*newdata$popres_class)
       fishers.vec[i]<-sum(fishers)
       
       ## predicted trips: GAM
       lam_m6<-exp(predict.gam(trc_m6,newdata = newdata)[,1])
       pi_m6<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))
       
       pred_m6 <- lam_m6*pi_m6
       trips<-round(pred_m6*newdata$popres_class)
       
       if(is.nan(sum(trips))){trips.vec[i]<-(10^9)}
       else{trips.vec[i]<-sum(trips)}
       
       print(paste("model estimation", i, "completed"))
       
     }
     
     MAPE.fish[j] <-median(abs((fishers.vec-T.fish)/T.fish))
     MAPE.trips[j]<-median(abs((trips.vec-T.trips)/T.trips))
     
     perc.fish<- quantile(abs((fishers.vec-T.fish)/T.fish), probs =  c(0.025,  0.05, 0.95,  0.975))
     perc.trips<-quantile(abs((trips.vec-T.trips)/T.trips), probs =  c(0.025,  0.05, 0.95,  0.975))
     
     
     MAPE.fish_inf90[j] <- perc.fish[2]
     MAPE.fish_sup90[j] <- perc.fish[3]
     MAPE.fish_inf95[j] <- perc.fish[1]
     MAPE.fish_sup95[j] <- perc.fish[4]
     MAPE.trips_inf90[j] <- perc.trips[2]
     MAPE.trips_sup90[j] <- perc.trips[3]
     MAPE.trips_inf95[j] <- perc.trips[1]
     MAPE.trips_sup95[j] <- perc.trips[4]
     
     print(paste("subsample simulation", j, "completed at", Sys.time()))
     
   }
   
   b<-Sys.time()
   a-b
   
   
   MAPE.fish<-round(MAPE.fish,3)
   MAPE.trips<-round(MAPE.trips,3)
   
   MAPE.fish_inf90<-round(MAPE.fish_inf90,3)
   MAPE.fish_sup90<-round(MAPE.fish_sup90,3)
   MAPE.fish_inf95<-round(MAPE.fish_inf95,3)
   MAPE.fish_sup95<-round(MAPE.fish_sup95,3)
   
   MAPE.trips_inf90<-round(MAPE.trips_inf90,3)
   MAPE.trips_sup90<-round(MAPE.trips_sup90,3)
   MAPE.trips_inf95<-round(MAPE.trips_inf95,3)
   MAPE.trips_sup95<-round(MAPE.trips_sup95,3)
   
   
   
   p.data<-data.frame("n" = rep(c(1000,2000,4000,6000,8000),2),
                      "MAPE" = c(MAPE.fish,MAPE.trips), 
                      "MAPE_inf90"= c(MAPE.fish_inf90,MAPE.trips_inf90),
                      "MAPE_sup90"= c(MAPE.fish_sup90,MAPE.trips_sup90),
                      "MAPE_inf95"= c(MAPE.fish_inf95,MAPE.trips_inf95),
                      "MAPE_sup95"= c(MAPE.fish_sup95,MAPE.trips_sup95),
                      "type" = rep(c("fishers","trips"),each = 5) )
   
   p.data$type <- factor(p.data$type, levels = c("trips", "fishers"))
   
   return(p.data)
   
 }
 


 #import the dataset
 data<-read.xlsx("data/gam_fish_data_v3.xlsx")
 
 ##solo primi rispondenti
 data<- data[!duplicated(data$fam_id),] 
 
 ##VTT defined as in Fezzi et al. 2014
 data$VTT<-0.75*(data$average_inc/2000)
 
 
 #compute travel cost with 1.5 euro per liter and fuel efficiency of 0.057 l/km
 #2019 data by ministero per la transizione ecologica e IEA (round trip calculation)
 
 data$tr_cost<- (data$VTT * (data$time_maps*2/3600)) + ((data$dist_maps*2/1000)*(1.5*0.057))
 
 
 ##subsetting for age 
 data<-data[data$age>=14,]
 
 ### dataset for predictions
 newdata<-read.xlsx("data/distanze_ageclass.xlsx")
 
 ##compute tr cost for predictions 
 newdata$VTT<-0.75*(newdata$Reddito.comune/2000)
 newdata$tr_cost<- ((newdata$VTT * (newdata$time_gmaps*2/3600)) + (newdata$dist_gmaps*2/1000)*(1.5*0.057))
 
 ## set male presence 
 newdata$male<-newdata$male/newdata$pop_res
 

 R = 1000
 n.vec<-c(1000,2000,4000,6000,8000)
 
 linear_pred<-subsample_simulation(data, model = 1, newdata,  R = R,true_total = c(1513000,28843000))
 
 loglinear_pred<-subsample_simulation(data, model = 1, newdata,  R = R,true_total = c(1874000,37837000))
 
 gam_pred<-subsample_simulation(data, model = 1, newdata,  R = R,true_total = c(1518000,34256000))
 





