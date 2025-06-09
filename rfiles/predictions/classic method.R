
## here we use the classic method "sampling estimator" to predict the number of fishers 

rm(list = ls())

library(mgcv)
library(openxlsx)
library (ggplot2)
library(gridExtra)
library(tidyverse)
library(stringdist)

setwd("C:/Users/HP/Desktop/Ricerca/gam fishery")


source("R files for estimation and plotting/fishers prediction.R") ## 


#import the dataset
data<-read.xlsx("data/gam_fish_data_v3.xlsx")


## table by province 
prov<- as.data.frame(tapply(data$pesca_d,data$prov,sum)/table(data$prov))
colnames(prov)<-c("prov","part")
prov$trip<- tapply(data$uscite,data$prov,sum)/table(data$prov)
prov$trip

## match the inclusion probability 

des<-read.xlsx("data/samp_design.xlsx")

## levenshein distance to match 

M<-stringdistmatrix(data$strata,des$stratum)  
ind<-apply(M,1,which.min)

pinkl.vec<-des$p_incl[ind]

data$p_incl<-pinkl.vec

## HT estimator 

library(survey)

HT_hat<-sum(data$pesca_d/data$p_incl)

## define the design 
design <- svydesign(
  ids = ~1,           
  strata = ~strata,   # Stratificazione
  probs = ~p_incl,        # inclusion prob
  data = data
)


# HT total estimator 
HT_part <- svytotal(~pesca_d, design)
HT_trip <- svytotal(~uscite, design)







