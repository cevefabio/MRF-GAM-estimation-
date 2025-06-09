

rm(list = ls()) 

setwd("~/GitHub/MRF-GAM-estimation-")

load("data/modelli")
source("rfiles/functions/parCI_zip.R")


library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
library(openxlsx)

## import the dataset
data<-read.xlsx("data/gam_fish_data_v3.xlsx")
data$VTT<-0.75*(data$average_inc/2000)
data$tr_cost<- ((data$VTT * (data$time_maps*2/3600)) + (data$dist_maps*2/1000)*(1.5*0.057))
data<-data[data$age>=14,]


## probability plot ## 
newdata<- data.frame("tr_cost" = seq(min(data$tr_cost),max(data$tr_cost),
                                     length.out = 1000))

## keep fix the age 
newdata$age<-46.2 ##average value 
newdata$male<- 0.5 ## average value 

## model predictions
newdata$fish_m1<-1-exp(-exp(predict.gam(trc_m4,newdata = newdata)[,2]))
newdata$fish_m2<-1-exp(-exp(predict.gam(trc_m5,newdata = newdata)[,2]))
newdata$fish_m3<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))

lam_m1<-exp(predict.gam(trc_m4,newdata = newdata)[,1])
pi_m1<-1-exp(-exp(predict.gam(trc_m4,newdata = newdata)[,2]))
newdata$trip_m1 <- lam_m1*pi_m1

lam_m2<-exp(predict.gam(trc_m5,newdata = newdata)[,1])
pi_m2<-1-exp(-exp(predict.gam(trc_m5,newdata = newdata)[,2]))
newdata$trip_m2 <- lam_m2*pi_m2

lam_m3<-exp(predict.gam(trc_m6,newdata = newdata)[,1])
pi_m3<-1-exp(-exp(predict.gam(trc_m6,newdata = newdata)[,2]))
newdata$trip_m3 <- lam_m3*pi_m3

## intervalli di confidenza MC 

lin_ci<- parCI_zip(trc_m4,newdata, n = 1000)
newdata$lin_count_inf<-lin_ci[[2]][1,]
newdata$lin_count_sup<-lin_ci[[2]][2,]
newdata$lin_ones_inf<- lin_ci[[1]][1,]
newdata$lin_ones_sup<- lin_ci[[1]][2,]

log_ci<- parCI_zip(trc_m5,newdata, n = 1000)
newdata$log_count_inf<-log_ci[[2]][1,]
newdata$log_count_sup<-log_ci[[2]][2,]
newdata$log_ones_inf<- log_ci[[1]][1,]
newdata$log_ones_sup<- log_ci[[1]][2,]


gam_ci<- parCI_zip(trc_m6,newdata, n = 10)
newdata$gam_count_inf<-gam_ci[[2]][1,]
newdata$gam_count_sup<-gam_ci[[2]][2,]
newdata$gam_ones_inf<- gam_ci[[1]][1,]
newdata$gam_ones_sup<- gam_ci[[1]][2,]


## LET'S DO SOME NICE PLOTS 

## linear model 

prob_m1<- ggplot(data = newdata, aes(x = tr_cost, y = fish_m1)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = lin_ones_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = lin_ones_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("probability") + xlab ( "") + ggtitle("linear") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,0.06)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

## log - linear model 

prob_m2<- ggplot(data = newdata, aes(x = tr_cost, y = fish_m2)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = log_ones_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = log_ones_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("") + xlab ( "") +  ggtitle("log-linear") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,0.06)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

## gam model 

prob_m3<- ggplot(data = newdata, aes(x = tr_cost, y = fish_m3)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = gam_ones_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = gam_ones_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("") + xlab ( "") + ggtitle("GAM") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,0.06)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

prob<-grid.arrange(prob_m1,prob_m3,prob_m2,ncol=3)

ggsave(filename = "modelli.prob.png",plot = prob,
       width = 280, height = 100 , units = "mm")

############################################
################### TRIPS #################
############################################


## linear model 

trip_m1<- ggplot(data = newdata, aes(x = tr_cost, y = trip_m1)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = lin_count_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = lin_count_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("fishing trips per resident") + xlab ( "") + ggtitle("") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,1.2)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

## log - linear model 

trip_m2<- ggplot(data = newdata, aes(x = tr_cost, y = trip_m2)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = log_count_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = log_count_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("") + xlab ( "") +  ggtitle("") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,1.2)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

## gam model 

trip_m3<- ggplot(data = newdata, aes(x = tr_cost, y = trip_m3)) +
  geom_line(aes( linetype = "average"), linewidth = 1) +
  geom_line(data = newdata, aes(x = tr_cost, y = gam_count_inf, linetype = "inf"),linewidth = 0.8) +
  geom_line(data = newdata, aes(x = tr_cost, y = gam_count_sup, linetype = "sup"), linewidth = 0.8) +
  ylab("") + xlab ( "Travel cost") + ggtitle("") +
  
  scale_linetype_manual(name='Regression Model',
                        breaks=c('average', 'inf', 'sup'),
                        values=c('average'='solid', 'inf'='dotted',
                                 'sup'='dotted')) +
  
  ylim(c(0,1.2)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

trip<-grid.arrange(trip_m1,trip_m3,trip_m2,ncol=3)


# ggsave(filename = "modelli.trips.png",plot = trip,
#        width = 280, height = 100 , units = "mm")


final_plot = grid.arrange(prob,trip, nrow = 2)

ggsave(final_plot, filename = "TC predictions plot.png", width = 16, height = 8)




