rm(list = ls())


library(openxlsx)
library (ggplot2)
library(gridExtra)
library(ggpubr)

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


## plot the trips

pesc<-data[data$pesca_d ==1, ]

p1<-ggplot(aes(x = uscite), data = pesc) +
    geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    theme_light()+
    ggtitle("") + 
    coord_cartesian(xlim= c(0,310), ylim=c(0,150)) + xlab("Number of annual trips") +
    ylab("frequency") +
  scale_x_continuous(breaks=c(1, 50,100,150,200,300))+
  
  theme(plot.title = element_text(size=15),  
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

 ggsave(filename = "trip distribution.png",plot = p1,
       width = 250, height = 140 , units = "mm")


















