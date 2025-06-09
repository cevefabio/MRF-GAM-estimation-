

setwd("~/GitHub/MRF-GAM-estimation-")

source("rfiles/plotting/Map_plot_discrete.R") 

library(openxlsx)
library(tidyverse)
library(cowplot)

rm(list=setdiff(ls(), c("italia","p1","p2","p3","p4","p5","p6")))

data<-read.xlsx("data/gam_fish_data_v3.xlsx")

province<-read.xlsx("data/province.xlsx")
colnames(province)<-c("sigla", "region")

## correct age typo
data<-data[data$age>=14,]

## compute fishers per province
pesc_prov<-as.data.frame(tapply(data$pesca_d,data$prov,sum))
pesc_prov$trips<-tapply(data$uscite,data$prov,sum)
pesc_prov$tot<-table(data$prov)

pesc_prov$prov<-rownames(pesc_prov)
colnames(pesc_prov)<-c("fishers","trips","tot","sigla")

df_actual<-inner_join(pesc_prov,province,by = "sigla")

df_actual$part_rate<-df_actual$fishers/df_actual$tot
df_actual$pc_trips<-df_actual$trips/df_actual$tot

italia_merge<- left_join(italia,df_actual,by = "region")

# run to obtain italy without non sampled areas
# italia_merge<- inner_join(italia,df_actual,by = "region") 




df  <- italia_merge %>%
  mutate(., cat = with(.,case_when(
    (part_rate >= 0  & part_rate < 0.01) ~ "0-1%",
    (part_rate >= 0.01 & part_rate < 0.02) ~ '1-2%',
    (part_rate >= 0.02 & part_rate < 0.03) ~ '2-3%',
    (part_rate >= 0.03 & part_rate < 1) ~ '> 3%'
    
  )))


p_actual<-ggplot() + geom_polygon( data=df, aes(x=long, y=lat, group=group,
                                                      fill = cat),
                                         color="black") + ylab("")+ xlab("") + 
  scale_fill_manual(values = c("lightyellow", "lightblue", "cyan3", "royalblue"), 
                    breaks=c( "0-1%", '1-2%', '2-3%','> 3%'), 
                    name = "part rate (%)") + 
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())




##-------- for trips -------------##


df1  <- italia_merge %>%
  mutate(., cat = with(., case_when(
    (pc_trips >= 0   & pc_trips < 0.2) ~ "0 - 0.2",
    (pc_trips >= 0.2 & pc_trips < 0.6) ~ '0.2 - 0.3',
    (pc_trips >= 0.6 & pc_trips < 1) ~ '0.6 - 1',
    (pc_trips >= 1  ) ~ '> 1'
    
  )))



p_actual_trips<-ggplot() + geom_polygon( data=df1, aes(x=long, y=lat, group=group,
                                                      fill = cat),
                                         color="black") + ylab("")+ xlab("") + 
  scale_fill_manual(values = c("gray", "lightgoldenrodyellow", "darkseagreen1", "yellowgreen", "mediumseagreen"), 
                    breaks=c(NA,"0 - 0.2", '0.2 - 0.3', '0.6 - 1','> 1'),
                    name = "Per-capita trips") + 
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#####################################################



p_actual +  p_actual_trips

# ggsave(filename = "insample_map.png", width =16 , height = 9)


