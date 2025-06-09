


setwd("~/GitHub/MRF-GAM-estimation-")

##get the model predictions predicitons
source("rfiles/predictions/fishers prediction.R") 

library(ggplot2)  
library(maps)
library(dplyr) 
library(openxlsx)
library(RColorBrewer)
library(grid)
library(patchwork)
library(png)
library(ggspatial)


italia <- read.xlsx("data/italy_mapdata.xlsx")


plot_list = list()

for (i in 1:3){
  italia_merge<- inner_join(italia,lista[[i]],by = "region")
  italia_merge$part_rate<-italia_merge$fishers/italia_merge$prov_pop
  
  df  <- italia_merge %>%
    mutate(., cat = with(., case_when(
      (part_rate >= 0  & part_rate < 0.01) ~ "0-1%",
      (part_rate >= 0.01 & part_rate < 0.02) ~ '1-2%',
      (part_rate >= 0.02 & part_rate < 0.03) ~ '2-3%',
      (part_rate >= 0.03 & part_rate < 1) ~ '> 3%'
      
    )))
  
  
  
  plot_list[[i]]<-ggplot() + geom_polygon( data=df, aes(x=long, y=lat, group=group,
                                                        fill = cat),
                                           color="black") + ylab("")+ xlab("") + 
    scale_fill_manual(values = c("lightyellow", "lightblue", "cyan3", "royalblue"), 
                      breaks=c( "0-1%", '1-2%', '2-3%','> 3%'), 
                      name = "part rate (%)") + 
    
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank())
  
}


###USCITE

for (i in 4:6){
  italia_merge<- inner_join(italia,lista[[i]],by = "region")
  italia_merge$trip<-italia_merge$fishers/italia_merge$prov_pop
  
  df  <- italia_merge %>%
    mutate(., cat = with(., case_when(
      (trip >= 0  & trip < 0.2) ~ "0 - 0.2",
      (trip >= 0.2 & trip < 0.6) ~ '0.2 - 0.3',
      (trip >= 0.6 & trip < 1) ~ '0.6 - 1',
      (trip >= 1 & trip < 3) ~ '> 1'
      
    )))
  
  
  
  plot_list[[i]]<-ggplot() + geom_polygon( data=df, aes(x=long, y=lat, group=group,
                                                        fill = cat),
                                           color="black") + ylab("")+ xlab("") + 
    scale_fill_manual(values = c("lightgoldenrodyellow", "darkseagreen1", "yellowgreen", "mediumseagreen"), 
                      breaks=c("0 - 0.2", '0.2 - 0.3', '0.6 - 1','> 1'),
                      name = "Per-capita trips") + 
    
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank())
}

######################################################
################## POLISH THE PLOTS ##################
######################################################

p1<- plot_list[[1]] + labs(title =   "Linear") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))

p2<- plot_list[[2]] + guides ( fill = "none") + labs(title =  "log-Linear") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))

p3<- plot_list[[3]] + guides ( fill = "none") + labs(title =   "GAM") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))


c1 <-p1+p3+p2 & theme()

part.plot<-c1 + plot_layout(guides = "collect")

########### TRIPS #################

p4<- plot_list[[4]] + labs(title =   "Linear") + guides ( fill = "none") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))

p5<- plot_list[[5]] + guides ( fill = "none") + labs(title =  "log-Linear") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))

p6<- plot_list[[6]]  + labs(title =   "GAM") +
  theme(plot.title = element_text(hjust = 0.5), 
        title = element_text(size=12))


c2 <-p4+p6+p5 & theme()

trip.plot<-c2 +
  annotation_north_arrow(
    location = "tr",  # top-right
    which_north = "true",  # or "grid"
    style = north_arrow_fancy_orienteering,  # or minimal, nautical, etc.
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  )  +
  plot_layout(guides = "collect") 

trip.plot

## add scale and compass rose


# ggsave(filename = "mappe uscite.png",plot = trip.plot,
#        width = 290, height = 120 , units = "mm")
 
# ggsave(filename = "mappe prob.png",plot = part.plot,
#        width = 290, height = 120 , units = "mm")


