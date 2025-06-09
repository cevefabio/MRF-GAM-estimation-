

rm(list =ls())

source("different effort evaluation.R")


# Definisci il position dodge
dodge <- position_dodge(width = 500)

p1 <- ggplot(data = p.data) + 
  # Barre di errore con spessore maggiore e stile diverso per tipo
  geom_errorbar(aes(x = n, ymin = MAPE_inf95, ymax = MAPE_sup95, color = type, linetype = type),
                width = 200,
                linewidth = 1.2,
                alpha = 0.9,
                position = dodge) +
  
  # Punti e linee
  geom_point(aes(x = n, y = MAPE, color = type), size = 2.5, position = dodge) +
  #geom_line(aes(x = n, y = MAPE, linetype = type, color = type), linewidth = 1, position = dodge) +
  
  # Limite asse Y
  #coord_cartesian(ylim = c(0, 1.3)) +
  
  # Colori e tipi di linea
  scale_color_manual(values = c("gray50", "gray10")) +
  scale_linetype_manual(values = c("dashed", "solid")) +  # es: fishers = dotted, trips = solid
  
  # Tema
  theme_light() +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14, vjust = -0.5),
    axis.title.y = element_text(face = "bold", size = 14), 
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  xlab("Subsample size") +
  ylab("MAPE") +
  scale_x_continuous(breaks = unique(p.data$n))+ 
  scale_y_continuous(labels = scales::label_percent())

p1

ggsave(filename = "effort_evaluation_barplot_linear.png",plot = p1,
       width = 250, height = 120 , units = "mm")


write.csv(p.data, file = "data.MAPE.GAM.csv")