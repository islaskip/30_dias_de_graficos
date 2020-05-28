---
title: "Identificación partidaria"
author: "Matías Isla"
---
## Cargar paquetes y datos

library(tidyverse)
library(readxl)
library(ggalluvial)
library(extrafont)

diputados <- read_excel("diputados_ind2.xlsx")

## Crear variable de frecuencias para poder realizar el diagrama sankey

diputados_freq <- diputados %>%                                                   
            group_by(partido_2017, partido_2020) %>%  
            summarise(Freq = n()) 
color_partido <- c("#00BFFF", "#008000", "#808080", "#8B0000", "#1E90FF", "#03BF00", "#FF4500", "#FFD700", "#FFA500", "#CD5C5C", "#E70080", "#FF0000", "#412560", "#135BB8", "#1D4C4F", "#000080") ## color extraido manualmente de paquete ggparliament
names(color_partido) <- c("Evópoli", "Federación Regionalista Verde Social", "Independiente", "Partido Comunista", "Partido Demócrata Cristiano", "Partido Ecologista Verde", "Partido Humanista", "Partido Liberal", "Partido Progesista", "Partido Socialista", "Poder", "Renovación Nacional", "Revolución Democrática", "Unión Demócrata Cristiana")

## Gráfico

diputados_freq %>% 
  ggplot(aes(axis1= partido_2017, axis2= partido_2020, y = Freq))+
  scale_x_discrete(limits = c("Partido 2017", "Partido 2020"), expand = c(.1, .05)) +
  scale_fill_manual(values = color_partido) +
  geom_alluvium(aes(fill = partido_2017))+
  labs(
    title = "Evolución de identificación partidaria de Diputadas y Diputados (Chile)",
    subtitle = "Comparación año 2017 y año 2020",
    caption = "Fuente de datos: Wikipedia"
  )+
  ylab("")+
  geom_stratum()+
  geom_text(stat= "stratum", infer.label = TRUE)+
  theme_minimal()+
  theme(text = element_text(family = "Roboto Condensed", size = 20),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 25, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 23),
        plot.caption = element_text(size = 14))

ggsave("28_05_2020_identificacion_partidaria.png", height = 25, width = 13)
