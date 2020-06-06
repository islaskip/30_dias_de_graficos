---
title: "Duración real carreras por área"
author: "Matías Isla"
---

# Paquetes y datos
library(tidyverse)
library(readxl)
library(ggrepel)
library(hrbrthemes)
library(extrafont)
carreras <- read_excel("Buscador-estadisticas-por-carrera_mifuturo_2020 (1).xlsx", 
    skip = 1)

# Cambiar nombres de columnas para poder manipularlas sin problema
names(carreras)[names(carreras) == "Área"] <- "area"
names(carreras)[names(carreras) == "Carrera genérica"] <- "carrera"

# Guardar top 3 de duración real por área 
top_3 <- carreras %>% 
  group_by(area) %>% 
  top_n(3, wt = duracion_real)

# Gráfico
carreras %>%
  ggplot(aes(x = reorder(area, duracion_real, FUN = median), y = duracion_real, fill = area, color=area))+
  geom_violin(alpha = 0.3, color="grey90")+
  geom_jitter(alpha = 0.3)+
  geom_label_repel(aes(label = carrera), size = 3, data = top_3, color = "white")+
  scale_y_continuous(breaks = seq(5, 22, by = 2))+
  guides(fill=FALSE, color=FALSE)+
  labs(
    title = "Duración real de carreras en instituciones de educación superior (Chile)",
    subtitle = "Distribución por áreas de estudio",
    caption = "Fuente de datos: Mifuturo.cl"
  )+
  xlab("Área de estudio")+
  ylab("Duración real de carrera en número de semestres")+
  theme_ipsum_rc()

ggsave("05_06_2020_duracion_real_carreras.png", dpi = 600, type = "cairo", width = 15, height = 7)



