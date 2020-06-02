---
title: "Puntaje indice de percepción de corrupción en Chile"
author: "Matías Isla"
---

# Cargar paquete y datos
library(tidyverse)
library(readxl)
library(hrbrthemes)
corruption_index <- read_excel("corrupcion/chile_corruption.xlsx")

# Formatear correctamente
corruption_index$Año <- as.numeric(corruption_index$Año)
corruption_index$Puntaje <- as.numeric(corruption_index$Puntaje)
corruption_index$Ranking <- as.numeric(corruption_index$Ranking)

#Gráfico
corruption_index %>% 
  ggplot(aes(Año, Puntaje))+
  geom_line(color="#69b3a2")+
  geom_point(color="#69b3a2")+
  geom_text(aes(label = Puntaje), hjust = 0.5,  vjust = -1, size = 2)+
  geom_vline(xintercept=2016, color="orange", size=.5) +
  annotate(geom="text", x=2016, y=6.3, 
             label="Implementación\nLey 20.880")+
  geom_vline(xintercept=2009, color="orange", size=.5) +
  annotate(geom="text", x=2009, y=6.3, 
             label="Implementación\nLey 20.285")+
  labs(
    title= "Evolución de puntaje de Chile en el Indice de Percepción de Corrupción",
    subtitle= "Puntaje máximo es 10, siendo 1 mayor riesgo de corrupción y 10 menor riesgo de corrupción",
    caption= "Elaboración propia. Fuente de datos: Corruption Perceptions Index"
  )+
  scale_x_continuous(breaks=seq(1996, 2019, 1))+
  theme_ipsum_rc()+
  theme(axis.text.x = element_text(angle=90),
        panel.grid.minor = element_blank())
ggsave("percepcion_corrupcion.png", dpi = 600, width = 9)
