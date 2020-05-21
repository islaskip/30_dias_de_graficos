---
title: "R Notebook"
output: html_notebook
---
## Paquetes y datos

library(tidyverse)
library(readxl)
library(ggtech)
library(extrafont)
ranking_colegios_psu_2019 <- read_excel("colegios psu/ranking_colegios_psu_2019.xlsx")
ranking_colegios_psu_2019$promedio_psu <- as.numeric(ranking_colegios_psu_2019$promedio_psu)
ranking_colegios_psu_2019$n_estudiantes <- as.numeric(ranking_colegios_psu_2019$n_estudiantes)
ranking_colegios_psu_2019$Ranking <- as.numeric(ranking_colegios_psu_2019$Ranking)

## Gráfico 

ranking_colegios_psu_2019 %>% 
  ggplot(aes(promedio_psu, fill = dependencia))+
  geom_histogram(bins=10)  +
  scale_fill_tech(theme="google")+
  labs(title="Colegios con los 100 mejores promedios PSU (2019)", 
       subtitle="Establecimientos con 5 o más alumnos",
       caption="Fuente de datos: Dirección de Análisis Institucional y Planificación de la Pontificia Universidad Católica de Chile",
       fill="Dependencia") +
  xlab("Promedio lenguaje y matemáticas")+
  ylab("Cantidad de colegios")+
 theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face="bold"),
      plot.caption = element_text(size= 9),
      text=element_text(size=13,  family="Roboto Condensed"))

ggsave("21_05_2020_promedios_psu.png", dpi=600, type = "cairo")

