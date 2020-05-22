---
title: "R Notebook"
output: html_notebook
---

## Paquetes y datos

library(tidyverse)
library(readxl)
library(viridis)
library(hrbrthemes)
library(extrafont)
placer_paises <- read_excel("placer_paises.xlsx", 
    col_types = c("text", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"))
names(placer_paises)[1] <- "placer"

## Pivot para agrupar variable con preferencias

placer_paises <- placer_paises %>%
  pivot_longer(-placer,
    names_to = "pais",
    values_to = "count"
  )

## Gráfico

placer_paises %>% 
  ggplot(aes(pais, reorder(placer, -count,), fill= count)) +  ## reorder() para ordenar por preferencias
  geom_tile()+
  geom_text(aes(label = count), color = "white") +
  scale_fill_viridis(discrete=FALSE) +
  labs(
  title = "Por favor ordene qué tanto disfruta las siguientes actividades\n desde más a menos placentera",
  subtitle = "Se muestran promedios por país",
  caption = "Fuente de datos: TENGA"
  )+
  xlab(NULL)+
  ylab(NULL)+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45, hjust= 1),
        legend.position = "none",
        plot.title = element_text(family = "Roboto Condensed", size = 14, hjust = 0.5),
        plot.subtitle = element_text(family = "Roboto Condensed",size = 12, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",size= 7),
      text=element_text(size=13,  family = "Roboto Condensed"))
      
ggsave("22_05_2020_placer_pais.png", dpi=600, type = "cairo")


