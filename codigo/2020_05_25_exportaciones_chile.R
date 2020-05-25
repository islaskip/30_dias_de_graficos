---
title: "Exportaciones Chile 2018"
author: "Matías Isla"
---

## Paquetes y datos

library(tidyverse)
library(readxl)
library(treemapify)
library(viridis)
library(hrbrthemes)
exportaciones <- read_excel("WITS-Partner.xlsx", 
    sheet = "Partner")

names(exportaciones)[names(exportaciones) == "Partner Name"] <- "partner"
names(exportaciones)[names(exportaciones) == "Export Partner Share (%)"] <- "porcentaje"
names(exportaciones)[names(exportaciones) == "Export (US$ Thousand)"] <- "en_miles_dolares"

## Gráfico

exportaciones %>%
  filter(!is.na(en_miles_dolares) & !partner %in% c("World", "East Asia & Pacific", "North America", "Latin America & Caribbean", "Europe & Central Asia", "South Asia", "Middle East & North Africa", "Sub-Saharan Africa", "Other Asia, nes")) %>% ## quitando continentes
  mutate (label = paste0(partner, "\n", porcentaje, "%")) %>% ## union de nombre y porcentaje para la etiqueta del gráfico
  ggplot(aes(area = en_miles_dolares, fill = porcentaje, label = label)) +
  geom_treemap()+
  geom_treemap_text(family = "Roboto Condensed", colour = "white", place = "centre",
                    grow = TRUE)+
  scale_fill_viridis()+
  labs(
    title="Exportaciones de Chile en el año 2018",
    subtitle="Distribución en paises de destino",
    caption="Fuente de datos: World Integrated Trade Solution"
  )+
  theme_ipsum_rc()+
  theme(legend.position = "none")
  
ggsave("25_05_2020_exportaciones_chile.png", dpi=600, type = "cairo")


