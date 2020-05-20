---
title: "Venta de celulares en China por trimestre"
author: "Matías Isla"
---

## Paquetes y datos

library(tidyverse)
library(readxl)
library(data.table) 
library(viridis)
library(scales)
library(zoo)
library(hrbrthemes)
library(ggthemes)

smartphone_shipments <- read_excel("smartphone-shipments-quarterly-in-china-q1-2014-q1-2020-by-vendor.xlsx", skip = 4,
    sheet = "Data")

## Compañias en columnas, aplicar pivot_longuer para poder acomodarlas en una sola

smartphone_bytype <- smartphone_shipments %>%
  mutate_if(is.numeric, as.character, is.factor, as.character) %>%
  pivot_longer(-...1,
    names_to = "company",
    values_to = "count"
  )
smartphone_bytype$count <- as.numeric(smartphone_bytype$count)
smartphone_bytype$...1<- as.Date(as.yearqtr(smartphone_bytype$...1, format = "Q%q '%y")) ## Columna de trimestres como fecha

## Gráfico

smartphone_bytype %>%
  filter(company != c("Lenovo*", "Samsung"), ...1 > "2015-10-01") %>% ## debido a falta de datos
  ggplot(aes(...1, count, fill=company)) + 
    geom_area(alpha=0.7, size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
    scale_x_date(breaks = date_breaks("3 months"),
                 date_labels = "%Y/%m")+
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, by = 20))+
  labs(
    title= "Venta de celulares en China por compañia",
    fill= "Compañia",
    caption= "Fuente de datos: IDC Media Center"
  )+
  xlab("Año en trimestres")+
  ylab("Unidades en millones")+
  theme_ipsum_rc(grid="XY") +
  theme(axis.text.x=element_text(size = 8, hjust=1, angle=60)) +
  theme(legend.position="bottom")

ggsave("20_05_2020_venta_celulares.png", dpi=600, type = "cairo")


