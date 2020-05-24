---
title: "Acceso a internet Chile"
author: "Matías Isla"
---

library(tidyverse)
library(readxl)
library(scales)
library(hrbrthemes)
conexiones <- read_excel("conexiones.xlsx", 
    col_types = c("date", "numeric", "numeric"))


scl = 193000
conexiones %>%
  ggplot(aes(x = fecha))+
  geom_line(aes(y = total_conexiones), color = "red")+
  geom_line(aes(y = cada_100_habitantes * scl), color = "blue")+
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Acceso cada 100 habitantes"))+
  labs(
    title= "Acceso a internet en Chile:",
    subtitle = "Evolución de conexiones fijas totales",
    caption = "Fuente de datos: Subsecretaría de Telecomunicaciones"
  )+
  ylab("N° total de conexiones")+
  xlab(NULL)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_ipsum_rc()+
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"))
    
ggsave("24_05_2020_acceso_internet.png", dpi=600, width = 10, height = 7, type = "cairo")



