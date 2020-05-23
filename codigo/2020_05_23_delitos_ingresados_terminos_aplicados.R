---
title: "Delitos ingresados y términos aplicados"
author: "Matías Isla"
---
## Paquetes y datos

library(tidyverse)
library(readxl)
library(tools)
library(hrbrthemes)

delitos <- read_excel("delitos.xlsx")
termino <- read_excel("termino.xlsx")


## Pivot
delitos <- delitos %>% 
      mutate(delito = str_to_sentence(as.character(delito)))
delitos <- delitos %>%
  pivot_longer(-delito,
               names_to = "year",
               values_to = "count") %>%
  arrange(year) %>%
  mutate(delito = fct_reorder(delito,
                               count,
                        last)) 

## Gráfico
delitos %>% 
  ggplot()+
  geom_path(aes(count, delito), arrow = arrow(length = unit(1.5, "mm"), type = "closed", ends = "last"),
             color = "grey60")+
  geom_text(aes(x = count,
              y = delito,
              label = count,
              hjust = ifelse(year == "2018", 1.4, -0.4),
              color = year),
          size = 3
          ) +
  labs(
    title = "Delitos ingresados por categoría de delito",
    subtitle= "Comparación total 2018 a total 2019",
    caption= "Fuente de datos: Fiscalia de Chile"
  )+
  xlab(NULL)+
  ylab(NULL)+
  coord_cartesian(
    xlim=c(-2000,230000))+
    theme_ipsum_rc(grid = FALSE)+
    theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank())
ggsave("22_05_2020_delitos_ingresados.png", width = 15, height = 10, dpi=600, type = "cairo")

## Pivot
termino <- termino %>% 
      mutate(terminos = str_to_sentence(as.character(terminos)))
termino <- termino %>%
  pivot_longer(-terminos,
               names_to = "year",
               values_to = "count") %>%
  arrange(year) %>%
  mutate(terminos = fct_reorder(terminos,
                               count,
                        last)) 
## Gráfico
termino %>% 
  ggplot()+
  geom_path(aes(count, terminos), arrow = arrow(length = unit(1.5, "mm"), type = "closed", ends = "last"),
             color = "grey60")+
  geom_text(aes(x = count,
              y = terminos,
              label = count,
              hjust = ifelse(year == "2018", 1.4, -0.4),
              color = year),
          size = 4
          ) +
  labs(
    title = "Términos aplicados",
    subtitle= "Comparación total 2018 a total 2019",
    caption= "Fuente de datos: Fiscalia de Chile"
  )+
  xlab(NULL)+
  ylab(NULL)+
  coord_cartesian(
    xlim=c(-2000,900000))+
    theme_ipsum_rc(grid = FALSE)+
    theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank())
ggsave("22_05_2020_terminos_aplicados.png", width = 15, height = 10, dpi=600, type = "cairo")


