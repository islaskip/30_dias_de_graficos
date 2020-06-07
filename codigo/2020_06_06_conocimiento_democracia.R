---
title: "Consumo de noticias y satisfacción con la democracia en Chile"
author: "Matías Isla"
---

#Cargar paquestes y datos
library(tidyverse)
library(haven)
library(scales)
library(ggrepel)
library(ggmosaic)
library(extrafont)

lapop <- read_dta("Chile LAPOP AmericasBarometer 2019 v1.0_W (1).dta")

#Etiquetar variables
lapop1$gi0n2 <- factor(lapop1$gi0n,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Diariamente", "Algunas veces\n a la semana", "... al mes", "... al año", "Nunca")
)

lapop1$pn4_2 <- factor(lapop1$pn4,
  levels = c("1", "2", "3", "4", "NA"),
  labels = c("Muy satisfecho(a)", "Satisfecho(a)", "Insatisfecho(a)", "Muy insatisfecho(a)", "No sabe/no responde")
)

# Gráfico
lapop1 %>%
  filter(!is.na(gi0n)) %>%
  ggplot() +
  geom_mosaic(aes(weight = count, x = product(gi0n2), fill = factor(pn4_2))) +
  labs(
    title = "Consumo de noticias y satisfacción con la democracia en Chile",
    subtitle = "Resultados año 2019",
    caption = "Fuente de datos: LAPOP (2019)"
  ) +
  ylab("En general, ¿qué grado de satisfacción tiene con la
 forma en que la democracia funciona en Chile?") +
  xlab("¿Con qué frecuencia sigue las noticias, ya sea en la televisión,
 la radio, los periódicos o el Internet?") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      size = 1.2,
      linetype = "dotted",
      colour = "grey80"
    ),
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(size = 15, face = "bold")
  )


ggsave("a.png", dpi = 600)
