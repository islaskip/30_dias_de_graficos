---
title: "Discursos presidenciales durante la pandemia"
author: "Matías Isla"
date: "12-05-2020"
output: html_document
---

## Abrir paquetes y datos

library(tidyverse)
library(tidytext)
library(dplyr)
library(SnowballC)
library(tm)
library(wordcloud)
library(hrbrthemes)
library(readxl)
X3dis <- read_excel("~/dis.xlsx",
  col_types = c("date", "text")
)
View(X3dis)

## Tokenización

discursos <- X3dis %>%
  unnest_tokens(word, texto)

## Conteo de palabras y orden descendiente

discursos %>%
  count(word) %>%
  arrange(desc(n))

## Eliminando palabras comunes (stop words)

esp_stop_words <- bind_rows(
  stop_words,
  tibble(
    word = stopwords::stopwords("es", "stopwords-iso"),
    lexicon = "custom"
  )
)

todos_discursos <- discursos %>%
  count(word) %>%
  anti_join(esp_stop_words) %>%
  arrange(desc(n))
todos_discursos

## Gráfico de palabras

todos_discursos <- discursos %>%
  count(word) %>%
  anti_join(esp_stop_words) %>%
  arrange(desc(n)) %>%
  filter(n > 25)

## Personlizar Stop Words y gráfico en orden descendiente

discursos_stop_words <- tribble(
  # Nombre de columnas concordantes con lexicon
  ~word, ~lexicon,
  # Palabras propias
  "va", "CUSTOM",
  "quiero", "CUSTOM",
  "día", "CUSTOM",
  "hoy", "CUSTOM",
  "día", "CUSTOM",
  "ser", "CUSTOM",
  "muchas", "CUSTOM",
  "van", "CUSTOM",
  "1", "CUSTOM",
  "mil", "CUSTOM",
  "2", "CUSTOM"
)

esp2_stop_words <- esp_stop_words %>%
  bind_rows(discursos_stop_words)

todos_discursos <- discursos %>%
  count(word) %>%
  anti_join(esp2_stop_words) %>%
  arrange(desc(n)) %>%
  filter(n > 24) %>%
  mutate(word2 = fct_reorder(word, n))

## Gráfico

ggplot(todos_discursos, aes(x = word2, y = n)) +
  geom_col(fill = "#D32F2F", colour = "#C62828") +
  coord_flip() +
  theme_ipsum_rc(subtitle_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(3, 3, 4, 3, "mm"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Chile: Comunicando la pandemia",
    subtitle = "Palabras más frecuentes en discursos presidenciales
    entre el 17 de abril y el 5 de mayo",
    x = "Palabras",
    y = "Frecuencia de palabras",
    caption = "Fuente: Prensa Presidencia"
  )

ggsave(paste0("12_05_2020_discursos_presidenciales", ".png"),
       width=6, height=6)
