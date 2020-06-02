---
title: "Wordcloud CRUCH y Udec"
author: "Matías Isla"
---
# Cargar paquetes y datos

library(tidyverse)
library(tidytext)
library(wordcloud)
library(readxl)
comunicados <- read_excel("comunicados_cruch.xlsx",
  col_types = c("date", "text")
)

# Separar palabras como tokens

comunicados_tokens <- comunicados %>%
  unnest_tokens(word, texto)
comunicados_tokens %>%
  count(word) %>%
  arrange(desc(n))
  
# Eliminar palabras comunes

esp_stop_words <- bind_rows(
  stop_words,
  tibble(
    word = stopwords::stopwords("es", "stopwords-iso"),
    lexicon = "custom"
  )
)

# Wordcloud de comunicados CRUCH

todos_comunicados <- comunicados_tokens%>%
  count(word) %>%
  anti_join(esp_stop_words) %>%
  arrange(desc(n))
todos_comunicados
pal <- brewer.pal(8,"Dark2") # seleccionar paleta de colores

png("02_06_2020_wordcloud_cruch.png", width = 6, height = 6, units = 'in', res = 300)
plot.new()
todos_comunicados %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pal))
dev.off()

# Extraer tweets del hashtag

library(rtweet)
udec <- search_tweets(
  "#udecsinconexión", n = 2000, include_rts = FALSE
)
udec_tokens <- udec %>%
  unnest_tokens(word, text)
  
# Añadir palabras no comunes pero innecesarias

udec_stop_words <- tribble(
  # Nombre de columnas concordantes con lexicon
  ~word, ~lexicon,
  # Palabras propias
  "https", "CUSTOM",
  "udecsinconexion", "CUSTOM",
  "t.co", "CUSTOM",
  "diadelcompleto", "CUSTOM"
)

# Combinar las stop words

esp2_stop_words <- esp_stop_words %>%
  bind_rows(udec_stop_words)

# Wordcloud udec

todos_udec <- udec_tokens%>%
  count(word) %>%
  anti_join(esp2_stop_words) %>%
  arrange(desc(n))

png("02_06_2020_wordcloud_udec.png", width = 6, height = 6, units = 'in', res = 300)
plot.new()
todos_udec %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pal))
dev.off()


