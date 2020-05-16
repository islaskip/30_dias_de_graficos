
###title: "Harry Potter y la piedra filosofal: Menciones entre personajes"
###author: "Matías Isla"

## Paquetes y datos

require(plyr)
library(tidytext)
library(readr)
library(igraph)
library(tools)  
library(extrafont)
library(arcdiagram)

potter1 <- read_delim("potter1.csv", ";", 
    escape_double = FALSE, col_types = cols(X3 = col_skip()), 
    trim_ws = TRUE) # https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset?select=Harry+Potter+3.csv


## Individualizar personajes

dialogo = potter1[,2]
personajes = potter1[,1]
chars = c(personajes)
char_names = table(chars)
y = as.vector(char_names)
yord = order(y, decreasing=TRUE)


## Grafíco de frecuencias

top_chars = head(char_names[yord], 20)
barplot(top_chars, las=2, cex.names=0.7, 
        border=NA, ylim=c(0,500), ylab="numero de dialogos")
title(c("Harry Potter 1"),
      cex.main=0.9)

## Personajes más mencionados
who_top_chars = names(top_chars)

## Tokenizar dialogos e igualar tokens con lista de personajes. Ahora la primera columna tendra al personaje que habla y la segunda al personaje que menciona.

potter_biagrams <- potter1 %>%
 unnest_tokens(bigram, Sentence)

bigrams_filtered <- potter_biagrams %>%
 filter(bigram %in% tolower(who_top_chars))

bigrams_filtered$Character <- toTitleCase(tolower(as.character(bigrams_filtered$Character)))
bigrams_filtered$bigram <- toTitleCase(tolower(as.character(bigrams_filtered$bigram)))

bigram_counts <- bigrams_filtered %>%
  group_by(Character) %>% 
 count(bigram, sort = TRUE) %>% 
  filter(n > 1)

## Diagrama de arco

bigram_graph <- bigram_counts %>%
 graph_from_data_frame()
vec <- get.edgelist(bigram_graph)

degrees<-(degree(graph.edgelist(vec, directed = TRUE)))
png("16_05_2020_harry_potter.png", width = 9, height = 5, units = 'in', res = 300)
arcplot(vec,lwd.arcs = 6, col.arcs = hsv(0, 0, 0.2, 0.25), cex.nodes = log(degrees), pch.nodes=21, cex.labels = 0.9, bg.nodes="#2f76a9", lwd.nodes = 2, main="Harry Potter y la piedra filosofal:\nMenciones entre personajes", family = "Raleway", sub="Matías Isla / @islaskip", font.sub=3,cex.sub=0.5)
dev.off()

