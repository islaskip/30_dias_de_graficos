---
title: "Elecciones covid-19"
author: "Matías Isla"
---

# Paquetes y datos

library(geojsonio)
library(tidyverse)
library(highcharter)
library(readxl)

elecciones <- read_excel("elecciones.xlsx")

# Obtención de mapa mundo

getContent <- function(url) {
  library(httr)
  content(GET(url))
}

world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")

world <- jsonlite::fromJSON(world, simplifyVector = FALSE)

# Highchart

highchart(type = "map") %>%
  hc_chart(backgroundColor = "white") %>%
  hc_add_series(
    mapData = world, 
    showInLegend = FALSE, 
    nullColor = "#f1faee",
    borderWidth = 0,
    data = elecciones,
    joinBy = c("name", "Country/region"),
    value = "value",
    name = "Estado de la elección"
    ) %>% 
  hc_tooltip(
    pointFormat = "<b>{point.Country/region}</b>: <br>{point.Status} <br>
    Fecha original: {point.Original date} <br>
    Nueva fecha: {point.Reprogramada al} <br>
    Tipo de elección: {point.Tipo}"
    ) %>%
  hc_colorAxis(dataClassColor="category", # Colorear 0 y 1 como variables categóricas
                 dataClasses = list(list(from=0, to=0, color="#ee6c4d", name="Reprogramada"),
                                  list(from=1, to=1, color="#457b9d", name="Pospuesta"))) %>% 
  hc_title(
    text = "<b>Elecciones reprogramadas y pospuestas por COVID-19</b>"
  ) %>% 
  hc_subtitle(
    text = "Fuente de datos: Foreign Policy (27 de mayo)"
  )

