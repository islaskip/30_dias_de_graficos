---
title: "Educación Superior Chile"
author: "Matías Isla"
date: "14-05-2020"
output: html_document
---
library(readxl)
library(plotly)
library(shiny)
library(dplyr)
library(tidyverse)

estadisticas_por_carrera_2020 <- read_excel("Universidades Chile/Buscador-estadisticas-por-carrera_mifuturo_2020.xlsx", 
                                                              skip = 1)
estadisticas_por_carrera_2020 <- estadisticas_por_carrera_2020[-c(249,250,251), ]


grafico <- plot_ly(estadisticas_por_carrera_2020, x = ~empleabilidad, y = ~titulados, type = 'scatter', mode = 'markers', color = ~area,
               hoverinfo = 'text',
               text = ~paste('</br> Carrera: ', carrera,
                             '</br> Area: ', area))

grafico  %>% layout(title="Educación superior en Chile:",
                      xaxis = list(title="Empleabilidad al primer año"),
                      yaxis = list(title="Cantidad de egresados"),
                    annotations = 
 list(x = 1, y = -0.1, text = "Fuente de datos: Mifuturo.cl", 
      showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto', xshift=0, yshift=0))
