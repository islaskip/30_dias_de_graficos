---
title: "Situaciones dif�ciles y costeo de ayuda m�dica"
author: "Mat�as Isla"
---

## Paquetes y datos
library(tidyr)
library(dplyr)
library(readxl)
library(waffle)
BBDD_jovenes <- read_excel("jovenes injuv/BBDD_jovenes.xlsx")
head(BBDD_jovenes)
count(BBDD_jovenes, EDAD)

##Filtrar no sabe/no responde
preguntas <- BBDD_jovenes %>% 
  filter(P141_1 %in% c(1, 2, 3, 4, 5) & 
          P141_2 %in% c(1, 2, 3, 4, 5) &
          P141_3 %in% c(1, 2, 3, 4, 5) &
          P141_4 %in% c(1, 2, 3, 4, 5) &
          P141_5 %in% c(1, 2, 3, 4, 5) &
          P141_6 %in% c(1, 2, 3, 4, 5))
preguntas2 <- BBDD_jovenes %>% 
  filter(P144_1 %in% c(1, 2, 3, 4) & 
          P144_2 %in% c(1, 2, 3, 4) &
          P144_3 %in% c(1, 2, 3, 4))
          
## Enumerar las respuestas dadas 
c_dormir <- preguntas %>% 
  count(P141_1)
c_hacer <- preguntas %>% 
  count(P141_2)
c_decisiones <- preguntas %>% 
  count(P141_3)
c_problemas <- preguntas %>% 
  count(P141_4)
c_confianza <- preguntas %>% 
  count(P141_5)
c_terminar <- preguntas %>% 
  count(P141_6)

c_consultas <- preguntas2 %>% 
  count(P144_1)
c_medicamentos <- preguntas2 %>% 
  count(P144_2)
c_examenes <- preguntas2 %>% 
  count(P144_3)

## Creación manual de variables para gráfico waffle
dormir <- c("Nunca"=3355, 
            "Rara vez"=2553, 
            "Algunas veces"=2530,
            "Casi siempre"=785,
            "Siempre"=305)
hacer <- c("Nunca"=3239, 
            "Rara vez"=2394, 
            "Algunas veces"=2900,
            "Casi siempre"=751,
            "Siempre"=244)
decisiones <- c("Nunca"=4605, 
            "Rara vez"=2483, 
            "Algunas veces"=1851,
            "Casi siempre"=438,
            "Siempre"=151)
problemas <- c("Nunca"=4544, 
            "Rara vez"=2572, 
            "Algunas veces"=1871,
            "Casi siempre"=371,
            "Siempre"=170)
confianza <- c("Nunca"=4507, 
            "Rara vez"=2316, 
            "Algunas veces"=1828,
            "Casi siempre"=552,
            "Siempre"=278)
terminar <- c("Nunca"=7598, 
            "Rara vez"=1118, 
            "Algunas veces"=582,
            "Casi siempre"=110,
            "Siempre"=73)

consultas <- c("Nada posible"=2245, 
            "Poco posible"=2065, 
            "Algo posible"=2855,
            "Muy posible"=2373)
medicamentos <- c("Nada posible"=2358, 
            "Poco posible"=2025, 
            "Algo posible"=2827,
            "Muy posible"=2328)
examenes <- c("Nada posible"=2465, 
            "Poco posible"=2042, 
            "Algo posible"=2810,
            "Muy posible"=2221)
## Primer waffle, cada gráfico por separado

waffle1 <- waffle(dormir/100, rows = 5, title = "Durante el último mes, ¿con qué frecuencia\nte has encontrado en las siguientes situaciones?")+
   labs(subtitle = "Sentirte con dificultades para dormir") +
   theme(plot.title = element_text(size = rel(1), face = "bold"),
          plot.subtitle = element_text(size = rel(0.8)),
          legend.position = "none")
waffle2 <- waffle(hacer/100, rows=5) +
    labs(subtitle = "Sentirte con pocas ganas de hacer cosas") +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)),
          legend.position = "none")
waffle3 <- waffle(decisiones/100, rows=5) +
    labs(subtitle = "Sentirte incapaz de tomar decisiones") +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)),
          legend.position = "none")
waffle4 <- waffle(confianza/100, rows=5) +
    labs(subtitle = "Sentirte que no puedes superar tus problemas o dificultades") +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)),
          legend.position = "none")
waffle5 <- waffle(problemas/100, rows=5) +
    labs(subtitle = "Sentirte con ganas de terminar con tu vida o suicidarte",
         caption = "Cada cuadrado representa 100 personas\n Fuente de datos: 9va Encuesta de Juventud - INJUV") +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)),
          plot.caption = element_text(size = rel(0.6)),
          legend.position = "bottom")

## Unir y guardar

png("27_05_2020_enfrentado_situaciones.png", res = 600, width=3000, height=4000)

  iron(waffle1,
       waffle2,
       waffle3,
       waffle4,
       waffle5)
  invisible(dev.off())

## Segundo waffle, cada gráfico por separado
waffle1_2 <- waffle(consultas/100, rows = 5, title = "Si quisieras o necesitaras recibir atención\nprofesional, ¿qué tan posible sería para ti\no tu familia costear esa atención por\nun periodo prolongado de tiempo…?")+
   labs(subtitle = "Consultas con un psicólogo o psiquiatra") +
   theme(plot.title = element_text(size = rel(0.8), face = "bold"),
          plot.subtitle = element_text(size = rel(0.6)),
         legend.position = "none")
waffle2_2 <- waffle(medicamentos/100, rows=5) +
    labs(subtitle = "Medicamentos para un tratamiento") +
    theme(plot.title = element_text(size = rel(0.8)),
          plot.subtitle = element_text(size = rel(0.6)),
          legend.position = "none")
waffle3_2 <- waffle(examenes/100, rows=5) +
    labs(subtitle = "Exámenes solicitados por un psiquiatra",
         caption = "Cada cuadrado representa 100 personas\n Fuente de datos: 9va Encuesta de Juventud - INJUV") +
    theme(plot.title = element_text(size = rel(0.8)),
          plot.subtitle = element_text(size = rel(0.6)),
          plot.caption = element_text(size = rel(0.4)),
          legend.position = "bottom",
          legend.text = element_text(size = rel(0.5)))

## Unir y guardar

png("27_05_2020_costear_ayuda.png", res = 600, width=3000, height=2500)

  iron(waffle1_2,
       waffle2_2,
       waffle3_2)
  invisible(dev.off())