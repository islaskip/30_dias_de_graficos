---
### title: "Socialización política por tramo de ingresos Chile"
### author: "Matías Isla"
---

## Cargar librerias y datos(fuente: http://www.injuv.gob.cl/noticias/9encuesta)

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(readxl)
BBDD_jovenes <- read_excel("jovenes injuv/BBDD_jovenes.xlsx")
head(BBDD_jovenes)
count(BBDD_jovenes, EDAD)

## Filtrar preguntas (eliminando "no sabe/no responden") y crear variables Boolean

preguntas <- BBDD_jovenes %>% 
  filter(P40_3 %in% c(1, 2) & B05 <= 7 & P38 <= 4) %>%
  mutate(pol3 = P40_3 == 1) %>% 
  mutate(pol4 = P38 >= 3)

## Gráficos

graf1 <- preguntas %>% 
  ggplot(aes(pol4, group=B05)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_y_continuous(labels=scales::percent,breaks = seq(0, 1,.1), limits = c(0, 0.9)) +
  scale_x_discrete(labels = c("Nada/Poco", "Interesado/Muy"))+
  labs(
    title= "¿Qué tan interesado/a estás en la política?",
    subtitle = "Desagregado por tramo de ingreso mensual hogar, participantes entre 15 y 29 años",
    caption= "Fuente de datos: 9va Encuesta de Juventud - INJUV")+
    xlab("")+
    ylab("Porcentaje")+
  facet_grid(~B05) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

graf2 <- preguntas %>% 
  ggplot(aes(pol3, group=B05)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_x_discrete(labels = c("No", "Sí"))+
  scale_y_continuous(labels=scales::percent,breaks = seq(0, 1,.1), limits = c(0, 0.715)) +
  labs(
    title= "¿Conversa de política con su familia, pares, amigos, amigas u otras personas?",
    subtitle = "Desagregado por tramo de ingreso mensual hogar, participantes entre 15 y 29 años",
    caption= "Fuente de datos: 9va Encuesta de Juventud - INJUV")+
    xlab("")+
    ylab("Porcentaje")+
  facet_grid(~B05)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

graf1
ggsave("15_05_2020_interes_politica.png")
graf2
ggsave("15_05_2020_socializacion_politica.png")



```

