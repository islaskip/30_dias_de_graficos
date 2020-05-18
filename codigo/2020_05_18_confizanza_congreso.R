###
### title: "LAPOP confianza en Congreso"
### author: "Matías Isla"
###

## Paquetes y datos

library(tidyverse)
library(haven)
library(ggridges)
library(ggthemes)
lapop_2010 <- read_dta("lapop_2010.dta")
lapop_2012 <- read_dta("lapop_2012.dta")
lapop_2014 <- read_dta("lapop_2014.dta")
lapop_2017 <- read_dta("lapop_2017.dta")
lapop_2019 <- read_dta("lapop_2019.dta")


## Fusionar bases de datos para guardar solo una variable por año

lapop_2010 <- lapop_2010 %>% 
  select(b13) %>% 
  mutate(year = "2010")
lapop_2012 <- lapop_2012 %>% 
  select(b13) %>% 
  mutate(year = "2012")
lapop_2014 <- lapop_2014 %>% 
  select(b13) %>% 
  mutate(year = "2014")
lapop_2017 <- lapop_2017 %>% 
  select(b13) %>% 
  mutate(year = "2017")
lapop_2019 <- lapop_2019 %>% 
  select(b13) %>% 
  mutate(year = "2019")

join1 <- full_join(lapop_2010, lapop_2012)
join2 <- full_join(join1, lapop_2014)
join3 <- full_join(join2, lapop_2017)
join_final <- full_join(join3, lapop_2019)

## Gráfico

join_final %>% 
  ggplot(aes(x = b13, y = reorder(year, sort(as.numeric(year), decreasing = TRUE)), fill = ..x..)) +
  geom_density_ridges_gradient()+
  scale_fill_distiller(palette = "Spectral", trans = "reverse")+
  scale_x_discrete(limits=c("Nada","2","3","4","5","6","Mucho"))+
  labs(
    title="Evolución de confianza al poder legislativo en Chile (2010-2019)",
    y=element_blank(),
    x="¿Hasta qué punto tiene confianza usted en el Congreso Nacional?",
    caption="Fuente de datos: Proyecto de Opinión Pública de América Latina (LAPOP)"
  )+
   theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust=0.5, face="bold"),
      plot.caption = element_text(size= 9),
      text=element_text(size=13,  family="Roboto Condensed"))
)
ggsave("18_05_2020_confianza_congreso.png", dpi=600)


 
 
 