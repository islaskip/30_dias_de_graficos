###
# title: "Pueblos originarios: País/Araucanía"
# author: "Matías Isla"
###

## Paquetes y datos

library(ggtech)
library(readxl)
library(tools)
library(tidyverse)
library(extrafont)
library(ggsci)
library(sf)


font_import(pattern = "ScholarSans75ExtraBold.ttf")

pueblos_originarios <- read_excel("pueblos_originarios.xls", 
                                  sheet = "Región", skip = 2)

colnames(pueblos_originarios)
names(pueblos_originarios)[5] <- "TOTAL_POBLACION_PERTENECIENTE"
names(pueblos_originarios)[9] <- "LICAN ANTAI"
names(pueblos_originarios)[3] <- "CODIGO_REGION"
names(pueblos_originarios)[2] <- "NOMBRE_REGION"


## Cada pueblo como una columna separada
### Pivot_longer() para juntarlos

datos_pais <- pueblos_originarios %>%
  group_by(ORDEN) %>% 
  filter(ORDEN %in% c(0, 36)) 

datos_pais <- subset(datos_pais, select = -c(ORDEN, TOTAL_POBLACION_PERTENECIENTE, CODIGO_REGION, SEXO))
tidy_inf <- datos_pais %>%
  mutate_if(is.numeric, as.character, is.factor, as.character) %>%
  pivot_longer(-NOMBRE_REGION,
    names_to = "pueblo_originario",
    values_to = "count"
  )

tidy_inf <- tidy_inf %>% 
  group_by(NOMBRE_REGION) %>% 
  mutate(percent = as.numeric(count) / sum(as.numeric(count)) * 100)


## Grafíco

tidy_inf %>% 
  ggplot(aes(x = as.character(NOMBRE_REGION), y = percent, fill = pueblo_originario)) +
  geom_col() +
  scale_fill_brewer(palette="Set3", labels=c("Aymara", "Colla", "Diaguita", "Kawésqar", "Lican Antai", "Mapuche", "Otro", "Pueblo ignorado", "Quechua", "Rapa Nui", "Yagán o Yámana")) +
  coord_polar("y")+
  theme_void() +
    labs(
      title="Pertenencia a pueblo originario:", 
      subtitle = "Comparación entre total país \n y la región de La Araucanía",
      fill=element_blank(), 
      caption = "Fuente de datos: Censo 2017"
      ) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face= "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(size= 9, hjust = 0.5),
          text=element_text(size=13,  family="Roboto Condensed"))
ggsave("17_05_2020_pueblos_originarios.png", dpi=600)

