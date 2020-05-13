## Cargar paquetes, data y filtrar por año

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(scales)
library(gganimate)
library(hrbrthemes)
library(lubridate)
infant_mortality <- read_excel("OECD top asked data/OECD-Health-Statistics-2018-Frequently-Requested-Data.xls",
  sheet = "Infant mortality rate"
)
im_chile <- infant_mortality %>%
  filter(...1 == "Chile")

## Convertir variables númericas a "character"para poder aplicar pivot
## (algunos años de la base de data vienen como "dbl")

tidy_inf <- im_chile %>%
  mutate_if(is.numeric, as.character, is.factor, as.character) %>%
  pivot_longer(-...1,
    names_to = "year",
    values_to = "count"
  )

## Cambiar año como "Date"
tidy_inf <- tidy_inf %>%
  mutate(count1 = as.double(count))
colnames(tidy_inf)[1] <- "country"
yr <- as.Date(as.character(tidy_inf$year), format = "%Y")
y <- year(yr)

## Gráfico y animación

plot <- tidy_inf %>%
  ggplot(aes(yr, count1, group = 1, xaxt = "5")) +
  geom_line(color = ft_cols$yellow) +
  geom_point(color = ft_cols$yellow) +
  scale_x_date(breaks = "4 years", labels = date_format("%Y")) +
  labs(
    x = "Año", y = "Muertes por cada mil nacimientos",
    title = "Evolución de Mortalidad infantil en Chile (1960-2016)",
    caption = "Fuente de datos: OCDE Health Statistics 2018"
  ) +
  theme_ft_rc(
    axis_title_size = 15
    
  ) +
  transition_reveal(yr)
animate(plot, height = 500, width =1000)

anim_save("~/GitHub/30_dias_de_graficos/graficos/13_05_2020_mortalidad_infantil_chile.gif")
