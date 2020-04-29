library(tidyverse)
library(stringr)

mapa_mundo <- map_data("world")

quantd_disc <- read_csv("data analysis/methodology/country_speeches.csv")

quantd_disc_mre <- quantd_disc %>% filter(position == "MRE")

quantd_disc_mre$region <- quantd_disc_mre$country

quantd_disc_mre <- quantd_disc_mre %>% select(4, 5)

teste <- inner_join(quantd_disc_mre, mapa_mundo)

ggplot(teste, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon()

#Resolver problemas dos paises missings, colocar zero. 