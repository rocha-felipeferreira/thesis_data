library(tidyverse)
library(stringr)
library(ggthemes)

mapa_mundo <- map_data("world")

quantd_disc <- read_csv("data analysis/methodology/country_speeches.csv")

quantd_disc_mre <- quantd_disc %>% filter(position == "MRE")

quantd_disc_mre$region <- quantd_disc_mre$country

quantd_disc_mre <- quantd_disc_mre %>% select(4, 5)

teste <- right_join(quantd_disc_mre, mapa_mundo)

teste <- mutate(teste, n = ifelse(is.na(n), 0, n))


ggplot(teste, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(colour = "black") + theme_void()


#Resolver problemas dos paises missings, colocar zero. 