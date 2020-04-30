library(tidyverse)
library(stringr)
library(ggthemes)
library(viridis)

mapa_mundo <- map_data("world")
mapa_mundo <- filter(mapa_mundo, region != "Antarctica")

quantd_disc <- read_csv("data analysis/methodology/country_speeches.csv")

quantd_disc_mre <- quantd_disc %>% filter(position == "MRE")

quantd_disc_mre$region <- quantd_disc_mre$country

quantd_disc_mre <- quantd_disc_mre %>% select(4, 5)

teste <- right_join(quantd_disc_mre, mapa_mundo)

teste <- mutate(teste, n = ifelse(is.na(n), 0, n))

teste$brk <- cut(teste$n, 
                    breaks = c(-1, 0, 15, 50, 585), 
                    labels = c("0", "1 - 14", "15 - 49", "50 - 585"))


ggplot(teste, aes(x = long, y = lat, group = group, fill = brk)) +
  geom_polygon(colour = "black", size = .2, alpha = .5) + theme_void() +
scale_fill_manual(values = c("0" = "gray80", "1 - 14" = "orange", "15 - 49" = "darkgreen", "50 - 585" = "blue")) 


s