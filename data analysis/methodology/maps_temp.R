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
  geom_polygon(colour = "black", size = .2, alpha = .9) + theme_void() +
  scale_fill_brewer(palette="PuBu") +
  theme(legend.text = element_text(size = rel(1)), 
        legend.title = element_blank(), 
        legend.position= c(0.09, 0.15), 
        plot.title = element_text(hjust = .5, face = "bold", size = rel(1))) +
  labs(title = "Frequência de Discursos dos Chanceleres por País")
  

# Presidentes


quantd_disc_pres <- quantd_disc %>% filter(position == "PRES") %>% 
  select(c(1,4))

colnames(quantd_disc_pres) <- c("region", "n")

teste_pres <- left_join(mapa_mundo, quantd_disc_pres)
teste_pres <- mutate(teste_pres, n = ifelse(is.na(n), 0, n))

teste_pres$brk <- cut(teste_pres$n, 
                 breaks = c(-1, 0, 15, 50, 432), 
                 labels = c("0", "1 - 14", "15 - 49", "50 - 432"))


ggplot(teste_pres, aes(x = long, y = lat, group = group, fill = brk)) +
  geom_polygon(colour = "black", size = .2, alpha = .9) + theme_void() +
  scale_fill_brewer(palette="PuBu") +
  theme(legend.text = element_text(size = rel(1)), 
        legend.title = element_blank(), 
        legend.position= c(0.09, 0.15), 
        plot.title = element_text(hjust = .5, face = "bold", size = rel(1))) +
  labs(title = "Frequência de Discursos dos Presidentes por País")


