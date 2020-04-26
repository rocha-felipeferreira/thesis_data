
# Packages ----------------------------------------------------------------

library(quanteda)
library(readtext)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(tidytext)

thesis_theme <- function(){
  theme(axis.title = element_text(size = rel(1.2), colour = "black", face = "bold.italic"),
        axis.text = element_text(size = rel(1.2), colour = "black"), 
        axis.ticks = element_blank(), 
        legend.text = element_text(size = rel(1.2)), 
        legend.title = element_blank())
}

# Data Source -------------------------------------------------------------

corpus_file_source <- "corpus/resenha de PEB/corpus_1995_2019/*.txt"

corpus_data_source <- readtext(file = corpus_file_source, 
                        docvarsfrom = "filenames", 
                        docvarnames = c("position", "speaker", "date", "language", "city", "country", "continent", "type")) %>% as_tibble()


# Fixing and Adding Variables ---------------------------------------------

corpus_data_source$position <- as_factor(corpus_data_source$position)

corpus_data_source$realm <- ifelse(corpus_data_source$country != "brasil", 1, 0) %>% 
  factor(., levels = c(0, 1), labels = c("domestic", "international"))
    
corpus_data_source$speaker <- factor(corpus_data_source$speaker, levels = c("FHC", "L.F.Lampreia", "C.Lafer", "LULA", "C.Amorim", "DILMA", "A.Patriota", "L.A.Figueredo", "M.Vieira", "J.Serra", "A.Nunes", "E.Araujo"), labels = c("FHC", "Lampreia", "Lafer", "Lula", "Amorim", "Dilma", "Patriota", "Figueiredo", "Vieira", "Serra", "Nunes", "Araújo"))

corpus_data_source$language <- as_factor(corpus_data_source$language)

corpus_data_source$city <- as_factor(corpus_data_source$city)

corpus_data_source$country <- as_factor(corpus_data_source$country)

corpus_data_source$continent <- as_factor(corpus_data_source$continent)

corpus_data_source$date <- dmy(corpus_data_source$date)

corpus_data_source$year <- year(corpus_data_source$date)

corpus_data_source$type <- str_remove_all(corpus_data_source$type, "\\*")

corpus_data_source$type <- as_factor(corpus_data_source$type)

corpus_data_source$dyads <- ifelse(corpus_data_source$date < "2001-01-28", "FHC and Lampreia", ifelse(corpus_data_source$date >= "2001-01-29" & corpus_data_source$date < "2003-01-01", "FHC and Lafer", ifelse(corpus_data_source$date >= "2003-01-01" & corpus_data_source$date <= "2010-12-31", "Lula and Amorim", ifelse(corpus_data_source$date >= "2011-01-01" & corpus_data_source$date < "2013-08-26", "Dilma and Patriota", ifelse(corpus_data_source$date >="2013-08-28" & corpus_data_source$date < "2015-01-01", "Dilma and Figueiredo", ifelse(corpus_data_source$date >= "2015-01-02" & corpus_data_source$date < "2016-05-13", "Dilma and Vieira", ifelse(corpus_data_source$date >= "2016-05-13" & corpus_data_source$date <= "2017-02-22", "Temer and Serra", ifelse(corpus_data_source$date > "2017-02-22" & corpus_data_source$date < "2019-01-02", "Temer and Nunes", "Bolsonaro and Araújo"))))))))

#fixing mistakes
corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_C.Lafer_26.01.2001_pt_sao.paulo_brasil_america_entrevista.txt"] <- "FHC and Lafer"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_C.Amorim_02.01.2011_pt_brasilia_brasil_america_discurso.txt"] <- "Lula and Amorim"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_A.Patriota_28.08.2013_pt_brasilia_brasil_america_discurso.txt"] <- "Dilma and Patriota"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_A.Patriota_28.08.2013_pt_brasilia_brasil_america_discurso*.txt"] <- "Dilma and Patriota"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_A.Patriota_29.08.2013_pt_brasilia_brasil_america_discurso.txt"] <- "Dilma and Patriota"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_A.Patriota_29.08.2013_pt_brasilia_brasil_america_discurso*.txt"] <- "Dilma and Patriota"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_M.Vieira_18.05.2016_pt_brasilia_brasil_america_discurso.txt"] <- "Dilma and Vieira"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_M.Vieira_26.06.2016_esp_varios_varios_america_artigo.txt"] <- "Dilma and Vieira"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_J.Serra_07.03.2017_pt_brasilia_brasil_america_discurso.txt"] <- "Temer and Serra"

corpus_data_source$dyads[corpus_data_source$doc_id == "MRE_A.Nunes_02.01.2019_pt_brasilia_brasil_america_discurso.txt"] <- "Temer and Nunes"

corpus_data_source$dyads <- as_factor(corpus_data_source$dyads)

# Done fixing the mistakes!

corpus_data_source$term_office_presid <- ifelse(corpus_data_source$date < "1999-01-01", "FHC1", ifelse(corpus_data_source$date >= "1999-01-01" & corpus_data_source$date < "2003-01-01", "FHC2", ifelse(corpus_data_source$date >= "2003-01-01" & corpus_data_source$date < "2007-01-01", "Lula1", ifelse(corpus_data_source$date >= "2007-01-01" & corpus_data_source$date < "2011-01-01", "Lula2", ifelse(corpus_data_source$date >= "2011-01-01" & corpus_data_source$date < "2015-01-01", "Dilma1", ifelse(corpus_data_source$date >= "2015-01-02" & corpus_data_source$date <= "2016-05-02", "Dilma2", ifelse(corpus_data_source$date > "2016-05-02" & corpus_data_source$date < "2019-01-02", "Temer", "Bolsonaro")))))))

#fixing mistakes

corpus_data_source$term_office_presid[corpus_data_source$doc_id == "MRE_A.Nunes_02.01.2019_pt_brasilia_brasil_america_discurso.txt"] <- "Temer"

corpus_data_source$term_office_presid[corpus_data_source$doc_id == "MRE_C.Amorim_02.01.2011_pt_brasilia_brasil_america_discurso.txt"] <- "Lula2"

corpus_data_source$term_office_presid[corpus_data_source$doc_id == "MRE_M.Vieira_26.06.2016_esp_varios_varios_america_artigo.txt"] <- "Dilma2"

# Done fixing the mistakes!

corpus_data_source$term_office_presid <- as_factor(corpus_data_source$term_office_presid)

corpus_data_source$diplomacy_as_profession <- ifelse(corpus_data_source$speaker == "Lampreia" | corpus_data_source$speaker == "Amorim" | corpus_data_source$speaker == "Patriota" | corpus_data_source$speaker == "Figueiredo" | corpus_data_source$speaker == "Vieira" | corpus_data_source$speaker == "Araújo", "Yes", ifelse(corpus_data_source$speaker == "Lafer" | corpus_data_source$speaker == "Serra" | corpus_data_source$speaker == "Nunes", "No", "Not the case")) %>% as_factor()

corpus_data_source$president_party <- ifelse(corpus_data_source$term_office_presid == "FHC1" | corpus_data_source$term_office_presid  == "FHC2", "PSDB", ifelse(corpus_data_source$term_office_presid  == "Lula1" | corpus_data_source$term_office_presid == "Lula2" | corpus_data_source$term_office_presid  == "Dilma1" | corpus_data_source$term_office_presid  == "Dilma2", "PT", ifelse(corpus_data_source$term_office_presid  == "Temer", "PMDB", "PSL*"))) %>% as_factor()

corpus_data_source$level_presid_diplomacy <- ifelse(corpus_data_source$term_office_presid == "FHC1" | corpus_data_source$term_office_presid == "FHC2", "Medium", ifelse(corpus_data_source$term_office_presid == "Lula1" | corpus_data_source$term_office_presid == "Lula2", "High", ifelse(corpus_data_source$term_office_presid == "Dilma1" |corpus_data_source$term_office_presid == "Dilma2" | corpus_data_source$term_office_presid == "Temer", "Low", "Not enough data"))) %>% as_factor()

corpus_data_source$speaker_reshaped <- corpus_data_source$speaker %>% as.character()

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "FHC" & corpus_data_source$date < "1999-01-01"] <- "FHC I"

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "FHC" & corpus_data_source$date >= "1999-01-01" & corpus_data_source$date < "2003-01-01"] <- "FHC II"

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "Lula" & corpus_data_source$date >= "2003-01-01" & corpus_data_source$date < "2007-01-01"] <- "Lula I"

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "Lula" & corpus_data_source$date >= "2007-01-01" & corpus_data_source$date < "2011-01-01"] <- "Lula II"

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "Amorim" & corpus_data_source$date >= "2003-01-01" & corpus_data_source$date < "2007-01-01"] <- "Amorim I"

corpus_data_source$speaker_reshaped[corpus_data_source$speaker == "Amorim" & corpus_data_source$date >= "2007-01-01" & corpus_data_source$date < "2011-01-01"] <- "Amorim II"

corpus_data_source$speaker_reshaped[corpus_data_source$doc_id == "MRE_C.Amorim_02.01.2011_pt_brasilia_brasil_america_discurso.txt"] <- "Amorim II"

# Speeches per Year ------------------------------------------------------

speech_year <- corpus_data_source %>% select(year, position)
speech_year <- speech_year %>% 
  group_by(year, position) %>% 
  count()

speech_year <- ggplot(speech_year, aes(x = year, y = n, fill = position)) + 
  geom_line() + geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c("MRE" = "black", "PRES" = "white")) +
  thesis_theme() +
  xlab("Ano") + ylab("Nº de Discursos") +
  scale_x_continuous(breaks = seq(1995, 2019, 3))

# ggsave(filename = "freq_disc_ano.png", plot = speech_year, dpi = 500, width = 7.6, height = 2.9) 

# Speaker -----------------------------------------------------------------

speaker <- corpus_data_source %>% select(speaker_reshaped) %>% 
  group_by(speaker_reshaped) %>% count()

speaker <- ggplot(speaker, aes(x = reorder(speaker_reshaped, n), y = n)) +
 geom_col(fill = "white", colour = "black") +
  geom_text(aes(label = n), hjust = 1.3) +
  coord_flip() +
  scale_y_discrete(expand = c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_text(size = rel(1.2), colour = "black")) 

# ggsave(filename = "freq_disc_emissor.png", plot = speaker, dpi = 500, width = 5.9, height = 3.6) 

# Average discourse length ----------------------------------------

average_discourse_length <- readtext(file = corpus_file_source, 
                                     docvarsfrom = "filenames", 
                                     docvarnames = c("position", "speaker", "date", "language", "city", "country", "continent", "type")) %>% corpus() %>% summary(1784)


disc_length <- ggplot(average_discourse_length, aes(x = Types, fill = position)) + 
  geom_histogram(colour = "black", alpha = .4, size = .6) + theme_minimal() +
  thesis_theme() + ylab("Contagem") + xlab("Nº de Types") +
  theme(panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = c("MRE" = "black", "PRES" = "white"))

# ggsave(filename = "freq_disc_length.png", plot = disc_length, dpi = 500, width = 5.7, height = 2.8) 


# Continent and Year ------------------------------------------------------

continent_year <- corpus_data_source %>% select(continent, year, position)
continent_year <- drop_na(continent_year)
continent_year <- continent_year %>% group_by_all() %>% count()
continent_year$continent <- str_to_upper(continent_year$continent)

continent_year$continent <- factor(continent_year$continent, levels = c("AMERICA", "ASIA", "AFRICA", "EUROPA"), labels = c("AMÉRICA", "ÁSIA", "ÁFRICA", "EUROPA"))

continent_plot <- ggplot(continent_year, aes(x = year, y = n, fill = position)) +
  geom_col(colour = "black") +
  facet_wrap(~continent) + thesis_theme() +
  xlab("Ano") + ylab("Nº de Discursos") + 
  scale_x_continuous(breaks = seq(1995, 2019, 6)) +
  scale_fill_manual(values = c("MRE" = "gray56", "PRES" = "white")) +
  theme(strip.text = element_text(size = rel(1.2)), 
        panel.background = element_rect(fill = "white", colour = "black"), panel.grid.major = element_line(colour = "gray80"), 
        strip.background = element_rect(colour = "black")) 

#ggsave(filename = "freq_disc_continent.png", plot = continent_plot, dpi = 500, width = 8, height = 3.6) 


# Realm and Languague-------------------------------------------------------------------

realm_languague <- corpus_data_source %>% select(language, realm) 
realm_languague$language <- fct_explicit_na(realm_languague$language)
realm_languague$realm <- fct_explicit_na(realm_languague$realm)

realm_languague %>% group_by_all() %>% count() %>% knitr::kable()


# Type of Speech ----------------------------------------------------------

table(corpus_data_source$type)


# Country -----------------------------------------------------------------

country_speeches <- corpus_data_source %>% select(country, position, continent) %>% drop_na()

country_speeches$continent <- str_to_upper(country_speeches$continent)
country_speeches$country <- str_replace_all(country_speeches$country, pattern = "\\.", replacement = " ")
country_speeches$country <- str_to_title(country_speeches$country)

country_speeches$continent <- factor(country_speeches$continent, levels = c("AMERICA", "ASIA", "AFRICA", "EUROPA"), labels = c("AMÉRICA", "ÁSIA", "ÁFRICA", "EUROPA"))

country_speeches <- country_speeches %>% group_by_all() %>% count() %>% ungroup()

# write_csv(country_speeches, "country_speeches.csv") #edit country names

country_speeches <- read_csv("data analysis/methodology/country_speeches.csv")

country_speeches_plot <- ggplot(country_speeches, aes(x = reorder(country, n), y = n)) +
  geom_line(aes(group = country)) + 
  geom_point(aes(fill = position), size = 2.5, shape = 21) + 
  facet_wrap(~continent, scales = "free", strip.position = "right") + coord_flip() + 
  thesis_theme() +
  scale_fill_manual(values = c("MRE" = "white", "PRES" = "black")) +
  theme(strip.text = element_text(size = rel(1.2)), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray75"),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), strip.background = element_rect(fill = "white", colour = "black"), 
        strip.text.y = element_text(face = "bold")) +
  ylab("Nº de Discursos") + xlab("Países")

country_speeches_plot

# ggsave(filename = "freq_disc_country.png", plot = country_speeches_plot, dpi = 500, width = 9, height = 11) 


# Speeches per Dyad -------------------------------------------------------

speech_dyad <- corpus_data_source %>% select(dyads, position) 
speech_dyad <- speech_dyad %>% 
  group_by(dyads, position) %>% 
  count()

ggplot(speech_dyad, aes(x = reorder(dyads, n), y = n, fill = position)) +
  geom_col(colour = "black", position = "dodge") + coord_flip() + 
  scale_fill_manual(values = c("MRE" = "turquoise4", "PRES" = "white")) 
