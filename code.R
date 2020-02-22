setwd("/home/agricolamz/work/materials/2019.10.23-24_IJaz_Alekseev/prezi")
library(tidyverse)
library(ggforce)
library(extrafont)
loadfonts()
theme_set(theme_bw()+
            theme(text = element_text(size = 19, family = "Brill")))

# create qrcodes ----------------------------------------------------------
library(qrcode)
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf")
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf",
           wColor = "#0099CC", bColor = "white")

# create a map ------------------------------------------------------------

map <- read_csv("data/andi_botlikh_villages.csv")
map$eng[3] <- " "
map$eng[c(1, 2, 4, 7, 9, 10, 14, 16, 17)] <- paste(".      ", map$eng[c(1, 2, 4, 7, 9, 10, 14, 16, 17)])

library(lingtypology)
map.feature(map$language,
            features = map$language,
            latitude = map$lat,
            longitude = map$lon,
            label = map$eng,
            label.hide = FALSE,
            width = log(map$population)*1.5,
            tile = "Stamen.TerrainBackground",
            zoom.level = 11) ->
  m1

map.feature(map$language,
            latitude = map$lat,
            longitude = map$lon,
            label = map$eng,
            width = log(map$population)*2,
            tile = "Esri.WorldStreetMap",
            minimap = TRUE, 
            zoom.level = 5) ->
  m2

leafsync::latticeView(m1, m2)


# calculate amount of people ----------------------------------------------

map %>% 
  group_by(language) %>% 
  summarise(sum(population))

# create botlikh venn diagram ---------------------------------------------
botlikh <- read_csv("data/botlikh.csv")

botlikh %>% 
  count(bind_id) %>% 
  na.omit() %>% 
  count() %>% 
  summarise(n = n - 77,
            sa = 8464 - n,
            aa = 6821 - n) %>% 
  pivot_longer(names_to = "type", values_to = "number", n:aa) %>% 
  mutate(prop = round(number/sum(number), 3),
         label = paste0(number, " lexemes\n", prop*100, "%"),
         x = c(-0.04, 0.25, -0.3), 
         r = 0,
         y  = 0) ->
  values

data.frame(x = c(0.15, -0.15),
           y = 0,
           y2 = c(0.39, 0.31),
           r = c((0.45+0.24)/2, (0.31+0.24)/2),
           label = c('Saidova, Abusov 2012\n8464 lexemes', 
                     'Alekseev, Azaev 2019\n6821 lexemes')) %>% 
  ggplot(aes(x0 = x, y0 = y, r = r, label = label)) +
  geom_circle(aes(fill = label), alpha = .2, size = 1, show.legend = FALSE) +
  geom_text(aes(x=x*1.9, y2), family = "Brill", size = 5)+
  geom_text(data = values, aes(x = x, y = y, label = label), family = "Brill", size = 6)+
  coord_fixed()+
  theme_void()+
  ylim(-0.4, 0.4) ->
  venn

ggsave(filename = "images/03_venn.png", venn, device = "png", width = 6, height = 5)

# analyse phonology -------------------------------------------------------
stress <- "́"

botlikh %>% 
  group_by(bind_id, reference) %>% 
  mutate(n = n()) %>% 
  filter(n<2) %>% 
  select(bind_id, reference, lemma, text) %>%
  pivot_wider(names_from = reference, values_from = c(lemma, text)) %>% 
  na.omit() %>% 
  rename(lemma_alekseev_azaev = `lemma_Alekseev 2006`,
         lemma_saidova_abusov = `lemma_Saidova, Abusov 2012`,
         text_alekseev_azaev = `text_Alekseev 2006`,
         text_saidova_abusov = `text_Saidova, Abusov 2012`) %>% 
  ungroup() %>% 
  mutate(new_id = 1:n()) %>%   # filter(new_id == 488)
  #  pivot_longer(names_to = "reference", values_to = "lemma") %>%
  mutate(lemma_alekseev_azaev = str_replace_all(lemma_alekseev_azaev, "i", "I"),
         lemma_alekseev_azaev = str_replace_all(lemma_alekseev_azaev, "І", "I"),
         lemma_saidova_abusov = str_replace_all(lemma_saidova_abusov, "i", "I"),
         lemma_saidova_abusov = str_replace_all(lemma_saidova_abusov, "І", "I"),
         ipa_aa = str_remove_all(lemma_alekseev_azaev, " ?//.*"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", stress, "ᴴ"), "-ʔ-a`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", "ᴴ"), "-ʔ-ã-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", stress), "-ʔ-a`-"),
         ipa_aa = str_replace_all(ipa_aa, "-а:", "-ʔ-aː-"),
         ipa_aa = str_replace_all(ipa_aa, "-а", "-ʔ-a-"),
         ipa_aa = str_remove_all(ipa_aa, "[-/=\\\\]"),
         ipa_aa = str_replace_all(ipa_aa, "ʔa", "-ʔ-a-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", stress, "ᴴ"), "-ã`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", "ᴴ"), "-ã-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", stress), "-a`-"),
         ipa_aa = str_replace_all(ipa_aa, "а", "-a-"),
         ipa_aa = str_replace_all(ipa_aa, "б", "-b-"),
         ipa_aa = str_replace_all(ipa_aa, "гъв", "-ʁʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "гъ", "-ʁ-"),
         ipa_aa = str_replace_all(ipa_aa, "гI", "-ʕ-"),
         ipa_aa = str_replace_all(ipa_aa, "гь", "-h-"),
         ipa_aa = str_replace_all(ipa_aa, "гь", "-h-"),
         ipa_aa = str_replace_all(ipa_aa, "гв", "-ɡʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "г", "-ɡ-"),
         ipa_aa = str_replace_all(ipa_aa, "дж", "-dʒ-"),
         ipa_aa = str_replace_all(ipa_aa, "д", "-d-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", stress, "ᴴ"), "-ẽ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", "ᴴ"), "-ẽ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", stress), "-e`-"),
         ipa_aa = str_replace_all(ipa_aa, "е", "-e-"),
         ipa_aa = str_replace_all(ipa_aa, "ж", "-ʒ-"),
         ipa_aa = str_replace_all(ipa_aa, "зв", "-zʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "з", "-z-"),
         ipa_aa = str_replace_all(ipa_aa, "з", "-z-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", stress, "ᴴ"), "-ĩ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", "ᴴ"), "-ĩ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", stress), "-i`-"),
         ipa_aa = str_replace_all(ipa_aa, "и", "-i-"),
         ipa_aa = str_replace_all(ipa_aa, "й", "-j-"),
         ipa_aa = str_replace_all(ipa_aa, "кIкIв", "-k'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кькьв", "-tɬ'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кIкI", "-k'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "кькь", "-tɬ'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "ккв", "-kːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "къв", "-qχ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кIв", "-k'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кьв", "-tɬ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кк", "-kː-"),
         ipa_aa = str_replace_all(ipa_aa, "кI", "-k'-"),
         ipa_aa = str_replace_all(ipa_aa, "кв", "-kʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "къ", "-qχ'-"),
         ipa_aa = str_replace_all(ipa_aa, "кь", "-tɬ'-"),
         ipa_aa = str_replace_all(ipa_aa, "к", "-k-"),
         ipa_aa = str_replace_all(ipa_aa, "лълъв", "-ɬːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лIлIв", "-tɬːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лълъ", "-ɬː-"),
         ipa_aa = str_replace_all(ipa_aa, "лIлI", "-tɬː-"),
         ipa_aa = str_replace_all(ipa_aa, "лъв", "-ɬʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лIв", "-tɬʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лъ", "-ɬ-"),
         ipa_aa = str_replace_all(ipa_aa, "лI", "-tɬ-"),
         ipa_aa = str_replace_all(ipa_aa, "л", "-l-"),
         ipa_aa = str_replace_all(ipa_aa, "м", "-m-"),
         ipa_aa = str_replace_all(ipa_aa, "н", "-n-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", stress, "ᴴ"), "-õ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", "ᴴ"), "-õ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", stress), "-o`-"),
         ipa_aa = str_replace_all(ipa_aa, "о", "-o-"),
         ipa_aa = str_replace_all(ipa_aa, "п", "-p-"),
         ipa_aa = str_replace_all(ipa_aa, "пI", "-p'-"),
         ipa_aa = str_replace_all(ipa_aa, "пп", "-pː-"),
         ipa_aa = str_replace_all(ipa_aa, "р", "-r-"),
         ipa_aa = str_replace_all(ipa_aa, "сс", "-sː-"),
         ipa_aa = str_replace_all(ipa_aa, "св", "-sʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "с", "-s-"), 
         ipa_aa = str_replace_all(ipa_aa, "тI", "-t'-"),
         ipa_aa = str_replace_all(ipa_aa, "тт", "-tː-"),
         ipa_aa = str_replace_all(ipa_aa, "т", "-t-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", stress, "ᴴ"), "-ũ'-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", "ᴴ"), "-ũ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", stress), "-u`-"),
         ipa_aa = str_replace_all(ipa_aa, "у", "-u-"),
         ipa_aa = str_replace_all(ipa_aa, "ф", "-f-"),
         ipa_aa = str_replace_all(ipa_aa, "ххв", "-χːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хьв", "-çʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хъв", "-qχʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хх", "-χː-"),
         ipa_aa = str_replace_all(ipa_aa, "хI", "-ħ-"),
         ipa_aa = str_replace_all(ipa_aa, "хъ", "-qχ-"),
         ipa_aa = str_replace_all(ipa_aa, "хь", "-ç-"),
         ipa_aa = str_replace_all(ipa_aa, "х", "-χ-"),
         ipa_aa = str_replace_all(ipa_aa, "цIцIв", "-ts'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "цIцI", "-ts'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "цIв", "-ts'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ццв", "-tsːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "цI", "-ts'-"),
         ipa_aa = str_replace_all(ipa_aa, "цц", "-tsː-"),
         ipa_aa = str_replace_all(ipa_aa, "цв", "-tsʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ц", "-ts-"),
         ipa_aa = str_replace_all(ipa_aa, "чIчIв", "-tʃ'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "чIчI", "-tʃ'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "чIв", "-tʃ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ччв", "-tʃːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "чI", "-tʃ'-"),
         ipa_aa = str_replace_all(ipa_aa, "чч", "-tʃː-"),
         ipa_aa = str_replace_all(ipa_aa, "чв", "-tʃʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ч", "-tʃ-"),
         ipa_aa = str_replace_all(ipa_aa, "ш", "-ʃ-"),
         ipa_aa = str_replace_all(ipa_aa, "щ", "-ʃː-"),
         ipa_aa = str_replace_all(ipa_aa, "ъв", "-ʔʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ъ", "-ʔ-"),
         ipa_aa = str_replace_all(ipa_aa, "я", "-j-a-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", stress, "ᴴ"), "-ʔ-ẽ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", "ᴴ"), "-ʔ-ẽ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", stress), "-ʔ-e`-"),
         ipa_aa = str_replace_all(ipa_aa, "э", "-ʔ-e-"),
         ipa_sa = str_replace_all(ipa_aa, "я", "-j-a-"),
         ipa_aa = str_replace_all(ipa_aa, "в", "-w-"),
         ipa_aa = str_replace_all(ipa_aa, "-’", "’"),
         ipa_aa = str_replace_all(ipa_aa, "-:", "ː"),
         ipa_aa = str_replace_all(ipa_aa, "-{2,}", "-"),
         ipa_aa = str_replace_all(ipa_aa, "-\\(", "\\("),
         ipa_aa = str_replace_all(ipa_aa, "-\\)", "\\)"),
         ipa_aa = str_remove_all(ipa_aa, "^-"),
         ipa_aa = str_remove_all(ipa_aa, "^ʔ-"),
         ipa_aa = str_remove_all(ipa_aa, "-$"),
         ipa_sa = str_remove_all(lemma_saidova_abusov, " ?//.*"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", stress, "ᴴ"), "-ʔ-a`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", "ᴴ"), "-ʔ-ã-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", stress), "-ʔ-a`-"),
         ipa_sa = str_replace_all(ipa_sa, "-а:", "-ʔ-aː-"),
         ipa_sa = str_replace_all(ipa_sa, "-а", "-ʔ-a-"),
         ipa_sa = str_remove_all(ipa_sa, "[-/=\\\\]"),
         ipa_sa = str_replace_all(ipa_sa, "ʔa", "-ʔ-a-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", stress, "ᴴ"), "-ã`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", "ᴴ"), "-ã-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", stress), "-a`-"),
         ipa_sa = str_replace_all(ipa_sa, "а", "-a-"),
         ipa_sa = str_replace_all(ipa_sa, "б", "-b-"),
         ipa_sa = str_replace_all(ipa_sa, "гъв", "-ʁʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "гъ", "-ʁ-"),
         ipa_sa = str_replace_all(ipa_sa, "гI", "-ʕ-"),
         ipa_sa = str_replace_all(ipa_sa, "гь", "-h-"),
         ipa_sa = str_replace_all(ipa_sa, "гь", "-h-"),
         ipa_sa = str_replace_all(ipa_sa, "гв", "-ɡʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "г", "-ɡ-"),
         ipa_sa = str_replace_all(ipa_sa, "дж", "-dʒ-"),
         ipa_sa = str_replace_all(ipa_sa, "д", "-d-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", stress, "ᴴ"), "-ẽ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", "ᴴ"), "-ẽ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", stress), "-e`-"),
         ipa_sa = str_replace_all(ipa_sa, "е", "-e-"),
         ipa_sa = str_replace_all(ipa_sa, "ж", "-ʒ-"),
         ipa_sa = str_replace_all(ipa_sa, "зв", "-zʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "з", "-z-"),
         ipa_sa = str_replace_all(ipa_sa, "з", "-z-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", stress, "ᴴ"), "-ĩ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", "ᴴ"), "-ĩ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", stress), "-i`-"),
         ipa_sa = str_replace_all(ipa_sa, "и", "-i-"),
         ipa_sa = str_replace_all(ipa_sa, "й", "-j-"),
         ipa_sa = str_replace_all(ipa_sa, "кIкIв", "-k'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кькьв", "-tɬ'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кIкI", "-k'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "кькь", "-tɬ'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "ккв", "-kːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "къв", "-qχ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кIв", "-k'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кьв", "-tɬ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кк", "-kː-"),
         ipa_sa = str_replace_all(ipa_sa, "кI", "-k'-"),
         ipa_sa = str_replace_all(ipa_sa, "кв", "-kʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "къ", "-qχ'-"),
         ipa_sa = str_replace_all(ipa_sa, "кь", "-tɬ'-"),
         ipa_sa = str_replace_all(ipa_sa, "к", "-k-"),
         ipa_sa = str_replace_all(ipa_sa, "лълъв", "-ɬːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лIлIв", "-tɬːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лълъ", "-ɬː-"),
         ipa_sa = str_replace_all(ipa_sa, "лIлI", "-tɬː-"),
         ipa_sa = str_replace_all(ipa_sa, "лъв", "-ɬʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лIв", "-tɬʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лъ", "-ɬ-"),
         ipa_sa = str_replace_all(ipa_sa, "лI", "-tɬ-"),
         ipa_sa = str_replace_all(ipa_sa, "л", "-l-"),
         ipa_sa = str_replace_all(ipa_sa, "м", "-m-"),
         ipa_sa = str_replace_all(ipa_sa, "н", "-n-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", stress, "ᴴ"), "-õ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", "ᴴ"), "-õ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", stress), "-o`-"),
         ipa_sa = str_replace_all(ipa_sa, "о", "-o-"),
         ipa_sa = str_replace_all(ipa_sa, "п", "-p-"),
         ipa_sa = str_replace_all(ipa_sa, "пI", "-p'-"),
         ipa_sa = str_replace_all(ipa_sa, "пп", "-pː-"),
         ipa_sa = str_replace_all(ipa_sa, "р", "-r-"),
         ipa_sa = str_replace_all(ipa_sa, "сс", "-sː-"),
         ipa_sa = str_replace_all(ipa_sa, "св", "-sʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "с", "-s-"), 
         ipa_sa = str_replace_all(ipa_sa, "тI", "-t'-"),
         ipa_sa = str_replace_all(ipa_sa, "тт", "-tː-"),
         ipa_sa = str_replace_all(ipa_sa, "т", "-t-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", stress, "ᴴ"), "-ũ'-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", "ᴴ"), "-ũ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", stress), "-u`-"),
         ipa_sa = str_replace_all(ipa_sa, "у", "-u-"),
         ipa_sa = str_replace_all(ipa_sa, "ф", "-f-"),
         ipa_sa = str_replace_all(ipa_sa, "ххв", "-χːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хьв", "-çʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хъв", "-qχʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хх", "-χː-"),
         ipa_sa = str_replace_all(ipa_sa, "хI", "-ħ-"),
         ipa_sa = str_replace_all(ipa_sa, "хъ", "-qχ-"),
         ipa_sa = str_replace_all(ipa_sa, "хь", "-ç-"),
         ipa_sa = str_replace_all(ipa_sa, "х", "-χ-"),
         ipa_sa = str_replace_all(ipa_sa, "цIцIв", "-ts'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "цIцI", "-ts'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "цIв", "-ts'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ццв", "-tsːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "цI", "-ts'-"),
         ipa_sa = str_replace_all(ipa_sa, "цц", "-tsː-"),
         ipa_sa = str_replace_all(ipa_sa, "цв", "-tsʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ц", "-ts-"),
         ipa_sa = str_replace_all(ipa_sa, "чIчIв", "-tʃ'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "чIчI", "-tʃ'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "чIв", "-tʃ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ччв", "-tʃːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "чI", "-tʃ'-"),
         ipa_sa = str_replace_all(ipa_sa, "чч", "-tʃː-"),
         ipa_sa = str_replace_all(ipa_sa, "чч", "-tʃː-"),
         ipa_sa = str_replace_all(ipa_sa, "чв", "-tʃʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ч", "-tʃ-"),
         ipa_sa = str_replace_all(ipa_sa, "ш", "-ʃ-"),
         ipa_sa = str_replace_all(ipa_sa, "щ", "-ʃː-"),
         ipa_sa = str_replace_all(ipa_sa, "ъв", "-ʔʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ъ", "-ʔ-"),
         ipa_sa = str_replace_all(ipa_sa, "ю", "-j-u-"),
         ipa_sa = str_replace_all(ipa_sa, "я", "-j-a-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", stress, "ᴴ"), "-ʔ-ẽ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", "ᴴ"), "-ʔ-ẽ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", stress), "-ʔ-e`-"),
         ipa_sa = str_replace_all(ipa_sa, "э", "-ʔ-e-"),
         ipa_sa = str_replace_all(ipa_sa, "в", "-w-"),
         ipa_sa = str_replace_all(ipa_sa, "-:", "ː"),
         ipa_sa = str_replace_all(ipa_sa, "-{2,}", "-"),
         ipa_sa = str_replace_all(ipa_sa, "-\\(", "\\("),
         ipa_sa = str_replace_all(ipa_sa, "-\\)", "\\)"),
         ipa_sa = str_remove_all(ipa_sa, "^-"),
         ipa_sa = str_remove_all(ipa_sa, "^ʔ-"),
         ipa_sa = str_remove_all(ipa_sa, "-$"))  %>% 
  filter(!str_detect(ipa_aa, " "),
         !str_detect(ipa_sa, " ")) %>% 
  mutate(str_aa = str_detect(ipa_aa, "`"),
         str_sa = str_detect(ipa_sa, "`")) %>% 
#   filter(str_aa, str_sa) %>% # remove rows with unstressed words
#   mutate(ipa_aa = str_remove_all(ipa_aa, "`"),
#         ipa_sa = str_remove_all(ipa_sa, "`")) %>%
  mutate(simmilar = ipa_aa == ipa_sa) ->
  to_analise

to_analise %>% 
  count(simmilar)


# create sressless pic ----------------------------------------------------

library(tidytext)
options(scipen = 999)
to_analise %>% 
  mutate(ipa_aa = str_remove_all(ipa_aa, "`"),
         ipa_sa = str_remove_all(ipa_sa, "`")) %>%
  select(ipa_aa, ipa_sa, bind_id) %>% 
  pivot_longer(names_to = "reference", values_to = "words", ipa_aa:ipa_sa) %>% 
  mutate(reference = str_remove(reference, "ipa_")) %>% 
  mutate(words = str_remove_all(words, "[\\(\\)]")) %>% 
  mutate(words = str_replace(words, "tʃI", "tʃ'")) %>% # repaire qχ'ʷa`ntʃ(I)i	
  unnest_tokens(sound, words, token = stringr::str_split, pattern = "-") %>% 
  count(reference, sound, sort = TRUE) %>% 
  filter(sound != "") %>%
  spread(reference, n, fill = 0.1) %>% 
  mutate(difference = sa - aa) %>% 
  ggplot(aes(sa, aa, label = sound, fill = difference)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  geom_point()+
  ggrepel::geom_label_repel(size = 6, segment.color =  "black", segment.alpha = 0.5, color = "white", family = "Brill")+
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_continuous(type = "viridis")+
  labs(x = "Number of utterances in [Alekseev, Azaev 2019] (log scale)",
       y = "Number of utterances in [Saidova, Abusov 2012] (log scale)",
       title = "Comparing two Botlikh dictionaries (without stress patterns)") ->
  botlikh_dicts

ggsave(filename = "images/06_botlikh_dicts_without_stress.png", botlikh_dicts, device = "png", width = 10, height = 7)  

# create pic with a stress ------------------------------------------------
to_analise %>% 
  select(ipa_aa, ipa_sa, bind_id) %>% 
  pivot_longer(names_to = "reference", values_to = "words", ipa_aa:ipa_sa) %>% 
  mutate(reference = str_remove(reference, "ipa_")) %>% 
  mutate(words = str_remove_all(words, "[\\(\\)]")) %>% 
  mutate(words = str_replace(words, "tʃI", "tʃ'")) %>% # repaire qχ'ʷa`ntʃ(I)i	
  unnest_tokens(sound, words, token = stringr::str_split, pattern = "-") %>% 
  count(reference, sound, sort = TRUE) %>% 
  filter(sound != "") %>%
  spread(reference, n, fill = 0.1) %>% 
  mutate(difference = sa - aa) %>% 
  ggplot(aes(sa, aa, label = sound, fill = difference)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  geom_point()+
  ggrepel::geom_label_repel(size = 6, segment.color =  "black", segment.alpha = 0.5, color = "white", family = "Brill")+
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_continuous(type = "viridis")+
  labs(x = "Number of utterances in [Alekseev, Azaev 2019] (log scale)",
       y = "Number of utterances in [Saidova, Abusov 2012] (log scale)",
       title = "Comparing two Botlikh dictionaries (with stress patterns)") ->
  botlikh_dicts

ggsave(filename = "images/07_botlikh_dicts_with_stress.png", botlikh_dicts, device = "png", width = 10, height = 7)  
options(scipen = 0)

# analyse Andi data -------------------------------------------------------
andi <- read_csv("data/andi_dict.csv")
andi %>% 
  filter(is.na(remove),
         !str_detect(rus, "pl"),
         !str_detect(rus, "obl"),
         !str_detect(rus, "¬pst")) %>% 
  mutate(zilo_l = str_replace_all(zilo_l, "(<CL>)|(-CL)|(CL-)", "b"),
         zilo_l = str_replace_all(zilo_l, "V-", "i"),
         zilo_l = str_remove_all(zilo_l, "(\\))|(\\()"),
         zilo_l = str_replace_all(zilo_l, "tɬ'ːʷ", "-tɬ'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "ts'ːʷ", "-ts'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tʃ'ːʷ", "-tʃ'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "ts'ʷ", "-ts'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "qχ'ʷ", "-qχ'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "tɬːʷ", "-tɬːʷ-"),
         zilo_l = str_replace_all(zilo_l, "t'ːʷ", "-t'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "ts'ː", "-ts'ː-"),
         zilo_l = str_replace_all(zilo_l, "tsːʷ", "-tsːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tʃ'ː", "-tʃ'ː-"),
         zilo_l = str_replace_all(zilo_l, "k'ːʷ", "-k'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tɬ'ʷ", "-tɬ'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "p'ːʷ", "-p'ːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tʃ'ʷ", "-tʃ'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "tʃːʷ", "-tʃːʷ-"),
         zilo_l = str_replace_all(zilo_l, "t'ʷ", "-t'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "tɬ'", "-tɬ'-"),
         zilo_l = str_replace_all(zilo_l, "p'ʷ", "-p'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "pːʷ", "-pːʷ-"),
         zilo_l = str_replace_all(zilo_l, "sːʷ", "-sːʷ-"),
         zilo_l = str_replace_all(zilo_l, "ts'", "-ts'-"),
         zilo_l = str_replace_all(zilo_l, "tʃ'", "-tʃ'-"),
         zilo_l = str_replace_all(zilo_l, "kːʷ", "-kːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tɬʷ", "-tɬʷ-"),
         zilo_l = str_replace_all(zilo_l, "tɬː", "-tɬː-"),
         zilo_l = str_replace_all(zilo_l, "õ", "-õ-"),
         zilo_l = str_replace_all(zilo_l, "p'ː", "-p'ː-"),
         zilo_l = str_replace_all(zilo_l, "qχʷ", "-qχʷ-"),
         zilo_l = str_replace_all(zilo_l, "çːʷ", "-çːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tsʷ", "-tsʷ-"),
         zilo_l = str_replace_all(zilo_l, "tsː", "-tsː-"),
         zilo_l = str_replace_all(zilo_l, "k'ː", "-k'ː-"),
         zilo_l = str_replace_all(zilo_l, "t'ː", "-t'ː-"),
         zilo_l = str_replace_all(zilo_l, "χːʷ", "-χːʷ-"),
         zilo_l = str_replace_all(zilo_l, "tʃː", "-tʃː-"),
         zilo_l = str_replace_all(zilo_l, "ʃːʷ", "-ʃːʷ-"),
         zilo_l = str_replace_all(zilo_l, "k'ʷ", "-k'ʷ-"),
         zilo_l = str_replace_all(zilo_l, "tːʷ", "-tːʷ-"),
         zilo_l = str_replace_all(zilo_l, "ʒʷ", "-ʒʷ-"),
         zilo_l = str_replace_all(zilo_l, "pː", "-pː-"),
         zilo_l = str_replace_all(zilo_l, "hʷ", "-hʷ-"),
         zilo_l = str_replace_all(zilo_l, "ẽ", "-ẽ-"),
         zilo_l = str_replace_all(zilo_l, "k'", "-k'-"),
         zilo_l = str_replace_all(zilo_l, "ɬː", "-ɬː-"),
         zilo_l = str_replace_all(zilo_l, "p'", "-p'-"),
         zilo_l = str_replace_all(zilo_l, "pʷ", "-pʷ-"),
         zilo_l = str_replace_all(zilo_l, "sʷ", "-sʷ-"),
         zilo_l = str_replace_all(zilo_l, "qχ", "-qχ-"),
         zilo_l = str_replace_all(zilo_l, "çː", "-çː-"),
         zilo_l = str_replace_all(zilo_l, "ʃʷ", "-ʃʷ-"),
         zilo_l = str_replace_all(zilo_l, "ɡʷ", "-ɡʷ-"),
         zilo_l = str_replace_all(zilo_l, "dʷ", "-dʷ-"),
         zilo_l = str_replace_all(zilo_l, "zʷ", "-zʷ-"),
         zilo_l = str_replace_all(zilo_l, "ĩ", "-ĩ-"),
         zilo_l = str_replace_all(zilo_l, "kː", "-kː-"),
         zilo_l = str_replace_all(zilo_l, "lʔ", "-lʔ-"),
         zilo_l = str_replace_all(zilo_l, "lʷ", "-lʷ-"),
         zilo_l = str_replace_all(zilo_l, "nʷ", "-nʷ-"),
         zilo_l = str_replace_all(zilo_l, "rʷ", "-rʷ-"),
         zilo_l = str_replace_all(zilo_l, "t'", "-t'-"),
         zilo_l = str_replace_all(zilo_l, "ħʷ", "-ħʷ-"),
         zilo_l = str_replace_all(zilo_l, "χʷ", "-χʷ-"),
         zilo_l = str_replace_all(zilo_l, "χː", "-χː-"),
         zilo_l = str_replace_all(zilo_l, "çʷ", "-çʷ-"),
         zilo_l = str_replace_all(zilo_l, "ʃː", "-ʃː-"),
         zilo_l = str_replace_all(zilo_l, "ʔʷ", "-ʔʷ-"),
         zilo_l = str_replace_all(zilo_l, "ã", "-ã-"),
         zilo_l = str_replace_all(zilo_l, "ʁʷ", "-ʁʷ-"),
         zilo_l = str_replace_all(zilo_l, "dː", "-dː-"),
         zilo_l = str_replace_all(zilo_l, "kʷ", "-kʷ-"),
         zilo_l = str_replace_all(zilo_l, "mʷ", "-mʷ-"),
         zilo_l = str_replace_all(zilo_l, "sː", "-sː-"),
         zilo_l = str_replace_all(zilo_l, "tʷ", "-tʷ-"),
         zilo_l = str_replace_all(zilo_l, "tː", "-tː-"),
         zilo_l = str_replace_all(zilo_l, "ũ", "-ũ-"),
         zilo_l = str_replace_all(zilo_l, "w", "-w-"),
         zilo_l = str_replace_all(zilo_l, "h", "-h-"),
         zilo_l = str_replace_all(zilo_l, "l", "-l-"),
         zilo_l = str_replace_all(zilo_l, "o", "-o-"),
         zilo_l = str_replace_all(zilo_l, "a", "-a-"),
         zilo_l = str_replace_all(zilo_l, "b", "-b-"),
         zilo_l = str_replace_all(zilo_l, "ʕ", "-ʕ-"),
         zilo_l = str_replace_all(zilo_l, "ʁ", "-ʁ-"),
         zilo_l = str_replace_all(zilo_l, "ʒ", "-ʒ-"),
         zilo_l = str_replace_all(zilo_l, "j", "-j-"),
         zilo_l = str_replace_all(zilo_l, "k", "-k-"),
         zilo_l = str_replace_all(zilo_l, "m", "-m-"),
         zilo_l = str_replace_all(zilo_l, "n", "-n-"),
         zilo_l = str_replace_all(zilo_l, "s", "-s-"),
         zilo_l = str_replace_all(zilo_l, "ɡ", "-ɡ-"),
         zilo_l = str_replace_all(zilo_l, "d", "-d-"),
         zilo_l = str_replace_all(zilo_l, "e", "-e-"),
         zilo_l = str_replace_all(zilo_l, "i", "-i-"),
         zilo_l = str_replace_all(zilo_l, "ɬ", "-ɬ-"),
         zilo_l = str_replace_all(zilo_l, "p", "-p-"),
         zilo_l = str_replace_all(zilo_l, "r", "-r-"),
         zilo_l = str_replace_all(zilo_l, "χ", "-χ-"),
         zilo_l = str_replace_all(zilo_l, "ç", "-ç-"),
         zilo_l = str_replace_all(zilo_l, "z", "-z-"),
         zilo_l = str_replace_all(zilo_l, "u", "-u-"),
         zilo_l = str_replace_all(zilo_l, "f", "-f-"),
         zilo_l = str_replace_all(zilo_l, "ħ", "-ħ-"),
         zilo_l = str_replace_all(zilo_l, "ʃ", "-ʃ-"),
         zilo_l = str_replace_all(zilo_l, "t", "-t-"),
         zilo_l = str_replace_all(zilo_l, "ʔ", "-ʔ-"),
         zilo_l = str_replace_all(zilo_l, "-{2,}", "-"),
         zilo_l = str_replace_all(zilo_l, "-'", "'"),
         zilo_l = str_replace_all(zilo_l, "-ʷ", "ʷ"),
         zilo_l = str_replace_all(zilo_l, "-ː", "ː"),
         zilo_l = str_replace_all(zilo_l, "t-ʃ", "tʃ"),
         zilo_l = str_replace_all(zilo_l, "t-ʃ'", "tʃ'"),
         zilo_l = str_replace_all(zilo_l, "t-ɬ", "tɬ"),
         zilo_l = str_replace_all(zilo_l, "t-s", "ts"),
         zilo_l = str_replace_all(zilo_l, "t-ɬ", "tɬ"),
         zilo_l = str_replace_all(zilo_l, "t-ʃʷ", "tʃʷ"),
         zilo_l = str_replace_all(zilo_l, "d-ʒʷ", "dʒʷ"),
         zilo_l = str_replace_all(zilo_l, "d-ʒ", "dʒ"),
         zilo_l = str_replace_all(zilo_l, "q-χ'", "qχ'"),
         zilo_l = str_replace_all(zilo_l, "q-χ", "qχ"),
         zilo_l = str_replace_all(zilo_l, "q-χʷ", "qχʷ"),
         zilo_l = str_replace_all(zilo_l, "t-ɬ'ː", "tɬ'ː"),
         zilo_l = str_replace_all(zilo_l, "t-ɬ'", "tɬ'"),
         zilo_l = str_remove_all(zilo_l, "^-"),
         zilo_l = str_remove_all(zilo_l, "-$"),
         zilo_l = str_remove_all(zilo_l, "<"),
         zilo_l = str_remove_all(zilo_l, ">"),
         zilo_l = str_replace_all(zilo_l, "1", "i")) %>% 
  select(zilo_l) %>% 
  unnest_tokens(sound, zilo_l, token = stringr::str_split, pattern = "-") %>% 
  count(sound, sort = TRUE) %>% 
  filter(sound != "ь",
         sound != "") %>% 
  mutate(reference = "zilo") ->
  zilo_data

  to_analise %>% 
    mutate(ipa_aa = str_remove_all(ipa_aa, "`"),
           ipa_sa = str_remove_all(ipa_sa, "`")) %>%
    select(ipa_aa, ipa_sa, bind_id) %>% 
    pivot_longer(names_to = "reference", values_to = "words", ipa_aa:ipa_sa) %>% 
    mutate(reference = str_remove(reference, "ipa_")) %>% 
    mutate(words = str_remove_all(words, "[\\(\\)]")) %>% 
    mutate(words = str_replace(words, "tʃI", "tʃ'")) %>% # repaire qχ'ʷa`ntʃ(I)i	
    unnest_tokens(sound, words, token = stringr::str_split, pattern = "-") %>% 
    count(reference, sound, sort = TRUE) %>% 
    filter(sound != "") ->
    botlikh_data

  botlikh_data %>% 
    bind_rows(zilo_data) %>% 
    group_by(reference) %>% 
    mutate(ratio = n/sum(n)) %>% 
    select(-n) %>% 
    spread(reference, ratio, fill = 0.1) %>% 
    mutate(difference_aa = sa - zilo,
           difference_sa = sa - zilo) ->
    merged_data

  merged_data %>% 
    ggplot(aes(sa, zilo, label = sound, fill = difference_sa)) + 
    geom_abline(slope = 1, intercept = 0, linetype = 2)+
    geom_point()+
    ggrepel::geom_label_repel(size = 6, segment.color =  "black", segment.alpha = 0.5, color = "white", family = "Brill")+
    scale_fill_continuous(type = "viridis")+
    labs(y = "Number of utterances in Andi (log scale)",
         x = "Number of utterances in [Saidova, Abusov 2012] (log scale)",
         title = "Comparing [Saidova, Abusov 2012] and Zilo data")+
    theme(legend.title = element_blank()) ->
    zilo_sa
  
ggsave(filename = "images/08_zilo_sa.png", zilo_sa, device = "png", width = 10, height = 7)  
  
merged_data %>% 
  ggplot(aes(aa, zilo, label = sound, fill = difference_aa)) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2)+
  geom_point()+
  ggrepel::geom_label_repel(size = 6, segment.color =  "black", segment.alpha = 0.5, color = "white", family = "Brill")+
  scale_fill_continuous(type = "viridis")+
  labs(y = "Number of utterances in Andi (log scale)",
       x = "Number of utterances in [Alekseev, Azaev 2019] (log scale)",
       title = "Comparing [Alekseev, Azaev 2019] and Zilo data") +
  theme(legend.title = element_blank()) ->
  zilo_aa

ggsave(filename = "images/09_zilo_aa.png", zilo_aa, device = "png", width = 10, height = 7)  

