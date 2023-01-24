library(tidyverse)
library (here)

# ler arquivos
caminho <- here ("DwC_output") 
files <- list.files(pattern = 'DF_occ.csv', recursive = T, path = caminho, full.names = TRUE) # -> includes all subfolders

# myfiles = lapply(files, read.csv)

tbl1 <- list.files(pattern = 'DF_occ.csv', recursive = T, path = caminho, full.names = TRUE) %>% 
  map_df(~ read_csv(., col_types = cols(.default = "c"), id = "name"))

peld_bentos <- read.csv( here ("DwC_output", "PELD_iloc_benthos", "DF_occ.txt")) %>% 
  mutate(name = "PELD_iloc_benthos")
peld_peixe <- read.csv(here ("DwC_output", "PELD_iloc_fish" , "DF_occ.txt"),sep=";") %>% 
  mutate(name = "PELD_iloc_fish")
RN_benthos <- read.csv(here ("DwC_output", "GLongo_NRoss_spatialData", "DF_occ_benthos.csv")) %>% 
  mutate(name = "RN_benthos")
RN_fish <- read.csv(here ("DwC_output","GLongo_NRoss_spatialData", "DF_occ_fish.csv")) %>% 
  mutate(name = "RN_fish")
abrolhos_fish <- read.csv(here ("DwC_output", "RFrancini_timeSeries_abrolhos", "DF_occ_fish.csv")) %>% 
  mutate(name = "abrolhos_fish")
abrolhos_bentos <- read.csv(here ("DwC_output", "RFrancini_timeSeries_abrolhos", "DF_occ_benthos.csv")) %>% 
  mutate(name = "abrolhos_bentos")
alcatrazes_fish <- read.csv(here ("DwC_output", "Alcatrazes_time_series", "DF_occ.csv")) %>% 
  mutate(name = "Alcatrazes_time_series")


nomes_datasets <- data.frame(nome = c("I", 
                                      "II", 
                                      "III", 
                                      "IV", 
                                      "V",
                                      "VI", 
                                      "VII", 
                                      "VIII", 
                                      "IX", 
                                      "X",
                                      "XI", 
                                      "XII",
                                      "XIII"),
                             
                             dataset = c(
                               # fish
                               "RMorais_spatialData",
                               "abrolhos_fish", 
                               "RJ_time_series",
                               "PELD_iloc_fish", 
                               "SC_time_series",
                               "GLongo_spatialData", 
                               "Alcatrazes_time_series",
                               "RN_fish",
                               # benthos
                               "abrolhos_bentos",
                               "RFrancini_spatialData", 
                               "AAued_spatialData",
                               "PELD_iloc_benthos", 
                               "RN_benthos")) %>% 
  mutate(nome = factor(nome, levels = c("I", 
                                        "II", 
                                        "III", 
                                        "IV",
                                        "V",
                                        "VI",
                                        "VII",
                                        "VIII", 
                                        "IX", 
                                        "X", 
                                        "XI", 
                                        "XII", 
                                        "XIII")))
  
  
## Fish

tbl <- bind_rows(peld_bentos %>% select(scientificNameAccepted, name),
                 peld_peixe %>% select(scientificNameAccepted, name),
                 tbl1 %>% select(scientificNameAccepted, name),
                 RN_benthos %>% select(scientificNameAccepted, name),
                 RN_fish %>% select(scientificNameAccepted, name),
                 abrolhos_fish %>% select(scientificNameAccepted, name),
                 abrolhos_bentos %>% select(scientificNameAccepted, name),
                 alcatrazes_fish %>% select(scientificNameAccepted, name)) %>% 
  distinct() %>% 
  mutate(name = gsub("D:/Pos_Doc_Sinbiose/ReefSYN_data/DwC_output/", "", name)) %>% 
  mutate(name = gsub("/DF_occ.csv", "", name))

# tbl %>% 
#   select(kingdom, class, family, scientificName, name) %>% 
#   filter(class %in% c("Actinopteri", "Elasmobranchii")) %>% 
#   distinct() %>% 
#   # pivot_wider(names_from = kingdom,
#   #             values_from = scientificName) %>% 
#   arrange(class, family) %>% 
#   knitr::kable()

# Get full taxonomical classification
fish <- tbl %>% 
  filter(!name %in% c("PELD_iloc_benthos", "AAued_spatialData", "RN_benthos", "RFrancini_spatialData", "abrolhos_bentos")) %>%
  distinct(scientificNameAccepted) %>% 
  arrange(scientificNameAccepted) %>% 
  filter(scientificNameAccepted != "Menaphorus punticulatus") %>%
  pull() %>% 
  worms::wormsbymatchnames(verbose=F)

# Table with species and datasets
#fish %>% 
#  select(kingdom:genus, valid_name, scientificname) %>% 
#  arrange(class, order, family, genus, valid_name) %>% 
#  mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
#  filter(is.na(tirar)) %>%
#  left_join(., tbl %>% 
#              select(scientificNameAccepted, name) %>% 
#              distinct() %>% 
#              mutate(name = gsub("D:/Pos_Doc_Sinbiose/ReefSYN_data/DwC_output/", "", name)) %>% 
#              mutate(name = gsub("/DF_occ.csv", "", name)) %>% 
#              filter(!is.na(scientificNameAccepted)) %>% 
#              rename(scientificname = scientificNameAccepted,
#                     dataset = name)) %>% 
#  select(-tirar) %>% 
#  distinct(order) %>% 
#  pull() %>% noquote()
  # pivot_wider(names_from = dataset, values_from = dataset) %>% 
  # knitr::kable()


  # fish %>% 
  #   select(kingdom:genus, valid_name) %>% 
  #   arrange(class, order, family, genus, valid_name) %>% 
  #   mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
  #   filter(is.na(tirar)) %>% 
  #   select(-tirar) %>% 
  #   lapply(list, unique) %>% noquote()
  
# 
a <- fish %>% 
  select(kingdom:genus, valid_name, scientificname) %>% 
  arrange(class, order, family, genus, valid_name) %>% 
  mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
  filter(is.na(tirar)) %>%
  left_join(., tbl %>% 
              select(scientificNameAccepted, name) %>% 
              distinct() %>% 
              mutate(name = gsub("D:/Pos_Doc_Sinbiose/ReefSYN_data/DwC_output/", "", name)) %>% 
              mutate(name = gsub("/DF_occ.csv", "", name)) %>% 
              filter(!is.na(scientificNameAccepted)) %>% 
              rename(scientificname = scientificNameAccepted,
                     dataset = name)) %>% 
  select(-tirar) %>% 
  group_by(dataset) %>% 
  summarise(Species = n_distinct(valid_name),
            Phylum = n_distinct(phylum),
            Class = n_distinct(class),
            Order = n_distinct(order),
            Family = n_distinct(family),
            Genus = n_distinct(genus)) %>% 
  pivot_longer(cols = Species:Genus, names_to = "taxa_rank", values_to = "richness") %>% 
  mutate(taxa_rank = factor(taxa_rank, levels = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))) %>% 
  left_join(nomes_datasets) %>% 
  ggplot(aes(y = richness, x = nome, fill = taxa_rank)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "",
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)) +
  labs(y = "Taxa richness (n)", x = "", title = "(a) Fish taxa") +
  scale_fill_brewer(palette="Spectral")
  
  
  
## Benthos
bentos <- tbl %>% 
  filter(name %in% c("PELD_iloc_benthos", 
                     "AAued_spatialData", 
                     "RN_benthos", 
                     "RFrancini_spatialData", 
                     "abrolhos_bentos")) %>%
  distinct(scientificNameAccepted) %>% 
  arrange(scientificNameAccepted) %>% 
  filter(!scientificNameAccepted %in% c(NA, "Turf", "Sand gravel", "Others", "Corticated",
                                "Foliose", "Filamentous", "Leathery", "Unknown")) %>% 
  pull() %>% 
  worms::wormsbymatchnames(verbose=F)

# bentos %>% 
#   select(kingdom:genus, valid_name) %>% 
#   arrange(kingdom, phylum, class, order, family, genus, valid_name) %>% 
#   mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
#   left_join(., tbl %>% 
#               select(scientificName, name) %>% 
#               distinct() %>% 
#               mutate(name = gsub("/Users/cesarcordeiro/Meu Drive/PARCERIAS/SiNBIOSE/dados/Darwin Core Format/", "", name)) %>% 
#               mutate(name = gsub("/DF_occ.csv", "", name)) %>% 
#               filter(!is.na(scientificName)) %>% 
#               rename(valid_name = scientificName,
#                      dataset = name)) %>% 
#   pivot_wider(names_from = dataset, values_from = dataset) %>%
#   knitr::kable()

# taxonomical resolution
b <- bentos %>% 
  select(kingdom:genus, valid_name, scientificname) %>% 
  arrange(kingdom, phylum, class, order, family, genus, valid_name) %>% 
  mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
  left_join(., tbl %>% 
              select(scientificNameAccepted, name) %>% 
              distinct() %>% 
              mutate(name = gsub("/Users/cesarcordeiro/Meu Drive/PARCERIAS/SiNBIOSE/dados/Darwin Core Format/", "", name)) %>% 
              mutate(name = gsub("/DF_occ.csv", "", name)) %>% 
              filter(!is.na(scientificNameAccepted)) %>% 
              rename(scientificname = scientificNameAccepted,
                     dataset = name)) %>%
  group_by(dataset) %>% 
  summarise(Species = n_distinct(valid_name),
            Phylum = n_distinct(phylum),
            Class = n_distinct(class),
            Order = n_distinct(order),
            Family = n_distinct(family),
            Genus = n_distinct(genus)) %>% 
  pivot_longer(cols = Species:Genus, names_to = "taxa_rank", values_to = "richness") %>% 
  mutate(taxa_rank = factor(taxa_rank, levels = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))) %>%
  left_join(nomes_datasets) %>% 
    ggplot(aes(y = richness, x = nome, fill = taxa_rank)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(legend.position = "",
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12)) +
    labs(y = "", x = "", title = "(b) Benthic taxa") +
  scale_fill_brewer(palette="Spectral")

# legenda
#### --- ###
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#### --- ###

c <- bentos %>% 
  select(kingdom:genus, valid_name, scientificname) %>% 
  arrange(kingdom, phylum, class, order, family, genus, valid_name) %>% 
  mutate(tirar = ifelse(genus == valid_name, "tirar", NA)) %>% 
  left_join(., tbl %>% 
              select(scientificNameAccepted, name) %>% 
              distinct() %>% 
              mutate(name = gsub("D:/Pos_Doc_Sinbiose/ReefSYN_data/DwC_output/", "", name)) %>% 
              mutate(name = gsub("/DF_occ.csv", "", name)) %>% 
              filter(!is.na(scientificNameAccepted)) %>% 
              rename(scientificname = scientificNameAccepted,
                     dataset = name)) %>%
  group_by(dataset) %>% 
  summarise(Species = n_distinct(valid_name),
            Phylum = n_distinct(phylum),
            Class = n_distinct(class),
            Order = n_distinct(order),
            Family = n_distinct(family),
            Genus = n_distinct(genus)) %>% 
  pivot_longer(cols = Species:Genus, names_to = "taxa_rank", values_to = "richness") %>% 
  mutate(taxa_rank = factor(taxa_rank, levels = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))) %>%
  left_join(nomes_datasets) %>% 
  ggplot(aes(y = richness, x = nome, fill = taxa_rank)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  scale_fill_brewer(palette="Spectral")

legenda <- get_legend(c + theme(legend.position = "right",
                                legend.title = element_blank())) 

# plot
library(patchwork)

(a | b) + legenda + plot_layout(ncol = 3, widths=c(2,2,1))


