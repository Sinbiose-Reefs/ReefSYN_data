### teste datasets
library(tidyverse)

##############################################################################
##############################################################################

# Dataset I
## RMorais_spatialData

# 2026
DF_eMOF_ab <- read_csv("DwC_output/OK datasetI_RMorais_spatialData/DF_eMOF_ab.csv")
DF_eMOF_sz <- read_csv("DwC_output/OK datasetI_RMorais_spatialData/DF_eMOF_sz.csv")
DF_occ <- read_csv("DwC_output/OK datasetI_RMorais_spatialData/DF_occ_new.csv")
event_core1 <- read_csv("DwC_output/OK datasetI_RMorais_spatialData/event_core_new.csv")

lapply(event_core, unique) # "stpauls_rocks"

# check location coordinates
event_core %>% 
  distinct(decimalLatitude, decimalLongitude, locality, location) %>% 
  arrange(location, locality) %>% 
  data.frame()

library(leaflet)
event_core %>% 
  distinct(decimalLatitude, decimalLongitude, locality, location) %>% 
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap background
  addMarkers(
    lat = ~decimalLatitude, 
    lng = ~decimalLongitude, 
    popup = ~locality,      # Appears when you click the marker
    label = ~location       # Appears when you hover over the marker
  )

# $site
# [1] "abrolhos"        "alcatrazes"      "arraial"         "btds_santos"    
# [5] "ceara"           "costa_corais"    "espirito_santo"  "ilha_grande"    
# [9] "ilhabela"        "ilhasc_norte"    "ilhasc_sul"      "laje_santos"    
# [13] "manuel_luis"     "rgnor_natal"     "rgnor_parrachos" "rgnor_sul"      
# [17] "noronha"         "rocas"           "stpauls_rocks"   "trindade" 

# check year
event_core %>% 
  filter(location == "arraial") %>% 
  distinct(locality, year) %>% data.frame()
  

lapply(DF_occ, unique) # recordedBy nome_sobrenome
# $recordedBy
# [1] "ramon_noguchi"      "gugaw_ferreira"     "cel_ferreira"       "bertran_feitoza"   
# [5] "eduardo_godoy"      "ca_rangel"          "thiago_mendes"      "jp_quimbayo"       
# [9] "renata_mazzei"      "renato_morais"      "sergio_floeter"     "ana_liedke"        
# [13] "jl_gasparini"       "jp_krajewski"       "hudson_pinheiro"    "gabriel_ferreira"  
# [17] "anaide_aued"        "claudio_sampaio"    "cesar_cordeiro"     "thiony_simon"      
# [21] "diego_barneche"     "anderson_batista"   "tiago_albuquerque"  "anchieta_nunes"    
# [25] "daniel_dinslaken"   NA                   "gabriel_correal"    "osmar_luiz"        
# [29] "marcelo_silveira"   "andrea_dalben"      "alexandre_siqueira" "max_levy"          
# [33] "guilherme_longo"    "luisa_fontoura"     "athila_bertoncini" 
lapply(DF_eMOF, unique)


# uniqueID
setdiff(unique(DF_eMOF_ab$eventID), unique(DF_occ$eventID))
setdiff(unique(DF_eMOF_sz$eventID), unique(DF_occ$eventID))
setdiff(unique(DF_occ$eventID), unique(DF_eMOF_sz$eventID))
setdiff(unique(DF_occ$eventID), unique(DF_eMOF_ab$eventID))
setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
setdiff(unique(DF_occ$eventID), unique(event_core$eventID))

# occurrenceID (nao precisa ter no event_core, soh no occ)
setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))

# especies
DF_occ %>%  
  select(scientificNameAccepted, verbatimIdentification) %>% 
  distinct() %>% 
  arrange(scientificNameAccepted) %>% 
  data.frame() 

DF_occ %>% 
  filter(is.na(scientificNameAccepted)) %>% 
  data.frame() %>% 
  distinct(verbatimIdentification)

DF_occ %>% 
  filter(is.na(scientificNameAccepted)) %>% 
  select(scientificNameAccepted, verbatimIdentification, kingdom:genus) %>% 
  # distinct() %>% 
  data.frame()

# menaphorus.punticulatus, sparisoma.spp, entomacordus.sp, ogcocephalus.vespertilio
# Sphyraena borealis?
# Sparisoma viride
# Serranus atricauda
# Scorpaenodes caribbaeus
# Scorpaena brachyptera
# Scomberomorus maculatus
# Sardinella brasiliensis?
# Prognathodes marcellae
# Platybelone argalus argalus -> Platybelone argalus
# Nicholsina collettei
# Muraena melanotis
# Microgobius carri
# Halichoeres rubrovirens

## updating missing or wrong information

# wrong coordinates
event_core %>% 
  mutate(decimalLatitude = ifelse(locality == "farilhoes", -20.522423, 
                                  ifelse(locality == "lixo", -20.524679, decimalLatitude)),
         decimalLongitude = ifelse(locality == "farilhoes", -29.331352, 
                                   ifelse(locality == "lixo", -29.328228, decimalLongitude)),
         year = ifelse(is.na(year) & locality == "ponta_escalvada_ilha_da_gipoia", 2007, # based on missing site among sites sampled in the same year
                       ifelse(is.na(year) & location == "arraial", 2011, year))) %>%  # based on missing site among sites sampled in the same or consecutive year) 
  select(-"...1") %>% 
  write.csv("DwC_output/OK datasetI_RMorais_spatialData/event_core_v2026.csv", row.names = FALSE)

# missing information and mispelled species
DF_occ %>% 
  mutate(scientificNameAccepted = ifelse(verbatimIdentification == "entomacordus.sp", "Entomacrodus", scientificNameAccepted),
         scientificNameID = ifelse(verbatimIdentification == "entomacordus.sp", "urn:lsid:marinespecies.org:taxname:204365", scientificNameID),
         taxonRank = ifelse(verbatimIdentification == "entomacordus.sp", "Genus", taxonRank),
         kingdom = ifelse(verbatimIdentification == "entomacordus.sp", "Animalia", kingdom),
         phylum = ifelse(verbatimIdentification == "entomacordus.sp", "Chordata", phylum),
         class = ifelse(verbatimIdentification == "entomacordus.sp", "Teleostei", class),
         order = ifelse(verbatimIdentification == "entomacordus.sp", "Blenniiformes", order),
         family = ifelse(verbatimIdentification == "entomacordus.sp", "Blenniidae", family),
         genus = ifelse(verbatimIdentification == "entomacordus.sp", "Entomacrodus", genus)) %>% 
  mutate(scientificNameAccepted = ifelse(verbatimIdentification == "sparisoma.spp", "Sparisoma", scientificNameAccepted),
         scientificNameID = ifelse(verbatimIdentification == "sparisoma.spp", "urn:lsid:marinespecies.org:taxname:126051", scientificNameID),
         taxonRank = ifelse(verbatimIdentification == "sparisoma.spp", "Genus", taxonRank),
         kingdom = ifelse(verbatimIdentification == "sparisoma.spp", "Animalia", kingdom),
         phylum = ifelse(verbatimIdentification == "sparisoma.spp", "Chordata", phylum),
         class = ifelse(verbatimIdentification == "sparisoma.spp", "Teleostei", class),
         order = ifelse(verbatimIdentification == "sparisoma.spp", "Eupercaria incertae sedis", order),
         family = ifelse(verbatimIdentification == "sparisoma.spp", "Labridae", family),
         genus = ifelse(verbatimIdentification == "sparisoma.spp", "Sparisoma", genus)) %>% 
  mutate(scientificNameAccepted = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Menaphorus punticulatus hybrid", scientificNameAccepted),
         taxonRank = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Hybrid", taxonRank),
         kingdom = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Animalia", kingdom),
         phylum = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Chordata", phylum),
         class = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Teleostei", class),
         order = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Perciformes", order),
         family = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Epinephelidae", family),
         genus = ifelse(verbatimIdentification == "menaphorus.punticulatus", "Cephalopholis fulva x Cephalopholis furcifer hybrid", genus)) %>% 
  select(-"...1") %>% 
  write.csv("DwC_output/OK datasetI_RMorais_spatialData/DF_occ_v2026.csv", row.names = FALSE)

# check inverted values of abundance and size
DF_occ %>% 
  filter(scientificNameAccepted %in% c("Sphyraena barracuda", "Caranx bartholomaei", "Muraena melanotis")) %>% 
  distinct(occurrenceID) %>% 
  pull()

DF_eMOF_ab %>%
  filter(occurrenceID %in% c(DF_occ %>% 
                               filter(scientificNameAccepted %in% c("Sphyraena barracuda", "Caranx bartholomaei", "Muraena melanotis")) %>% 
                               distinct(occurrenceID) %>% 
                               pull()),
         measurementValue > 60) %>% 
  distinct(occurrenceID) %>%
  data.frame()


DF_eMOF_ab %>%
  filter(occurrenceID %in% c(DF_occ %>% 
                               filter(scientificNameAccepted == "Muraena melanotis") %>% 
                               distinct(occurrenceID) %>% pull()),
         measurementValue > 50) %>% 
  distinct(occurrenceID) %>%
  data.frame()

DF_eMOF_ab %>% 
  mutate(measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_boia_2012_2710_occ19" & measurementType == "abundance", 25, measurementValue),
         measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_enseada_2009_2794_occ887" & measurementType == "abundance", 25, measurementValue),
         measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_enseada_2009_2816_occ1056" & measurementType == "abundance", 25, measurementValue)) %>% 
  write.csv("DwC_output/OK datasetI_RMorais_spatialData/DF_eMOF_ab_v2026.csv", row.names = FALSE)


DF_eMOF_sz %>%
  mutate(measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_boia_2012_2710_occ19" & measurementType == "total length", 70, measurementValue),
         measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_enseada_2009_2794_occ887" & measurementType == "total length", 90, measurementValue),
         measurementValue = ifelse(occurrenceID == "BR:ReefSYN:SISBIOTA-MAR-UVC:BrazilianOceanicIslands:stpauls_rocks_enseada_2009_2816_occ1056" & measurementType == "total length", 90, measurementValue)) %>% 
  write.csv("DwC_output/OK datasetI_RMorais_spatialData/DF_eMOF_sz_v2026.csv", row.names = FALSE)

rm(list = ls())

# 2026-05-30 falta checar dados sobrepostos com PELD ILOC

##############################################################################
##############################################################################

## AAued_spatialData XI (OK)

DF_eMOF <- read_csv("DwC_output/OK datasetXI_AAued_spatialData/Aued_eMOF_new.csv") 
DF_occ <- read_csv("DwC_output/OK datasetXI_AAued_spatialData/DF_occ_new.csv")
event_core <- read_csv("DwC_output/OK datasetXI_AAued_spatialData/event_core_new.csv")

### MODIFY
bentos_long_format$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:SISBIOTA-MAR:", 
           bentos_long_format$higherGeography,
           sep=""),
    bentos_long_format$site,sep=":"),
  bentos_long_format$locality,
  bentos_long_format$year,
  bentos_long_format$photoquadrat,
  bentos_long_format$modifiedDepth,
  sep="_")

# 2026
event_core %>% 
  mutate(decimalLatitude = ifelse(locality == "farilhoes", -20.522423, decimalLatitude),
         decimalLongitude = ifelse(locality == "farilhoes", -29.331352, decimalLongitude)) %>% 
  write.csv("DwC_output/OK datasetXI_AAued_spatialData/event_core_v2026.csv", row.names = FALSE)

rm(list = ls())

##############################################################################
##############################################################################

## Alcatrazes VII
DF_eMOF_ab <- read_csv("DwC_output/OK datasetVII_Alcatrazes_time_series/DF_eMOF_ab.csv") 
DF_eMOF_sz <- read_csv("DwC_output/OK datasetVII_Alcatrazes_time_series/DF_eMOF_sz.csv") 
DF_occ <- read_csv("DwC_output/OK datasetVII_Alcatrazes_time_series/DF_occ_new.csv")
event_core <- read_csv("DwC_output/OK datasetVII_Alcatrazes_time_series/event_core_new.csv")

lapply(event_core, unique) 
lapply(DF_occ, unique) # recordedBy nomes nao padronizados

# uniqueID
setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
setdiff(unique(DF_occ$eventID), unique(event_core$eventID))

# occurrenceID (nao precisa ter no event_core, soh no occ)
setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))

# Chromis limbaughi errado, trocar por Stegastes pictus 
# Gymnothorax mordax = Gymnothorax miliaris

# oversized species
left_join(DF_occ %>% 
            select(eventID, occurrenceID, scientificNameAccepted),
          DF_eMOF_sz %>% 
            select(-"...1")) %>%
  # filter(scientificNameAccepted %in% spp[1:20]) %>%
  # filter(scientificNameAccepted %in% spp[21:40]) %>%
  # filter(scientificNameAccepted %in% spp[41:60]) %>%
  # filter(scientificNameAccepted %in% spp[61:80]) %>%
  # filter(scientificNameAccepted %in% spp[81:100]) %>%
  filter(scientificNameAccepted %in% spp[101:120]) %>%
  ggplot(aes(x=measurementValue)) +
  geom_histogram() +
  facet_wrap(~scientificNameAccepted, ncol = 3, scales = "free")


left_join(DF_occ %>% 
            select(eventID, occurrenceID, scientificNameAccepted),
          DF_eMOF_sz) %>%
  filter(scientificNameAccepted == "Abudefduf saxatilis" & measurementValue > 25 |
           scientificNameAccepted == "Rypticus" & measurementValue > 50 |
           scientificNameAccepted == "Azurina multilineata" & measurementValue > 20 |
           scientificNameAccepted == "Haemulon aurolineatum" & measurementValue > 30 |
           scientificNameAccepted == "Bodianus rufus" & measurementValue == 40 |
           scientificNameAccepted == "Diplodus argenteus" & measurementValue > 40 |
           scientificNameAccepted == "Haemulon aurolineatum" & measurementValue > 30 |
           scientificNameAccepted == "Halichoeres poeyi" & measurementValue > 25 |
           scientificNameAccepted == "Pomacanthus paru" & measurementValue ==  60 |
           scientificNameAccepted == "Stegastes fuscus" & measurementValue > 15 |
           scientificNameAccepted == "Elacatinus figaro" & measurementValue > 5 |
           scientificNameAccepted == "Holacanthus tricolor" & measurementValue > 25 |
           scientificNameAccepted == "Malacoctenus delalandii" & measurementValue > 5 |
           scientificNameAccepted == "Pempheris schomburgkii" & measurementValue > 15 |
           scientificNameAccepted == "Stegastes variabilis" & measurementValue > 10 |
           scientificNameAccepted == "Coryphopterus" & measurementValue > 7) %>% 
  data.frame() %>% 
  distinct(occurrenceID) %>% 

##############################################################################
##############################################################################
