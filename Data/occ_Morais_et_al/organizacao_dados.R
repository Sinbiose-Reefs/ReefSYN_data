# ------------------------------------------- #
## Organizing data of Morais et al. 2017


require(openxlsx); require(here); require(worrms); require(dplyr); require(rfishbase); require(dplyr); require(tibble)


# load original spreadsheet Morais
dados <- read.csv (here ("Data", "occ_Morais_et_al",
                         "census_br_Morais_et_al_2017.csv"),h=T,
                   sep=";")

## load species list, diet and functional groups (creatd by André L. Luza e Juan Quimbayo)
# to fill the abbreviations
# three different spreadsheets

# species
lista_sp <- read.xlsx(
  here ("Data","occ_Morais_et_al", "species_list_Morais_2017.xlsx"),
  sheet = 1,
  startRow = 13,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = FALSE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  sep.names = ".",
  namedRegion = NULL,
  na.strings = "NA",
  fillMergedCells = FALSE
)

# diets
lista_dieta <- read.xlsx(
  here ("Data","occ_Morais_et_al", "species_list_Morais_2017.xlsx"),
  sheet = 2,
  startRow = 1,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = FALSE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  sep.names = ".",
  namedRegion = NULL,
  na.strings = "NA",
  fillMergedCells = FALSE
)


# functional groups
lista_grupos_funcionais <- read.xlsx(
  here ("Data","occ_Morais_et_al", "species_list_Morais_2017.xlsx"),
  sheet = 3,
  startRow = 1,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = FALSE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  sep.names = ".",
  namedRegion = NULL,
  na.strings = "NA",
  fillMergedCells = FALSE
)


## correcting abbreviations 
dados$namesToSearch <- lista_sp$ScientificName [match (as.character(dados$code), 
                                                        lista_sp$Abbreviation)] # scientificName
dados$Diet <- lista_dieta$DietLabel [match (as.character(dados$diet_2012), 
                                            lista_dieta$Abbreviation)] # diet
dados$FunctionalGroup <- lista_grupos_funcionais$FunctionalGroupLabel [match (as.character(dados$func_gr), 
                                                                              lista_grupos_funcionais$Abbreviation)] # functional groups





# ------------------------------------------------------------------------------------
# ADJUSTING DATES
# diverse formats




### adjust dates (format YYYY-mm-dd)
# call it eventDate
# "verbatimDate" = original dates
dados$eventDate <- gsub ("_","-",dados$verbatimDate)
dados$eventDate <- gsub ("jan","01",dados$eventDate)
dados$eventDate <- gsub ("mar","03",dados$eventDate)
dados$eventDate <- gsub ("apr","04",dados$eventDate)
dados$eventDate <- gsub ("jun","06",dados$eventDate)
dados$eventDate <- gsub ("jul","07",dados$eventDate)
dados$eventDate <- gsub ("aug","08",dados$eventDate)
dados$eventDate <- gsub ("oct","10",dados$eventDate)
dados$eventDate <- gsub ("nov","11",dados$eventDate)
dados$eventDate <- gsub ("1909","2009",dados$eventDate)
dados$eventDate <- as.Date (dados$eventDate, format= "%d-%m-%Y") # finally transform into the format we want 

### adjust year (call it year, format %Y)
dados$year <- format(as.Date(dados$eventDate, format="%d-%m-%Y"),"%Y")

## deal with cases of missing month and day 
# substituir os NAs pelos anos, ja que na coluna de datas originais ("verbatimDate") so tem info do ano
dados$year[which (nchar (as.character (dados$verbatimDate)) < 5 & 
                         is.na(dados$eventDate) == T)] <- as.character (dados$verbatimDate) [which (nchar (as.character (dados$verbatimDate)) < 5 & 
                                                                                                      is.na(dados$eventDate) == T)]


## month, to deal with cases of missing the sampling day
dados$month <- format (dados$eventDate,"%Y-%m")


## there is some dates with improper dates
mes_ano <- (as.character (dados$verbatimDate) [which (nchar (as.character (dados$verbatimDate)) > 5 & is.na(as.Date (dados$eventDate)) == T)])
mes_ano <- gsub ("_","-",mes_ano)
mes_ano <- gsub ("mar","03",mes_ano)
mes_ano <- gsub ("apr","04",mes_ano)
mes_ano <- gsub ("jun","06",mes_ano)
mes_ano <- gsub ("jul","07",mes_ano)
mes_ano <- gsub ("aug","08",mes_ano)
mes_ano <- gsub ("nov","11",mes_ano)

# again some cases of complete dates
mes_ano<- ifelse (nchar (mes_ano) == 10,
                  substr (mes_ano, 4,10),
                  mes_ano)

## format YYYY-mm
mes_ano <- paste(substr (mes_ano, 4,8), substr (mes_ano, 1,2),"01",sep="-")
## Unica maneira de colar na eventDate eh dizer que os dados sem dia de coleta foram coletados no dia 01 daquele mes
mes_ano <- format(as.Date (mes_ano,format="%Y-%m-%d"), "%Y-%m")
# code for a more simple date
## mes_ano <- format(as.Date(mes_ano, format="%Y-%m-%d"),"%Y-%m")

# transformar no formato data 
dados$month [which (nchar (as.character (dados$verbatimDate)) > 5 & is.na(dados$eventDate) == T)] <- mes_ano

# barplot para saber o ano de mais coleta de dados
# pela contagem do numero de linhas por ano
#barplot (table(as.numeric (format (as.Date (dados$eventDate), "%Y"))),
#        las=2,xlab="Year",ylab="Frequency")

# gather year - month data to find the year
dados[is.na(dados$year),"year"] <- substr(dados[is.na(dados$year),"month"],1,4)


# ------------------------------------------------------------------------------------
# ADJUSTING SITES, DEPTHS




## shallow and deep samples
dados$eventDepth <- ifelse (dados$depth_m >= 8 , "deep","shallow")

# djust RNorte sites according to Guilherme Longo
dados$locality[which( dados$locality == "rgnor_norte")] <- "rgnor_natal"

# adjust tartaruga, trindade (we also have "tartarugas" at "Rocas' Atoll")
dados [which(dados$locality == "trindade" & dados$site == "tartarugas"),"site"] <- "tartarugas_trindade"


# removing locality names from location name
dados$site<-gsub ("arvoredo_", "",dados$site)


# adjusting site names based on other datasets
dados$site<-(iconv(dados$site, "ASCII", "UTF-8", sub=""))
dados$site <- tolower(dados$site)



# adjust
unique(dados$site)[order(unique(dados$site))]
dados$site[which(dados$site == "pta_agua")] <- "ponta_agua"
dados$site[which(dados$site == "pta_leste")] <- "ponta_leste"
dados$site[which(dados$site == "praia_porto")] <- "praia_do_porto"
dados$site[which(dados$site == "maramut")] <- "maramuta"
dados$site[which(dados$site == "cagarras_noronha")] <- "cagarras"

unique(dados$site )[order(unique(dados$site ))]


# adjusting colnames to match DwC
dados$verbatimSite <- dados$locality
dados$verbatimLocality <- dados$site

# adjust
dados$site <- dados$verbatimSite
dados$locality <- dados$verbatimLocality

# geographic location
dados$higherGeography <- ifelse (dados$site %in% c("stpauls_rocks",
                                                                     "rocas",
                                                                     "noronha",
                                                                     "trindade"),
                                        "BrazilianOceanicIslands",
                                        "BrazilianCoast")




# ------------------------------------------------------
# CREATING IDS






# creating parentIDs
dados$parentEventID <- paste (
                            paste ( 
                              paste ("BR:ReefSYN:SISBIOTA-MAR-UVC:", 
                                     dados$higherGeography,
                                     sep=""),
                              dados$site,sep=":"),
                            dados$locality,
                            dados$year,
                            sep="_")


# creating eventIds
dados$eventID <-  paste (
                        paste ( 
                          paste ("BR:ReefSYN:SISBIOTA-MAR-UVC:", 
                                 dados$higherGeography,
                                 sep=""),
                          dados$site,sep=":"),
                        dados$locality,
                        dados$year,
                        dados$transect_id,
                        sep="_")


# creating occurrenceIDs
dados$occurrenceID <-  paste (
                        paste ( 
                          paste ("BR:ReefSYN:SISBIOTA-MAR-UVC:", 
                                 dados$higherGeography,
                                 sep=""),
                          dados$site,sep=":"),
                        dados$locality,
                        dados$year,
                        dados$transect_id,
                        paste ("occ",seq(1,nrow(dados)),sep=""),
                        sep="_")





# ---------------------------------------------------------------------
# ADJUSTING COORDINATES


dados$verbatimLatitude <- dados$lat
dados$verbatimLongitude <- dados$lon



## coordinates to spatial points
# adjusting longitude of one coordinate on land
## "perua preta" which falls within the continent
dados [grep("perua",dados$site),"lon"] <- as.numeric(-35.082658) 
# adjusting longitude of one coordinate on land
# arvoredo_engenho
dados [grep("engenho",dados$site),"lon"] <- as.numeric(-48.369148) 
# baia das tartarugas
dados [grep("baia_da_tartaruga",dados$site),"lat"] <- as.numeric(-27.289852)
dados [grep("baia_da_tartaruga",dados$site),"lon"] <- as.numeric(-48.368578)
# arvoredo capim
dados [grep("capim",dados$site),"lat"] <- as.numeric(-27.283922)
dados [grep("capim",dados$site),"lon"] <- as.numeric(-48.374285)
# arvoredo rancho
dados [grep("rancho_norte",dados$site),"lat"] <- as.numeric(-27.278325)
dados [grep("rancho_norte",dados$site),"lon"] <- as.numeric(-48.375279)
# saco dagua
dados [grep("saco_dagua",dados$site),"lat"] <- as.numeric(-27.274033)
dados [grep("saco_dagua",dados$site),"lon"] <- as.numeric(-48.367183)


# verbatimIdentification
dados$verbatimIdentification <- dados$namesToSearch

# replace dot by "_"
source ("R/functions.R")
dados$namesToSearch <- firstup(gsub ("\\.","_", dados$namesToSearch))

# check taxonomic issues
dados$namesToSearch [which(dados$namesToSearch == "Platybelone_argalus")] <- "Platybelone_argalus_argalus"
dados$namesToSearch [which(dados$namesToSearch == "Nicholsina_usta_collettei")] <- "Nicholsina_collettei"
dados$namesToSearch [which(dados$namesToSearch == "Labrisomus_kalisherae")] <- "Gobioclinus_kalisherae"
dados$namesToSearch [which(dados$namesToSearch == "Eucinostomus_lefroyi")] <- "Ulaema_lefroyi"
dados$namesToSearch [which(dados$namesToSearch == "Haemulon_plumieri")] <- "Haemulon_plumierii"
dados$namesToSearch [which(dados$namesToSearch == "Hypanus_americana")] <- "Hypanus_americanus"
dados$namesToSearch [which(dados$namesToSearch == "Dasyatis_americana")] <- "Hypanus_americanus"
dados$namesToSearch [which(dados$namesToSearch == "Caranx_plumbeus")] <- "Carcharhinus_plumbeus"
dados$namesToSearch [which(dados$namesToSearch == "Carcharhinus_perezi")] <- "Carcharhinus_perezii"
dados$namesToSearch [which(dados$namesToSearch == "Lutjanus_mohogani")] <- "Lutjanus_mahogoni"
dados$namesToSearch [which(dados$namesToSearch == "Epinephelus_niveatus")] <- "Hyporthodus_niveatus"
dados$namesToSearch [which(dados$namesToSearch == "Epinephelus_cruentatus")] <- "Cephalopholis_cruentata"
dados$namesToSearch [which(dados$namesToSearch == "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus_spinosus"
dados$namesToSearch [which(dados$namesToSearch == "Coryphopterus_spb" )] <-  "Coryphopterus_spp"
dados$namesToSearch [which(dados$namesToSearch == "Dasyatis_americana" )] <-  "Hypanus americanus"
dados$namesToSearch [which(dados$namesToSearch == "Diplodus_argenteus_argenteus" )] <-  "Diplodus_argenteus"
dados$namesToSearch [which(dados$namesToSearch == "Emblemariopsis_signifera" )] <-  "Emblemariopsis_signifer"
dados$namesToSearch [which(dados$namesToSearch == "Kyphosus_incisor" )] <-  "Kyphosus_vaigiensis"
dados$namesToSearch [which(dados$namesToSearch == "Kyphosus_bigibbus" )] <-  "Kyphosus_sp"
dados$namesToSearch [which(dados$namesToSearch == "Malacoctenus_sp1" )] <-  "Malacoctenus_brunoi"
dados$namesToSearch [which(dados$namesToSearch == "Malacoctenus_sp2" )] <-  "Malacoctenus_lianae"
dados$namesToSearch [which(dados$namesToSearch == "Malacoctenus_sp3" )] <-  "Malacoctenus lianae"
dados$namesToSearch [which(dados$namesToSearch == "Nicholsina_usta_usta" )] <-  "Nicholsina_usta_usta"
dados$namesToSearch [which(dados$namesToSearch == "Nicholsina_usta_collettei" )] <-  "Nicholsina_usta"
dados$namesToSearch [which(dados$namesToSearch == "Anthias_salmopuntatus" )] <- "Choranthias_salmopunctatus"
dados$namesToSearch [which(dados$namesToSearch == "Emblemariosis_sp" )] <- "Emblemariopsis_sp"

# replacing "_" by " "
dados$namesToSearch<-gsub ("_"," ",dados$namesToSearch)
dados$namesToSearch <- tolower (dados$namesToSearch)

# non identified species
dados$identificationQualifier <- ifelse (sapply (strsplit (dados$namesToSearch, " "), "[", 2) %in% c("sp","sp1", "sp2", "sp3"),
                                                  "sp",
                                                  NA)

# species to search
dados$namesToSearch [which(dados$identificationQualifier == "sp")] <- gsub (" sp*.",
                                                                            "",
                                                                            dados$namesToSearch [which(dados$identificationQualifier == "sp")])





# matching with worms
worms_record <- lapply (unique(dados$namesToSearch), function (i) 
        
                        tryCatch (
                
                             wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
                                
                                error = function (e) print(NA)
                                
                                
                                )
                        
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
dados$scientificNameAccepted<-(df_worms_record$scientificname [match (dados$namesToSearch,
                                                              tolower (df_worms_record$scientificname))])
# taxon rank of the identified level
dados$taxonRank <- (df_worms_record$rank [match (dados$namesToSearch,
                                                 tolower (df_worms_record$scientificname))])
# aphiaID
dados$scientificNameID<-(df_worms_record$lsid [match (dados$namesToSearch,
                                                      tolower (df_worms_record$scientificname))])
# kingdom
dados$kingdom<-(df_worms_record$kingdom [match (dados$namesToSearch,
                                                tolower (df_worms_record$scientificname))])
# phylum
dados$phylum<-(df_worms_record$phylum [match (dados$namesToSearch,
                                                tolower (df_worms_record$scientificname))])
# class
dados$class<-(df_worms_record$class [match (dados$namesToSearch,
                                            tolower (df_worms_record$scientificname))])
# order
dados$order<-(df_worms_record$order [match (dados$namesToSearch,
                                            tolower (df_worms_record$scientificname))])
# family
dados$family<-(df_worms_record$family [match (dados$namesToSearch,
                                              tolower (df_worms_record$scientificname))])

# genus
dados$genus<-(df_worms_record$genus [match (dados$namesToSearch,
                                              tolower (df_worms_record$scientificname))])



# taxonomic updates
# species
dados$scientificNameAccepted[grep ("multilineata", dados$scientificNameAccepted)] <- "Azurina multilineata"
dados$scientificNameAccepted[grep ("bartholomaei", dados$scientificNameAccepted)] <- "Caranx bartholomaei"
dados$scientificNameAccepted[grep ("polygonius", dados$scientificNameAccepted)] <- "Acanthostracion polygonium"
dados$scientificNameAccepted[grep ("Hypanus americanus", dados$scientificNameAccepted)] <- "Hypanus berthalutzea"

# genus
dados$genus[grep ("multilineata", dados$scientificNameAccepted)] <- "Azurina"
dados$genus[grep ("bartholomaei", dados$scientificNameAccepted)] <- "Caranx"


# --------------------------------------------------------------------------------------
# ADJUSTING MEASUREMENT VALUES






# split abundance and size data
abundance <- dados[,which(colnames(dados) != "size_cm")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abun")] <- "measurementValue"
# measurementUncertainty
abundance$measurementUncertainty  <- NA


# size
size <- dados[,which(colnames(dados) != "abun")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "size_cm")] <- "measurementValue"





# --------------------------------------------------------------------------------------
# ADJUSTING MEASUREMENT UNCERTAINTY ON SIZE ESTIMATES





# measurement uncertainty
# flagging too large individuals (following Quimbayo et al. 2021, and then Fishbase)
traits_db <- read.csv (here ("Data", 
                                      "trait_Quimbayo_et_al",
                                      "Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),h=T,
                                sep=";")


# taxonomic updates
# species
traits_db$Name[grep ("multilineata", traits_db$Name)] <- "Azurina multilineata"
traits_db$Name[grep ("bartholomaei", traits_db$Name)] <- "Caranx bartholomaei"
traits_db$Name[grep ("Acanthostracion polygonius", traits_db$Name)] <- "Acanthostracion polygonium"

traits_db$Body_size <- as.numeric(gsub (",",".", traits_db$Body_size)) # adjust variable

# max size from Quimbayo
size$measurementUncertainty <- traits_db[match (size$scientificNameAccepted, (traits_db$Name)),"Body_size"]

# flag
size$measurementUncertainty<-(ifelse (size$measurementValue <= size$measurementUncertainty, 
        "size_OK",
        "larger_than_reported_in_Quimbayo_et_al"
        ))



# use fishbase for remaining species

fish_to_search <- unique(size [which(size$measurementUncertainty == "larger_than_reported_in_Quimbayo_et_al"),
                               "scientificNameAccepted"])
# search
search_fish<-data.frame (fb_tbl("species",version = "latest") %>% 
                        mutate(sci_name = paste(Genus, Species)) %>%
                        filter(sci_name %in% fish_to_search)) #%>% 
                                #dplyr::select(sci_name, Length))



# flag based on fishbase
subset_larger <- size[which(size$measurementUncertainty == "larger_than_reported_in_Quimbayo_et_al"),]
subset_larger$measurementUncertainty <- (search_fish[match (subset_larger$scientificNameAccepted,
                                                            (search_fish$sci_name)),
                                                     "Length"])
# check
subset_larger$measurementUncertainty<-(ifelse (subset_larger$measurementValue <= subset_larger$measurementUncertainty, 
                                                                      "size_OK",
                                                                      "larger_than_reported_in_fishbase"
))

# flag
size[which(rownames(size) %in% rownames(subset_larger)),"measurementUncertainty"] <- subset_larger$measurementUncertainty

# finally try to find NAs
fish_to_search_NA<- unique(size[is.na(size$measurementUncertainty),"scientificNameAccepted"])

# search
search_fish_NA<-data.frame (fb_tbl("species") %>% 
                                 mutate(sci_name = paste(Genus, Species)) %>%
                                 filter(sci_name %in% fish_to_search_NA) %>% 
                                    dplyr::select(sci_name, Length))

# by hand following fishbase
search_fish_NA [which(search_fish_NA$sci_name == "Malacoctenus brunoi"), "Length"] <- 4.4
search_fish_NA [which(search_fish_NA$sci_name == "Acanthurus bahianus"), "Length"] <- 38.1
search_fish_NA [which(search_fish_NA$sci_name == "Prognathodes brasiliensis"), "Length"] <- 12

# flag based on fishbase
subset_NA <- size[is.na(size$measurementUncertainty),]
subset_NA$measurementUncertainty <- (search_fish_NA[match (subset_NA$scientificNameAccepted, 
                                                           (search_fish_NA$sci_name)),"Length"])
# Malacoctenus lianae by hand usingfishbase
subset_NA [which(subset_NA$scientificNameAccepted == "Malacoctenus lianae"), "measurementUncertainty"] <- 4.5
subset_NA [which(subset_NA$scientificNameAccepted == "Stephanolepis hispidus"), "measurementUncertainty"] <- 36.9
subset_NA [which(subset_NA$scientificNameAccepted == "Carcharhinus perezii"), "measurementUncertainty"] <- 300

# check
subset_NA$measurementUncertainty<-(ifelse (subset_NA$measurementValue <= subset_NA$measurementUncertainty, 
                                               "size_OK",
                                               "larger_than_reported_in_fishbase"
))

# flag
size[which(rownames(size) %in% rownames(subset_NA)),"measurementUncertainty"] <- subset_NA$measurementUncertainty


# bind edited data
dados_bind <- rbind (abundance,
                     size)


unique(dados_bind[which(dados_bind$measurementType == "total length" & 
                          is.na(dados_bind$measurementUncertainty)),"scientificNameAccepted"])


# -----------------------------------------------------------------------
# DWC DESCRIPTORS






# method
dados_bind$samplingProtocol <- "Underwater visual survey - 20 x 2m" 

# effort
dados_bind$samplingEffort <- 1# "one observer per transect"

# sampleSizeValue belt transects 20*2
dados_bind$sampleSizeValue <- 20*2# 

# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"

# recordedBy
colnames(dados_bind)[which(colnames(dados_bind) == "observer")] <- "recordedBy"

# depth
colnames(dados_bind)[which(colnames(dados_bind) == "depth_m")] <- "minimumDepthinMeters"
dados_bind$maximumDepthinMeters <- dados_bind$minimumDepthinMeters

# country and code
dados_bind$Country <- "Brazil"
dados_bind$countryCode <- "BR"

# basisOfRecord
dados_bind$basisOfRecord <- "HumanObservation"

# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# geodeticDatum
dados_bind$geodeticDatum <- "decimal degrees"
dados_bind$decimalLatitude <- dados_bind$lat # lat
dados_bind$decimalLongitude <- dados_bind$lon # long


# licence
dados_bind$licence <- "CC BY-NC"
# language
dados_bind$language <- "en"
# citation
dados_bind$bibliographicCitation <- "Morais, R.A., Ferreira, C.E.L. and Floeter, S.R. (2017), Spatial patterns of fish standing biomass across Brazilian reefs. J Fish Biol, 91: 1642-1667. https://doi.org/10.1111/jfb.13482"




# ----------------------------------------------------------------------------------------------
# Formatting according to DwC





# measurement or facts
DF_eMOF <- dados_bind [,c("eventID", 
                          "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "measurementUncertainty")]


# occurrence
DF_occ <- dados_bind  [,c("eventID", 
                          "occurrenceID",
                          "basisOfRecord",
                          "verbatimIdentification",
                          "scientificNameAccepted",
                          "scientificNameID",
                          "taxonRank",
                          "kingdom",
                          "phylum",
                          "class",
                          "order",
                          "family",
                          "genus",
                          "recordedBy", "organismQuantityType", "occurrenceStatus",
                          "licence",
                          "language",
                          "bibliographicCitation")]



# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(dados_bind,eventID,higherGeography,verbatimLocality,site,locality) %>% 
                                  
                                  summarise(year = mean(as.numeric(year)),
                                            eventDate = mean(eventDate),
                                            minimumDepthinMeters = mean(minimumDepthinMeters),
                                            maximumDepthinMeters = mean(maximumDepthinMeters),
                                            samplingProtocol = unique(samplingProtocol),
                                            samplingEffort = unique(samplingEffort),
                                            sampleSizeValue = mean(sampleSizeValue),
                                            sampleSizeUnit = unique(sampleSizeUnit),
                                            decimalLongitude = mean(decimalLongitude),
                                            decimalLatitude = mean(decimalLatitude),
                                            geodeticDatum = unique(geodeticDatum),
                                            Country = unique(Country),
                                            countryCode = unique(countryCode))
)


# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)




# save
# write to txt format

write.csv(DF_occ, file =here("DwC_output",
                               "RMorais_spatialData",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "RMorais_spatialData",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "RMorais_spatialData",
                                   "event_core.csv"))
rm(list=ls())

# # ========================================================
# quality checks
#Hmisc::describe(DF_occ)
#Hmisc::describe(event_core)
#Hmisc::describe(DF_eMOF)
#
## obis check
#require("obistools")
## check occ
#report <- report(DF_occ)
#print(report)
## eventd
#eventid <- check_eventids(event_core)
#print(eventid)
#event_core [1385,]
#
#