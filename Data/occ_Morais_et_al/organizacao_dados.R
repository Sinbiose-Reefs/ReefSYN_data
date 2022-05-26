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
dados$ScientificName <- lista_sp$ScientificName [match (as.character(dados$code), lista_sp$Abbreviation)] # scientificName
dados$Diet <- lista_dieta$DietLabel [match (as.character(dados$diet_2012), lista_dieta$Abbreviation)] # diet
dados$FunctionalGroup <- lista_grupos_funcionais$FunctionalGroupLabel [match (as.character(dados$func_gr), 
                                                                              lista_grupos_funcionais$Abbreviation)] # functional groups





# ------------------------------------------------------------------------------------
# ADJUSTING DATES





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

### adjust year (call it eventYear, format %Y)
dados$eventYear <- format(as.Date(dados$eventDate, format="%d-%m-%Y"),"%Y")

## deal with cases of missing month and day 
# substituir os NAs pelos anos, ja que na coluna de datas originais ("verbatimDate") so tem info do ano
dados$eventYear[which (nchar (as.character (dados$verbatimDate)) < 5 & 
                         is.na(dados$eventDate) == T)] <- as.character (dados$verbatimDate) [which (nchar (as.character (dados$verbatimDate)) < 5 & 
                                                                                                      is.na(dados$eventDate) == T)]


## eventMonth, to deal with cases of missing the sampling day
dados$eventMonth <- format (dados$eventDate,"%Y-%m")


## existem alguns anos com um formato inadequado, onde tem os meses de coleta
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
dados$eventMonth [which (nchar (as.character (dados$verbatimDate)) > 5 & is.na(dados$eventDate) == T)] <- mes_ano

# barplot para saber o ano de mais coleta de dados
# pela contagem do numero de linhas por ano
#barplot (table(as.numeric (format (as.Date (dados$eventDate), "%Y"))),
#        las=2,xlab="Year",ylab="Frequency")



# ------------------------------------------------------------------------------------
# ADJUSTING SITES, DEPTHS




## definir se eh uma transeccao do fundo ou do raso
dados$eventDepth <- ifelse (dados$depth_m >= 8 , "deep","shallow")

# ajustar os sitios de rio grande do norte, de acordo com G Longo
dados$locality[which( dados$locality == "rgnor_norte")] <- "rgnor_natal"

# ajustar tartaruga, trindade (tb tem tartaruga em rocas)
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

unique(dados$site )[order(unique(dados$site ))]








# ------------------------------------------------------
# CREATING IDS







# creating parentIDs
dados$parentEventID <- paste (paste ("BR:SISBIOTA-MAR:",
                               dados$locality,sep=""), 
                        dados$site, 
                        dados$eventYear,
                        sep="_")


# creating eventIds
dados$eventID <- paste (paste ("BR:SISBIOTA-MAR:",
                        dados$locality,sep=""), 
                        dados$site, 
                        dados$eventYear,
                        dados$transect_id,
                        sep="_")


# creating occurrenceIDs
dados$occurrenceID <- paste (paste ("BR:SISBIOTA-MAR:",
                               dados$locality,sep=""), 
                        dados$site, 
                        dados$eventYear,
                        dados$transect_id,
                        paste ("occ",seq(1,nrow(dados)),sep=""),
                        sep="_")





# ---------------------------------------------------------------------
# ADJUSTING COORDINATES






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
dados$verbatimIdentification <- dados$ScientificName

# replace dot by "_"
source ("R/functions.R")
dados$ScientificName <- firstup(gsub ("\\.","_", dados$ScientificName))

# check taxonomic issues
dados$ScientificName [which(dados$ScientificName == "Platybelone_argalus")] <- "Platybelone_argalus_argalus"
dados$ScientificName [which(dados$ScientificName == "Nicholsina_usta_collettei")] <- "Nicholsina_collettei"
dados$ScientificName [which(dados$ScientificName == "Labrisomus_kalisherae")] <- "Gobioclinus_kalisherae"
dados$ScientificName [which(dados$ScientificName == "Eucinostomus_lefroyi")] <- "Ulaema_lefroyi"
dados$ScientificName [which(dados$ScientificName == "Haemulon_plumieri")] <- "Haemulon_plumierii"
dados$ScientificName [which(dados$ScientificName == "Hypanus_americana")] <- "Hypanus_americanus"
dados$ScientificName [which(dados$ScientificName == "Dasyatis_americana")] <- "Hypanus_americanus"
dados$ScientificName [which(dados$ScientificName == "Caranx_plumbeus")] <- "Carcharhinus_plumbeus"
dados$ScientificName [which(dados$ScientificName == "Lutjanus_mohogani")] <- "Lutjanus_mahogani"
dados$ScientificName [which(dados$ScientificName == "Epinephelus_niveatus")] <- "Hyporthodus_niveatus"
dados$ScientificName [which(dados$ScientificName == "Epinephelus_cruentatus")] <- "Cephalopholis_cruentata"
dados$ScientificName [which(dados$ScientificName == "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus_spinosus"
dados$ScientificName [which(dados$ScientificName == "Coryphopterus_spb" )] <-  "Coryphopterus_spp"
dados$ScientificName [which(dados$ScientificName == "Dasyatis_americana" )] <-  "Hypanus americanus"
dados$ScientificName [which(dados$ScientificName == "Diplodus_argenteus_argenteus" )] <-  "Diplodus_argenteus"
dados$ScientificName [which(dados$ScientificName == "Emblemariopsis_signifera" )] <-  "Emblemariopsis_signifer"
dados$ScientificName [which(dados$ScientificName == "Kyphosus_incisor" )] <-  "Kyphosus_vaigiensis"
dados$ScientificName [which(dados$ScientificName == "Kyphosus_bigibbus" )] <-  "Kyphosus_sp"
dados$ScientificName [which(dados$ScientificName == "Malacoctenus_sp1" )] <-  "Malacoctenus_brunoi"
dados$ScientificName [which(dados$ScientificName == "Malacoctenus_sp2" )] <-  "Malacoctenus_lianae"
dados$ScientificName [which(dados$ScientificName == "Malacoctenus_sp3" )] <-  "Malacoctenus lianae"
dados$ScientificName [which(dados$ScientificName == "Nicholsina_usta_usta" )] <-  "Nicholsina_usta_usta"
dados$ScientificName [which(dados$ScientificName == "Nicholsina_usta_collettei" )] <-  "Nicholsina_usta"
dados$ScientificName [which(dados$ScientificName == "Anthias_salmopuntatus" )] <- "Choranthias_salmopunctatus"
dados$ScientificName [which(dados$ScientificName == "Emblemariosis_sp" )] <- "Emblemariopsis_sp"

# replacing "_" by " "
dados$ScientificName<-gsub ("_"," ",dados$ScientificName)
dados$ScientificName <- tolower (dados$ScientificName)

# matching with worms
worms_record <- lapply (unique(dados$ScientificName), function (i) 
        
                        tryCatch (
                
                             wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
                                
                                error = function (e) print(NA)
                                
                                
                                )
                        
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
dados$scientificNameOBIS<-(df_worms_record$scientificname [match (dados$ScientificName,tolower (df_worms_record$scientificname))])
dados$scientificNameID<-(df_worms_record$lsid [match (dados$ScientificName,tolower (df_worms_record$scientificname))])
dados$kingdom<-(df_worms_record$kingdom [match (dados$ScientificName,tolower (df_worms_record$scientificname))])
dados$class<-(df_worms_record$class [match (dados$ScientificName,tolower (df_worms_record$scientificname))])
dados$family<-(df_worms_record$family [match (dados$ScientificName,tolower (df_worms_record$scientificname))])





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


traits_db$Body_size <- as.numeric(gsub (",",".", traits_db$Body_size)) # adjust variable
# max size from Quimbayo
size$measurementUncertainty <- traits_db[match (size$ScientificName, tolower(traits_db$Name)),"Body_size"]

# flag
size$measurementUncertainty<-(ifelse (size$measurementValue <= size$measurementUncertainty, 
        "smaller_equal_to_the_reported_in_Quimbayo_et_al",
        "larger_than_reported_in_Quimbayo_et_al"
        ))



# use fishbase for remaining species


fish_to_search <- unique(size [which(size$measurementUncertainty == "larger_than_reported_in_Quimbayo_et_al"),"ScientificName"])
fish_to_search <-firstup(fish_to_search)
# search
search_fish<-data.frame (fb_tbl("species",version = "latest") %>% 
                        mutate(sci_name = paste(Genus, Species)) %>%
                        filter(sci_name %in% fish_to_search)) #%>% 
                                #dplyr::select(sci_name, Length))



# flag based on fishbase
subset_larger <- size[which(size$measurementUncertainty == "larger_than_reported_in_Quimbayo_et_al"),]
subset_larger$measurementUncertainty <- (search_fish[match (subset_larger$ScientificName,tolower(search_fish$sci_name)),"Length"])
# check
subset_larger$measurementUncertainty<-(ifelse (subset_larger$measurementValue <= subset_larger$measurementUncertainty, 
                                                                      "smaller_equal_to_the_reported_in_fishbase",
                                                                      "larger_than_reported_in_fishbase"
))

# flag
size[which(rownames(size) %in% rownames(subset_larger)),"measurementUncertainty"] <- subset_larger$measurementUncertainty

# finally try to find NAs
fish_to_search_NA<- unique(size[is.na(size$measurementUncertainty),"ScientificName"])
fish_to_search_NA <-firstup(fish_to_search_NA)

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
subset_NA$measurementUncertainty <- (search_fish_NA[match (subset_NA$ScientificName, tolower (search_fish_NA$sci_name)),"Length"])
# Malacoctenus lianae by hand usingfishbase
subset_NA [which(subset_NA$ScientificName == "Malacoctenus lianae"), "measurementUncertainty"] <- 4.5

# check
subset_NA$measurementUncertainty<-(ifelse (subset_NA$measurementValue <= subset_NA$measurementUncertainty, 
                                               "smaller_equal_to_the_reported_in_fishbase",
                                               "larger_than_reported_in_fishbase"
))
# flag
size[which(rownames(size) %in% rownames(subset_NA)),"measurementUncertainty"] <- subset_NA$measurementUncertainty

# bind edited data
dados_bind <- rbind (abundance,
                     size)





# -----------------------------------------------------------------------
# DWC DESCRIPTORS






# method
dados_bind$samplingProtocol <- "underwater visual survey - 20 x 2m"
# effort
dados_bind$samplingEffort <- 1# "one observer per transect"
# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS)
dados_bind$sampleSizeValue <- 40# plotarea?radii?"
# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(dados_bind)[which(colnames(dados_bind) == "observer")] <- "recordedBy"
# depth
colnames(dados_bind)[which(colnames(dados_bind) == "depth_m")] <- "minimumDepthinMeters"
dados_bind$maximumDepthinMeters <- dados_bind$minimumDepthinMeters
# scientificName
colnames(dados_bind)[which(colnames(dados_bind) == "ScientificName")] <- "scientificName"
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
# locationID (less local)
colnames(dados_bind)[which(colnames(dados_bind) == "locality")] <- "locationID"
# locality (more local)
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "locality"
# geographic location
dados_bind$higherGeographyID <- ifelse (dados_bind$locationID %in% c("stpauls_rocks",
                                                                   "rocas",
                                                                  "noronha",
                                                                  "trindade"),
                                                "BrazilianIslands",
                                                "BrazilianCoast")

# adjusting site
dados_bind$locality[which(dados_bind$locality == "cagarras_noronha")] <- "cagarras"



# ----------------------------------------------------------------------------------------------
# Formatting according to DwC





# measurement or facts
DF_eMOF <- dados_bind [,c("eventID", "occurrenceID","verbatimIdentification",
                          "scientificName","scientificNameID","scientificNameOBIS",
                          "kingdom","class","family",
                          "measurementValue", "measurementType","measurementUnit",
                          "measurementUncertainty")]


# occurrence
DF_occ <- dados_bind  [,c("eventID", "occurrenceID","basisOfRecord","verbatimIdentification",
                          "scientificName","scientificNameID","scientificNameOBIS",
                          "kingdom","class","family",
                          "recordedBy", "organismQuantityType", "occurrenceStatus")]



# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(dados_bind, eventID,higherGeographyID,verbatimLocality,locationID,locality) %>% 
                                  
                                  summarise(eventYear = mean(as.numeric(eventYear)),
                                            eventDate = mean(eventDate),
                                            minimumDepthinMeters = mean(minimumDepthinMeters),
                                            maximumDepthinMeters = mean(maximumDepthinMeters),
                                            samplingProtocol = unique(samplingProtocol),
                                            samplingEffort = unique(samplingEffort),
                                            sampleSizeValue = mean(sampleSizeValue),
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

write.table(DF_occ, file =here("DwC_output",
                               "RMorais_spatialData",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)

write.table(DF_eMOF, file =here("DwC_output",
                                "RMorais_spatialData",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)


write.table(event_core, file =here("DwC_output",
                                   "RMorais_spatialData",
                                   "event_core.txt"),sep=",",
            quote = FALSE)


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