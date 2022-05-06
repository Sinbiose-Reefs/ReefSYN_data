# ---------------------------------#
#    Time series (TS) formatting   #
# ---------------------------------#

# packages
require(here); require(reshape);require(dplyr)

# load functions (formatting data to array)
source (here ("R", "function_formatting_data.R"))
# list of long formats (array into a long format)
source (here ("R", "function_array_to_long.R"))





# years in the time series
years <- seq (2000,2020)





# ------------------------------------------------------------ #
# fish time series (J Quimbayo) in oceanic islands
# occurrence data




fish_DF_eMOF <- read.table(here ("DwC_output", 
                               "PELD_iloc_fish",
                              "DF_eMOF.txt"),
                           sep=";",encoding= "UTF-8")

# event core
fish_event_core <-  read.csv(here ("DwC_output", 
                                   "PELD_iloc_fish",
                                   "event_core.txt"),sep=";", 
                             encoding= "UTF-8")

# remove Ascension (not in BR)
fish_event_core <- fish_event_core [which(fish_event_core$island != "Ascension"),]
fish_DF_eMOF <- fish_DF_eMOF [-grep("Ascension", fish_DF_eMOF$eventID),]

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventDate")
fish_TS_data <- fish_event_core [match (fish_DF_eMOF$eventID,
                                        fish_event_core$eventID),
                                 variables_we_want]
fish_TS_data$eventDate<-substr(fish_TS_data$eventDate,1,4) # only year
# bind the occurrence data
fish_TS_data<- cbind (fish_TS_data,
                      fish_DF_eMOF)

# let's work with abundance
fish_TS_data <- fish_TS_data[which(fish_TS_data$measurementType == "abundance"),]
unique(fish_TS_data$locality)[order(unique(fish_TS_data$locality))]

# formatted fish data peld
array_fish_peld <- formatting_fish_data_PELD(data = fish_TS_data,years=years)




# ----------------------------------- #
# benthic time series (Cordeiro C) in the oceanic islands
# ----------------------------------- #




# occurrence data
benthos_DF_eMOF <- read.csv(here ("DwC_output",
                                  "PELD_iloc_benthos",
                               "DF_eMOF.txt"),sep=",",encoding= "UTF-8")

# event core
benthos_event_core<-read.csv(here ("DwC_output", 
               "PELD_iloc_benthos",
               "event_core.txt"),sep=",", encoding= "UTF-8",
         row.names=NULL)


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventDate")
benthos_TS_data <- benthos_event_core [match (benthos_DF_eMOF$eventID,
                                              benthos_event_core$eventID),
                                 variables_we_want]

# bind the occurrence data
benthos_TS_data<- cbind (benthos_TS_data,
                         benthos_DF_eMOF)

# load occ id table
benthos_DF_occ2 <- read.csv(here ("DwC_output", 
                                  "PELD_iloc_benthos",
                                  "DF_occ.txt"),sep=",",encoding= "UTF-8")

# bind taxon
benthos_TS_data <- cbind(benthos_TS_data,
                         scientificName=benthos_DF_occ2$scientificName)


# formatting
array_benthos_peld <- formatting_benthic_data_PELD(data = benthos_TS_data,years=years)




# ----------------------------------------------------- 
# benthos (Ronaldo Francini-Filho)
# variables we want from here





variables_we_want <- c("locality","eventDate")

# occurrence data
benthos_DF_eMOF_RF <- read.csv(here ("DwC_output",
                                    "RFrancini_timeSeries_abrolhos",
                                  "DF_eMOF_benthos.txt"),sep=",",encoding= "UTF-8",
                               row.names=NULL)
# event core
benthos_event_core_RF <-  read.csv(here ("DwC_output", 
                                      "RFrancini_timeSeries_abrolhos",
                                      "event_core_benthos.txt"),sep=",", encoding= "UTF-8",
                                   row.names=NULL)

# matching event IDs to find site and locality (variables_we_want)
benthos_TS_data_RF <- benthos_event_core_RF [match (benthos_DF_eMOF_RF$eventID,
                                              benthos_event_core_RF$eventID),
                                       variables_we_want]
benthos_TS_data_RF$eventDate<-substr(benthos_TS_data_RF$eventDate,1,4) # only year
# bind the occurrence data
benthos_TS_data_RF<- cbind (benthos_TS_data_RF,
                            benthos_DF_eMOF_RF)
# format
array_benthos_RF <- formatting_benthic_data_RF(data = benthos_TS_data_RF,years=years)






# ----------------------------------------------------- 
# fish Abrolhos (Ronaldo Francini-Filho)
# occurrence data





fish_DF_eMOF_RF <- read.csv(here ("DwC_output",
                                  "RFrancini_timeSeries_abrolhos",
                                  "DF_eMOF_fish.txt"),sep=",",encoding= "UTF-8",
                            row.names=NULL)
# event core
fish_event_core_RF <-  read.csv(here ("DwC_output", 
                                      "RFrancini_timeSeries_abrolhos",
                                      "event_core_fish.txt"),sep=",", 
                                encoding= "UTF-8",
                                row.names=NULL)

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventDate")
fish_TS_data_RF <- fish_event_core_RF [match (fish_DF_eMOF_RF$eventID,
                                              fish_event_core_RF$eventID),
                                             variables_we_want]
# bind the occurrence data
fish_TS_data_RF<- cbind (fish_TS_data_RF,
                         fish_DF_eMOF_RF)

# format
array_fish_RF <- formatting_fish_data_RF(data = fish_TS_data_RF,years=years)






# =============================================================================
# fish time series, Santa Catarina (SC)
# occurrence data





fish_DF_eMOF_SC <- read.csv(here ("DwC_output",
                                  "SC_time_series",
                                  "DF_eMOF.txt"),sep=",",
                            encoding= "UTF-8",
                          row.names=NULL)

# event core
fish_event_core_SC <-  read.csv(here ("DwC_output", 
                                      "SC_time_series",
                                      "event_core.txt"),sep=",", 
                                encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
fish_TS_data_SC <- fish_event_core_SC [match (fish_DF_eMOF_SC$eventID,
                                              fish_event_core_SC$eventID),
                                       variables_we_want]
# bind the occurrence data
fish_TS_data_SC<- cbind (fish_TS_data_SC,
                         fish_DF_eMOF_SC)

# format
array_fish_SC <- formatting_fish_data_SC(data = fish_TS_data_SC,years=years)





# =============================================================================
# fish time series, arraial do cabo (RJ)
# occurrence data





fish_DF_eMOF_RJ <- read.csv(here ("DwC_output",
                                  "RJ_time_series",
                                  "DF_eMOF.txt"),sep=",",
                            encoding= "UTF-8",
                            row.names=NULL)
# event core
fish_event_core_RJ <-  read.csv(here ("DwC_output", 
                                      "RJ_time_series",
                                      "event_core.txt"),sep=",", 
                                encoding= "UTF-8",
                                row.names=NULL)

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
fish_TS_data_RJ <- fish_event_core_RJ [match (fish_DF_eMOF_RJ$eventID,
                                              fish_event_core_RJ$eventID),
                                       variables_we_want]
# bind the occurrence data
fish_TS_data_RJ<- cbind (fish_TS_data_RJ,
                         fish_DF_eMOF_RJ)

# format
array_fish_RJ <- formatting_fish_data_SC(data = fish_TS_data_RJ,years=years)





#-----------------------------------------------------------------------------
# Fish snapshots (Morais et al. 2017)





fish_DF_eMOF_morais <- read.csv(here ("DwC_output",
                                  "RMorais_spatialData",
                                  "DF_eMOF.txt"),sep=",",
                            encoding= "UTF-8",
                            row.names=NULL)
# event core
fish_event_core_morais <-  read.csv(here ("DwC_output", 
                                      "RMorais_spatialData",
                                      "event_core.txt"),sep=",", 
                                encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
fish_SN_data_morais <- fish_event_core_morais [match (fish_DF_eMOF_morais$eventID,
                                                      fish_event_core_morais$eventID),
                                       variables_we_want]
# bind the occurrence data
fish_SN_data_morais<- cbind (fish_SN_data_morais,
                         fish_DF_eMOF_morais)
# filtering abundance data
fish_SN_data_morais<-fish_SN_data_morais[which(fish_SN_data_morais$measurementType == "abundance"),]

# formatting
array_fish_morais <- formatting_fish_data_SC(data = fish_SN_data_morais,
                                             years=years)





# ============================================
# benthic snapshots (Aued et al. 2018 & Francini-Filho et al.)





benthos_DF_eMOF_aued <- read.csv(here ("DwC_output",
                                      "AAued_spatialData",
                                      "DF_eMOF.txt"),sep=",",
                                encoding= "UTF-8",
                                row.names=NULL)
# event core
benthos_event_core_aued <-  read.csv(here ("DwC_output", 
                                          "AAued_spatialData",
                                          "event_core.txt"),sep=",", 
                                    encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
benthos_SN_data_aued <- benthos_event_core_aued [match (benthos_DF_eMOF_aued$eventID,
                                                        benthos_event_core_aued$eventID),
                                               variables_we_want]
# bind the occurrence data
benthos_SN_data_aued<- cbind (benthos_SN_data_aued,
                              benthos_DF_eMOF_aued)

# formatting
array_benthos_aued <- formatting_fish_data_SC(data = benthos_SN_data_aued,
                                             years=years)





# ========================================================================
# R Francini
# benthos snapshot (SN)





benthos_DF_eMOF_RF_SN <- read.csv(here ("DwC_output",
                                     "RFrancini_spatialData",
                                     "DF_eMOF.txt"),sep=",",
                               encoding= "UTF-8",
                               row.names=NULL)
# event core
benthos_event_core_RF_SN <-  read.csv(here ("DwC_output", 
                                         "RFrancini_spatialData",
                                         "event_core.txt"),sep=",", 
                                   encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
benthos_SN_data_RF <- benthos_event_core_RF_SN [match (benthos_DF_eMOF_RF_SN$eventID,
                                                       benthos_event_core_RF_SN$eventID),
                                             variables_we_want]
# bind the occurrence data
benthos_SN_data_RF<- cbind (benthos_SN_data_RF,
                            benthos_DF_eMOF_RF_SN)
# formatting
array_benthos_SN_RF <- formatting_fish_data_SC(data = benthos_SN_data_RF,
                                                years=years)





# ========================================================================
# GLongo- 61 degrees lat, fish snapshot (Longo et al. 2019)






fish_DF_eMOF_longo <- read.csv(here ("DwC_output",
                                      "GLongo_spatialData",
                                      "DF_eMOF.txt"),sep=",",
                                encoding= "UTF-8",
                                row.names=NULL)
# event core
fish_event_core_longo <-  read.csv(here ("DwC_output", 
                                          "GLongo_spatialData",
                                          "event_core.txt"),sep=",", 
                                    encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
fish_SN_data_longo <- fish_event_core_longo [match (fish_DF_eMOF_longo$eventID,
                                                    fish_event_core_longo$eventID),
                                               variables_we_want]
# bind the occurrence data
fish_SN_data_longo<- cbind (fish_SN_data_longo,
                             fish_DF_eMOF_longo)
# filtering bite rates data
fish_SN_data_longo<-fish_SN_data_longo[which(fish_SN_data_longo$measurementType == "foraging behavior"),]
fish_SN_data_longo$measurementValue <- as.numeric (fish_SN_data_longo$measurementValue)
# formatting
array_fish_longo <- formatting_fish_data_SC(data = fish_SN_data_longo,
                                             years=years)





# ==========================================================================
# GLongo RN
# Ross et al. 2019





fish_DF_eMOF_ross <- read.csv(here ("DwC_output",
                                     "GLongo_NRoss_spatialData",
                                     "DF_eMOF_fish.txt"),sep=",",
                               encoding= "UTF-8",
                               row.names=NULL)
# event core
fish_event_core_ross <-  read.csv(here ("DwC_output", 
                                         "GLongo_NRoss_spatialData",
                                         "event_core_fish.txt"),sep=",", 
                                   encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
fish_SN_data_ross <- fish_event_core_ross [match (fish_DF_eMOF_ross$eventID,
                                                  fish_event_core_ross$eventID),
                                             variables_we_want]
# bind the occurrence data
fish_SN_data_ross<- cbind (fish_SN_data_ross,
                            fish_DF_eMOF_ross)

# formatting
array_fish_ross <- formatting_fish_data_SC(data = fish_SN_data_ross,
                                            years=years)





# ===========================================================
# benthos





benthos_DF_eMOF_ross <- read.csv(here ("DwC_output",
                                    "GLongo_NRoss_spatialData",
                                    "DF_eMOF_benthos.txt"),sep=",",
                              encoding= "UTF-8",
                              row.names=NULL)
# event core
benthos_event_core_ross <-  read.csv(here ("DwC_output", 
                                        "GLongo_NRoss_spatialData",
                                        "event_core_benthos.txt"),sep=",", 
                                  encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","eventYear")
benthos_SN_data_ross <-benthos_event_core_ross [match (benthos_DF_eMOF_ross$eventID,
                                                       benthos_event_core_ross$eventID),
                                           variables_we_want]
# bind the occurrence data
benthos_SN_data_ross<- cbind (benthos_SN_data_ross,
                              benthos_DF_eMOF_ross)

# formatting
array_benthos_ross <- formatting_fish_data_SC(data = benthos_SN_data_ross,
                                           years=years)







# =============================================================================

# list of sites for checking
require(dplyr)
sites_fish <- rbind (
        data.frame (dplyr::group_by(fish_event_core, higherGeographyID,island, locality) %>% 
              summarise(mean = mean(maximumDepthInMeters)) %>% 
              select (island, locality) %>%
              rename (locationID = island),
            source = "peld fish time series (CCordeiro)"),
        data.frame (group_by(fish_event_core_RF,higherGeographyID, locationID, locality) %>% 
                      summarise(mean = mean( minimumDepthinMeters)) %>% 
                      select (higherGeographyID,locationID, locality),
                    source = "fish Abrolhos time series (RFRanciniFilho)"),
        data.frame (group_by(fish_event_core_SC,higherGeographyID, locationID, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters)) %>% 
              select (higherGeographyID,locationID, locality),
            source = "fish SC time series (SFloeter)"),
        data.frame (group_by(fish_event_core_RJ,higherGeographyID, locationID, locality) %>% 
                      summarise(mean = mean( minimumDepthinMeters)) %>% 
                      select (higherGeographyID,locationID, locality),
                    source = "fish RJ time series (TMendes)"),
        data.frame (group_by(fish_event_core_morais,higherGeographyID, locationID, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters)) %>% 
              select (higherGeographyID,locationID, locality),
            source = "fish snapshot (RMorais)"),
        data.frame (group_by(fish_event_core_longo,higherGeographyID, locationID, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters)) %>% 
              select (higherGeographyID,locationID, locality),
            source = "fish snapshot (GOLongo)"),
        data.frame (group_by(fish_event_core_ross,higherGeographyID, locationID, locality) %>% 
              summarise(mean = mean( maximumDepthInMeters)) %>% 
              select (higherGeographyID,locationID, locality),
            source = "fish snapshot (RGNorte) (GOLongo,NRoos)"))
sites_fish$locationID<- iconv(sites_fish$locationID, to="UTF-8")
sites_fish$locality<- iconv(sites_fish$locality, to="UTF-8")

# save
require(openxlsx)
write.xlsx (sites_fish, file = here("DwC_output","CheckSitesFish.xlsx"))

# benthos
sites_benthos <- rbind (
  
    data.frame (group_by(benthos_event_core, higherGeographyID,island, locality) %>% 
                  summarise(mean = mean( sampleSizeValue)) %>% 
                  select (higherGeographyID,island, locality) %>%
                  rename (locationID = island),
                source = "peld benthos time series (CCordeiro)"),
    data.frame (group_by(benthos_event_core_RF,higherGeographyID, locationID, locality) %>% 
                  summarise(mean = mean( minimumDepthinMeters)) %>% 
                  select (higherGeographyID,locationID, locality),
                source = "benthos Abrolhos time series (RFRanciniFilho)"),
    data.frame (group_by(benthos_event_core_aued,higherGeographyID, locationID, locality) %>% 
                  summarise(mean = mean( minimumDepthinMeters)) %>% 
                  select (higherGeographyID,locationID, locality),
                source = "benthos snapshot (AAued)"),
    data.frame (group_by(benthos_event_core_RF_SN,higherGeographyID, locationID, locality) %>% 
                  summarise(mean = mean( minimumDepthinMeters)) %>% 
                  select (higherGeographyID,locationID, locality),
                source = "benthos snapshot (RFranciniFilho)"),
    data.frame (group_by(benthos_event_core_ross,higherGeographyID, locationID, locality) %>% 
                  summarise(mean = mean( maximumDepthInMeters)) %>% 
                  select (higherGeographyID,locationID, locality),
                source = "benthos snapshot (RGNorte) (GOLongo,NRoos)"))
sites_benthos$locationID<- iconv(sites_benthos$locationID, to="UTF-8")
sites_benthos$locality<- iconv(sites_benthos$locality, to="UTF-8")

# save
write.xlsx (sites_benthos, file = here("DwC_output","CheckSitesBenthos.xlsx"))


# =============================================================================
# list of arrays (benthos)
list_benthic_data <- list (array_benthos_peld,
                           array_benthos_RF,
                           array_benthos_aued,
                           array_benthos_SN_RF,
                           array_benthos_ross)

# benthic data into long format
DF_long_benthos <- lapply (list_benthic_data, array_into_long_format,group="benthos")
DF_long_benthos<-do.call(rbind,DF_long_benthos) # melt

# list of sites/localities and geo location
site_geo_benthos <- list (benthos_event_core [,c("locality", "higherGeographyID")],
                           benthos_event_core_RF [,c("locality", "higherGeographyID")],
                           benthos_event_core_aued [,c("locality", "higherGeographyID")],
                           benthos_event_core_RF_SN [,c("locality", "higherGeographyID")],
                            benthos_event_core_ross [,c("locality", "higherGeographyID")])
# adjust colnames
site_geo_benthos<-lapply (site_geo_benthos, function (i){colnames(i)<- c("locationID","higherGeographyID");i})
site_geo_benthos <- do.call(rbind,site_geo_benthos)#melt
# match
DF_long_benthos$region <-site_geo_benthos[match (DF_long_benthos$site,
                                                 site_geo_benthos$locationID),"higherGeographyID"]
DF_long_benthos$site <- tolower (DF_long_benthos$site)
# order names --- the list seems largely ok

unique(DF_long_benthos$site)[order(unique(DF_long_benthos$site))]
unique(DF_long_benthos$species)[order(unique(DF_long_benthos$species))]


# valid worms species
# matching with worms
require(worrms)
worms_record <- lapply (unique(DF_long_benthos$species), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
names(worms_record)<- unique(DF_long_benthos$species)
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
DF_long_benthos$speciesWorms <- df_worms_record[match (DF_long_benthos$species,
                                                       rownames(df_worms_record)),"valid_name"]

# remove NA in speciesWorms
DF_long_benthos_valid_names <-(DF_long_benthos [which(is.na(DF_long_benthos$speciesWorms) != T),])

# bind functional groups (cca,turf, fleshy algae)
DF_long_benthos_valid_names <- rbind(DF_long_benthos_valid_names,
                                     
  DF_long_benthos [which(DF_long_benthos$species %in% c(
  "cyanobacteria",
  "crustose coralline algae",
  "calcareous articulate algae",
  'filamentous algae',
  "foliose algae",
  "corticated algae",
  "leathery algae"
  
  ) ),])

# check unique taxa
unique(DF_long_benthos_valid_names$species)[order(unique(DF_long_benthos_valid_names$species))]

# save
# save (DF_long_benthos_valid_names, file="data_for_modeling_benthos.RData")
dir.create ("format_occupancy_models")
write.csv (DF_long_benthos_valid_names, file = here ("format_occupancy_models",
                                                     "DF_long_benthos_valid_names.csv"))

# ===============================

# list of arrays (fish)
list_fish_data <- list (array_fish_peld,
                        array_fish_RF,
                        array_fish_SC,
                        array_fish_RJ,
                        array_fish_morais,
                        array_fish_longo,
                        array_fish_ross
                        )
# fish data into long format
DF_long_fish <- lapply (list_fish_data, array_into_long_format,group="fish")
DF_long_fish <- do.call(rbind,DF_long_fish) # melt

# list of sites/locality and higherGeographyID
site_geo_fish <- list (fish_event_core [,c("locality", "higherGeographyID")],
                        fish_event_core_RF [,c("locality", "higherGeographyID")],
                        fish_event_core_SC [,c("locality", "higherGeographyID")],
                       fish_event_core_RJ [,c("locality", "higherGeographyID")],
                      fish_event_core_longo [,c("locality", "higherGeographyID")],
                      fish_event_core_ross [,c("locality", "higherGeographyID")],
                      fish_event_core_morais [,c("locality", "higherGeographyID")])

# adjust colnames
site_geo_fish<-lapply (site_geo_fish, function (i){colnames(i)<- c("locationID","higherGeographyID");i})
site_geo_fish <- do.call(rbind,site_geo_fish)#melt
# adjusting site names
site_geo_fish$locationID<-(iconv(site_geo_fish$locationID, "ASCII", "UTF-8", sub=""))
site_geo_fish$locationID <- tolower(site_geo_fish$locationID)
# adjust
site_geo_fish$locationID[which(site_geo_fish$locationID == "farilhc5es")] <- "farilhoes"
site_geo_fish$locationID[which(site_geo_fish$locationID == "farol_dois")] <- "farol dois"
site_geo_fish$locationID[which(site_geo_fish$locationID == "tc:nel")] <- "tunel"
site_geo_fish$locationID[which(site_geo_fish$locationID == "tartarugas trindade")] <- "tartarugas_trindade"
site_geo_fish$locationID[which(site_geo_fish$locationID == "salc#o")] <- "salao"
site_geo_fish$locationID[which(site_geo_fish$locationID == "sao pedro  fora")] <- "sao_pedro_fora"
site_geo_fish$locationID[which(site_geo_fish$locationID == "saco_da_agua")] <- "saco_dagua"
site_geo_fish$locationID[which(site_geo_fish$locationID == "saco_do_capim")] <- "capim"
site_geo_fish$locationID[which(site_geo_fish$locationID == "saco_do_engenho")] <- "engenho"
site_geo_fish$locationID[which(site_geo_fish$locationID == "pta_agua")] <- "ponta_agua"
site_geo_fish$locationID[which(site_geo_fish$locationID == "pta_leste")] <- "ponta_leste"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia da conceic'c#o")] <- "conceicao"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia do sueste")] <- "sueste"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia do eme")] <- "eme"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia do sancho")] <- "sancho"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia das cabritas")] <- "cabritas"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia_porto")] <- "praia do porto"
site_geo_fish$locationID[which(site_geo_fish$locationID == "praia da calheta")] <- "calheta"
site_geo_fish$locationID[which(site_geo_fish$locationID == "ponta norte")] <- "ponta_norte"
site_geo_fish$locationID[which(site_geo_fish$locationID == "podes crer")] <- "podes_crer"
site_geo_fish$locationID[which(site_geo_fish$locationID == "saco_ingleses")] <- "ingleses"
site_geo_fish$locationID[which(site_geo_fish$locationID == "paredc#o")] <- "paredao"
site_geo_fish$locationID[which(site_geo_fish$locationID == "arvoredo_saco_dagua")] <- "saco_dagua"
site_geo_fish$locationID[which(site_geo_fish$locationID == "cemitério")] <- "cemiterio"
site_geo_fish$locationID[which(site_geo_fish$locationID == "laje dois irmc#os")] <- "laje_dois_irmaos"
site_geo_fish$locationID[which(site_geo_fish$locationID == "maramut")] <- "maramuta"
site_geo_fish$locationID[which(site_geo_fish$locationID == "martin vaz oeste")] <- "martin_vaz_oeste"
site_geo_fish$locationID[which(site_geo_fish$locationID == "batentedasagulhas")] <- "batente_das_agulhas"
site_geo_fish$locationID[which(site_geo_fish$locationID == "falsa_barreta")] <- "falsa barreta"
site_geo_fish$locationID[which(site_geo_fish$locationID == "piscina_das_rocas")] <- "piscina das rocas"
site_geo_fish$locationID[which(site_geo_fish$locationID == "gavetas ")] <- "gavetas"
site_geo_fish$locationID[which(site_geo_fish$locationID == "mestrevicente")] <- "mestre_vicente"
site_geo_fish$locationID[which(site_geo_fish$locationID == "naufrc!gio")] <- "naufragio"
site_geo_fish$locationID[which(site_geo_fish$locationID == "poita_do_zeca")] <- "poita do zeca"
site_geo_fish$locationID[which(site_geo_fish$locationID == "ponta_da_sapata")] <- "ponta da sapata"
site_geo_fish$locationID[which(site_geo_fish$locationID == "bc3ia")] <- "boia"
site_geo_fish$locationID[which(site_geo_fish$locationID == "cabec'o tartaruga")] <- "cabeco_tartaruga"
site_geo_fish$locationID[which(site_geo_fish$locationID == "campim")] <- "capim"
unique(site_geo_fish$locationID )[order(unique(site_geo_fish$locationID ))]

# do the same match for the sites in the dataframe
# adjusting sites
DF_long_fish$site<-(iconv(DF_long_fish$site, "ASCII", "UTF-8", sub=""))
DF_long_fish$site <- tolower(DF_long_fish$site)

# order names
# adjust
DF_long_fish$site[which(DF_long_fish$site == "farilhc5es")] <- "farilhoes"
DF_long_fish$site[which(DF_long_fish$site == "farol_dois")] <- "farol dois"
DF_long_fish$site[which(DF_long_fish$site == "tc:nel")] <- "tunel"
DF_long_fish$site[which(DF_long_fish$site == "tartarugas trindade")] <- "tartarugas_trindade"
DF_long_fish$site[which(DF_long_fish$site == "salc#o")] <- "salao"
DF_long_fish$site[which(DF_long_fish$site == "sao pedro  fora")] <- "sao_pedro_fora"
DF_long_fish$site[which(DF_long_fish$site == "saco_da_agua")] <- "saco_dagua"
DF_long_fish$site[which(DF_long_fish$site == "saco_do_capim")] <- "capim"
DF_long_fish$site[which(DF_long_fish$site == "saco_do_engenho")] <- "engenho"
DF_long_fish$site[which(DF_long_fish$site == "pta_agua")] <- "ponta_agua"
DF_long_fish$site[which(DF_long_fish$site == "pta_leste")] <- "ponta_leste"
DF_long_fish$site[which(DF_long_fish$site == "praia da conceic'c#o")] <- "conceicao"
DF_long_fish$site[which(DF_long_fish$site == "praia do sueste")] <- "sueste"
DF_long_fish$site[which(DF_long_fish$site == "praia do eme")] <- "eme"
DF_long_fish$site[which(DF_long_fish$site == "praia do sancho")] <- "sancho"
DF_long_fish$site[which(DF_long_fish$site == "praia das cabritas")] <- "cabritas"
DF_long_fish$site[which(DF_long_fish$site == "praia_porto")] <- "praia do porto"
DF_long_fish$site[which(DF_long_fish$site == "praia da calheta")] <- "calheta"
DF_long_fish$site[which(DF_long_fish$site == "ponta norte")] <- "ponta_norte"
DF_long_fish$site[which(DF_long_fish$site == "podes crer")] <- "podes_crer"
DF_long_fish$site[which(DF_long_fish$site == "saco_ingleses")] <- "ingleses"
DF_long_fish$site[which(DF_long_fish$site == "paredc#o")] <- "paredao"
DF_long_fish$site[which(DF_long_fish$site == "arvoredo_saco_dagua")] <- "saco_dagua"
DF_long_fish$site[which(DF_long_fish$site == "cemitério")] <- "cemiterio"
DF_long_fish$site[which(DF_long_fish$site == "laje dois irmc#os")] <- "laje_dois_irmaos"
DF_long_fish$site[which(DF_long_fish$site == "maramut")] <- "maramuta"
DF_long_fish$site[which(DF_long_fish$site == "martin vaz oeste")] <- "martin_vaz_oeste"
DF_long_fish$site[which(DF_long_fish$site == "batentedasagulhas")] <- "batente_das_agulhas"
DF_long_fish$site[which(DF_long_fish$site == "falsa_barreta")] <- "falsa barreta"
DF_long_fish$site[which(DF_long_fish$site == "piscina_das_rocas")] <- "piscina das rocas"
DF_long_fish$site[which(DF_long_fish$site == "gavetas ")] <- "gavetas"
DF_long_fish$site[which(DF_long_fish$site == "mestrevicente")] <- "mestre_vicente"
DF_long_fish$site[which(DF_long_fish$site == "naufrc!gio")] <- "naufragio"
DF_long_fish$site[which(DF_long_fish$site == "poita_do_zeca")] <- "poita do zeca"
DF_long_fish$site[which(DF_long_fish$site == "ponta_da_sapata")] <- "ponta da sapata"
DF_long_fish$site[which(DF_long_fish$site == "bc3ia")] <- "boia"
DF_long_fish$site[which(DF_long_fish$site == "cabec'o tartaruga")] <- "cabeco_tartaruga"
DF_long_fish$site[which(DF_long_fish$site == "campim")] <- "capim"
(unique(DF_long_fish$site)[order(unique(DF_long_fish$site))])

# making the match
DF_long_fish$region <-site_geo_fish[match (DF_long_fish$site,site_geo_fish$locationID),"higherGeographyID"]

# worms's validation
worms_record_fish <- lapply (unique(DF_long_fish$species), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
names(worms_record_fish)<- unique(DF_long_fish$species)
df_worms_record_fish <- data.frame(do.call(rbind,worms_record_fish))
# match
DF_long_fish$speciesWorms <- df_worms_record_fish[match (DF_long_fish$species,
                                                      rownames(df_worms_record_fish)),"valid_name"]

# remove NA in speciesWorms
DF_long_fish_valid_names<-(DF_long_fish [which(is.na(DF_long_fish$speciesWorms) != T),])

# df ready for analysis
write.csv (DF_long_fish_valid_names, file = "DF_long_fish_valid_names.csv")
