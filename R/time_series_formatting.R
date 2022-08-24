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
years <- seq (2001,2020)





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

# include region
fish_event_core$higherGeographyID

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

# adjust rownames
rownames(array_fish_peld)[which(rownames(array_fish_peld) == "praia_das_cabritas")]<-"cabritas"


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

# speciesName first up
benthos_TS_data$scientificName <-firstup(benthos_TS_data$scientificName)


# formatting
array_benthos_peld <- formatting_benthic_data_PELD(data = benthos_TS_data,years=years)




# ----------------------------------------------------- 
# benthos (Ronaldo Francini-Filho)
# variables we want from here





variables_we_want <- c("locality","eventDate")

# occurrence data
benthos_DF_eMOF_RF <- read.csv(here ("DwC_output",
                                    "RFrancini_timeSeries_abrolhos",
                                  "DF_eMOF_benthos.csv"),sep=",",encoding= "UTF-8",
                               row.names=NULL)
# event core
benthos_event_core_RF <-  read.csv(here ("DwC_output", 
                                      "RFrancini_timeSeries_abrolhos",
                                      "event_core_benthos.csv"),sep=",", encoding= "UTF-8",
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
                                  "DF_eMOF_fish.csv"),sep=",",encoding= "UTF-8",
                            row.names=NULL)


# event core
fish_event_core_RF <-  read.csv(here ("DwC_output", 
                                      "RFrancini_timeSeries_abrolhos",
                                      "event_core_fish.csv"),sep=",", 
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
                                  "DF_eMOF.csv"),sep=",",
                            encoding= "UTF-8",
                          row.names=NULL)

# event core
fish_event_core_SC <-  read.csv(here ("DwC_output", 
                                      "SC_time_series",
                                      "event_core.csv"),sep=",", 
                                encoding= "UTF-8")


# occ
fish_occ_SC <-  read.csv(here ("DwC_output", 
                                      "SC_time_series",
                                      "DF_occ.csv"),sep=",", 
                                encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
fish_TS_data_SC <- fish_event_core_SC [match (fish_DF_eMOF_SC$eventID,
                                              fish_event_core_SC$eventID),
                                       variables_we_want]
# bind the occurrence data
fish_TS_data_SC<- cbind (fish_TS_data_SC,
                         fish_DF_eMOF_SC)

# remove NAs
fish_TS_data_SC<-fish_TS_data_SC[which(is.na(fish_TS_data_SC$locality)!=T), ]
fish_TS_data_SC$measurementValue<-as.numeric(fish_TS_data_SC$measurementValue)

# format
array_fish_SC <- formatting_fish_data_SC(data = fish_TS_data_SC,years=years)





# =============================================================================
# fish time series, arraial do cabo (RJ)
# occurrence data





fish_DF_eMOF_RJ <- read.csv(here ("DwC_output",
                                  "RJ_time_series",
                                  "DF_eMOF.csv"),sep=",",
                            encoding= "UTF-8",
                            row.names=NULL)



# event core
fish_event_core_RJ <-  read.csv(here ("DwC_output", 
                                      "RJ_time_series",
                                      "event_core.csv"),sep=",", 
                                encoding= "UTF-8",
                                row.names=NULL)

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
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
                                  "DF_eMOF.csv"),sep=",",
                            encoding= "UTF-8",
                            row.names=NULL)

# event core
fish_event_core_morais <-  read.csv(here ("DwC_output", 
                                      "RMorais_spatialData",
                                      "event_core.csv"),sep=",", 
                                encoding= "UTF-8")



# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
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





## ============================================
## benthic snapshots (Aued et al. 2018 & Francini-Filho et al.)






benthos_DF_eMOF_aued <- read.csv(here ("DwC_output",
                                      "AAued_spatialData",
                                      "DF_eMOF.csv"),sep=",",
                                encoding= "UTF-8",
                                row.names=NULL)
# event core
benthos_event_core_aued <-  read.csv(here ("DwC_output", 
                                          "AAued_spatialData",
                                          "event_core.csv"),sep=",", 
                                    encoding= "UTF-8")




# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
benthos_SN_data_aued <- benthos_event_core_aued [match (benthos_DF_eMOF_aued$eventID,
                                                        benthos_event_core_aued$eventID),
                                               variables_we_want]
# bind the occurrence data
benthos_SN_data_aued<- cbind (benthos_SN_data_aued,
                              benthos_DF_eMOF_aued)

# formatting
array_benthos_aued <- formatting_fish_data_SC(data = benthos_SN_data_aued,
                                             years=years)





## ========================================================================
## R Francini
## benthos snapshot (SN)





benthos_DF_eMOF_RF_SN <- read.csv(here ("DwC_output",
                                     "RFrancini_spatialData",
                                     "DF_eMOF.csv"),sep=",",
                               encoding= "UTF-8",
                               row.names=NULL)
# event core
benthos_event_core_RF_SN <-  read.csv(here ("DwC_output", 
                                         "RFrancini_spatialData",
                                         "event_core.csv"),sep=",", 
                                   encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
benthos_SN_data_RF <- benthos_event_core_RF_SN [match (benthos_DF_eMOF_RF_SN$eventID,
                                                       benthos_event_core_RF_SN$eventID),
                                             variables_we_want]
# bind the occurrence data
benthos_SN_data_RF<- cbind (benthos_SN_data_RF,
                            benthos_DF_eMOF_RF_SN)
# formatting
array_benthos_SN_RF <- formatting_fish_data_SC(data = benthos_SN_data_RF,
                                                years=years)





## ========================================================================
# GLongo- 61 degrees lat, fish snapshot (Longo et al. 2019)






fish_DF_eMOF_longo <- read.csv(here ("DwC_output",
                                      "GLongo_spatialData",
                                      "DF_eMOF.csv"),sep=",",
                                encoding= "UTF-8",
                                row.names=NULL)
# event core
fish_event_core_longo <-  read.csv(here ("DwC_output", 
                                          "GLongo_spatialData",
                                          "event_core.csv"),sep=",", 
                                    encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
fish_SN_data_longo <- fish_event_core_longo [match (fish_DF_eMOF_longo$eventID,
                                                    fish_event_core_longo$eventID),
                                               variables_we_want]
## bind the occurrence data
fish_SN_data_longo<- cbind (fish_SN_data_longo,
                             fish_DF_eMOF_longo)
## filtering bite rates data
fish_SN_data_longo<-fish_SN_data_longo[which(fish_SN_data_longo$measurementType == "foraging behavior"),]
fish_SN_data_longo$measurementValue <- as.numeric (fish_SN_data_longo$measurementValue)


## formatting
array_fish_longo <- formatting_fish_data_SC(data = fish_SN_data_longo,
                                             years=years)





# ==========================================================================
# GLongo RN
# Ross et al. 2019
#
#
#
#

fish_DF_eMOF_ross <- read.csv(here ("DwC_output",
                                     "GLongo_NRoss_spatialData",
                                     "DF_eMOF_fish.csv"),sep=",",
                               encoding= "UTF-8",
                               row.names=NULL)
# event core
fish_event_core_ross <-  read.csv(here ("DwC_output", 
                                         "GLongo_NRoss_spatialData",
                                         "event_core_fish.csv"),sep=",", 
                                   encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
fish_SN_data_ross <- fish_event_core_ross [match (fish_DF_eMOF_ross$eventID,
                                                  fish_event_core_ross$eventID),
                                             variables_we_want]
# bind the occurrence data
fish_SN_data_ross<- cbind (fish_SN_data_ross,
                            fish_DF_eMOF_ross)

# formatting
array_fish_ross <- formatting_fish_data_SC(data = fish_SN_data_ross,
                                            years=years)





## ===========================================================
## benthos





benthos_DF_eMOF_ross <- read.csv(here ("DwC_output",
                                    "GLongo_NRoss_spatialData",
                                    "DF_eMOF_benthos.csv"),sep=",",
                              encoding= "UTF-8",
                              row.names=NULL)
# event core
benthos_event_core_ross <-  read.csv(here ("DwC_output", 
                                        "GLongo_NRoss_spatialData",
                                        "event_core_benthos.csv"),sep=",", 
                                  encoding= "UTF-8")

# matching event IDs to find site and locality (variables_we_want)
variables_we_want <- c("locality","year")
benthos_SN_data_ross <-benthos_event_core_ross [match (benthos_DF_eMOF_ross$eventID,
                                                       benthos_event_core_ross$eventID),
                                           variables_we_want]
# bind the occurrence data
benthos_SN_data_ross<- cbind (benthos_SN_data_ross,
                              benthos_DF_eMOF_ross)

# formatting
array_benthos_ross <- formatting_fish_data_SC(data = benthos_SN_data_ross,
                                           years=years)







## =============================================================================





# list of sites for checking
require(dplyr)
sites_fish <- rbind (
        data.frame (dplyr::group_by(fish_event_core, higherGeography,island, locality) %>% 
              summarise(mean = mean(maximumDepthInMeters,na.rm=T),
                        sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                        decimalLatitude = mean(decimalLatitude,na.rm=T),
                        decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
              select (higherGeography,island, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude) %>%
              rename (site = island),
            source = "peld fish time series (CCordeiro)"),
        
        data.frame (group_by(fish_event_core_RF,higherGeography, site, locality) %>% 
                      summarise(mean = mean( minimumDepthinMeters,na.rm=T),
                                sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                                decimalLatitude = mean(decimalLatitude,na.rm=T),
                                decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                      select (higherGeography, site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                    source = "fish Abrolhos time series (RFRanciniFilho)"),
        
        data.frame (group_by(fish_event_core_SC,higherGeography, site, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters,na.rm=T),
                        sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                        decimalLatitude = mean(decimalLatitude,na.rm=T),
                        decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
            source = "fish SC time series (SFloeter)"),
        
        data.frame (group_by(fish_event_core_RJ,higherGeography, site, locality) %>% 
                      summarise(mean = mean( minimumDepthinMeters,na.rm=T),
                                sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                                decimalLatitude = mean(decimalLatitude,na.rm=T),
                                decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                      select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                    source = "fish RJ time series (TMendes)"),
        
        data.frame (group_by(fish_event_core_morais,higherGeography, site, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters,na.rm=T),
                        sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                        decimalLatitude = mean(decimalLatitude,na.rm=T),
                        decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
            source = "fish snapshot (RMorais)"),
        
        data.frame (group_by(fish_event_core_longo,higherGeography, site, locality) %>% 
              summarise(mean = mean( minimumDepthinMeters,na.rm=T),
                        sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                        decimalLatitude = mean(decimalLatitude,na.rm=T),
                        decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
            source = "fish snapshot (GOLongo)"),
        
        data.frame (group_by(fish_event_core_ross,higherGeography, site, locality) %>% 
              summarise(mean = mean( maximumDepthInMeters,na.rm=T),
              sampleSizeValue = mean (sampleSizeValue,na.rm=T),
              decimalLatitude = mean(decimalLatitude,na.rm=T),
              decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
          select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
            source = "fish snapshot (RGNorte) (GOLongo,NRoos)"))

sites_fish$site<- iconv(sites_fish$site, to="UTF-8")
sites_fish$locality<- iconv(sites_fish$locality, to="UTF-8")

# save
require(openxlsx)
#write.xlsx (sites_fish, file = here("DwC_output","CheckSitesFish.xlsx"))


## site covariates
# benthos

sites_benthos <- rbind (
  
    data.frame (group_by(benthos_event_core, higherGeography,island, locality) %>% 
                  summarise(mean = mean( meanDepthInMeters,na.rm=T),
                            sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                            decimalLatitude = mean(decimalLatitude,na.rm=T),
                            decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                  select (higherGeography,island, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude) %>%
                rename (site = island),
                source = "peld benthos time series (CCordeiro)"),
    
    data.frame (group_by(benthos_event_core_RF,higherGeography, site, locality) %>% 
                  summarise(mean = mean(maximumDepthinMeters,na.rm=T),
                            sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                            decimalLatitude = mean(decimalLatitude,na.rm=T),
                            decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                  select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                source = "benthos Abrolhos time series (RFRanciniFilho)"),
    
    data.frame (group_by(benthos_event_core_aued,higherGeography, site, locality) %>% 
                  summarise(mean = mean( maximumDepthinMeters,na.rm=T),
                            sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                            decimalLatitude = mean(decimalLatitude,na.rm=T),
                            decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                  select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                source = "benthos snapshot (AAued)"),
    
    data.frame (group_by(benthos_event_core_RF_SN,higherGeography, site, locality) %>% 
                  summarise(mean = mean( maximumDepthinMeters,na.rm=T),
                            sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                            decimalLatitude = mean(decimalLatitude,na.rm=T),
                            decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                  select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                source = "benthos snapshot (RFranciniFilho)"),
    
    data.frame (group_by(benthos_event_core_ross,higherGeography, site, locality) %>% 
                  summarise(mean = mean( maximumDepthInMeters,na.rm=T),
                            sampleSizeValue = mean (sampleSizeValue,na.rm=T),
                            decimalLatitude = mean(decimalLatitude,na.rm=T),
                            decimalLongitude = mean(decimalLongitude,na.rm=T)) %>% 
                  select (higherGeography,site, locality, mean, sampleSizeValue, decimalLatitude, decimalLongitude),
                source = "benthos snapshot (RGNorte) (GOLongo,NRoos)"))

sites_benthos$site<- iconv(sites_benthos$site, to="UTF-8")
sites_benthos$locality<- iconv(sites_benthos$locality, to="UTF-8")

# save
#write.xlsx (sites_benthos, file = here("DwC_output","CheckSitesBenthos.xlsx"))


# =============================================================================



# list of arrays (benthos)
list_benthic_data <- list (array_benthos_peld,
                           array_benthos_RF,
                           array_benthos_aued,
                           array_benthos_SN_RF,
                           array_benthos_ross)
# naming
names (list_benthic_data) <- c("PELD","Abrolhos", "Sisbiota", "Francini_Coast", "Ross_RN")

# benthic data into long format
DF_long_benthos <- lapply (seq (1,length(list_benthic_data)), function (i) 
  
          array_into_long_format(array_data = list_benthic_data[[i]],
                                 group="benthos",
                                 db = names (list_benthic_data)[[i]])
)

# melt
DF_long_benthos<-do.call(rbind,DF_long_benthos) # melt

# list of sites/localities and geo location
site_geo_benthos <- list (benthos_event_core [,c("locality", "higherGeography")],
                           benthos_event_core_RF [,c("locality", "higherGeography")],
                           benthos_event_core_aued [,c("locality", "higherGeography")],
                           benthos_event_core_RF_SN [,c("locality", "higherGeography")],
                            benthos_event_core_ross [,c("locality", "higherGeography")])
# adjust colnames
site_geo_benthos<-lapply (site_geo_benthos, function (i){colnames(i)<- c("site","higherGeography");i})
site_geo_benthos <- do.call(rbind,site_geo_benthos)#melt
# match
DF_long_benthos$region <-site_geo_benthos[match (DF_long_benthos$site,
                                                 site_geo_benthos$site),"higherGeography"]
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
  "Cyanobacteria",
  "Crustose coralline algae",
  "Calcareous articulate algae",
  'Filamentous algae',
  "Foliose algae",
  "Corticated algae",
  "Leathery algae"
  
  ) ),])

# check unique taxa
unique(DF_long_benthos_valid_names$species)[order(unique(DF_long_benthos_valid_names$species))]

# save
# save (DF_long_benthos_valid_names, file="data_for_modeling_benthos.RData")
dir.create ("format_occupancy_models")
write.csv (DF_long_benthos_valid_names, file = here ("format_occupancy_models",
                                                     "DF_long_benthos_valid_names.csv"),
           quote=F)



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
names (list_fish_data) <- c("PELD","Abrolhos", "SC", "RJ", "Sisbiota", "Sisbiota_RN", "Ross_RN")

# fish data into long format
DF_long_fish <- lapply (seq (1,length(list_fish_data)), function (i) 
  
  array_into_long_format(list_fish_data[[i]],
                         group="fish",
                         db = names (list_fish_data)[[i]])
)

# melt
DF_long_fish <- do.call(rbind,DF_long_fish) # melt

# list of sites/locality and higherGeography
site_geo_fish <- list (fish_event_core [,c("locality", "higherGeography")],
                        fish_event_core_RF [,c("locality", "higherGeography")],
                        fish_event_core_SC [,c("locality", "higherGeography")],
                       fish_event_core_RJ [,c("locality", "higherGeography")],
                       fish_event_core_morais [,c("locality", "higherGeography")],
                      fish_event_core_longo [,c("locality", "higherGeography")],
                      fish_event_core_ross [,c("locality", "higherGeography")])

# adjust colnames
site_geo_fish<-lapply (site_geo_fish, function (i){colnames(i)<- c("site","higherGeography");i})
site_geo_fish <- do.call(rbind,site_geo_fish)#melt
# adjusting site names
site_geo_fish$site<-(iconv(site_geo_fish$site, "ASCII", "UTF-8", sub=""))
site_geo_fish$site <- tolower(site_geo_fish$site)
# adjust
site_geo_fish$site[which(site_geo_fish$site == "cagarras_noronha")] <- "cagarras"
site_geo_fish$site[which(site_geo_fish$site == "praia_das_cabritas")] <- "cabritas"
site_geo_fish$site[which(site_geo_fish$site == "praia_do_porto")] <- "porto"
unique(site_geo_fish$site )[order(unique(site_geo_fish$site ))]

# do the same match for the sites in the dataframe
# adjusting sites
DF_long_fish$site<-(iconv(DF_long_fish$site, "ASCII", "UTF-8", sub=""))
DF_long_fish$site <- tolower(DF_long_fish$site)

# order names
# adjust
DF_long_fish$site[which(DF_long_fish$site == "cagarras_noronha")] <- "cagarras"
DF_long_fish$site[which(DF_long_fish$site == "praia_das_cabritas")] <- "cabritas"
DF_long_fish$site[which(DF_long_fish$site == "praia_do_porto")] <- "porto"
(unique(DF_long_fish$site)[order(unique(DF_long_fish$site))])


# making the match
DF_long_fish$region <-site_geo_fish[match (DF_long_fish$site,site_geo_fish$site),
                                    "higherGeography"]

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

# check spp
unique(DF_long_fish_valid_names$species)[order(unique(DF_long_fish_valid_names$species))]

# df ready for analysis
write.csv (DF_long_fish_valid_names, file = here ("format_occupancy_models","DF_long_fish_valid_names.csv"),
           quote=F)


# ------------------------------------------------------------------

# save site covariates
sites_fish$locality[which(sites_fish$locality == "praia_das_cabritas")] <- "cabritas"
sites_fish$locality[which(sites_fish$locality == "praia_do_porto")] <- "porto"
unique(sites_fish$locality )[order(unique(sites_fish$locality ))]
# fish_site_covariates
fish_site_covariates <- (sites_fish [match(unique(DF_long_fish$site), sites_fish$locality),])


# -----------------------
# NOAA Ocean Data




# parameters
SSTstartDate <- "2012-01-01"  ## define start date of your time series 

## set dataset source (monthly SST)
## the list of datasets is here
## https://coastwatch.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=griddap
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/index.html?page=1&itemsPerPage=1000


# SST
SSTsource <- info("jplMURSST41mday")

# create a cluster of 'ncores', 
ncores <- 5
cl <- makeCluster (ncores)
# load data and functions in each core 
clusterExport(cl, c("SSTsource",
                    "SSTstartDate",
                    "fish_site_covariates"))
# load packages in each core
clusterEvalQ(cl,library("rerddap"))

## Get sst 
SST <- parLapply (cl, seq (1,nrow (fish_site_covariates)), function (i) {
  
  tryCatch(
    
    griddap(SSTsource, 
            time=c(SSTstartDate, "last"),
            longitude = c(fish_site_covariates$decimalLongitude[i],
                          fish_site_covariates$decimalLongitude[i]),
            latitude = c(fish_site_covariates$decimalLatitude[i], 
                         fish_site_covariates$decimalLatitude[i]), 
            fields = "sst", # to have the field you need to go to the graph option in the website (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018r6718day.graph) and going to "color"
            fmt = "csv"),
    error = function(e) return ("NULL"))}#{print(e); print("retrying...")}
)
stopCluster (cl)




# turbidity
DEPTHsource <- info("erdMH1kd4901day")
cl <- makeCluster (ncores) #cluster
# load data and functions in each core 
clusterExport(cl, c("DEPTHsource", 
                    "SSTstartDate",
                    "fish_site_covariates"))
# load packages in each core
clusterEvalQ(cl,library("rerddap"))

## Get depth
DEPTH <- parLapply (cl, seq (1,nrow (fish_site_covariates)), function (i) {
  
  tryCatch(
    
    griddap(DEPTHsource, 
            time=c(SSTstartDate, "last"),
            longitude = c(fish_site_covariates$decimalLongitude[i],
                          fish_site_covariates$decimalLongitude[i]),
            latitude = c(fish_site_covariates$decimalLatitude[i], 
                         fish_site_covariates$decimalLatitude[i]), 
            fields = "k490", # to have the field you need to go to the graph option in the website (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018r6718day.graph) and going to "color"
            fmt = "csv"),
    error = function(e) return ("NULL"))}#{print(e); print("retrying...")}
)
stopCluster (cl) # stop cluster





# productivity (Chlorophyll-a)
PRODsource <- info("erdMH1chla8day")
cl <- makeCluster (ncores) # cluster

# load data and functions in each core 
clusterExport(cl, c("PRODsource", 
                    "SSTstartDate",
                    "fish_site_covariates"))
# load packages in each core
clusterEvalQ(cl,library("rerddap"))

## Get productivity
PRODUCTIVITY <- parLapply (cl, seq (1,nrow (fish_site_covariates)), function (i) {
  
  tryCatch(
    
    griddap(PRODsource, 
            time=c(SSTstartDate, "last"),
            longitude = c(fish_site_covariates$decimalLongitude[i],
                          fish_site_covariates$decimalLongitude[i]),
            latitude = c(fish_site_covariates$decimalLatitude[i], 
                         fish_site_covariates$decimalLatitude[i]), 
            fields = "chlorophyll", # to have the field you need to go to the graph option in the website (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018r6718day.graph) and going to "color"
            fmt = "csv"),
    error = function(e) return ("NULL"))}#{print(e); print("retrying...")}
)

stopCluster (cl)



# salinity source
SALINsource <- info("jplAquariusSSSDailyV5")

##
cl <- makeCluster (ncores)

## Get sst cl <- makeCluster (ncores)
# load data and functions in each core 
clusterExport(cl, c("SALINsource", 
                    "SSTstartDate",
                    "fish_site_covariates"))
# load packages in each core
clusterEvalQ(cl,library("rerddap"))

# run
## Get productivity

SALINITY <- parLapply (cl, seq (1,nrow (fish_site_covariates)), function (i) {
  
  tryCatch(
    
    griddap(SALINsource, 
            time=c(SSTstartDate, "last"),
            longitude = c(fish_site_covariates$decimalLongitude[i],
                          fish_site_covariates$decimalLongitude[i]),
            latitude = c(fish_site_covariates$decimalLatitude[i], 
                         fish_site_covariates$decimalLatitude[i]), 
            fields = "sss", # to have the field you need to go to the graph option in the website (https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVH2018r6718day.graph) and going to "color"
            fmt = "csv"),
    error = function(e) return ("NULL"))}#{print(e); print("retrying...")}
)

stopCluster (cl)





# aggregate per year
## summarized  variables
# sst
SST_site <- unlist (lapply (SST, function (i)  {
  
  i$year <- substr(i$time,1,4)

  # summarize
  i %>% group_by (year) %>%
    summarise(sst = mean(sst,na.rm=T))
  
  
}))
# turbidity
kd490_site <- unlist (lapply (DEPTH, function (i) 
  
  summarise(k490 = mean(k490,na.rm=T))
  
))
# PROD
prod_site <- unlist (lapply (PRODUCTIVITY, function (i) 
  
  i %>% summarise(chlorophyll = mean(chlorophyll,na.rm=T))
  
))
# salinity
salinity_site <- unlist (lapply (SALINITY, function (i) 
  
  i %>% summarise(sss = mean(sss,na.rm=T))
  
))


# site covs
site_covs <- data.frame (sites = sites,
                         sst = SST_site,
                         turbidity = kd490_site,
                         productivity = prod_site,
                         salinity = salinity_site,
                         depth = site_depth)









# df ready for analysis
write.csv (fish_site_covariates, file = here ("format_occupancy_models","fish_site_covariates.csv"))


# benthos
benthos_site_covariates <- (sites_benthos [match(unique(DF_long_benthos$site), tolower (sites_benthos$locality)),])
# df ready for analysis
write.csv (benthos_site_covariates, file = here ("format_occupancy_models","benthos_site_covariates.csv"))








