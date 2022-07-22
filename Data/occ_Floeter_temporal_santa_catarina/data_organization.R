
# load Floeter et al. data

# SC time series
require(here); require(openxlsx); require(worrms); require(dplyr)



# fish data
sc_time_series <- read.csv(here ("Data",
                                 "occ_Floeter_temporal_santa_catarina",
                                 "censos_sc.csv"),
                           sep=";")

# location
sc_location <- read.xlsx(here ("Data",
                                 "occ_Floeter_temporal_santa_catarina",
                                 "location.xlsx"),
                           sep=";")




# ----------------------------------------------------------------------------
# ADJUSTING MEASUREMENT VALUE





# split abundance and size data
abundance <- sc_time_series[,which(colnames(sc_time_series) != "total_lenght")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abundance")] <- "measurementValue"

# size
size <- sc_time_series[,which(colnames(sc_time_series) != "abundance")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "total_lenght")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (abundance,
                     size)

# method
dados_bind$samplingProtocol <- "underwater visual survey - 20 x 2m"
# effort
dados_bind$samplingEffort <- 1#"one observer per transect"
# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS)
dados_bind$sampleSizeValue <- 40 # area
# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(dados_bind)[which(colnames(dados_bind) == "observer")] <- "recordedBy"
# depth
colnames(dados_bind)[which(colnames(dados_bind) == "depth")] <- "depthInMeters"
# into number
dados_bind$depthInMeters<- gsub(",",".",dados_bind$depthInMeters)
dados_bind$depthInMeters <- as.numeric((dados_bind$depthInMeters))
# set min and max
colnames(dados_bind)[which(colnames(dados_bind) == "depthInMeters")] <- "minimumDepthinMeters"
dados_bind$maximumDepthinMeters <- dados_bind$minimumDepthinMeters
# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# coordinates
dados_bind$decimalLongitude<- gsub(",",".",dados_bind$longitude)# long
dados_bind$decimalLongitude <- as.numeric((dados_bind$decimalLongitude))
dados_bind$decimalLatitude<- gsub(",",".",dados_bind$latitude) #lat
dados_bind$decimalLatitude <- as.numeric((dados_bind$decimalLatitude))




# ----------------------------------------------------------------------------
# ADJUSTING COLNAMES, DATES



# scientificName
colnames(dados_bind)[which(colnames(dados_bind) == "species")] <- "scientificName"
# locality
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "locality"
# more general == site
colnames(dados_bind)[which(colnames(dados_bind) == "location")] <- "site"

# editing month
dados_bind$month [which(dados_bind$month == "december")] <- 12
dados_bind$month [which(dados_bind$month == "april")] <- 04
dados_bind$month [which(dados_bind$month == "march")] <- 03
dados_bind$month [which(dados_bind$month == "january")] <- 01
dados_bind$month [which(dados_bind$month == "february")] <- 02
dados_bind$month [which(dados_bind$month == "november")] <- 11
dados_bind$month [which(dados_bind$month == "may")] <- 05
dados_bind$month [which(dados_bind$month == "july")] <- 07
dados_bind$month [which(dados_bind$month == "august")] <- 08
dados_bind$month [which(dados_bind$month == "september")] <- 09
dados_bind$month [which(dados_bind$month == "october")] <- 10

# eventDate
dados_bind$eventDate <- as.Date (paste(dados_bind$year, 
               dados_bind$month,
               dados_bind$day,sep="-"))



# country and code
dados_bind$Country <- "Brazil"
dados_bind$countryCode <- "BR"
# basisOfRecord
dados_bind$basisOfRecord <- "HumanObservation"
# geodeticDatum
dados_bind$geodeticDatum <- "decimal degrees"
# geographic location
dados_bind$higherGeography <- "BrazilianCoast"





# ----------------------------------------------------------------------------
# ADJUSTING SITES




# verbatimLocality
dados_bind$verbatimLocality<- dados_bind$locality


# adjusting species scientific name
# replacing "_" by " "
dados_bind$locality <-(iconv(dados_bind$locality, "ASCII", "UTF-8", sub=""))
dados_bind$locality <- tolower(dados_bind$locality)


# adjust
dados_bind$locality[which(dados_bind$locality == "saco_da_agua")] <- "saco_dagua"
dados_bind$locality[which(dados_bind$locality == "saco_do_capim")] <- "capim"
dados_bind$locality[which(dados_bind$locality == "saco_do_engenho")] <- "engenho"
unique(dados_bind$locality )[order(unique(dados_bind$locality ))]




# ----------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAME



# replace "_"
dados_bind$verbatimIdentification <- gsub ("_", " ", dados_bind$scientificName)
dados_bind$species_to_search <- gsub ("_", " ", dados_bind$scientificName)


# solve a couple of problems in identification
dados_bind$species_to_search [grep ("decapteru macarellus", dados_bind$species_to_search)] <- "decapterus macarellus"

# non identified species
dados_bind$identificationQualifier <- ifelse (sapply (strsplit (dados_bind$verbatimIdentification, " "), "[[", 2) == "sp",
        "sp",
        NA)

# species to search
dados_bind$species_to_search [which(dados_bind$identificationQualifier == "sp")] <- gsub (" sp",
                                                                                          "",
                                                                                          dados_bind$species_to_search [which(dados_bind$identificationQualifier == "sp")])

# matching with worms
worms_record <- lapply (unique(dados_bind$species_to_search), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)



# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))


# valid name WORMS
dados_bind$scientificName <- (df_worms_record$scientificname [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])
# id
dados_bind$scientificNameID<-(df_worms_record$lsid [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
dados_bind$taxonRank <- (df_worms_record$rank [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])

# kingdom
dados_bind$kingdom <-(df_worms_record$kingdom [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])
# class
dados_bind$class<-(df_worms_record$class [match (dados_bind$species_to_search,
                                                 tolower(df_worms_record$scientificname))])
# family
dados_bind$family<-(df_worms_record$family [match (dados_bind$species_to_search,
                                                   tolower(df_worms_record$scientificname))])




# ----------------------------------------------------------------------------
# CREATING IDS







# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                                 dados_bind$higherGeography,
                                                 sep=""),
                                          dados_bind$site,sep=":"), 
                                           dados_bind$locality, 
                                          dados_bind$year,
                              sep="_")



# creating eventIds
dados_bind$eventID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                           dados_bind$higherGeography,
                                           sep=""),
                                    dados_bind$site,sep=":"),  
                                    dados_bind$locality, 
                                    dados_bind$year,
                                    substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                        sep="_")




# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                                dados_bind$higherGeography,
                                                sep=""),
                                         dados_bind$site,sep=":"),  
                                  dados_bind$locality, 
                                  dados_bind$year,
                                  substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                             paste ("occ",seq(1,nrow(dados_bind)),sep=""),
                             sep="_")




# licence
dados_bind$licence <- "CC BY-NC"

# language
dados_bind$language <- "en"





# ----------------------------------------------------------------------------
#  Formatted according to DwC







DF_eMOF <- dados_bind [,c("eventID", "occurrenceID",
                          "verbatimIdentification",
                          "scientificNameID",
                          "scientificName",
                          "taxonRank",
                          "kingdom",
                          "class",
                          "family",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit")]



DF_occ <- dados_bind [,c("eventID", 
                         "occurrenceID",
                         "basisOfRecord",
                         "verbatimIdentification",
                         "scientificNameID",
                         "scientificName",
                         "taxonRank",
                         "kingdom",
                         "class","family",
                         "recordedBy",
                         "organismQuantityType", 
                         "occurrenceStatus",
                         "licence",
                         "language")]



# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort),
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
write.csv(DF_occ, file =here("DwC_output",
                               "SC_time_series",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "SC_time_series",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "SC_time_series",
                                   "event_core.csv"))

## end
