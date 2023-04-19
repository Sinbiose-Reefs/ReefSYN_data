
# load Quimbayo et al. data

# Alcarazes time series
require(here); require(openxlsx); require(worrms); require(dplyr)




# fish data
alcatrazes_time_series <- read.xlsx(here ("Data",
                                 "occ_Quimbayo_temporal_alcatrazes",
                                 "uvcs_alcatrazes_2023_01_19.xlsx"),
                           sheet="Fishes",
                           detectDates = F)



# -------------------------------------
# ADJUST SIZE AND ABUNDANCE DATA (LONG FORMAT)


# split abundance and size data
abundance <- alcatrazes_time_series[,which(colnames(alcatrazes_time_series) != "size_cm")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abun")] <- "measurementValue"

# size
size <- alcatrazes_time_series[,which(colnames(alcatrazes_time_series) != "abun")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "size_cm")] <- "measurementValue"

# bind edited data
alcatrazes_time_series <- rbind (abundance,
                     size)



# ----------------------------------------------------------------------------
# ADJUSTING AND CHECKING SCIENTIFIC NAMES
alcatrazes_time_series$verbatimIdentification <- paste (alcatrazes_time_series$genus,
                                                        alcatrazes_time_series$species,
                                                        sep = " ")
# checking 
alcatrazes_time_series$verbatimIdentification[grep ("spp", alcatrazes_time_series$verbatimIdentification)]# genus level
alcatrazes_time_series$verbatimIdentification <- gsub (" spp", "", alcatrazes_time_series$verbatimIdentification)

# matching with worms
worms_record <- lapply (unique(alcatrazes_time_series$verbatimIdentification), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)



# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# check matching
unique(alcatrazes_time_series$verbatimIdentification) %in% tolower (df_worms_record$scientificname)

# valid name WORMS
alcatrazes_time_series$scientificNameAccepted <- (df_worms_record$scientificname [match (alcatrazes_time_series$verbatimIdentification,
                                                                             tolower (df_worms_record$scientificname))])
# id
alcatrazes_time_series$scientificNameID <-(df_worms_record$lsid [match (alcatrazes_time_series$verbatimIdentification,
                                                           tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
alcatrazes_time_series$taxonRank <- (df_worms_record$rank [match (alcatrazes_time_series$verbatimIdentification,
                                                      tolower (df_worms_record$scientificname))])

# kingdom
alcatrazes_time_series$kingdom <-(df_worms_record$kingdom [match (alcatrazes_time_series$verbatimIdentification,
                                                      tolower (df_worms_record$scientificname))])
# phylum
alcatrazes_time_series$phylum <-(df_worms_record$phylum [match (alcatrazes_time_series$verbatimIdentification,
                                                    tolower (df_worms_record$scientificname))])


# class
alcatrazes_time_series$class<-(df_worms_record$class [match (alcatrazes_time_series$verbatimIdentification,
                                                 tolower (df_worms_record$scientificname))])
# order
alcatrazes_time_series$order<-(df_worms_record$order [match (alcatrazes_time_series$verbatimIdentification,
                                                 tolower (df_worms_record$scientificname))])

# family
alcatrazes_time_series$family<-(df_worms_record$family [match (alcatrazes_time_series$verbatimIdentification,
                                                   tolower (df_worms_record$scientificname))])

# genus
alcatrazes_time_series$genus <-(df_worms_record$genus [match (alcatrazes_time_series$verbatimIdentification,
                                                               tolower (df_worms_record$scientificname))])





# taxonomic updates
# species
alcatrazes_time_series$scientificNameAccepted[grep ("multilineata", alcatrazes_time_series$scientificNameAccepted)] <- "Azurina multilineata"
alcatrazes_time_series$scientificNameAccepted[grep ("polygonius", alcatrazes_time_series$scientificNameAccepted)] <- "Acanthostracion polygonium"

# genus
alcatrazes_time_series$genus[grep ("multilineata", alcatrazes_time_series$scientificNameAccepted)] <- "Azurina"





# ----------------------------------------------------------------------------
# ADJUSTING SAMPLING DATES AND METHODOLOGICAL METADATA
# https://sparkbyexamples.com/r-programming/dates-and-times-in-r/#:~:text=Times%20in%20R%20are%20represented,since%201970%2D01%2D01%20.
# eventTime

# day and month inverted
alcatrazes_time_series$verbatimDay <- alcatrazes_time_series$day
alcatrazes_time_series$verbatimMonth <- alcatrazes_time_series$month


# paste 0 if sole number
# correct day
alcatrazes_time_series$day <- (ifelse (alcatrazes_time_series$verbatimMonth<10, 
                                        paste0("0",alcatrazes_time_series$verbatimMonth),
                                       alcatrazes_time_series$verbatimMonth))

# correct month
alcatrazes_time_series$month <- alcatrazes_time_series$verbatimDay

# eventDate
alcatrazes_time_series$eventDate <- as.Date (paste(alcatrazes_time_series$year, 
                                                   alcatrazes_time_series$month,
                                                   alcatrazes_time_series$day,
                                                   sep="-"))
# time
# https://stackoverflow.com/questions/54474404/how-to-detect-time-when-reading-from-an-excel-sheet-using-r

# start UVC time
start_uvc <- as.numeric(alcatrazes_time_series$hour_start)*24 
minutes <- round((start_uvc %% 1)*60, digits = 0) 
minutes<- as.numeric(ifelse (minutes < 10, 
                             paste0("0",minutes), 
                             minutes)) # #if minutes is a single digit need to insert a preceding 0
hours <- round(start_uvc - minutes/60, digits = 0)
alcatrazes_time_series$startTime <- ifelse (is.na(alcatrazes_time_series$hour_start), 
                                          NA,
                                          (paste0(hours, ":", minutes))
)


# end UVC time
end_uvc <- as.numeric(alcatrazes_time_series$hour_end)*24 
minutes <- round((end_uvc %% 1)*60, digits = 0) 
minutes<- as.numeric(ifelse (minutes < 10, 
                             paste0("0",minutes), 
                             minutes)) # #if minutes is a single digit need to insert a preceding 0
hours <- round(end_uvc - minutes/60, digits = 0)
alcatrazes_time_series$endTime <- ifelse (is.na(alcatrazes_time_series$hour_end), 
                                          NA,
                                          (paste0(hours, ":", minutes))
)

# eventTime
alcatrazes_time_series$eventTime <- ifelse (is.na(alcatrazes_time_series$startTime),
                                            NA,
                                            paste (alcatrazes_time_series$startTime,
                                                  paste (alcatrazes_time_series$endTime,"Z",sep=""),
                                           sep = "Z/")
)


# method
alcatrazes_time_series$samplingProtocol <- "Underwater visual survey - 20 x 2m" #  
# effort
alcatrazes_time_series$samplingEffort <- 1 #"one observer per transect"
# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS)
alcatrazes_time_series$sampleSizeValue <- 40 # area
# sampleSizeUnit
alcatrazes_time_series$sampleSizeUnit <- "squared meters"
# recordedBy
alcatrazes_time_series$recordedBy <- alcatrazes_time_series$observer
# occurrenceStatus
alcatrazes_time_series$occurrenceStatus <- "presence"
# basisOfRecord
alcatrazes_time_series$basisOfRecord <- "HumanObservation"

# coordinates
alcatrazes_time_series$decimalLongitude<- alcatrazes_time_series$longitude
alcatrazes_time_series$decimalLatitude <- alcatrazes_time_series$latitude
# geodeticDatum
alcatrazes_time_series$geodeticDatum <- "decimal degrees"

# depth of sampling
alcatrazes_time_series$maximumDepthinMeters <- as.numeric(alcatrazes_time_series$depth)
alcatrazes_time_series$minimumDepthinMeters <- as.numeric(alcatrazes_time_series$depth)

# site and locality are inverted
alcatrazes_time_series$verbatimSite <- alcatrazes_time_series$locality
alcatrazes_time_series$verbatimLocality <- alcatrazes_time_series$site

# site and locality
alcatrazes_time_series$site <- alcatrazes_time_series$verbatimSite
alcatrazes_time_series$locality <- alcatrazes_time_series$verbatimLocality

# country and code
alcatrazes_time_series$Country <- "Brazil"
alcatrazes_time_series$countryCode <- "BR"
# geographic location
alcatrazes_time_series$higherGeography <- "BrazilianCoast"




# adjust site names
alcatrazes_time_series$locality[which(alcatrazes_time_series$locality == "oratorio")] <- "saco_do_oratorio"
alcatrazes_time_series$locality[which(alcatrazes_time_series$locality == "portinho")] <- "portinho_centro"
 

# ----------------------------------------------------------------------------
# CREATING IDS





# IDs
# creating parentIDs
alcatrazes_time_series$parentEventID <- paste (paste (paste ("BR:ReefSYN:ALCATRAZES-TIME-SERIES:", 
                                                             alcatrazes_time_series$higherGeography,
                                                 sep=""),
                                                 alcatrazes_time_series$site,sep=":"),
                                               alcatrazes_time_series$locality,
                                           alcatrazes_time_series$year,
                              sep="_")



# creating eventIds
alcatrazes_time_series$eventID <- paste (paste (paste ("BR:ReefSYN:ALCATRAZES-TIME-SERIES:", 
                                           alcatrazes_time_series$higherGeography,
                                           sep=""),
                                    alcatrazes_time_series$site,sep=":"),  
                                    alcatrazes_time_series$locality,
                                    alcatrazes_time_series$year,
                             alcatrazes_time_series$transect_id_locality,
                        sep="_")




# creating occurrenceIDs
alcatrazes_time_series$occurrenceID <- paste (paste (paste ("BR:ReefSYN:ALCATRAZES-TIME-SERIES:", 
                                                            alcatrazes_time_series$higherGeography,
                                                sep=""),
                                                alcatrazes_time_series$site,sep=":"),  
                                              alcatrazes_time_series$locality,
                                              alcatrazes_time_series$year,
                                              alcatrazes_time_series$transect_id_locality,
                             paste ("occ",seq(1,nrow(alcatrazes_time_series)),sep=""),
                             sep="_")




# licence
alcatrazes_time_series$licence <- "CC BY-NC"

# language
alcatrazes_time_series$language <- "en"





# ----------------------------------------------------------------------------
#  Formatted according to DwC







DF_eMOF <- alcatrazes_time_series [,c("eventID", 
                          "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit")]



DF_occ <- alcatrazes_time_series [,c("eventID", 
                         "occurrenceID",
                         "basisOfRecord",
                         "verbatimIdentification",
                         "scientificNameID",
                         "scientificNameAccepted",
                         "taxonRank",
                         "kingdom",
                         "phylum",
                         "class",
                         "order",
                         "family",
                         "genus",
                         "recordedBy",
                         "organismQuantityType", 
                         "occurrenceStatus",
                         "licence",
                         "language")]



# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(alcatrazes_time_series, 
                                   eventID,higherGeography,site,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      eventTime = unique(eventTime),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      seaSurfaceTemperature = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort),
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


# create dir
dir.create(here ("DwC_output",
                 "Alcatrazes_time_series"))


# save
# write to txt format
write.csv(DF_occ, file =here("DwC_output",
                             "Alcatrazes_time_series",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                              "Alcatrazes_time_series",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                 "Alcatrazes_time_series",
                                   "event_core.csv"))

## end
rm(list=ls())
