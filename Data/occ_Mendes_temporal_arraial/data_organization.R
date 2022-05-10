# load TS arraial - Mendes et al. 
# SC time series
require(openxlsx); require(here); require(worrms); require(dplyr)

# fish data
arraial_time_series <- read.csv(here ("Data",
                                 "occ_Mendes_temporal_arraial",
                                 "uvcs_arraial_2003_2021.csv"),
                           sep=",")




# --------------------------------------------------------------------------
# ADJUSTING MEASUREMENT VALUES





# split abundance and size data
abundance <- arraial_time_series[,which(colnames(arraial_time_series) != "size_cm")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abun")] <- "measurementValue"

# size
size <- arraial_time_series[,which(colnames(arraial_time_series) != "abun")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "size_cm")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (abundance,
                     size)




# --------------------------------------------------------------------------
# DWC DESCRIPTORS






# method
dados_bind$samplingProtocol <- "underwater visual survey - 20 x 2m"
# effort
dados_bind$samplingEffort <- 1# "one observer per transect"
# sampleSizeValue
dados_bind$sampleSizeValue <- 40 # area
# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(dados_bind)[which(colnames(dados_bind) == "observer")] <- "recordedBy"
# depth
colnames(dados_bind)[which(colnames(dados_bind) == "prof_media")] <- "minimumDepthinMeters"
dados_bind$maximumDepthinMeters <- dados_bind$minimumDepthinMeters
# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# coordinates
dados_bind$decimalLongitude<- dados_bind$lon# long
dados_bind$decimalLatitude<- dados_bind$lat #lat

# scientificName
colnames(dados_bind)[which(colnames(dados_bind) == "Taxa")] <- "scientificName"
# locality
colnames(dados_bind)[which(colnames(dados_bind) == "locality")] <- "locationID"
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "locality"
# year
colnames(dados_bind)[which(colnames(dados_bind) == "year")] <- "eventYear"




# --------------------------------------------------------------------------
# ADJUSTING DATES




# eventDate
dados_bind$eventMonth <- (dados_bind$month)
dados_bind$eventMonth [which(dados_bind$eventMonth == "jan")] <- 01
dados_bind$eventMonth [which(dados_bind$eventMonth == "feb")] <- 02
dados_bind$eventMonth [which(dados_bind$eventMonth == "mar")] <- 03
dados_bind$eventMonth [which(dados_bind$eventMonth == "apr")] <- 04
dados_bind$eventMonth [which(dados_bind$eventMonth == "may")] <- 05
dados_bind$eventMonth [which(dados_bind$eventMonth == "jun")] <- 06
dados_bind$eventMonth [which(dados_bind$eventMonth == "jul")] <- 07
dados_bind$eventMonth [which(dados_bind$eventMonth == "aug")] <- 08
dados_bind$eventMonth [which(dados_bind$eventMonth == "sep")] <- 09
dados_bind$eventMonth [which(dados_bind$eventMonth == "oct")] <- 10
dados_bind$eventMonth [which(dados_bind$eventMonth == "nov")] <- 11
dados_bind$eventMonth [which(dados_bind$eventMonth == "dec")] <- 12

# date
dados_bind$eventDate <- as.Date (paste(dados_bind$eventYear, 
               dados_bind$eventMonth,
               dados_bind$day,sep="-"))

# country and code
dados_bind$Country <- "Brazil"
dados_bind$countryCode <- "BR"
# basisOfRecord
dados_bind$basisOfRecord <- "HumanObservation"
# geodeticDatum
dados_bind$geodeticDatum <- "decimal degrees"
# geographic location
dados_bind$higherGeographyID <- "BrazilianCoast"





# --------------------------------------------------------------------------
# ADJUSTING SPECIES NAMES




# verbatimIdentification
dados_bind$verbatimIdentification <-dados_bind$scientificName 

# matching names with worms
worms_record <- lapply (unique(dados_bind$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# df_worms_record[which(df_worms_record$match_type == "near_1"),]
# match
dados_bind$scientificNameOBIS<-(df_worms_record$scientificname [match (dados_bind$scientificName,
                                                           (df_worms_record$scientificname))])
dados_bind$scientificNameID<-(df_worms_record$lsid [match (dados_bind$scientificName,
                                                            (df_worms_record$scientificname))])
dados_bind$kingdom <-(df_worms_record$kingdom [match (dados_bind$scientificName,
                                                       (df_worms_record$scientificname))])
# class
dados_bind$class<-(df_worms_record$class [match (dados_bind$scientificName,
                                                 (df_worms_record$scientificname))])
# family
dados_bind$family<-(df_worms_record$family [match (dados_bind$scientificName,
                                                   (df_worms_record$scientificname))])





# --------------------------------------------------------------------------
# ADJUSTING SITES




# adjusting site names
dados_bind$locality<-(iconv(dados_bind$locality, "ASCII", "UTF-8", sub=""))
dados_bind$locality <- tolower(dados_bind$locality)
# unique(dados_bind$locality)[order(unique(dados_bind$locality))]




# --------------------------------------------------------------------------
# CREATING IDS


# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste ("BR:RJ_TIME_SERIES:",
                                          dados_bind$locationID,sep=""), 
                                           dados_bind$locality, 
                                          dados_bind$eventYear,
                              sep="_")

# creating eventIds
dados_bind$eventID <- paste (paste ("BR:RJ_TIME_SERIES-MAR:",
                                    dados_bind$locationID,sep=""), 
                                    dados_bind$locality, 
                                    dados_bind$eventYear,
                                    dados_bind$unique_id,
                        sep="_")

# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste ("BR:RJ_TIME_SERIES-MAR:",
                                         dados_bind$locationID,sep=""), 
                                  dados_bind$locality, 
                                  dados_bind$eventYear,
                                  dados_bind$unique_id,
                             paste ("occ",seq(1,nrow(dados_bind)),sep=""),
                             sep="_")






# --------------------------------------------------------------------------
# Formatted according to DwC






DF_eMOF <- dados_bind [,c("eventID", "occurrenceID",
                          "verbatimIdentification",
                          "scientificName",
                          "scientificNameID",
                          "scientificNameOBIS",
                          "kingdom","class","family",
                          "measurementValue", "measurementType","measurementUnit")]


DF_occ <- dados_bind [,c("eventID", "occurrenceID","basisOfRecord",
                         "verbatimIdentification",
                         "scientificName",
                         "scientificNameID",
                         "scientificNameOBIS",
                         "kingdom","class","family",
                         "recordedBy", "organismQuantityType", "occurrenceStatus")]


# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeographyID,locationID,locality) %>% 
                            
                                summarise(eventYear = mean(eventYear),
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
# txt format
write.table(DF_occ, file =here("DwC_output",
                               "RJ_time_series",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)

write.table(DF_eMOF, file =here("DwC_output",
                                "RJ_time_series",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)


write.table(event_core, file =here("DwC_output",
                                   "RJ_time_series",
                                   "event_core.txt"),sep=",",
            quote = FALSE)

## end
