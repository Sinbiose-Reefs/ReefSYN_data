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
colnames(dados_bind)[which(colnames(dados_bind) == "location")] <- "locationID"
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "locality"
# year
colnames(dados_bind)[which(colnames(dados_bind) == "year")] <- "eventYear"
# eventDate
dados_bind$eventMonth <- (dados_bind$month)
dados_bind$eventMonth [which(dados_bind$eventMonth == "december")] <- 12
dados_bind$eventMonth [which(dados_bind$eventMonth == "april")] <- 04
dados_bind$eventMonth [which(dados_bind$eventMonth == "march")] <- 03
dados_bind$eventMonth [which(dados_bind$eventMonth == "january")] <- 01
dados_bind$eventMonth [which(dados_bind$eventMonth == "february")] <- 02
dados_bind$eventMonth [which(dados_bind$eventMonth == "november")] <- 11
dados_bind$eventMonth [which(dados_bind$eventMonth == "may")] <- 05
dados_bind$eventMonth [which(dados_bind$eventMonth == "july")] <- 07
dados_bind$eventMonth [which(dados_bind$eventMonth == "august")] <- 08
dados_bind$eventMonth [which(dados_bind$eventMonth == "september")] <- 09
dados_bind$eventMonth [which(dados_bind$eventMonth == "october")] <- 10

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





# ----------------------------------------------------------------------------
# ADJUSTING SITES






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


dados_bind$scientificName <- gsub ("_", " ", dados_bind$scientificName)


# matching with worms
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
dados_bind$scientificNameID<-(df_worms_record$lsid [match (dados_bind$scientificName,
                                                           tolower (df_worms_record$scientificname))])
dados_bind$kingdom <-(df_worms_record$kingdom [match (dados_bind$scientificName,
                                                      tolower (df_worms_record$scientificname))])
# class
dados_bind$class<-(df_worms_record$class [match (dados_bind$scientificName,
                                                 tolower(df_worms_record$scientificname))])
# family
dados_bind$family<-(df_worms_record$family [match (dados_bind$scientificName,
                                                   tolower(df_worms_record$scientificname))])






# ----------------------------------------------------------------------------
# CREATING IDS







# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste ("BR:SC_TIME_SERIES:",
                                          dados_bind$locationID,sep=""), 
                                           dados_bind$locality, 
                                          dados_bind$eventYear,
                              sep="_")



# creating eventIds
dados_bind$eventID <- paste (paste ("BR:SC_TIME_SERIES-MAR:",
                                    dados_bind$locationID,sep=""), 
                                    dados_bind$locality, 
                                    dados_bind$eventYear,
                                    substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                        sep="_")




# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste ("BR:SC_TIME_SERIES-MAR:",
                                         dados_bind$locationID,sep=""), 
                                  dados_bind$locality, 
                                  dados_bind$eventYear,
                                  substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                             paste ("occ",seq(1,nrow(dados_bind)),sep=""),
                             sep="_")







# ----------------------------------------------------------------------------
#  Formatted according to DwC







DF_eMOF <- dados_bind [,c("eventID", "occurrenceID","scientificName","scientificNameID","kingdom","class","family",
                          "measurementValue", "measurementType","measurementUnit")]



DF_occ <- dados_bind [,c("eventID", "occurrenceID","basisOfRecord","scientificName","scientificNameID","kingdom","class","family",
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
# write to txt format
write.table(DF_occ, file =here("DwC_output",
                               "SC_time_series",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)

write.table(DF_eMOF, file =here("DwC_output",
                                "SC_time_series",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)


write.table(event_core, file =here("DwC_output",
                                   "SC_time_series",
                                   "event_core.txt"),sep=",",
            quote = FALSE)

## end
