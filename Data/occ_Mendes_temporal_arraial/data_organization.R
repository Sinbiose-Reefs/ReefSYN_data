

# load TS arraial - Mendes et al. 
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
dados_bind$samplingProtocol <- "Underwater visual survey - 20 x 2m" # 
# effort
dados_bind$samplingEffort <- 1# "one observer per transect"
# sampleSizeValue
dados_bind$sampleSizeValue <- 20*2 # area
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
colnames(dados_bind)[which(colnames(dados_bind) == "Taxa")] <- "namesToSearch"

# adjusting site & locality (are the opposite)
dados_bind$verbatimSite <- dados_bind$locality
dados_bind$verbatimLocality <- dados_bind$site
# adjust
dados_bind$site <- dados_bind$verbatimSite
dados_bind$locality <- dados_bind$verbatimLocality






# --------------------------------------------------------------------------
# ADJUSTING DATES




# eventDate
dados_bind$month [which(dados_bind$month == "jan")] <- 01
dados_bind$month [which(dados_bind$month == "feb")] <- 02
dados_bind$month [which(dados_bind$month == "mar")] <- 03
dados_bind$month [which(dados_bind$month == "apr")] <- 04
dados_bind$month [which(dados_bind$month == "may")] <- 05
dados_bind$month [which(dados_bind$month == "jun")] <- 06
dados_bind$month [which(dados_bind$month == "jul")] <- 07
dados_bind$month [which(dados_bind$month == "aug")] <- 08
dados_bind$month [which(dados_bind$month == "sep")] <- 09
dados_bind$month [which(dados_bind$month == "oct")] <- 10
dados_bind$month [which(dados_bind$month == "nov")] <- 11
dados_bind$month [which(dados_bind$month == "dec")] <- 12

# adjust day
dados_bind$day <- ifelse (dados_bind$day < 10, 
                          paste0("0", dados_bind$day),
                          dados_bind$day)

# date
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





# --------------------------------------------------------------------------
# ADJUSTING SPECIES NAMES




# verbatimIdentification
dados_bind$verbatimIdentification <-dados_bind$namesToSearch 

# non identified species
dados_bind$identificationQualifier <- ifelse (sapply (strsplit (dados_bind$namesToSearch, " "), "[", 2) %in% c("sp", "sp2", "sp3", "spp"),
                                                    "sp",
                                                    NA)

# species to search
dados_bind$namesToSearch [which(dados_bind$identificationQualifier == "sp")] <- gsub (" sp",
                                                                                                   "",
                                                                                      dados_bind$namesToSearch [which(dados_bind$identificationQualifier == "sp")])
dados_bind$namesToSearch[which(dados_bind$namesToSearch == "Myliobatis goodei ")]<- "Myliobatis goodei"

# check again in the future
dados_bind$namesToSearch[which(dados_bind$namesToSearch == "Malacoctenus aff. triangulatus")]<- "Malacoctenus triangulatus" 


# matching names with worms
worms_record <- lapply (unique(dados_bind$namesToSearch), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# df_worms_record[which(df_worms_record$match_type == "near_1"),]
# match
dados_bind$scientificNameAccepted<-(df_worms_record$scientificname [match ( (dados_bind$namesToSearch),
                                                                   (df_worms_record$scientificname))])

dados_bind$scientificNameID<-(df_worms_record$lsid [match ( (dados_bind$namesToSearch),
                                                            (df_worms_record$scientificname))])
# taxon rank of the identified level
dados_bind$taxonRank <- (df_worms_record$rank [match ( (dados_bind$namesToSearch),
                                                       (df_worms_record$scientificname))])
# kingdom
dados_bind$kingdom <-(df_worms_record$kingdom [match ( (dados_bind$namesToSearch),
                                                       (df_worms_record$scientificname))])
# phylum
dados_bind$phylum <-(df_worms_record$phylum [match ( (dados_bind$namesToSearch),
                                                      (df_worms_record$scientificname))])
# class
dados_bind$class<-(df_worms_record$class [match ( (dados_bind$namesToSearch),
                                                 (df_worms_record$scientificname))])
# order
dados_bind$order <-(df_worms_record$order [match ( (dados_bind$namesToSearch),
                                                 (df_worms_record$scientificname))])
# family
dados_bind$family<-(df_worms_record$family [match ( (dados_bind$namesToSearch),
                                                   (df_worms_record$scientificname))])
# genus
dados_bind$genus<-(df_worms_record$genus [match ( (dados_bind$namesToSearch),
                                                    (df_worms_record$scientificname))])


# check what remains
unique(dados_bind [is.na(dados_bind$scientificNameAccepted),"namesToSearch"])



# taxomic updates 
# spp
dados_bind$scientificNameAccepted[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina multilineata"
dados_bind$scientificNameAccepted[grep ("polygonius", dados_bind$scientificNameAccepted)] <- "Acanthostracion polygonium"

# genus too
dados_bind$genus[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina"





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
dados_bind$parentEventID <- paste (
                              paste ( 
                                paste ("BR:ReefSYN:RJ_TIME_SERIES:", 
                                       dados_bind$higherGeography,
                                       sep=""),
                                dados_bind$site,sep=":"),
                              dados_bind$locality,
                              dados_bind$year,
                              sep="_")


# creating eventIds
dados_bind$eventID <- paste (
                              paste ( 
                                paste ("BR:ReefSYN:RJ_TIME_SERIES:", 
                                       dados_bind$higherGeography,
                                       sep=""),
                                dados_bind$site,sep=":"),
                              dados_bind$locality,
                              dados_bind$year,
                                    dados_bind$unique_id,
                        sep="_")

# creating occurrenceIDs
dados_bind$occurrenceID <- paste (
                                  paste ( 
                                    paste ("BR:ReefSYN:RJ_TIME_SERIES:", 
                                           dados_bind$higherGeography,
                                           sep=""),
                                    dados_bind$site,sep=":"),
                                  dados_bind$locality,
                                  dados_bind$year,
                                  dados_bind$unique_id,
                             paste ("occ",seq(1,nrow(dados_bind)),sep=""),
                             sep="_")






# --------------------------------------------------------------------------
# Formatted according to DwC






DF_eMOF <- dados_bind [,c("eventID", 
                          "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit")]


DF_occ <- dados_bind [,c("eventID", 
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
                         "recordedBy", 
                         "organismQuantityType", 
                         "occurrenceStatus")]


# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,site,locality) %>% 
                            
                                summarise(year = mean(year),
                                          eventDate = mean(eventDate),
                                          minimumDepthinMeters = mean(minimumDepthinMeters),
                                          maximumDepthinMeters = mean(maximumDepthinMeters),
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

# save
# txt format
write.csv(DF_occ, file =here("DwC_output",
                               "RJ_time_series",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "RJ_time_series",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "RJ_time_series",
                                   "event_core.csv"))

## end
rm(list=ls())
