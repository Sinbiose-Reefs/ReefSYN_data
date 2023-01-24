# ------------------------ #
# Coastal time series      #
#       Abrolhos           #
# ------------------------ #

require(openxlsx); require(here); require(worrms); require(dplyr)

# -------------- #
# load fish TS
# -------------- #

fish_TS <- read.xlsx(here ("Data", 
                           "occ_Francini_temporal_abrolhos",
                           "#####TEMPORAL FISH ABROLHOS 2001-2014.xlsx"))

# cols of data information
data_info <- c("SAMPLE","REEF","SITE","HAB","DEPTH","YEAR","MET","SAMP")

# data frame with data information
fish_df <- fish_TS[,data_info]

# list of fish and size
fish_size_list <- colnames(fish_TS)[which(colnames(fish_TS) %in% colnames(fish_df) == F)]






# -------------------------------------------------------------
# ADJUST SPECIES NAMES AND SIZE CLASSES







# separate and bind with data information
# long format
fish_long_format <- lapply (fish_size_list, function (species)
  
    cbind (fish_df, 
           namesToSearch = species,
           measurementValue = fish_TS[,which(colnames(fish_TS) == species)])
    
)
fish_long_format <- do.call(rbind,fish_long_format) # melt the list

# verbatimIdentification 
fish_long_format$verbatimIdentification <- fish_long_format$namesToSearch

# separate scientificName from size class
sp_size <- strsplit(fish_long_format$namesToSearch, "\\.")

# some have points in incorrect places (causing length to be > 2)
# check things to correct
# unique(sapply (sp_size,"[",2))
incorrect_sp_size<- sp_size[which(lapply (sp_size,length) > 2)]
incorrect_sp_size <- (lapply (incorrect_sp_size, function (incorrect)
  # paste the separated characters  
  paste(incorrect[2],
        incorrect[3],
      sep=""))
)

# now we have cases like this: parablenius.sp.<10
# in sp_size it was transformed into sp<10; let's correct that removing "SP"
incorrect_sp_size <- (gsub ("SP","",incorrect_sp_size))

# only correct size
corr_size <- sapply (sp_size,"[",2)
corr_size [which(lapply (sp_size,length) > 2)]   <- incorrect_sp_size
        
# bind in the table
fish_long_format$sizeClass <- corr_size
# check NAs (lack of point between sp abbreviation and size class)
# e.g., "ABUSAX10-20"
new_correction <- fish_long_format [is.na(fish_long_format$sizeClass),"namesToSearch"]
new_correction <- (substr(new_correction,nchar(new_correction)-4,nchar(new_correction)))
new_correction <- gsub("OL","",new_correction)
new_correction <- gsub("UL","",new_correction)
new_correction <- gsub("ASP","",new_correction)
new_correction <- gsub("CIL","",new_correction)

# back to the table
fish_long_format [is.na(fish_long_format$sizeClass),"sizeClass"] <- new_correction
  
# now let's correct the scientificName(that still have the sizeClass)
namesToSearch <- lapply (seq(1,nrow(fish_long_format)), function (i) 
  
  gsub (fish_long_format$sizeClass[i],
      "",
      fish_long_format$namesToSearch[i])
)

# removing special characters      
namesToSearch<-((gsub("[0-9]+", "", namesToSearch))) # rm numbers
namesToSearch<-((gsub("\\.", "", namesToSearch))) # rm point
namesToSearch<-((gsub(">", "", namesToSearch))) # rm >
namesToSearch<-((gsub("<", "", namesToSearch))) # rm <

# haemulon brasiliensis did not exist
# halichoeres brasiliensis according to R Francini-Filho
namesToSearch [which(namesToSearch == "HAEBRA")] <- "HALBRA"

# replace in the table
fish_long_format$namesToSearch <- namesToSearch

# uncertain taxonomy (CHRFLA?)
fish_long_format$uncertainTaxonomy <- NA
fish_long_format[grep("\\?",fish_long_format$namesToSearch),"uncertainTaxonomy"] <- "uncertain"
# finally rm ? from scientificName
fish_long_format$namesToSearch <- gsub ("\\?","",fish_long_format$namesToSearch)


# adjusting scientific name
fish_SN <- read.xlsx(here ("Data", 
                           "occ_Francini_temporal_abrolhos",
                           "Checking.xlsx"),sheet=1)



# matching
sn_to_replace <- fish_SN [match (fish_long_format$namesToSearch,
                                 fish_SN$abbreviation),
                          "scientificName"]
# replace
fish_long_format$namesToSearch <- sn_to_replace
# replace "_" by " "
fish_long_format$namesToSearch <- gsub ("_"," ",fish_long_format$namesToSearch)



# non identified species
fish_long_format$identificationQualifier <- ifelse (sapply (strsplit (fish_long_format$namesToSearch, " "), "[", 2) %in% 
                                                      c("sp", "sp2", "sp3", "spp"),
                                                  "sp",
                                                  NA)

# species to search
fish_long_format$namesToSearch [which(fish_long_format$identificationQualifier == "sp")] <- gsub (" sp*.",
                                                                                              "",
                                                                                              fish_long_format$namesToSearch [which(fish_long_format$identificationQualifier == "sp")])


# few adjusts
fish_long_format$namesToSearch[which(fish_long_format$namesToSearch == "upeneus parvos")] <- "upeneus parvus"



# worms'checking

worms_record <- lapply (unique(fish_long_format$namesToSearch), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# match
# no match
# valid name worms
fish_long_format$scientificNameAccepted <- (df_worms_record$scientificname [match (fish_long_format$namesToSearch,
                                                                         tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
fish_long_format$taxonRank <- (df_worms_record$rank [match (fish_long_format$namesToSearch,
                                                            tolower (df_worms_record$scientificname))])

# aphiaID
fish_long_format$scientificNameID<-(df_worms_record$lsid [match (fish_long_format$namesToSearch, 
                                                                 tolower(df_worms_record$scientificname))])

# kingdom
fish_long_format$kingdom <-(df_worms_record$kingdom [match (fish_long_format$namesToSearch, 
                                                            tolower(df_worms_record$scientificname))])

# phylum
fish_long_format$phylum <-(df_worms_record$phylum [match (fish_long_format$namesToSearch, 
                                                            tolower(df_worms_record$scientificname))])

# class
fish_long_format$class <-(df_worms_record$class [match (fish_long_format$namesToSearch, 
                                                        tolower(df_worms_record$scientificname))])

# order
fish_long_format$order <-(df_worms_record$order [match (fish_long_format$namesToSearch, 
                                                        tolower(df_worms_record$scientificname))])

# family
fish_long_format$family <-(df_worms_record$family [match (fish_long_format$namesToSearch, 
                                                          tolower(df_worms_record$scientificname))])

# genus
fish_long_format$genus <-(df_worms_record$genus [match (fish_long_format$namesToSearch, 
                                                          tolower(df_worms_record$scientificname))])


# -------------------------------------------------------------
# ADJUSTING HABITATS, SITES, DEPTHS





# adjusting habitat (based on Francini-Filho et al. 2013)
fish_long_format$HAB[which(fish_long_format$HAB == "TP")] <- "pinnacles_top"
fish_long_format$HAB[which(fish_long_format$HAB == "PA")] <- "pinnacles_wall"
fish_long_format$HAB[which(fish_long_format$HAB == "RO")] <- "rocky_reef"


# depth (following Francini-Filho et al. 2013, PlosOne, e54260)
fish_long_format$minimumDepthinMeters <- NA
fish_long_format$maximumDepthinMeters <- NA
fish_long_format[which(fish_long_format$HAB == "pinnacles_top"),"minimumDepthinMeters"]<- 2
fish_long_format[which(fish_long_format$HAB == "pinnacles_top"),"maximumDepthinMeters"]<- 6
fish_long_format[which(fish_long_format$HAB == "pinnacles_wall"),"minimumDepthinMeters"]<- 3
fish_long_format[which(fish_long_format$HAB == "pinnacles_wall"),"maximumDepthinMeters"]<- 15
fish_long_format[which(fish_long_format$HAB == "rocky_reef"),"minimumDepthinMeters"]<- 3 
fish_long_format[which(fish_long_format$HAB == "rocky_reef"),"maximumDepthinMeters"]<- 8 





# -------------------------------------------------------------
# ADJUSTING COORDINATES





# gathering coordinates from Erika Santana's dataset
# open  data to match region
occ_Francini_et_al <- read.xlsx(here("Data","occ_Francini_et_al",
                                     "Compiled_quadrats_benthic_versao2.0_ate30m.xlsx"),
                                sheet = 1, colNames = TRUE,detectDates=F)



# data from Abrolhos 2008
occ_Francini_et_al <- occ_Francini_et_al[which(occ_Francini_et_al$REGION == "ABROLHOS"),]
# PEST == PLEST (according to R FRANCINI)
fish_long_format$SITE[which(fish_long_format$SITE == "PEST")] <- "PLEST"

# which sites from occ_Francini_et_al are in fish_long_format
total_sites <- unique(c(occ_Francini_et_al$SITE, toupper (fish_long_format$SITE)))
total_sites <- total_sites[order(total_sites)]# order

# match to have coordinates
match_sites_coords <- occ_Francini_et_al[match (total_sites, toupper (occ_Francini_et_al$SITE)),c("REEF","SITE","LAT","LONG")]


# sites lacking coordinates
match_sites_coords[which(total_sites %in% match_sites_coords$SITE == F),"SITE"]<-total_sites[which(total_sites %in% match_sites_coords$SITE == F)]
match_sites_coords[match_sites_coords$SITE == "AMP3",c("LAT", "LONG")] <- c(-16.914, -39.030)
match_sites_coords[match_sites_coords$SITE == "A3",c("LAT", "LONG")] <- c(-16.903, -39.031)
match_sites_coords[match_sites_coords$SITE == "PA1",c("LAT", "LONG")] <- c(-17.699161805482,-38.942426147792)
match_sites_coords[match_sites_coords$SITE == "PLEST",c("LAT", "LONG")] <- c(-17.783,-39.051)# parcel das paredes caravelas Bahia - google earth



# matching site to have the coordinates
coords <- (match_sites_coords [match (fish_long_format$SITE,match_sites_coords$SITE), c("REEF","SITE","LAT","LONG")])



# table(coords$SITE == fish_long_format$SITE) # check matching
fish_long_format<- cbind (fish_long_format,
                          coords[,c("LAT", "LONG")])





# -------------------------------------------------------------
# ADJUSTING sITES





# adjusting site (actually locality) names
fish_long_format$SITE<-(iconv(fish_long_format$SITE, "ASCII", "UTF-8", sub=""))
fish_long_format$SITE <- tolower(fish_long_format$SITE)
unique(fish_long_format$SITE )[order(unique(fish_long_format$SITE ))]





# ---------------------------------------------------------------------------------
# DWC DESCRIPTORS





# geodeticDatum
fish_long_format$geodeticDatum <- "decimal degrees"

# edit colnames
# coords
colnames(fish_long_format)[which(colnames(fish_long_format) == "LAT")] <- "decimalLatitude"
colnames(fish_long_format)[which(colnames(fish_long_format) == "LONG")] <- "decimalLongitude"

# locationID
colnames(fish_long_format)[which(colnames(fish_long_format) == "REEF")] <- "site"

# locality
colnames(fish_long_format)[which(colnames(fish_long_format) == "SITE")] <- "locality"

# measurementType
fish_long_format$measurementType <- "abundance"
# measurementUnit
fish_long_format$measurementUnit <- "individuals"
# method
fish_long_format$samplingProtocol <- "Stationary visual survey - 4 x 2m" #  
# effort
fish_long_format$samplingEffort <- 1 # "one observer per point"

# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS, https://www.int-res.com/abstracts/meps/v367/p283-293/)
fish_long_format$sampleSizeValue <- 4*2 # plotarea?radii?" (or pi*(4^2))

# sampleSizeUnit (based on Minte-Vera et al. 2008 MEPS)
fish_long_format$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(fish_long_format)[which(colnames(fish_long_format) == "SAMP")] <- "recordedBy"
# year
colnames(fish_long_format)[which(colnames(fish_long_format) == "YEAR")] <- "year"
# date
fish_long_format$eventDate <- fish_long_format$year

# sample
colnames(fish_long_format)[which(colnames(fish_long_format) == "SAMPLE")] <- "sample"
# basisOfRecord
fish_long_format$basisOfRecord <- "HumanObservation"
# occurrenceStatus
fish_long_format$occurrenceStatus <- "presence"
# country and code
fish_long_format$Country <- "Brazil"
fish_long_format$countryCode <- "BR"
# habitat
colnames(fish_long_format)[which(colnames(fish_long_format) == "HAB")] <- "habitat"
# geographic location
fish_long_format$higherGeography <- "BrazilianCoast"
# organismQuantityType
fish_long_format$organismQuantityType <- "individuals"





# ------------------------------------------------------------------------------------------
# CREATING IDS





# creating parentEventids
fish_long_format$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           fish_long_format$higherGeography,
           sep=""),
    fish_long_format$site,sep=":"),
  fish_long_format$locality,
  fish_long_format$year,
  sep="_")
  
  

# creating eventids
fish_long_format$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           fish_long_format$higherGeography,
           sep=""),
    fish_long_format$site,sep=":"),
  fish_long_format$locality,
       fish_long_format$year,
       fish_long_format$sample,
       sep="_")


# occurrenceID
fish_long_format$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           fish_long_format$higherGeography,
           sep=""),
    fish_long_format$site,sep=":"),
  fish_long_format$locality,
  fish_long_format$year,
  fish_long_format$sample,
  paste ("occ",seq(1,nrow(fish_long_format)),sep=""),
  sep="_")






# ------------------------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF <- fish_long_format [,c("eventID", 
                                "occurrenceID",
                                 "measurementValue", 
                                "measurementType",
                                "measurementUnit")]



DF_occ <- fish_long_format  [,c("eventID", "occurrenceID","basisOfRecord",
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

event_core <- data.frame (group_by(fish_long_format, eventID,higherGeography,site,locality) %>% 
                                 
                                 summarise(year = mean(year),
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


# write to txt

write.csv(DF_occ, file =here("DwC_output",
                               "RFrancini_timeSeries_abrolhos",
                               "DF_occ_fish.csv"))



write.csv(DF_eMOF, file =here("DwC_output",
                                "RFrancini_timeSeries_abrolhos",
                                "DF_eMOF_fish.csv"))



write.csv(event_core, file =here("DwC_output",
                                   "RFrancini_timeSeries_abrolhos",
                                   "event_core_fish.csv"))

# end
rm (list=ls())



