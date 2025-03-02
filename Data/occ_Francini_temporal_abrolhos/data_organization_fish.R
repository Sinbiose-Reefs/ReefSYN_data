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



# taxonomic updates
# species
fish_long_format$scientificNameAccepted[grep ("multilineata", fish_long_format$scientificNameAccepted)] <- "Azurina multilineata"
fish_long_format$scientificNameAccepted[grep ("bartholomaei", fish_long_format$scientificNameAccepted)] <- "Caranx bartholomaei"
fish_long_format$scientificNameAccepted[grep ("polygonius", fish_long_format$scientificNameAccepted)] <- "Acanthostracion polygonium"
fish_long_format$scientificNameAccepted[grep ("Dasyatis americana", fish_long_format$scientificNameAccepted)] <- "Hypanus berthalutzae"
fish_long_format$scientificNameAccepted[grep ("Emblemariopsis signifera", fish_long_format$scientificNameAccepted)] <- "Emblemariopsis signifer"
fish_long_format$scientificNameAccepted[grep ("Haemulon plumieri", fish_long_format$scientificNameAccepted)] <- "Haemulon plumierii"
fish_long_format$scientificNameAccepted[grep ("Labrisomus kalisherae", fish_long_format$scientificNameAccepted)] <- "Goblioclinus kalisherae"
fish_long_format$scientificNameAccepted[grep ("perezi", fish_long_format$scientificNameAccepted)] <- "Carcharhinus perezii"
fish_long_format$scientificNameAccepted[grep ("Haemulon steindachneri", fish_long_format$scientificNameAccepted)] <- "Haemulon atlanticus"
fish_long_format$scientificNameAccepted[grep ("Sphoeroides spengleri", fish_long_format$scientificNameAccepted)] <- "Sphoeroides camila"


# genus
fish_long_format$genus[grep ("multilineata", fish_long_format$scientificNameAccepted)] <- "Azurina"
fish_long_format$genus[grep ("bartholomaei", fish_long_format$scientificNameAccepted)] <- "Caranx"
fish_long_format$genus[grep ("Hypanus berthalutzea", fish_long_format$scientificNameAccepted)] <- "Hypanus"
fish_long_format$genus[grep ("Goblioclinus kalisherae", fish_long_format$scientificNameAccepted)] <- "Goblioclinus"


# family
fish_long_format$family[which(fish_long_format$family == "Scaridae")] <- "Labridae"


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

# effort (we row in the table is a sample (SAMPLE column))

fish_long_format <- fish_long_format %>% 
  group_by(YEAR,site,locality,HAB) %>%
  summarise(samplingEffort = length(unique(SAMPLE))) %>% 
  left_join(fish_long_format)

# check 
#fish_long_format %>% 
#  filter (site == "TIMBEBAS" & 
#            YEAR == "2014" &
#            locality == "tim3" &
#            HAB == "pinnacles_top") %>% 
#  select (samplingEffort) %>%
#  unique
            
            

# tolower site names
fish_long_format$site <- tolower (fish_long_format$site)


# measurementType
fish_long_format$measurementType <- "abundance"
# measurementUnit
fish_long_format$measurementUnit <- "individuals"
# method
fish_long_format$samplingProtocol <- "Stationary visual survey - 4 x 2m" #  

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
fish_long_format$occurrenceStatus <- ifelse (fish_long_format$measurementValue==0,
                                             "absence",
                                             "presence")
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
  paste ("occ",seq(1,
                   nrow(fish_long_format %>% 
                          filter (measurementType == "abundance"))),sep=""),
  sep="_")




# ---------------------------------------------------------

# separate abundance from size class

size_classes <- fish_long_format
size_classes$organismQuantityType <- "individuals"
size_classes$measurementValue <- size_classes$sizeClass
size_classes$measurementType <- "total length"
size_classes$measurementUnit <- "cm"


# bind abundance and size
# abundance into character to avoid error
fish_long_format$measurementValue<-as.character(fish_long_format$measurementValue)
fish_long_format <- rbind (fish_long_format,
                           size_classes)
# event remark
fish_long_format$measurementRemarks <- "Authors used categories of total length"




# adjusting recordedBy

fish_SN_observers <- read.xlsx(here ("Data", 
                           "occ_Francini_temporal_abrolhos",
                           "Checking.xlsx"),sheet=2)



# match

fish_long_format$recordedBy <- fish_SN_observers [match (fish_long_format$recordedBy,fish_SN_observers$abbreviation),
                   "recordedBy"]


# sites into locations
colnames(fish_long_format)[which(colnames(fish_long_format) == "site")] <- "location"






# ------------------------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF <- fish_long_format [,c("eventID", "occurrenceID",
                                 "measurementValue", 
                                "measurementType",
                                "measurementUnit",
                                "measurementRemarks")]



DF_occ <- fish_long_format  [,c("eventID", "occurrenceID","basisOfRecord",
                                #"verbatimIdentification",
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

event_core <- data.frame (group_by(fish_long_format, eventID,higherGeography,location,locality) %>% 
                                 
                                 summarise(year = mean(year),
                                           eventDate = mean(eventDate),
                                           minimumDepthinMeters = mean(minimumDepthinMeters),
                                           maximumDepthinMeters = mean(maximumDepthinMeters),
                                           habitat = unique(habitat),
                                           samplingProtocol = unique(samplingProtocol),
                                           samplingEffort = mean(samplingEffort,na.rm=T),
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


# write to txt

write.csv(DF_occ, file =here("DwC_output",
                               "II",
                               "DF_occ_fish.csv"))



write.csv(DF_eMOF, file =here("DwC_output",
                                "II",
                                "DF_eMOF_fish.csv"))



write.csv(event_core, file =here("DwC_output",
                                   "II",
                                   "event_core_fish.csv"))




# tentar separar datasets em presenca e ausencia


DF_eMOF_presence <- fish_long_format%>%
  
  filter (occurrenceStatus == "presence") %>%
  
  select (c("eventID", "occurrenceID",
                                "measurementValue", 
                                "measurementType",
                                "measurementUnit",
                                "measurementRemarks"))



DF_occ_presence <- fish_long_format%>%
  
  filter (occurrenceStatus == "presence") %>%
  select (c("eventID", "occurrenceID","basisOfRecord",
                                #"verbatimIdentification",
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
                                "occurrenceStatus"))

# aggregate data by eventIDs to have event_core

event_core_presence <- data.frame (group_by(fish_long_format%>%
                                             
                                             filter (occurrenceStatus == "presence")
                                           , eventID,higherGeography,location,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      habitat = unique(habitat),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort,na.rm=T),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)




# write to txt

write.csv(DF_occ_presence, file =here("DwC_output",
                             "II",
                             "DF_occ_fish_presence.csv"))



write.csv(DF_eMOF_presence, file =here("DwC_output",
                              "II",
                              "DF_eMOF_fish_presence.csv"))



write.csv(event_core_presence, file =here("DwC_output",
                                 "II",
                                 "event_core_fish_presence.csv"))





# absences




DF_eMOF_absence <- fish_long_format%>%
  
  filter (occurrenceStatus == "absence") %>%
  
  select (c("eventID", "occurrenceID",
            "measurementValue", 
            "measurementType",
            "measurementUnit",
            "measurementRemarks"))



DF_occ_absence <- fish_long_format%>%
  
  filter (occurrenceStatus == "absence") %>%
  select (c("eventID", "occurrenceID","basisOfRecord",
            #"verbatimIdentification",
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
            "occurrenceStatus"))

# aggregate data by eventIDs to have event_core

event_core_absence <- data.frame (group_by(fish_long_format%>%
                                              
                                              filter (occurrenceStatus == "absence")
                                            , eventID,higherGeography,location,locality) %>% 
                                     
                                     summarise(year = mean(year),
                                               eventDate = mean(eventDate),
                                               minimumDepthinMeters = mean(minimumDepthinMeters),
                                               maximumDepthinMeters = mean(maximumDepthinMeters),
                                               habitat = unique(habitat),
                                               samplingProtocol = unique(samplingProtocol),
                                               samplingEffort = mean(samplingEffort,na.rm=T),
                                               sampleSizeValue = mean(sampleSizeValue),
                                               sampleSizeUnit = unique(sampleSizeUnit),
                                               decimalLongitude = mean(decimalLongitude),
                                               decimalLatitude = mean(decimalLatitude),
                                               geodeticDatum = unique(geodeticDatum),
                                               Country = unique(Country),
                                               countryCode = unique(countryCode))
)




# write to txt

write.csv(DF_occ_absence, file =here("DwC_output",
                                      "II",
                                      "DF_occ_fish_absence.csv"))



write.csv(DF_eMOF_absence, file =here("DwC_output",
                                       "II",
                                       "DF_eMOF_fish_absence.csv"))



write.csv(event_core_absence, file =here("DwC_output",
                                          "II",
                                          "event_core_fish_absence.csv"))






# end
rm (list=ls())



