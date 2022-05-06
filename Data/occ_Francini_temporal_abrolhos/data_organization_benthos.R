
require(openxlsx); require(reshape); require(worrms); require(dplyr); require(here)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# load benthic Time series, Abrolhos Bank
# 2003-2005


benthos_TS_03_05 <- read.xlsx(here ("Data", 
                                    "occ_Francini_temporal_abrolhos",
                                    "TEMPORAL BENTHOS ABROLHOS 2003-2005.xlsx"))

# cols of data information
data_info <-  c("RECIFE","PONTO","HABITAT","PROF","ANO")

# data frame with data information
benthos_df <- benthos_TS_03_05[,data_info]






# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# LONG FORMAT






# list of fish and size
benthos_df_list <- colnames(benthos_TS_03_05)[which(colnames(benthos_TS_03_05) %in% colnames(benthos_df) == F)]

# separate and bind with data information
# long format
benthos_long_format <- lapply (benthos_df_list, function (species)
  
  cbind (benthos_df, 
         scientificName = species,
         measurementValue = benthos_TS_03_05[,which(colnames(benthos_TS_03_05) == species)])
  
)
benthos_long_format <- do.call(rbind,benthos_long_format) # melt the list

# corrections
# point into site
colnames(benthos_long_format) <- c("locality","site","habitat","depthInMeters",            
                                   "eventDate" , "scientificName","measurementValue")






# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES







# correcting scientificName (names based on Aued et al. 2019)
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "ACA")]  <- "calcareous articulate algae"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "AFI")]  <- "filamentous algae"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "AFRO")]  <- "frondose algae"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "CV")]  <- "scleractinia"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "SPJA")]  <- "porifera"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "MI")]  <- "millepora sp"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "OCTO")]  <- "alcyonaria"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "OÇO")]  <- "echinoidea"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "ZO")]  <- "zoantharia"
benthos_long_format$scientificName[which(benthos_long_format$scientificName == "CV+MI")]  <- "scleractinia"





# adjusting spp names
# matching with worms

worms_record <- lapply (unique(benthos_long_format$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))


# df_worms_record[which(df_worms_record$match_type == "near_1"),]
# match
# no match
benthos_long_format$scientificNameID<-(df_worms_record$lsid [match (benthos_long_format$scientificName, tolower(df_worms_record$scientificname))])
benthos_long_format$kingdom <-(df_worms_record$kingdom [match (benthos_long_format$scientificName,tolower (df_worms_record$scientificname))])
benthos_long_format$class <-(df_worms_record$class [match (benthos_long_format$scientificName,tolower (df_worms_record$scientificname))])
benthos_long_format$family <-(df_worms_record$family [match (benthos_long_format$scientificName,tolower (df_worms_record$scientificname))])





# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# CREATING IDS,
# DWC FORMAT


# adjusting habitat (based on Francini-Filho et al. 2013)
benthos_long_format$habitat[which(benthos_long_format$habitat == "TP")] <- "pinnacles_top"
benthos_long_format$habitat[which(benthos_long_format$habitat == "PA")] <- "pinnacles_wall"


# creating parentEventids
benthos_long_format$parentEventID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format$locality,sep=""),
  benthos_long_format$eventDate,
  sep="_")



# creating eventids
benthos_long_format$eventID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format$locality,sep=""),
  benthos_long_format$site,
  benthos_long_format$eventDate,
  
  sep="_")



# occurrenceID
benthos_long_format$occurrenceID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format$locality,sep=""),
  benthos_long_format$site,
  benthos_long_format$eventDate,
  paste ("occ",seq(1,nrow(benthos_long_format)),sep=""),
  sep="_")


# eventYear
benthos_long_format$eventYear<-benthos_long_format$eventDate
# method
benthos_long_format$samplingProtocol <- "point-intercept  lines"
# samplingEffort
benthos_long_format$samplingEffort <- 4 # four lines
# sampleSizeValue
benthos_long_format$sampleSizeValue <- 10 # length of each line
# sampleSizeUnit
benthos_long_format$sampleSizeUnit <- "meters"
# country and code
benthos_long_format$Country <- "Brazil"
benthos_long_format$countryCode <- "BR"
# basisOfRecord
benthos_long_format$basisOfRecord <- "HumanObservation"
# occurrenceStatus
benthos_long_format$occurrenceStatus <- "presence"
# organismQuantityType
benthos_long_format$organismQuantityType <- "Percentage cover"
# measurementType
benthos_long_format$measurementType <- "Percentage cover"
# measurementUnit
benthos_long_format$measurementUnit <- "dimensionless"
# recordedBy
benthos_long_format$recordedBy <- "Ronaldo Francini-Filho"
# geographic location
benthos_long_format$higherGeographyID <- "BrazilianCoast"
# locationID and locality
colnames(benthos_long_format)[which(colnames(benthos_long_format) == "locality")] <- "locationID"
colnames(benthos_long_format)[which(colnames(benthos_long_format) == "site")] <- "locality"

# depth
# set min and max
colnames(benthos_long_format)[which(colnames(benthos_long_format) == "depthInMeters")] <- "minimumDepthinMeters"
benthos_long_format$maximumDepthinMeters <- benthos_long_format$minimumDepthinMeters






# ----------------------------------------------------------------------------------------------------------------------------------------------------------
#  ADJUSTING COORDINATES
# gathering coordinates from Erika Santana's dataset








# open  data to match region
occ_Francini_et_al <- read.xlsx(here("Data","occ_Francini_et_al",
                                     "Compiled_quadrats_benthic_versao2.0_ate30m.xlsx"),
                                sheet = 1, colNames = TRUE,detectDates=F)



# data from Abrolhos 2008
occ_Francini_et_al <- occ_Francini_et_al[which(occ_Francini_et_al$REGION == "ABROLHOS"),]
# which sites from occ_Francini_et_al are in fish_long_format
total_sites <- unique(c(occ_Francini_et_al$SITE, toupper (benthos_long_format$locality)))
total_sites <- total_sites[order(total_sites)]# order


# match to have coordinates
match_sites_coords <- occ_Francini_et_al[match (total_sites, toupper (occ_Francini_et_al$SITE)),
                                         c("REEF","SITE","LAT","LONG")]



# sites lacking coordinates
match_sites_coords[which(total_sites %in% match_sites_coords$SITE == F),"SITE"]<-total_sites[which(total_sites %in% match_sites_coords$SITE == F)]

match_sites_coords[match_sites_coords$SITE == "AMP3",c("LAT", "LONG")] <- c(-16.914, -39.030)
match_sites_coords[match_sites_coords$SITE == "A3",c("LAT", "LONG")] <- c(-16.903, -39.031)


# PA1 AINDA NAO SABEMOS

match_sites_coords[match_sites_coords$SITE == "PA1",c("LAT", "LONG")] <- c(-17.771998,-39.045509)# parcel das paredes caravelas Bahia - google earth


# match_sites_coords[match_sites_coords$SITE == "PEST",c("LAT", "LONG")] <- c(-17.771998,-39.045509)# parcel das paredes caravelas Bahia - google earth
# PEST  == PLEST




# matching site to have the coordinates
coords <- (match_sites_coords [match (benthos_long_format$locality,match_sites_coords$SITE), c("REEF","SITE","LAT","LONG")])

# table(coords$SITE == fish_long_format$SITE) # check matching
benthos_long_format<- cbind (benthos_long_format,
                                  coords[,c("LAT", "LONG")])



# geodeticDatum
benthos_long_format$geodeticDatum <- "decimal degrees"
colnames(benthos_long_format)[which(colnames(benthos_long_format) %in% c("LAT", "LONG"))] <- c("decimalLatitude","decimalLongitude")



# sites to lower
benthos_long_format$locationID <- tolower (benthos_long_format$locationID)
benthos_long_format$locality <- tolower (benthos_long_format$locality)





# ------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF <- benthos_long_format [,c("eventID", "occurrenceID","scientificName","scientificNameID","kingdom","class","family",
                                   "measurementValue", "measurementType","measurementUnit")]

DF_occ <- benthos_long_format [,c("eventID", "occurrenceID","basisOfRecord","scientificName","scientificNameID","kingdom","class","family",
                                  "recordedBy", "organismQuantityType", "occurrenceStatus")]


# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])
event_core <- data.frame (group_by(benthos_long_format, eventID,higherGeographyID,locationID,locality) %>% 
                            
                            summarise(eventYear = mean(eventYear),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = unique(samplingEffort),
                                      sampleSizeValue = unique(sampleSizeValue),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)






# ----------------------------------------------------------------------------------------------------------------------------------------------------------




# formatting the remaining years
# load benthic TS
# 2006-2014






# -------------- #
benthos_TS_2006 <- read.xlsx(here ("Data", 
                                   "occ_Francini_temporal_abrolhos",
                                   "#####TEMPORAL BENTHOS ABROLHOS 2006-2014.xlsx"))

# cols of data information
data_info <-  c("REEF","SITE","HAB","REGION","YEAR", "quad", "FIX.QUAD")

# data frame with data information
benthos_df_2006 <- benthos_TS_2006[,data_info]

# list of fish and size
benthos_df_list <- colnames(benthos_TS_2006)[which(colnames(benthos_TS_2006) %in% colnames(benthos_df_2006) == F)]

# separate and bind with data information
# long format
benthos_long_format_2006 <- lapply (benthos_df_list, function (species)
  
  cbind (benthos_df_2006, 
         scientificName = species,
         measurementValue = benthos_TS_2006[,which(colnames(benthos_TS_2006) == species)])
  
)

benthos_long_format_2006 <- do.call(rbind,benthos_long_format_2006) # melt the list

# corrections
colnames(benthos_long_format_2006) <- c("locationID","locality","habitat","region",            
                                        "eventDate" , "quadrat","quadratID",
                                        "scientificName","measurementValue")





# ------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES






# correcting scientific name  (names based on Aued et al. 2019)
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "CYANO")]  <- "cyanobacteria"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "CCA")]  <- "crustose coralline algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "CAA")]  <- "calcareous articulate algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "TURF")]  <- "filamentous algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "fire-coral")]  <- "millepora alcicornis"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "CYANO.+.TURF")]  <- "cianobacterias calcareous turf"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "HALIMEDA")]  <- "halimeda"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "FLESHY.ALGAE")]  <- "foliose algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "OTHER.ORGANISMS")]  <- "other organisms"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "SPONGE")]  <- "porifera"


# adjusting spp names
benthos_long_format_2006$scientificName <-  (gsub("\\."," ",benthos_long_format_2006$scientificName))
benthos_long_format_2006$scientificName <-(iconv(benthos_long_format_2006$scientificName, "UTF-8", "ASCII//TRANSLIT", sub=""))
benthos_long_format_2006$scientificName <- tolower(benthos_long_format_2006$scientificName)


# the group "cianobacterias calcareous turf" from Francini et al. (time series) divided into 2 groups
cianobacterias_calcareous_turf <- benthos_long_format_2006[which(benthos_long_format_2006$scientificName == "cianobacterias calcareous turf"),]
cianobacterias_calcareous_turf$scientificName[which(cianobacterias_calcareous_turf$scientificName == "cianobacterias calcareous turf")] <- "cyanobacteria" # transform the other
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "cianobacterias calcareous turf")] <- "filamentous algae"
benthos_long_format_2006<-rbind (benthos_long_format_2006, cianobacterias_calcareous_turf) # bind one new group




# adjust based on knowledge of Cesar Cordeiro
unique(benthos_long_format_2006$scientificName)[order(unique(benthos_long_format_2006$scientificName))]
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "aiolochoria crassa")] <- "aiolochroia crassa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "meandrina braziliensis")] <- "meandrina brasiliensis"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "millepora")] <- "millepora sp"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "montastrea cavernosa")] <- "montastraea cavernosa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "mussismilia")] <- "mussismilia spp"
benthos_long_format_2006$scientificName[grep("neospongodes atl*", benthos_long_format_2006$scientificName)] <- "neospongodes atlantica"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "siderastrea spp ")] <- "siderastrea spp"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "siderastrea")] <- "siderastrea spp"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "siderastrea sp")] <- "siderastrea spp"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "ventricaria ventricosa")] <- "valonia ventricosa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "zooanthus sociatus")] <- "zoanthus sociatus"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "zoanthid")] <- "zoantharia"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "zoanthus sp ")] <- "zoantharia"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "palythoa")] <- "palythoa sp "
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "padina")] <- "padina sp"

# broader groups
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "leathery")] <- "leathery algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "spirobidae - polycchaete")] <- "spirorbidae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "briozoa")] <- "bryozoa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "bryozoan")] <- "bryozoa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "hidrozoan")] <- "hydrozoa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "outro hydrozoa")] <- "hydrozoa"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "poliqueta")] <- "polychaeta"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "polichaeta")] <- "polychaeta"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName %in% c("ascidea colonial" ,                  
                                                                                 "ascidian",
                                                                                 "outra ascidia"))] <- "ascidiacea"
# octocoral and anthozoa
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "outro anthozoa")] <- "anthozoa"
benthos_long_format_2006$scientificName[grep("octocoral",benthos_long_format_2006$scientificName)] <- "alcyonaria"
# sponge
benthos_long_format_2006$scientificName[grep("sponge",benthos_long_format_2006$scientificName)] <- "porifera"
# echinoderms
benthos_long_format_2006$scientificName[grep("ourigo",benthos_long_format_2006$scientificName)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
benthos_long_format_2006$scientificName[grep("sea urchin",benthos_long_format_2006$scientificName)] <- "echinoidea"
benthos_long_format_2006$scientificName[grep("outro echinoderma",benthos_long_format_2006$scientificName)] <- "echinodermata"
benthos_long_format_2006$scientificName[grep("crinside",benthos_long_format_2006$scientificName)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
benthos_long_format_2006$scientificName[grep("estrela",benthos_long_format_2006$scientificName)] <- "asteroidea"

# cca and caa
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "crostose coralline algae")] <- "crustose coralline algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "cianobacterias")] <- "cyanobacteria"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName %in% c("amphiroa", 
                                                                                 "amphiroa sp", 
                                                                                 "amphiroideae", 
                                                                                 "jania amphiroa", 
                                                                                 "jania sp",
                                                                                 "unknown articulated coralline algae"))] <- "amphiroideae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "filamentous")] <- "filamentous algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "green filamentous algae")] <- "filamentous algae"
# algae
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName %in% c("fleshy algae",
                                                                                 "foliaceous algae",
                                                                                 "foliose",
                                                                                 "frondose algae", 
                                                                                 "unknown foliose"))] <- "foliose algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "calcareous turf")] <- "calcareous articulate algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "corticated")] <- "corticated algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "unknown corticated")] <- "corticated algae"
benthos_long_format_2006$scientificName[which(benthos_long_format_2006$scientificName == "sargassum sp")] <- "leathery algae"


# check names
unique(benthos_long_format_2006$scientificName)[order(unique(benthos_long_format_2006$scientificName))]


# remove plot data
benthos_long_format_2006<-benthos_long_format_2006 [which(benthos_long_format_2006$scientificName %in% 
                                                            c("sand","rock") != T),]



# matching with worms
worms_record <- lapply (unique(benthos_long_format_2006$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# df_worms_record[which(df_worms_record$match_type == "near_1"),]
# match
# no match
benthos_long_format_2006$scientificNameID<-(df_worms_record$lsid [match (benthos_long_format_2006$scientificName, tolower(df_worms_record$scientificname))])
benthos_long_format_2006$kingdom <-(df_worms_record$kingdom [match (benthos_long_format_2006$scientificName, tolower(df_worms_record$scientificname))])
benthos_long_format_2006$class <-(df_worms_record$class [match (benthos_long_format_2006$scientificName, tolower(df_worms_record$scientificname))])
benthos_long_format_2006$family <-(df_worms_record$family [match (benthos_long_format_2006$scientificName, tolower(df_worms_record$scientificname))])





# ------------------------------------------------------------------------
# creating eventID







# creating parentEventids
benthos_long_format_2006$parentEventID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format_2006$locationID,sep=""),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  sep="_")



# creating eventids
benthos_long_format_2006$eventID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format_2006$locationID,sep=""),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  benthos_long_format_2006$quadrat,
  benthos_long_format_2006$quadratID,
  sep="_")



# occurrenceID
benthos_long_format_2006$occurrenceID <- paste (
  paste ("BR:Abrolhos:", 
         benthos_long_format_2006$locationID,sep=""),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  benthos_long_format_2006$quadrat,
  benthos_long_format_2006$quadratID,
  paste ("occ",seq(1,nrow(benthos_long_format_2006)),sep=""),
  sep="_")




# eventYear
benthos_long_format_2006$eventYear<-benthos_long_format_2006$eventDate
# method
benthos_long_format_2006$samplingProtocol <- "fixed photo-quadrats"
# samplingEffort
benthos_long_format_2006$samplingEffort <- 10 # 10 qudrats, 15 images per quadrat
# sampleSizeValue
benthos_long_format_2006$sampleSizeValue <- 0.7
# sampleSizeUnit
benthos_long_format_2006$sampleSizeUnit <- "squared meters"
# country and code
benthos_long_format_2006$Country <- "Brazil"
benthos_long_format_2006$countryCode <- "BR"
# basisOfRecord
benthos_long_format_2006$basisOfRecord <- "HumanObservation"
# occurrenceStatus
benthos_long_format_2006$occurrenceStatus <- "presence"
# organismQuantityType
benthos_long_format_2006$organismQuantityType <- "Percentage cover"
# measurementType
benthos_long_format_2006$measurementType <- "Percentage cover"
# measurementUnit
benthos_long_format_2006$measurementUnit <- "dimensionless"
# recordedBy
benthos_long_format_2006$recordedBy <- "Ronaldo Francini-Filho"
# no depth data
benthos_long_format_2006$depthInMeters <- NA
# geographic location
benthos_long_format_2006$higherGeographyID <- "BrazilianCoast"
# adjusting habitat (based on Francini-Filho et al. 2013)
benthos_long_format_2006$habitat[which(benthos_long_format_2006$habitat == "TP")] <- "pinnacles_top"
benthos_long_format_2006$habitat[which(benthos_long_format_2006$habitat == "PA")] <- "pinnacles_wall"
benthos_long_format_2006$habitat[which(benthos_long_format_2006$habitat == "RO")] <- "rocky_reef"

# depth (following Francini-Filho et al. 2013, PlosOne, e54260)
benthos_long_format_2006$minimumDepthinMeters <- NA
benthos_long_format_2006$maximumDepthinMeters <- NA
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "pinnacles_top"),"minimumDepthinMeters"]<- 2
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "pinnacles_top"),"maximumDepthinMeters"]<- 6
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "pinnacles_wall"),"minimumDepthinMeters"]<- 3
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "pinnacles_wall"),"maximumDepthinMeters"]<- 15
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "rocky_reef"),"minimumDepthinMeters"]<- 3
benthos_long_format_2006[which(benthos_long_format_2006$habitat == "rocky_reef"),"maximumDepthinMeters"]<- 8


# matching site to have the coordinates
coords <- (match_sites_coords [match (benthos_long_format_2006$locality,match_sites_coords$SITE), c("REEF","SITE","LAT","LONG")])

# table(coords$SITE == fish_long_format$SITE) # check matching
benthos_long_format_2006<- cbind (benthos_long_format_2006,
                          coords[,c("LAT", "LONG")])


# geodeticDatum
benthos_long_format_2006$geodeticDatum <- "decimal degrees"
colnames(benthos_long_format_2006)[which(colnames(benthos_long_format_2006) %in% c("LAT", "LONG"))] <- c("decimalLatitude","decimalLongitude")


# tolower sites
benthos_long_format_2006$locationID<- tolower (benthos_long_format_2006$locationID)
benthos_long_format_2006$locality<- tolower (benthos_long_format_2006$locality)



# ------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF_2006 <- benthos_long_format_2006 [,c("eventID", "occurrenceID","scientificName","scientificNameID","kingdom","class","family",
                                             "measurementValue", "measurementType","measurementUnit")]

DF_occ_2006 <- benthos_long_format_2006 [,c("eventID", "occurrenceID","basisOfRecord","scientificName","scientificNameID","kingdom","class","family",
                                            "recordedBy", "organismQuantityType", "occurrenceStatus")]


# aggregate data by eventIDs to have event_core
event_core_2006 <- data.frame (group_by(benthos_long_format_2006, eventID,higherGeographyID,locationID,locality) %>% 
                            
                            summarise(eventYear = mean(eventYear),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = unique(samplingEffort),
                                      sampleSizeValue =unique(sampleSizeValue),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)




# ------------------------------------------------------------
# bind data < and > 2006




DF_eMOF <- rbind(DF_eMOF, DF_eMOF_2006) # DF_eMOF 
DF_occ <- rbind(DF_occ, DF_occ_2006) # DF_occ
event_core <- rbind(event_core, event_core_2006) # event_core

# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)



# write to txt format


write.table(DF_occ, file =here("DwC_output",
                               "RFrancini_timeSeries_abrolhos",
                               "DF_occ_benthos.txt"),sep=",",
            quote = FALSE)
write.table(DF_eMOF, file =here("DwC_output",
                                "RFrancini_timeSeries_abrolhos",
                                "DF_eMOF_benthos.txt"),sep=",",
            quote = FALSE)

write.table(event_core, file =here("DwC_output",
                                   "RFrancini_timeSeries_abrolhos",
                                   "event_core_benthos.txt"),sep=",",
            quote = FALSE)
