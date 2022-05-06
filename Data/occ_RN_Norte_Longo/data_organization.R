# organize data from G Longo, reefs from Rio Grande do Norte
# DATASETS OF FISH AND BENTHOS

source("R/functions.R") # function for coordinate transformation


require(here); require(openxlsx); require(dplyr); require(reshape); require(worrms)



# ------------------------------------------------------
# ORGANIZE THE FISH DATASET

# Ross, Longo et al. (RN fish)
occ_Ross_et_al <- read.xlsx(here("Data","occ_RN_Norte_Longo",
                                 "Censos_peixes_RN.xlsx"),
                            sheet = 1, colNames = TRUE,
                            detectDates=F)

# second sheet, species names
sp_Ross_et_al <- read.xlsx(here("Data","occ_RN_Norte_Longo",
                                "Censos_peixes_RN.xlsx"),
                           sheet = 2, colNames = TRUE,detectDates=T)

# match these last two
occ_Ross_et_al$scientificName <- sp_Ross_et_al [match (occ_Ross_et_al$spp, 
                                                       sp_Ross_et_al$code),"spp"]
  



# remove 2016 data from Rio Grande do Norte dataset (only Scaridae were collected according to info from Natalia Roos)
# occ_Ross_et_al <- occ_Ross_et_al [which(occ_Ross_et_al$year == "2017"),]





# ------------------------------------------------------
# ADJUSTING COORDINATES AND SPECIES





## transform 'dd mm ss' coordinates into decimal degrees
# adjust
occ_Ross_et_al$latDec <- gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al$lat)))
occ_Ross_et_al$longDec <- gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al$long)))

# transform
occ_Ross_et_al$decimalLatitude <- angle2dec(occ_Ross_et_al$latDec)*-1 # below Equator
occ_Ross_et_al$decimalLongitude <- angle2dec(occ_Ross_et_al$longDec)*-1 # west of Greenwich

# resolve NA taking the coordinate from the previous record (is the same)
occ_Ross_et_al [which(is.na(occ_Ross_et_al$longitude)),"decimalLongitude"] <- occ_Ross_et_al [which(is.na(occ_Ross_et_al$decimalLongitude))-1,"decimalLongitude"]



# taxonomic issues
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Platybelone argalus")] <- "Platybelone argalus argalus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Nicholsina usta collettei")] <- "Nicholsina collettei"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Labrisomus kalisherae")] <- "Gobioclinus kalisherae"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Diplodus argenteus argenteus")] <- "Diplodus argenteus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Eucinostomus lefroyi")] <- "Ulaema lefroyi"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Haemulon plumieri")] <- "Haemulon plumierii"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Hypanus americana")] <- "Hypanus americanus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Dasyatis americana")] <- "Hypanus americanus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Caranx plumbeus")] <- "Carcharhinus plumbeus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Lutjanus mohogani")] <- "Lutjanus mahogani"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Epinephelus niveatus")] <- "Hyporthodus niveatus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Epinephelus cruentatus")] <- "Cephalopholis cruentata"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus spinosus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Coryphopterus_spb" )] <-  "Coryphopterus spp"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Dasyatis_americana" )] <-  "Hypanus americanus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Diplodus_argenteus_argenteus" )] <-  "Diplodus argenteus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Emblemariopsis_signifera" )] <-  "Emblemariopsis signifer"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Kyphosus_incisor" )] <-  "Kyphosus vaigiensis"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Kyphosus_bigibbus" )] <-  "Kyphosus sp"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Nicholsina_usta_usta" )] <-  "Nicholsina usta usta"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Nicholsina_usta_collettei" )] <-  "Nicholsina usta"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Anthias_salmopuntatus" )] <- "Choranthias salmopunctatus"
occ_Ross_et_al$scientificName [which(occ_Ross_et_al$scientificName == "Emblemariosis_sp" )] <- "Emblemariopsis sp"
# tolower
occ_Ross_et_al$scientificName <- tolower(occ_Ross_et_al$scientificName)


# check spp names
# matching with worms
worms_record <- lapply (unique(occ_Ross_et_al$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
occ_Ross_et_al$scientificNameID<-(df_worms_record$lsid [match (occ_Ross_et_al$scientificName,tolower (df_worms_record$scientificname))])
occ_Ross_et_al$kingdom<-(df_worms_record$kingdom [match (occ_Ross_et_al$scientificName,tolower (df_worms_record$scientificname))])
occ_Ross_et_al$class<-(df_worms_record$class [match (occ_Ross_et_al$scientificName,tolower (df_worms_record$scientificname))])
occ_Ross_et_al$family<-(df_worms_record$family [match (occ_Ross_et_al$scientificName,tolower (df_worms_record$scientificname))])





# ----------------------------------------------------------------------
# FORMATING DATES





# create events 
occ_Ross_et_al$eventYear <-occ_Ross_et_al$year # year
occ_Ross_et_al$eventMonth <- occ_Ross_et_al$month  # mont
occ_Ross_et_al$eventMonth <- ifelse(nchar(occ_Ross_et_al$eventMonth) == 1, # adjust format
       paste ("0", occ_Ross_et_al$eventMonth,sep=""),
       occ_Ross_et_al$eventMonth)
occ_Ross_et_al$eventDay <- occ_Ross_et_al$day# day
occ_Ross_et_al$eventDay <- ifelse(nchar(occ_Ross_et_al$eventDay) == 1, # adjust format
                                    paste ("0", occ_Ross_et_al$eventDay,sep=""),
                                  occ_Ross_et_al$eventDay)

# as date
occ_Ross_et_al$eventDate <- as.Date (paste(occ_Ross_et_al$eventYear,
                                    occ_Ross_et_al$eventMonth,
                                    occ_Ross_et_al$eventDay,
                                    sep="-"))







# ----------------------------------------------------------------------
# FORMATING SITES, REGIONS, DEPTHS ...






# region (unique is NE)
occ_Ross_et_al$region <- "ne_reefs" # lower Geo ID?

# locality
# according to Aued & Morais
occ_Ross_et_al$regionalization <- occ_Ross_et_al$local
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "pirangi")] <- "rgnor_sul"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "riodofogo")] <- "rgnor_parrachos"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "baiaformosa")] <- "rgnor_sul"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "diogolopes")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "caicaradonorte")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "portodomangue")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "natal")] <- "rgnor_natal"

# fixing locals(reefs)
occ_Ross_et_al$locationID <- occ_Ross_et_al$local
occ_Ross_et_al$locationID [which(occ_Ross_et_al$locationID == "riodofogo")] <- "parrachos_de_rio_do_fogo"

# sitename = locality
occ_Ross_et_al$locality <- occ_Ross_et_al$sitename
# adjusting site names
occ_Ross_et_al$locality<-(iconv(occ_Ross_et_al$locality, "ASCII", "UTF-8", sub=""))
occ_Ross_et_al$locality <- tolower(occ_Ross_et_al$locality)
# adjust
unique(occ_Ross_et_al$locality)[order(unique(occ_Ross_et_al$locality))]
occ_Ross_et_al$locality[which(occ_Ross_et_al$locality == "batentedasagulhas")] <- "batente_das_agulhas"
occ_Ross_et_al$locality[which(occ_Ross_et_al$locality == "mestrevicente")] <- "mestre_vicente"
unique(occ_Ross_et_al$locality )[order(unique(occ_Ross_et_al$locality ))]


# event depth
occ_Ross_et_al$minimumDepthInMeters <- occ_Ross_et_al$depth
occ_Ross_et_al$maximumDepthInMeters <-occ_Ross_et_al$minimumDepthInMeters 






# ----------------------------------------------------------------------
# CREATING IDS AND CREATING DWC DESCRIPTORS







## occID for the dataset
occ_Ross_et_al$occurrenceID <- paste (
                               paste ("BR:RioGrandeDoNorte:", 
                                     occ_Ross_et_al$locationID,sep=""),
                               occ_Ross_et_al$locality, # check
                               occ_Ross_et_al$eventYear,
                               occ_Ross_et_al$transectidtot,
                              paste ("occ",seq(1,nrow(occ_Ross_et_al)),sep=""),
                              sep="_")



## fazer eventID para este estudo
occ_Ross_et_al$eventID <- paste (
                              paste ("BR:RioGrandeDoNorte:", 
                                     occ_Ross_et_al$locationID,sep=""),
                              occ_Ross_et_al$locality,
                              occ_Ross_et_al$eventYear,
                              occ_Ross_et_al$transectidtot,
                              sep="_")



## fazer parentID para este estudo
occ_Ross_et_al$parentEventID <- paste (
                                    paste ("BR:RioGrandeDoNorte:", 
                                           occ_Ross_et_al$locationID,sep=""),
                                            occ_Ross_et_al$locality,
                                    occ_Ross_et_al$eventYear,
                                    sep="_")
                                  


# dwc format
# country and code
occ_Ross_et_al$Country <- "Brazil"
occ_Ross_et_al$countryCode <- "BR"
# basisOfRecord
occ_Ross_et_al$basisOfRecord <- "HumanObservation"
# occurrenceStatus
occ_Ross_et_al$occurrenceStatus <- "presence"
# organismQuantityType
occ_Ross_et_al$organismQuantityType <- "Relative cover"
# geodeticDatum
occ_Ross_et_al$geodeticDatum <- "decimal degrees"
# geographic location
occ_Ross_et_al$higherGeographyID <- "BrazilianCoast"
# method
occ_Ross_et_al$samplingProtocol <- "underwater visual survey - 20 x 2m"
# effort
## check roos et al. 2020/2019
occ_Ross_et_al$samplingEffort <- 1
# sampleSizeValue
occ_Ross_et_al$sampleSizeValue <- 40# plotarea?radii?"
# sampleSizeUnit
occ_Ross_et_al$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(occ_Ross_et_al)[which(colnames(occ_Ross_et_al) == "coletor")] <- "recordedBy"
# separate size from abundance (counts)
abundance <- occ_Ross_et_al[,which(colnames(occ_Ross_et_al) != "size")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abundance")] <- "measurementValue"

# size
size <- occ_Ross_et_al[,which(colnames(occ_Ross_et_al) != "abundance")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "size")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (abundance,
                     size)





# -----------------------------------------------------------------------------
# DwC FORMAT







DF_eMOF <- dados_bind [,c("eventID", "occurrenceID","scientificName","scientificNameID","kingdom","class","family",
                          "measurementValue", "measurementType","measurementUnit")]


DF_occ <- dados_bind [,c("eventID", "occurrenceID","basisOfRecord","scientificName","scientificNameID","kingdom","class","family",
                         "recordedBy", "organismQuantityType", "occurrenceStatus")]




# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])

event_core <- data.frame (group_by(dados_bind, eventID,higherGeographyID,locationID,locality) %>% 
                            
                            summarise(eventYear = mean(eventYear),
                                      eventDate = mean(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(as.numeric(samplingEffort)),
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
                               "GLongo_NRoss_spatialData",
                               "DF_occ_fish.txt"),sep=",",
            quote = FALSE)
write.table(DF_eMOF, file =here("DwC_output",
                                "GLongo_NRoss_spatialData",
                                "DF_eMOF_fish.txt"),sep=",",
            quote = FALSE)

write.table(event_core, file =here("DwC_output",
                                   "GLongo_NRoss_spatialData",
                                   "event_core_fish.txt"),sep=",",
            quote = FALSE)








# ---------------------------------------------------------------------------

# NOW ORGANIZE THE BENTHIC DATASET








# Ross, Longo et al. (RN fish)
occ_Ross_et_al_benthos <- read.xlsx(here("Data","occ_RN_Norte_Longo",
                                                "Bentos_RN_tet.xlsx"),
                            sheet = 1, colNames = TRUE,detectDates=T)





# ------------------------------------------------------------------------
# ADJUST COORDINATES





## transform 'dd mm ss' coordinates into decimal degrees
# adjust
occ_Ross_et_al_benthos$decimalLatitude <- gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al_benthos$lat)))
occ_Ross_et_al_benthos$decimalLongitude <- gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al_benthos$long)))

# transform
occ_Ross_et_al_benthos$decimalLatitude <- angle2dec(occ_Ross_et_al_benthos$decimalLatitude)*-1 # below Equator
occ_Ross_et_al_benthos$decimalLongitude <- angle2dec(occ_Ross_et_al_benthos$decimalLongitude)*-1 # west of Greenwich





# ------------------------------------------------------------------------
# ADJUST TAXON NAMES






# replace space by "_"
colnames(occ_Ross_et_al_benthos)[which(colnames(occ_Ross_et_al_benthos) == "sppName")] <- "scientificName"
occ_Ross_et_al_benthos$scientificName  <-  (gsub("\\."," ",occ_Ross_et_al_benthos$scientificName ))
occ_Ross_et_al_benthos$scientificName  <- (iconv(occ_Ross_et_al_benthos$scientificName , "ASCII", "UTF-8", sub=""))
occ_Ross_et_al_benthos$scientificName <- tolower(occ_Ross_et_al_benthos$scientificName)


# adjust names
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "turf")] <-"filamentous algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "cca")] <-"crustose coralline algae"


# adjust based on knowledge of Cesar Cordeiro
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "aiolochoria crassa")] <- "aiolochroia crassa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "meandrina braziliensis")] <- "meandrina brasiliensis"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "millepora")] <- "millepora sp"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "montastrea cavernosa")] <- "montastraea cavernosa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "mussismilia")] <- "mussismilia spp"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "neospongodes atlbntica")] <- "neospongodes atlantica"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "siderastrea spp ")] <- "siderastrea spp"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "siderastrea")] <- "siderastrea spp"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "siderastrea sp")] <- "siderastrea spp"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "ventricaria ventricosa")] <- "valonia ventricosa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "zooanthus sociatus")] <- "zoanthus sociatus"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "zoanthid")] <- "zoantharia"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "zoanthus sp ")] <- "zoantharia"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "palythoa")] <- "palythoa sp "
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "padina")] <- "padina sp"

# broader groups
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "leathery")] <- "leathery algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "spirobidae - polycchaete")] <- "spirorbidae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "briozoa")] <- "bryozoa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "bryozoan")] <- "bryozoa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "hidrozoan")] <- "hydrozoa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "outro hydrozoa")] <- "hydrozoa"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "poliqueta")] <- "polychaeta"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "polichaeta")] <- "polychaeta"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"
# octocoral and anthozoa
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "outro anthozoa")] <- "anthozoa"
occ_Ross_et_al_benthos$scientificName[grep("octocoral",occ_Ross_et_al_benthos$scientificName)] <- "alcyonaria"
# sponge
occ_Ross_et_al_benthos$scientificName[grep("sponge",occ_Ross_et_al_benthos$scientificName)] <- "porifera"
# echinorms
occ_Ross_et_al_benthos$scientificName[grep("ourigo",occ_Ross_et_al_benthos$scientificName)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
occ_Ross_et_al_benthos$scientificName[grep("sea urchin",occ_Ross_et_al_benthos$scientificName)] <- "echinoidea"
occ_Ross_et_al_benthos$scientificName[grep("outro echinoderma",occ_Ross_et_al_benthos$scientificName)] <- "echinodermata"
occ_Ross_et_al_benthos$scientificName[grep("crinside",occ_Ross_et_al_benthos$scientificName)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
occ_Ross_et_al_benthos$scientificName[grep("estrela",occ_Ross_et_al_benthos$scientificName)] <- "asteroidea"

# cca and caa
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "crostose coralline algae")] <- "crustose coralline algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "cianobacterias")] <- "cyanobacteria"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName %in% c("amphiroa", 
                                                             "amphiroa sp", 
                                                             "amphiroideae", 
                                                             "jania amphiroa", 
                                                             "jania sp",
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "filamentous")] <- "filamentous algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "green filamentous algae")] <- "filamentous algae"

# algae
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName %in% c("fleshy algae",
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "calcareous turf")] <- "calcareous articulate algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "corticated")] <- "corticated algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "unknown corticated")] <- "corticated algae"
occ_Ross_et_al_benthos$scientificName[which(occ_Ross_et_al_benthos$scientificName == "sargassum sp")] <- "leathery algae"

# check unique taxa
unique(occ_Ross_et_al_benthos$scientificName)[order(unique(occ_Ross_et_al_benthos$scientificName))]

# remove plot data
occ_Ross_et_al_benthos <-occ_Ross_et_al_benthos[which(occ_Ross_et_al_benthos$scientificName %in% c("tape","sand","shadow","rock", "rodolith", 
                                                                                                   "unknown" ,"unknown algae") != T),]

# matching with worms
worms_record <- lapply (unique(occ_Ross_et_al_benthos$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
occ_Ross_et_al_benthos$scientificNameID<-(df_worms_record$lsid [match (occ_Ross_et_al_benthos$scientificName,tolower (df_worms_record$scientificname))])
occ_Ross_et_al_benthos$kingdom<-(df_worms_record$kingdom [match (occ_Ross_et_al_benthos$scientificName,tolower(df_worms_record$scientificname))])
occ_Ross_et_al_benthos$class<-(df_worms_record$class [match (occ_Ross_et_al_benthos$scientificName,tolower(df_worms_record$scientificname))])
occ_Ross_et_al_benthos$family<-(df_worms_record$family [match (occ_Ross_et_al_benthos$scientificName,tolower(df_worms_record$scientificname))])





# ------------------------------------------------------------------------
# ADJUSTING DATES





# create events (based on fish data)
occ_Ross_et_al_benthos <- cbind (occ_Ross_et_al_benthos, 
        occ_Ross_et_al [match (occ_Ross_et_al_benthos$sitecode, occ_Ross_et_al$sitecode), 
                        c("eventYear","eventMonth", "eventDay"),])

# as date
occ_Ross_et_al_benthos$eventDate <- as.Date (paste(occ_Ross_et_al_benthos$eventYear,
                                                   occ_Ross_et_al_benthos$eventMonth,
                                                   occ_Ross_et_al_benthos$eventDay,
                                           sep="-"))




# ------------------------------------------------------------------------
# ADJUSTING SITES, DEPTHS



# event ID
# region (unique is NE)
occ_Ross_et_al_benthos$region <- "ne_reefs"

# locality
# according to Aued & Morais
occ_Ross_et_al_benthos$locationID <- occ_Ross_et_al_benthos$local
# adjust natal (NA)
occ_Ross_et_al_benthos$locationID[is.na(occ_Ross_et_al_benthos$locationID)] <- "NA"
# adjust other
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "PI")] <- "pirangi"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "RF")] <- "parrachos_de_rio_do_fogo"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "BF")] <- "baiaformosa"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "DL")] <- "diogolopes"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "CN")] <- "caicaradonorte"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "PM")] <- "portodomangue"
occ_Ross_et_al_benthos$locationID [which(occ_Ross_et_al_benthos$locationID == "NA")] <- "natal"

# reef (location ID)
occ_Ross_et_al_benthos$locality <- occ_Ross_et_al_benthos$site
occ_Ross_et_al_benthos$locality[which(occ_Ross_et_al_benthos$locality == "batentedasagulhas")] <- "batente_das_agulhas"
occ_Ross_et_al_benthos$locality[which(occ_Ross_et_al_benthos$locality == "mestrevicente")] <- "mestre_vicente"

unique(occ_Ross_et_al_benthos$locality)[order(unique(occ_Ross_et_al_benthos$locality))]

# event depth
occ_Ross_et_al_benthos$minimumDepthInMeters <- occ_Ross_et_al_benthos$depth
occ_Ross_et_al_benthos$maximumDepthInMeters <- occ_Ross_et_al_benthos$minimumDepthInMeters

## fazer occID para este estudo
occ_Ross_et_al_benthos$occurrenceID <- paste (
  paste ("BR:RioGrandeDoNorte:", 
         occ_Ross_et_al_benthos$locationID,sep=""),
          occ_Ross_et_al_benthos$locality, # check
          occ_Ross_et_al_benthos$eventYear,
          occ_Ross_et_al_benthos$transect,
        paste ("occ",seq(1,nrow(occ_Ross_et_al_benthos)),sep=""),
  sep="_")

## fazer eventID para este estudo
occ_Ross_et_al_benthos$eventID <- paste (
  paste ("BR:RioGrandeDoNorte:", 
         occ_Ross_et_al_benthos$locationID,sep=""),
        occ_Ross_et_al_benthos$locality,
        occ_Ross_et_al_benthos$eventYear,
        occ_Ross_et_al_benthos$transect,
  sep="_")


## fazer parentID para este estudo
occ_Ross_et_al_benthos$parentEventID <- paste (
  paste ("BR:RioGrandeDoNorte:", 
         occ_Ross_et_al_benthos$locationID,sep=""),
        occ_Ross_et_al_benthos$locality,
        occ_Ross_et_al_benthos$eventYear,
  sep="_")



# dwc format
# country and code
occ_Ross_et_al_benthos$Country <- "Brazil"
occ_Ross_et_al_benthos$countryCode <- "BR"
# organismQuantityType
occ_Ross_et_al_benthos$organismQuantityType <- "Percentage cover"
# basisOfRecord
occ_Ross_et_al_benthos$basisOfRecord <- "HumanObservation"
# occurrenceStatus
occ_Ross_et_al_benthos$occurrenceStatus <- "presence"
# geodeticDatum
occ_Ross_et_al_benthos$geodeticDatum <- "decimal degrees"
# higherGeograpyhID
occ_Ross_et_al_benthos$higherGeographyID <- "BrazilianCoast"
# method
occ_Ross_et_al_benthos$samplingProtocol <- "photoquadrats"
# effort
## check roos et al. 2020/2019
occ_Ross_et_al_benthos$samplingEffort <- 2 # two squared meters
# sampleSizeValue
occ_Ross_et_al_benthos$sampleSizeValue <- 10# number of photos
# sampleSizeUnit
occ_Ross_et_al_benthos$sampleSizeUnit <- "number of photos per transect"
# recordedBy
occ_Ross_et_al_benthos$recordedBy <- "NataliaRoss"
# cover
occ_Ross_et_al_benthos$measurementValue <- occ_Ross_et_al_benthos$`Cov%.per.species`
# measurementType
occ_Ross_et_al_benthos$measurementType <- "Percentage cover"
# measurementUnit
occ_Ross_et_al_benthos$measurementUnit <- "dimensionless"





# --------------------------------------------------------------------
# Formatted according to DwC





DF_eMOF <- occ_Ross_et_al_benthos [,c("eventID", "occurrenceID","scientificName","scientificNameID","kingdom","class","family",
                                      "measurementValue", "measurementType","measurementUnit")]



DF_occ <- occ_Ross_et_al_benthos [,c("eventID", "occurrenceID","basisOfRecord","scientificName","scientificNameID","kingdom","class","family",
                                     "recordedBy", "organismQuantityType", "occurrenceStatus")]

# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])
#event_core <- aggregate (minimumDepthInMeters ~ parentEventID + eventID +
#                           higherGeographyID+locationID +locality + 
#                           eventDate+ eventYear+
#                           minimumDepthInMeters+maximumDepthInMeters+
#                           samplingProtocol+samplingEffort+sampleSizeValue+
#                           decimalLongitude+decimalLatitude+
#                           geodeticDatum+
#                           Country+countryCode, 
#                         data = occ_Ross_et_al_benthos,
#                         FUN = mean)
#
event_core <- data.frame (group_by(occ_Ross_et_al_benthos, eventID,higherGeographyID,locationID,locality) %>% 
                            
                            summarise(eventYear = mean(eventYear),
                                      eventDate = mean(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
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
write.table(DF_occ, file =here("DwC_output",
                               "GLongo_NRoss_spatialData",
                               "DF_occ_benthos.txt"),sep=",",
            quote = FALSE)
write.table(DF_eMOF, file =here("DwC_output",
                                "GLongo_NRoss_spatialData",
                                "DF_eMOF_benthos.txt"),sep=",",
            quote = FALSE)

write.table(event_core, file =here("DwC_output",
                                   "GLongo_NRoss_spatialData",
                                   "event_core_benthos.txt"),sep=",",
            quote = FALSE)

# end
