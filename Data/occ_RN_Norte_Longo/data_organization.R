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





# remove DATA from Parrachos (longer monitoring, will be formatted separately) 
occ_Ross_et_al <- occ_Ross_et_al [which(occ_Ross_et_al$local != "riodofogo"),]





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
#occ_Ross_et_al [which(is.na(occ_Ross_et_al$longitude)),"decimalLongitude"] <- occ_Ross_et_al [which(is.na(occ_Ross_et_al$decimalLongitude))-1,"decimalLongitude"]


# verbatimIdentification
occ_Ross_et_al$namesToSearch <- occ_Ross_et_al$scientificName
occ_Ross_et_al$verbatimIdentification <- occ_Ross_et_al$namesToSearch


# taxonomic issues
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Platybelone argalus")] <- "Platybelone argalus argalus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Nicholsina usta collettei")] <- "Nicholsina collettei"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Labrisomus kalisherae")] <- "Gobioclinus kalisherae"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Diplodus argenteus argenteus")] <- "Diplodus argenteus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Eucinostomus lefroyi")] <- "Ulaema lefroyi"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Haemulon plumieri")] <- "Haemulon plumierii"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Hypanus americana")] <- "Hypanus americanus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Dasyatis americana")] <- "Hypanus americanus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Caranx plumbeus")] <- "Carcharhinus plumbeus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Lutjanus mohogani")] <- "Lutjanus mahogani"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Epinephelus niveatus")] <- "Hyporthodus niveatus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Epinephelus cruentatus")] <- "Cephalopholis cruentata"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus spinosus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Coryphopterus_spb" )] <-  "Coryphopterus spp"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Dasyatis_americana" )] <-  "Hypanus americanus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Diplodus_argenteus_argenteus" )] <-  "Diplodus argenteus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Emblemariopsis_signifera" )] <-  "Emblemariopsis signifer"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Kyphosus_incisor" )] <-  "Kyphosus vaigiensis"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Kyphosus_bigibbus" )] <-  "Kyphosus sp"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Nicholsina_usta_usta" )] <-  "Nicholsina usta usta"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Nicholsina_usta_collettei" )] <-  "Nicholsina usta"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Anthias_salmopuntatus" )] <- "Choranthias salmopunctatus"
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$namesToSearch == "Emblemariosis_sp" )] <- "Emblemariopsis sp"

# tolower
occ_Ross_et_al$namesToSearch <- tolower(occ_Ross_et_al$namesToSearch)

# genera level
occ_Ross_et_al$identificationQualifier <- ifelse (sapply (strsplit (occ_Ross_et_al$namesToSearch, " "), "[", 2) %in% c("sp","sp1", "sp2", "sp3"),
                                                            "sp",
                                                            NA)

# species to search
occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$identificationQualifier == "sp")] <- gsub (" sp",
                                                                                              "",
                                                                                              occ_Ross_et_al$namesToSearch [which(occ_Ross_et_al$identificationQualifier == "sp")])



# check spp names
# matching with worms
worms_record <- lapply (unique(occ_Ross_et_al$namesToSearch), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# melt
df_worms_record <- data.frame(do.call(rbind,worms_record))

# match
occ_Ross_et_al$scientificNameAccepted<-(df_worms_record$scientificname [match (occ_Ross_et_al$namesToSearch,
                                                                                 tolower (df_worms_record$scientificname))])
# taxon rank of the identified level
occ_Ross_et_al$taxonRank <- (df_worms_record$rank [match (occ_Ross_et_al$namesToSearch,
                                                                    tolower (df_worms_record$scientificname))])
# aphiID
occ_Ross_et_al$scientificNameID<-(df_worms_record$lsid [match (occ_Ross_et_al$namesToSearch,
                                                                         tolower (df_worms_record$scientificname))])
# kingdom
occ_Ross_et_al$kingdom<-(df_worms_record$kingdom [match (occ_Ross_et_al$namesToSearch,
                                                                   tolower (df_worms_record$scientificname))])
# phylum
occ_Ross_et_al$phylum<-(df_worms_record$phylum [match (occ_Ross_et_al$namesToSearch,
                                                         tolower (df_worms_record$scientificname))])
# class
occ_Ross_et_al$class<-(df_worms_record$class [match (occ_Ross_et_al$namesToSearch,
                                                               tolower (df_worms_record$scientificname))])
# order
occ_Ross_et_al$order<-(df_worms_record$order [match (occ_Ross_et_al$namesToSearch,
                                                       tolower (df_worms_record$scientificname))])
# family
occ_Ross_et_al$family<-(df_worms_record$family [match (occ_Ross_et_al$namesToSearch,
                                                                 tolower (df_worms_record$scientificname))])

# genus
occ_Ross_et_al$genus <-(df_worms_record$genus [match (occ_Ross_et_al$namesToSearch,
                                                       tolower (df_worms_record$scientificname))])




# taxonomic updates
# species
occ_Ross_et_al$scientificNameAccepted[grep ("multilineata", occ_Ross_et_al$scientificNameAccepted)] <- "Azurina multilineata"
occ_Ross_et_al$scientificNameAccepted[grep ("bartholomaei", occ_Ross_et_al$scientificNameAccepted)] <- "Caranx bartholomaei"
occ_Ross_et_al$scientificNameAccepted[grep ("Hypanus americanus", occ_Ross_et_al$scientificNameAccepted)] <- "Hypanus berthalutzea"
occ_Ross_et_al$scientificNameAccepted[grep ("Kyphosus vaigensis", occ_Ross_et_al$scientificNameAccepted)] <- "Kyphosus vaigiensis"

# genus
occ_Ross_et_al$genus[grep ("multilineata", occ_Ross_et_al$scientificNameAccepted)] <- "Azurina"
occ_Ross_et_al$genus[grep ("bartholomaei", occ_Ross_et_al$scientificNameAccepted)] <- "Caranx"



# ----------------------------------------------------------------------
# FORMATING DATES





# edit dates
# month
occ_Ross_et_al$verbatimMonth <- occ_Ross_et_al$month  # month
occ_Ross_et_al$month <- ifelse(nchar(occ_Ross_et_al$month) == 1, # adjust format
                                         paste0 ("0", occ_Ross_et_al$month),
                                         occ_Ross_et_al$month)
# day
occ_Ross_et_al$verbatimDay <- occ_Ross_et_al$day  # month
occ_Ross_et_al$day <- ifelse(nchar(occ_Ross_et_al$day) == 1, # adjust format
                                       paste0 ("0", occ_Ross_et_al$day),
                                       occ_Ross_et_al$day)

# as date
occ_Ross_et_al$eventDate <- as.Date (paste(occ_Ross_et_al$year,
                                                     occ_Ross_et_al$month,
                                                     occ_Ross_et_al$day,
                                                     sep="-"))








# ----------------------------------------------------------------------
# FORMATING SITES, REGIONS, DEPTHS ...





# geography
occ_Ross_et_al$higherGeography <- "BrazilianCoast"

# region (unique is NE)
occ_Ross_et_al$region <- "ne_reefs" # lower Geo ID?

# sites& localities
occ_Ross_et_al$verbatimSite <- (occ_Ross_et_al$local)
occ_Ross_et_al$verbatimLocality <- (occ_Ross_et_al$sitename)

# locality
# according to Aued & Morais
occ_Ross_et_al$regionalization <- occ_Ross_et_al$local
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "pirangi")] <- "rgnor_sul"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "baiaformosa")] <- "rgnor_sul"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "diogolopes")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "caicaradonorte")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "portodomangue")] <- "rgnor_set"
occ_Ross_et_al$regionalization [which(occ_Ross_et_al$regionalization == "natal")] <- "rgnor_natal"


# fixing locals(reefs)
occ_Ross_et_al$site <- occ_Ross_et_al$verbatimSite
# sitename = locality
occ_Ross_et_al$locality <- occ_Ross_et_al$verbatimLocality


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





# licence
occ_Ross_et_al$licence <- "CC BY-NC"
# language
occ_Ross_et_al$language <- "en"
# citation
occ_Ross_et_al$bibliographicCitation <- "Roos NC, Pennino MG, Carvalho AR, Longo GO (2019) Drivers of abundance and biomass of Brazilian parrotfishes. Mar Ecol Prog Ser 623:117-130. https://doi.org/10.3354/meps13005"
# eventRemarks
occ_Ross_et_al$eventRemarks <- ifelse (occ_Ross_et_al$year == "2016", "Only Scarini and Acanthuridae were sampled",
                                                 NA)





## occurrenceID
occ_Ross_et_al$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_fish_monitoring:", 
           occ_Ross_et_al$higherGeography,
           sep=""),
    occ_Ross_et_al$site,sep=":"),
  occ_Ross_et_al$locality,
  occ_Ross_et_al$year,
  occ_Ross_et_al$transectidtot,
  paste ("occ",seq(1,nrow(occ_Ross_et_al)),sep=""),
  sep="_")



##  eventID 
occ_Ross_et_al$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_fish_monitoring:", 
           occ_Ross_et_al$higherGeography,
           sep=""),
    occ_Ross_et_al$site,sep=":"),
  occ_Ross_et_al$locality,
  occ_Ross_et_al$year,
  occ_Ross_et_al$transectidtot,
  sep="_")



##  parentID 
occ_Ross_et_al$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_fish_monitoring:", 
           occ_Ross_et_al$higherGeography,
           sep=""),
    occ_Ross_et_al$site,sep=":"),
  occ_Ross_et_al$locality,
  occ_Ross_et_al$year,
  sep="_")




# dwc format
# country and code
occ_Ross_et_al$Country <- "Brazil"
occ_Ross_et_al$countryCode <- "BR"

# basisOfRecord
occ_Ross_et_al$basisOfRecord <- "HumanObservation"

# occurrenceStatus
occ_Ross_et_al$occurrenceStatus <- "presence"

# geodeticDatum
occ_Ross_et_al$geodeticDatum <- "decimal degrees"

# method
occ_Ross_et_al$samplingProtocol <- "Underwater visual survey - 20 x 2m"

# effort
## check roos et al. 2020/2019
occ_Ross_et_al$samplingEffort <- 1 # one observer

# sampleSizeValue
occ_Ross_et_al$sampleSizeValue <- 20*2#

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







DF_eMOF <- dados_bind [,c("eventID", 
                          "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "eventRemarks")]


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
                         "occurrenceStatus",
                         "licence",
                         "language",
                         "bibliographicCitation",
                         "eventRemarks")]




# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(as.numeric(samplingEffort)),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
                          
                          
)







# ------------------------------------------------------
# ORGANIZE THE FISH DATASET (MONITORING OF PARRACHOS -- 2016 - 2023)





# Ross, Longo et al. (RN fish)
occ_Ross_et_al_parrachos <- read.xlsx(here("Data","occ_RN_Norte_Longo",
                                 "Censos_Rio do Fogo_20042023.xlsx"),
                            sheet = 1, colNames = TRUE,
                            detectDates=F)

# remove spaces
occ_Ross_et_al_parrachos$spp <- gsub (" ", "", occ_Ross_et_al_parrachos$spp)

# second sheet, species names
sp_Ross_et_al <- read.xlsx(here("Data","occ_RN_Norte_Longo",
                                "Censos_Rio do Fogo_20042023.xlsx"),
                           sheet = 2, colNames = TRUE,detectDates=T)

# match these last two
occ_Ross_et_al_parrachos$namesToSearch <- sp_Ross_et_al [match (occ_Ross_et_al_parrachos$spp, 
                                                       sp_Ross_et_al$code),"spp"]
  

unique(occ_Ross_et_al_parrachos[is.na(occ_Ross_et_al_parrachos$namesToSearch),"spp"])
unique(occ_Ross_et_al_parrachos[is.na(occ_Ross_et_al_parrachos$namesToSearch),"obs"])





# ------------------------------------------------------
# ADJUSTING COORDINATES AND SPECIES


unique(occ_Ross_et_al_parrachos[is.na(occ_Ross_et_al_parrachos$lat),("site")])


## transform 'dd mm ss' coordinates into decimal degrees
# adjust
occ_Ross_et_al_parrachos$latDec <- gsub ("\"", "",gsub ("º", " ", gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al_parrachos$lat)))))
occ_Ross_et_al_parrachos$longDec <- gsub ("\"", "",gsub ("º", " ", gsub ("'"," ",gsub ("''","",gsub ("°"," ",occ_Ross_et_al_parrachos$long)))))

# transform
occ_Ross_et_al_parrachos$decimalLatitude <- angle2dec(occ_Ross_et_al_parrachos$latDec)*-1 # below Equator
occ_Ross_et_al_parrachos$decimalLongitude <- angle2dec(occ_Ross_et_al_parrachos$longDec)*-1 # west of Greenwich

# check if coordinates are ok
unique(occ_Ross_et_al_parrachos$site[is.na(occ_Ross_et_al_parrachos$decimalLatitude)])
unique(occ_Ross_et_al_parrachos$site[is.na(occ_Ross_et_al_parrachos$decimalLongitude)])


# verbatimIdentification
occ_Ross_et_al_parrachos$verbatimIdentification <- occ_Ross_et_al_parrachos$namesToSearch



# taxonomic issues
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Platybelone argalus")] <- "Platybelone argalus argalus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Nicholsina usta collettei")] <- "Nicholsina collettei"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Labrisomus kalisherae")] <- "Gobioclinus kalisherae"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Diplodus argenteus argenteus")] <- "Diplodus argenteus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Eucinostomus lefroyi")] <- "Ulaema lefroyi"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Haemulon plumieri")] <- "Haemulon plumierii"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Hypanus americana")] <- "Hypanus americanus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Dasyatis americana")] <- "Hypanus americanus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Caranx plumbeus")] <- "Carcharhinus plumbeus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Lutjanus mohogani")] <- "Lutjanus mahogoni"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Epinephelus niveatus")] <- "Hyporthodus niveatus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Epinephelus cruentatus")] <- "Cephalopholis cruentata"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus spinosus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Coryphopterus spb" )] <-  "Coryphopterus spp"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Dasyatis americana" )] <-  "Hypanus americanus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Diplodus argenteus argenteus" )] <-  "Diplodus argenteus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Emblemariopsis signifera" )] <-  "Emblemariopsis signifer"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Kyphosus incisor" )] <-  "Kyphosus vaigiensis"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Kyphosus bigibbus" )] <-  "Kyphosus sp"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Nicholsina usta usta" )] <-  "Nicholsina usta usta"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Nicholsina usta collettei" )] <-  "Nicholsina usta"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Anthias salmopuntatus" )] <- "Choranthias salmopunctatus"
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$namesToSearch == "Emblemariosis sp" )] <- "Emblemariopsis sp"

# tolower
occ_Ross_et_al_parrachos$namesToSearch <- tolower(occ_Ross_et_al_parrachos$namesToSearch)


# genera level
occ_Ross_et_al_parrachos$identificationQualifier <- ifelse (sapply (strsplit (occ_Ross_et_al_parrachos$namesToSearch, " "), "[", 2) %in% c("sp","sp1", "sp2", "sp3"),
                                         "sp",
                                         NA)

# species to search
occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$identificationQualifier == "sp")] <- gsub (" sp",
                                                                            "",
                                                                            occ_Ross_et_al_parrachos$namesToSearch [which(occ_Ross_et_al_parrachos$identificationQualifier == "sp")])


# check spp names
# matching with worms
worms_record_parrachos <- lapply (unique(occ_Ross_et_al_parrachos$namesToSearch), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# melt
df_worms_record <- data.frame(do.call(rbind,worms_record_parrachos))

# match
occ_Ross_et_al_parrachos$scientificNameAccepted<-(df_worms_record$scientificname [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                                       tolower (df_worms_record$scientificname))])
# taxon rank of the identified level
occ_Ross_et_al_parrachos$taxonRank <- (df_worms_record$rank [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                        tolower (df_worms_record$scientificname))])
# aphiID
occ_Ross_et_al_parrachos$scientificNameID<-(df_worms_record$lsid [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                               tolower (df_worms_record$scientificname))])
# kingdom
occ_Ross_et_al_parrachos$kingdom<-(df_worms_record$kingdom [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                         tolower (df_worms_record$scientificname))])
# phylum
occ_Ross_et_al_parrachos$phylum<-(df_worms_record$phylum [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                                   tolower (df_worms_record$scientificname))])
# class
occ_Ross_et_al_parrachos$class<-(df_worms_record$class [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                     tolower (df_worms_record$scientificname))])
# order
occ_Ross_et_al_parrachos$order<-(df_worms_record$order [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                               tolower (df_worms_record$scientificname))])
# family
occ_Ross_et_al_parrachos$family<-(df_worms_record$family [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                       tolower (df_worms_record$scientificname))])

# genus
occ_Ross_et_al_parrachos$genus <-(df_worms_record$genus [match (occ_Ross_et_al_parrachos$namesToSearch,
                                                                 tolower (df_worms_record$scientificname))])



# taxonomic updates
# species
occ_Ross_et_al_parrachos$scientificNameAccepted[grep ("multilineata", occ_Ross_et_al_parrachos$scientificNameAccepted)] <- "Azurina multilineata"
occ_Ross_et_al_parrachos$scientificNameAccepted[grep ("bartholomaei", occ_Ross_et_al_parrachos$scientificNameAccepted)] <- "Caranx bartholomaei"
#occ_Ross_et_al_parrachos$scientificNameAccepted[grep ("polygonius", occ_Ross_et_al_parrachos$scientificNameAccepted)] <- "Acanthostracion polygonium"

# genus
occ_Ross_et_al_parrachos$genus[grep ("multilineata", occ_Ross_et_al_parrachos$scientificNameAccepted)] <- "Azurina"
occ_Ross_et_al_parrachos$genus[grep ("bartholomaei", occ_Ross_et_al_parrachos$scientificNameAccepted)] <- "Caranx"


# ----------------------------------------------------------------------
# FORMATING DATES





# edit dates
# month
occ_Ross_et_al_parrachos$verbatimMonth <- occ_Ross_et_al_parrachos$month  # month
occ_Ross_et_al_parrachos$month <- ifelse(nchar(occ_Ross_et_al_parrachos$month) == 1, # adjust format
       paste ("0", occ_Ross_et_al_parrachos$month,sep=""),
       occ_Ross_et_al_parrachos$month)

# day
occ_Ross_et_al_parrachos$verbatimDay <- occ_Ross_et_al_parrachos$day  # month
occ_Ross_et_al_parrachos$day <- ifelse(nchar(occ_Ross_et_al_parrachos$day) == 1, # adjust format
                                    paste ("0", occ_Ross_et_al_parrachos$day,sep=""),
                                  occ_Ross_et_al_parrachos$day)

# as date
occ_Ross_et_al_parrachos$eventDate <- as.Date (paste(occ_Ross_et_al_parrachos$year,
                                    occ_Ross_et_al_parrachos$month,
                                    occ_Ross_et_al_parrachos$day,
                                    sep="-"))







# ----------------------------------------------------------------------
# FORMATING SITES, REGIONS, DEPTHS ...






# region (unique is NE)
occ_Ross_et_al_parrachos$region <- "ne_reefs" # lower Geo ID?

# sites& localities
occ_Ross_et_al_parrachos$verbatimSite <- (occ_Ross_et_al_parrachos$locality)
occ_Ross_et_al_parrachos$verbatimLocality <- (occ_Ross_et_al_parrachos$site)

# sites
occ_Ross_et_al_parrachos$site <- "parrachos_de_rio_do_fogo"
occ_Ross_et_al_parrachos$locality <- occ_Ross_et_al_parrachos$verbatimLocality

# event depth
occ_Ross_et_al_parrachos$minimumDepthInMeters <- occ_Ross_et_al_parrachos$depth
occ_Ross_et_al_parrachos$maximumDepthInMeters <-occ_Ross_et_al_parrachos$minimumDepthInMeters 

# geography
occ_Ross_et_al_parrachos$higherGeography <- "BrazilianCoast"

# licence
occ_Ross_et_al_parrachos$licence <- "CC BY-NC"
# language
occ_Ross_et_al_parrachos$language <- "en"
# citation
occ_Ross_et_al_parrachos$bibliographicCitation <- "Roos NC, Pennino MG, Carvalho AR, Longo GO (2019) Drivers of abundance and biomass of Brazilian parrotfishes. Mar Ecol Prog Ser 623:117-130. https://doi.org/10.3354/meps13005"
# eventRemarks
occ_Ross_et_al_parrachos$eventRemarks <- ifelse (occ_Ross_et_al_parrachos$year == "2016", "Only Scarini and Acanthuridae were sampled",
        NA)




# ----------------------------------------------------------------------
# CREATING IDS AND CREATING DWC DESCRIPTORS






## occID for the dataset
occ_Ross_et_al_parrachos$occurrenceID <- paste (
                                paste ( 
                                  paste ("BR:ReefSYN:Parrachos_fish_monitoring:", 
                                         occ_Ross_et_al_parrachos$higherGeography,
                                         sep=""),
                                  occ_Ross_et_al_parrachos$site,sep=":"),
                                occ_Ross_et_al_parrachos$locality,
                                occ_Ross_et_al_parrachos$year,
                               occ_Ross_et_al_parrachos$transectidtot,
                              paste ("occ",seq(1,nrow(occ_Ross_et_al_parrachos)),sep=""),
                              sep="_")



## fazer eventID para este estudo
occ_Ross_et_al_parrachos$eventID <- paste (
                              paste ( 
                                paste ("BR:ReefSYN:Parrachos_fish_monitoring:", 
                                       occ_Ross_et_al_parrachos$higherGeography,
                                       sep=""),
                                occ_Ross_et_al_parrachos$site,sep=":"),
                              occ_Ross_et_al_parrachos$locality,
                              occ_Ross_et_al_parrachos$year,
                              occ_Ross_et_al_parrachos$transectidtot,
                              sep="_")



## fazer parentID para este estudo
occ_Ross_et_al_parrachos$parentEventID <- paste (
                                  paste ( 
                                    paste ("BR:ReefSYN:Parrachos_fish_monitoring:", 
                                           occ_Ross_et_al_parrachos$higherGeography,
                                           sep=""),
                                    occ_Ross_et_al_parrachos$site,sep=":"),
                                  occ_Ross_et_al_parrachos$locality,
                                  occ_Ross_et_al_parrachos$year,
                                  sep="_")

                                  


# dwc format
# country and code
occ_Ross_et_al_parrachos$Country <- "Brazil"
occ_Ross_et_al_parrachos$countryCode <- "BR"

# basisOfRecord
occ_Ross_et_al_parrachos$basisOfRecord <- "HumanObservation"

# occurrenceStatus
occ_Ross_et_al_parrachos$occurrenceStatus <- "presence"

# geodeticDatum
occ_Ross_et_al_parrachos$geodeticDatum <- "decimal degrees"

# method
occ_Ross_et_al_parrachos$samplingProtocol <- "Underwater visual survey - 20 x 2m"

# effort
## check roos et al. 2020/2019
occ_Ross_et_al_parrachos$samplingEffort <- 1

# sampleSizeValue
occ_Ross_et_al_parrachos$sampleSizeValue <- 20*2#

# sampleSizeUnit
occ_Ross_et_al_parrachos$sampleSizeUnit <- "squared meters"

# recordedBy
colnames(occ_Ross_et_al_parrachos)[which(colnames(occ_Ross_et_al_parrachos) == "coletor")] <- "recordedBy"

# separate size from abundance (counts)
abundance <- occ_Ross_et_al_parrachos[,which(colnames(occ_Ross_et_al_parrachos) != "size")] # abundance

# measurementType
abundance$measurementType <- "abundance"

# organismQuantityType
abundance$organismQuantityType <- "abundance"

# measurementUnit
abundance$measurementUnit <- "individuals"

# measurementValue
colnames(abundance)[which(colnames(abundance) == "abundance")] <- "measurementValue"

# size
size <- occ_Ross_et_al_parrachos[,which(colnames(occ_Ross_et_al_parrachos) != "abundance")]
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "size")] <- "measurementValue"



# bind edited data
dados_bind_parrachos <- rbind (abundance,
                     size)





# -----------------------------------------------------------------------------
# DwC FORMAT







DF_eMOF_parrachos <- dados_bind_parrachos [,c("eventID", 
                          "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "eventRemarks")]


DF_occ_parrachos <- dados_bind_parrachos [,c("eventID", 
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
                         "occurrenceStatus",
                         "licence",
                         "language",
                         "bibliographicCitation",
                         "eventRemarks")]




# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])

event_core_parrachos <- data.frame (group_by(dados_bind_parrachos, 
                                             eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(as.numeric(samplingEffort)),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))


)


# bind parrachos and other sites in RN
DF_occ <- rbind (DF_occ,
                 DF_occ_parrachos)
DF_eMOF <- rbind (DF_eMOF,
                  DF_eMOF_parrachos)
event_core <- rbind (event_core,
                     event_core_parrachos)


# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)

# save
# txt format
write.csv(DF_occ, file =here("DwC_output",
                               "GLongo_NRoss_spatialData",
                               "DF_occ_fish.csv"))
write.csv(DF_eMOF, file =here("DwC_output",
                                "GLongo_NRoss_spatialData",
                                "DF_eMOF_fish.csv"))
write.csv(event_core, file =here("DwC_output",
                                   "GLongo_NRoss_spatialData",
                                   "event_core_fish.csv"))








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




# verbatimIdentification
occ_Ross_et_al_benthos$verbatimIdentification <- occ_Ross_et_al_benthos$sppName

# replace space by "_"
occ_Ross_et_al_benthos$taxonOrGroup <- occ_Ross_et_al_benthos$verbatimIdentification
occ_Ross_et_al_benthos$taxonOrGroup  <-  (gsub("\\."," ",occ_Ross_et_al_benthos$taxonOrGroup ))
occ_Ross_et_al_benthos$taxonOrGroup  <- (iconv(occ_Ross_et_al_benthos$taxonOrGroup , "ASCII", "UTF-8", sub=""))
occ_Ross_et_al_benthos$taxonOrGroup <- tolower(occ_Ross_et_al_benthos$taxonOrGroup)


# adjust based on knowledge of Cesar Cordeiro

occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "ventricaria ventricosa")] <- "valonia ventricosa"

# broader groups
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "anemona")] <- "actiniaria"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "leathery")] <- "leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "spirobidae - polycchaete")] <- "spirorbinae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "briozoa")] <- "bryozoa"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "bryozoan")] <- "bryozoa"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "hidrozoan")] <- "hydrozoa"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "outro hydrozoa")] <- "hydrozoa"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "poliqueta")] <- "polychaeta"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "polichaeta")] <- "polychaeta"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup %in% c("ascidea colonial" ,                  
                                                                             "ascidian",
                                                                             "outra ascidia"))] <- "ascidiacea"

# octocoral and anthozoa
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "outro anthozoa")] <- "anthozoa"
occ_Ross_et_al_benthos$taxonOrGroup[grep("octocoral",occ_Ross_et_al_benthos$taxonOrGroup)] <- "octocorallia" # "alcyonaria" nao eh aceito
# sponge
occ_Ross_et_al_benthos$taxonOrGroup[grep("sponge",occ_Ross_et_al_benthos$taxonOrGroup)] <- "porifera"
# echinoderms
occ_Ross_et_al_benthos$taxonOrGroup[grep("ourigo",occ_Ross_et_al_benthos$taxonOrGroup)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
occ_Ross_et_al_benthos$taxonOrGroup[grep("sea urchin",occ_Ross_et_al_benthos$taxonOrGroup)] <- "echinoidea"
occ_Ross_et_al_benthos$taxonOrGroup[grep("outro echinoderma",occ_Ross_et_al_benthos$taxonOrGroup)] <- "echinodermata"
occ_Ross_et_al_benthos$taxonOrGroup[grep("crinside",occ_Ross_et_al_benthos$taxonOrGroup)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
occ_Ross_et_al_benthos$taxonOrGroup[grep("estrela",occ_Ross_et_al_benthos$taxonOrGroup)] <- "asteroidea"


### melhor nao indicar grupo morfo-anatomico (MAG) como taxonOrGroup. Esse MAG nao tem compativel pra inseir no DwC/OBIS
### Vou indicar o nivel taxonomico compativel com o que tiver e o resto deixamos como estava 
### Os grupos morfo-anatomicos podem ser adicionados com merge de tabela referencia depois

# cca and caa 
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "turf")] <-"filamentous algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "cca")] <- "corallinales" # "crustose coralline algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "cianobacterias")] <- "cyanobacteria"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup %in% c("amphiroideae", # "amphiroideae"
                                                                             "jania amphiroa", # "amphiroideae"
                                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "filamentous")] <- "filamentous algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "green filamentous algae")] <- "chlorophyta" # "filamentous algae"

# algae
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup %in% c("fleshy algae", # usando o taxaOrGroup podemos manter nessa categoria e ficaria sem scientificName
                                                                             "foliaceous algae",
                                                                             "foliose",
                                                                             "frondose algae", 
                                                                             "unknown foliose"))] <- "foliose algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "calcareous turf")] <- "corallinales" # "calcareous articulate algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "corticated")] <- "corticated algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "unknown corticated")] <- "corticated algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "sargassum sp")] <- "sargassum" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "jania sp")] <- "jania" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "amphiroa sp")] <- "amphiroa" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "schizoporella sp")] <- "schizoporella" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "siderastrea spp")] <- "siderastrea" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "mussismilia spp")] <- "mussismilia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "millepora sp")] <- "millepora" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "laurencia sp")] <- "laurencia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "digenia sp")] <- "digenia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "digenia sp ")] <- "digenia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "galaxaura sp")] <- "galaxaura" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "agaricia sp")] <- "agaricia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "codium spp")] <- "codium" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "porites sp")] <- "porites" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "padina sp")] <- "padina" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "leptogorgia sp")] <- "leptogorgia" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "chaetomorpha sp")] <- "chaetomorpha" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "caulerpa sp")] <- "caulerpa" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "dictyota sp")] <- "dictyota" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "parazoanthus cf axinellae")] <- "parazoanthus" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "siderastrea sp")] <- "siderastrea" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "pseudosuberites sp ")] <- "pseudosuberites" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "ulva sp")] <- "ulva" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "dictyopteris sp")] <- "dictyopteris" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "millepora brasiliensis")] <- "millepora braziliensis" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "aiolochoria crassa")] <- "aiolochroia crassa" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "zooanthus sociatus")] <- "zoanthus sociatus" # leathery algae"
occ_Ross_et_al_benthos$taxonOrGroup[which(occ_Ross_et_al_benthos$taxonOrGroup == "amphimedon aff  compressa")] <- "amphimedon" # leathery algae"


# check unique taxa
unique(occ_Ross_et_al_benthos$taxonOrGroup)[order(unique(occ_Ross_et_al_benthos$taxonOrGroup))]

# remove plot data
occ_Ross_et_al_benthos <-occ_Ross_et_al_benthos[which(occ_Ross_et_al_benthos$taxonOrGroup %in% c("tape","sand","shadow","rock", "rodolith", 
                                                                                                   "unknown" ,"unknown algae") != T),]

# matching with worms
worms_record <- lapply (unique(occ_Ross_et_al_benthos$taxonOrGroup), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# melt
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
# sp
occ_Ross_et_al_benthos$scientificNameAccepted<-(df_worms_record$scientificname [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower (df_worms_record$scientificname))])
# aphiaID
occ_Ross_et_al_benthos$scientificNameID<-(df_worms_record$lsid [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower (df_worms_record$scientificname))])
# taxon rank of the identified level
occ_Ross_et_al_benthos$taxonRank <- (df_worms_record$rank [match (occ_Ross_et_al_benthos$taxonOrGroup,
                                                                 tolower (df_worms_record$scientificname))])
# kingdom
occ_Ross_et_al_benthos$kingdom<-(df_worms_record$kingdom [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])

# phylum
occ_Ross_et_al_benthos$phylum<-(df_worms_record$phylum [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])

# class
occ_Ross_et_al_benthos$class<-(df_worms_record$class [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])

# order
occ_Ross_et_al_benthos$order <- (df_worms_record$order [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])

# family
occ_Ross_et_al_benthos$family<-(df_worms_record$family [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])

# genus
occ_Ross_et_al_benthos$genus <-(df_worms_record$genus [match (occ_Ross_et_al_benthos$taxonOrGroup,tolower(df_worms_record$scientificname))])


unique(occ_Ross_et_al_benthos[is.na(occ_Ross_et_al_benthos$scientificName),"taxonOrGroup"])

# update
occ_Ross_et_al_benthos$scientificNameAccepted[grep ("Montastrea cavernosa", occ_Ross_et_al_benthos$scientificNameAccepted)] <- "Montastraea cavernosa"
occ_Ross_et_al_benthos$genus[grep ("Montastraea cavernosa", occ_Ross_et_al_benthos$scientificNameAccepted)] <- "Montastraea"


# ------------------------------------------------------------------------
# ADJUSTING DATES





# create events (based on fish data)
occ_Ross_et_al_benthos <- cbind (occ_Ross_et_al_benthos, 
        occ_Ross_et_al [match (occ_Ross_et_al_benthos$sitecode, occ_Ross_et_al$sitecode), 
                        c("year","month", "day"),])

# imputing NAs
row_to_start <- min(as.numeric(rownames(occ_Ross_et_al_benthos[which(occ_Ross_et_al_benthos$year == "2017"),])))
occ_Ross_et_al_benthos[1:row_to_start-1,"year"] <- "2016"
occ_Ross_et_al_benthos[row_to_start: nrow(occ_Ross_et_al_benthos),"year"] <- "2017"
# adjust year
occ_Ross_et_al_benthos$year <- as.numeric (occ_Ross_et_al_benthos$year)

# table((occ_Ross_et_al_benthos$year))

# as date
occ_Ross_et_al_benthos$eventDate <- as.Date (paste(occ_Ross_et_al_benthos$year,
                                                   occ_Ross_et_al_benthos$month,
                                                   occ_Ross_et_al_benthos$day,
                                           sep="-"))




# ------------------------------------------------------------------------
# ADJUSTING SITES, DEPTHS



# event ID
# region (unique is NE)
occ_Ross_et_al_benthos$region <- "ne_reefs"

# locality
# according to Aued & Morais
occ_Ross_et_al_benthos$site <- occ_Ross_et_al_benthos$local

# adjust natal (NA)
occ_Ross_et_al_benthos$site[is.na(occ_Ross_et_al_benthos$site)] <- "NA"
# adjust other
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "PI")] <- "pirangi"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "RF")] <- "parrachos_de_rio_do_fogo"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "BF")] <- "baiaformosa"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "DL")] <- "diogolopes"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "CN")] <- "caicaradonorte"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "PM")] <- "portodomangue"
occ_Ross_et_al_benthos$site [which(occ_Ross_et_al_benthos$site == "NA")] <- "natal"


# verbatimLocality
occ_Ross_et_al_benthos$verbatimLocality <- occ_Ross_et_al_benthos$sitecode
# reef (locality)
occ_Ross_et_al_benthos$locality <- occ_Ross_et_al_benthos$sitecode
unique(occ_Ross_et_al_benthos$locality)[order(unique(occ_Ross_et_al_benthos$locality))]

# event depth
occ_Ross_et_al_benthos$minimumDepthInMeters <- occ_Ross_et_al_benthos$depth
occ_Ross_et_al_benthos$maximumDepthInMeters <- occ_Ross_et_al_benthos$minimumDepthInMeters

# geography
occ_Ross_et_al_benthos$higherGeography <- "BrazilianCoast"

## fazer occID para este estudo
occ_Ross_et_al_benthos$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_benthos_monitoring:", 
           occ_Ross_et_al_benthos$higherGeography,
           sep=""),
    occ_Ross_et_al_benthos$site,sep=":"),
  occ_Ross_et_al_benthos$locality,
  occ_Ross_et_al_benthos$year,
          occ_Ross_et_al_benthos$transect,
        paste ("occ",seq(1,nrow(occ_Ross_et_al_benthos)),sep=""),
  sep="_")

## fazer eventID para este estudo
occ_Ross_et_al_benthos$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_benthos_monitoring:", 
           occ_Ross_et_al_benthos$higherGeography,
           sep=""),
    occ_Ross_et_al_benthos$site,sep=":"),
  occ_Ross_et_al_benthos$locality,
  occ_Ross_et_al_benthos$year,
        occ_Ross_et_al_benthos$transect,
  sep="_")


## fazer parentID para este estudo
occ_Ross_et_al_benthos$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RioGrandeDoNorte_benthos_monitoring:", 
           occ_Ross_et_al_benthos$higherGeography,
           sep=""),
    occ_Ross_et_al_benthos$site,sep=":"),
  occ_Ross_et_al_benthos$locality,
  occ_Ross_et_al_benthos$year,
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
# method
occ_Ross_et_al_benthos$samplingProtocol <- "Photoquadrats - 0.25 x 0.25 m"
# effort
## check roos et al. 2020/2019
occ_Ross_et_al_benthos$samplingEffort <- 10# number of photos across the 20 m transect
# sampleSizeValue
occ_Ross_et_al_benthos$sampleSizeValue <- 0.25*0.25 # squared meters
# sampleSizeUnit
occ_Ross_et_al_benthos$sampleSizeUnit <- "squared meters"
# recordedBy
occ_Ross_et_al_benthos$recordedBy <- "Natalia Roos"
# cover
occ_Ross_et_al_benthos$measurementValue <- occ_Ross_et_al_benthos$`Cov%.per.species`
# measurementType
occ_Ross_et_al_benthos$measurementType <- "Percentage cover"
# measurementUnit
occ_Ross_et_al_benthos$measurementUnit <- "dimensionless"

# licence
occ_Ross_et_al_benthos$licence <- "CC BY-NC"
# language
occ_Ross_et_al_benthos$language <- "en"
# citation
occ_Ross_et_al_benthos$bibliographicCitation <- "Roos NC, Pennino MG, Carvalho AR, Longo GO (2019) Drivers of abundance and biomass of Brazilian parrotfishes. Mar Ecol Prog Ser 623:117-130. https://doi.org/10.3354/meps13005"



# eventRemarks
occ_Ross_et_al_benthos$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curators Andre Luza and Cesar Cordeiro to have the complete dataset with verbatimIdentification"

# remove these MAGs
occ_Ross_et_al_benthos$verbatimIdentification[ is.na(occ_Ross_et_al_benthos$scientificNameAccepted)]
occ_Ross_et_al_benthos <- occ_Ross_et_al_benthos [which(is.na(occ_Ross_et_al_benthos$scientificNameAccepted) !=T),]






# --------------------------------------------------------------------
# Formatted according to DwC





DF_eMOF <- occ_Ross_et_al_benthos [,c("eventID", 
                                      "occurrenceID",
                                      "measurementValue",
                                      "measurementType",
                                      "measurementUnit",
                                      "eventRemarks")]



DF_occ <- occ_Ross_et_al_benthos [,c("eventID", 
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
                                     "language",
                                     "bibliographicCitation")]

# aggregate data by eventIDs to have event_core
# do the lines have the same information? (check this by calculating the sd of depth)

event_core <- data.frame (group_by(occ_Ross_et_al_benthos, eventID,higherGeography,site,verbatimLocality, locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
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
# save
write.csv(DF_occ, file =here("DwC_output",
                               "GLongo_NRoss_spatialData",
                               "DF_occ_benthos.csv"))
write.csv(DF_eMOF, file =here("DwC_output",
                                "GLongo_NRoss_spatialData",
                                "DF_eMOF_benthos.csv"))
write.csv(event_core, file =here("DwC_output",
                                   "GLongo_NRoss_spatialData",
                                   "event_core_benthos.csv"))

# end
rm(list=ls())
