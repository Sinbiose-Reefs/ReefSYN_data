
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
                                   "eventDate" , "taxonOrGroup","measurementValue")






# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES






benthos_long_format$verbatimIdentification <- benthos_long_format$taxonOrGroup


# correcting scientificName (names based on Aued et al. 2019)
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "ACA")]  <- "calcareous articulate algae"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "AFI")]  <- "filamentous algae"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "AFRO")]  <- "frondose algae"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "CV")]  <- "scleractinia"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "SPJA")]  <- "porifera"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "MI")]  <- "millepora"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "OCTO")]  <- "octocorallia" # "alcyonaria" nao eh aceito
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "OÇO")]  <- "echinoidea"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "ZO")]  <- "zoantharia"
benthos_long_format$taxonOrGroup[which(benthos_long_format$taxonOrGroup == "CV+MI")]  <- "scleractinia"





# adjusting spp names
# matching with worms

worms_record <- lapply (unique(benthos_long_format$taxonOrGroup), function (i) 
  
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
# valid name worms
benthos_long_format$scientificNameAccepted<-(df_worms_record$scientificname [match (benthos_long_format$taxonOrGroup, 
                                                                            tolower(df_worms_record$scientificname))])


# rank
benthos_long_format$taxonRank<-(df_worms_record$rank [match (benthos_long_format$taxonOrGroup,
                                                            tolower(df_worms_record$scientificname))])

# ID
benthos_long_format$scientificNameID<-(df_worms_record$lsid [match (benthos_long_format$taxonOrGroup,
                                                                    tolower(df_worms_record$scientificname))])

# kingdom
benthos_long_format$kingdom <-(df_worms_record$kingdom [match (benthos_long_format$taxonOrGroup,
                                                               tolower (df_worms_record$scientificname))])
# phylum
benthos_long_format$phylum <-(df_worms_record$phylum [match (benthos_long_format$taxonOrGroup,
                                                               tolower (df_worms_record$scientificname))])

# class
benthos_long_format$class <-(df_worms_record$class [match (benthos_long_format$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])
# order
benthos_long_format$order <-(df_worms_record$order [match (benthos_long_format$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])

# family
benthos_long_format$family <-(df_worms_record$family [match (benthos_long_format$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])

# genus
benthos_long_format$genus <-(df_worms_record$genus [match (benthos_long_format$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])




# ----------------------------------------------------------------------------------------------------------------------------------------------------------
# CREATING IDS,
# DWC FORMAT


# adjusting habitat (based on Francini-Filho et al. 2013)
benthos_long_format$habitat[which(benthos_long_format$habitat == "TP")] <- "pinnacles_top"
benthos_long_format$habitat[which(benthos_long_format$habitat == "PA")] <- "pinnacles_wall"

# geographic location
benthos_long_format$higherGeography <- "BrazilianCoast"


# creating parentEventids
benthos_long_format$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format$higherGeography,
           sep=""),
    benthos_long_format$locality,sep=":"),
  benthos_long_format$site,
  benthos_long_format$eventDate,
  sep = "_")



# creating eventids
benthos_long_format$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format$higherGeography,
           sep=""),
    benthos_long_format$locality,sep=":"),
  benthos_long_format$site,
  benthos_long_format$eventDate,
  
  sep="_")




# occurrenceID
benthos_long_format$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format$higherGeography,
           sep=""),
    benthos_long_format$locality,sep=":"),
  benthos_long_format$site,
  benthos_long_format$eventDate,
  paste ("occ",seq(1,nrow(benthos_long_format)),sep=""),
  sep="_")



# year
benthos_long_format$year<- benthos_long_format$eventDate

# method
benthos_long_format$samplingProtocol <- "Point-intercept  lines - 10m"

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

# site and locality
benthos_long_format$verbatimSite <- benthos_long_format$site # point number
benthos_long_format$verbatimLocality <- benthos_long_format$locality

# chance names (site and locality are not correct -- the converse)
benthos_long_format$site <- benthos_long_format$verbatimLocality
benthos_long_format$locality <- benthos_long_format$verbatimSite



# depth
# set min and max
colnames(benthos_long_format)[which(colnames(benthos_long_format) == "depthInMeters")] <- "minimumDepthinMeters"
benthos_long_format$maximumDepthinMeters <- benthos_long_format$minimumDepthinMeters



# licence
benthos_long_format$licence <- "CC BY-NC"

# language
benthos_long_format$language <- "en"

# citation
benthos_long_format$bibliographicCitation <- "Francini-Filho RB, Coni EOC, Meirelles PM, Amado-Filho GM, Thompson FL, et al. (2013) Dynamics of Coral Reef Benthic Assemblages of the Abrolhos
Bank, Eastern Brazil: Inferences on Natural and Anthropogenic Drivers. PLoS ONE 8(1): e54260. doi:10.1371/journal.pone.0054260"


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

# adjusting coordinates (based on information provided by RF-Filho)
match_sites_coords[match_sites_coords$SITE == "AMP3",c("LAT", "LONG")] <- c(-16.914, -39.030)
match_sites_coords[match_sites_coords$SITE == "A3",c("LAT", "LONG")] <- c(-16.903, -39.031)
match_sites_coords[match_sites_coords$SITE == "PA1",c("LAT", "LONG")] <- c(-17.699161805482,-38.942426147792)#
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
benthos_long_format$site <- tolower (benthos_long_format$site)
benthos_long_format$locality <- tolower (benthos_long_format$locality)




# eventRemarks
benthos_long_format$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curators Andre Luza and Cesar Cordeiro to have the complete dataset with verbatimIdentification"

# remove these MAGs
benthos_long_format <- benthos_long_format [which(is.na(benthos_long_format$scientificNameAccepted) !=T),]


# ------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF <- benthos_long_format [,c("eventID", 
                                   "occurrenceID",
                                   "measurementValue", 
                                   "measurementType",
                                   "measurementUnit",
                                   "eventRemarks")]

DF_occ <- benthos_long_format [,c("eventID", "occurrenceID","basisOfRecord",
                                  "verbatimIdentification",
                                  "scientificNameAccepted",
                                  "taxonRank",
                                  "scientificNameID",
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
# do the lines have the same information? (check this by calculating the sd of depth)
# sd(fish_long_format[which(fish_long_format$eventID == unique_eventIDs[100]),"depthInMeters"])
event_core <- data.frame (group_by(benthos_long_format, eventID,higherGeography,site,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = unique(samplingEffort),
                                      sampleSizeValue = unique(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
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
colnames(benthos_long_format_2006) <- c("site","locality","habitat","region",            
                                        "eventDate" , "quadrat","quadratID",
                                        "taxonOrGroup","measurementValue")





# ------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES





benthos_long_format_2006$verbatimIdentification <- benthos_long_format_2006$taxonOrGroup

# correcting scientific name  (names based on Aued et al. 2019)
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "CYANO")]  <- "cyanobacteria"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "CCA")]  <- "crustose coralline algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "CAA")]  <- "calcareous articulate algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "TURF")]  <- "filamentous algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "fire-coral")]  <- "millepora alcicornis"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "CYANO.+.TURF")]  <- "cianobacterias calcareous turf"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "HALIMEDA")]  <- "halimeda"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "FLESHY.ALGAE")]  <- "foliose algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "OTHER.ORGANISMS")]  <- "other organisms"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "SPONGE")]  <- "porifera"


# adjusting spp names

benthos_long_format_2006$taxonOrGroup <-  (gsub("\\."," ",benthos_long_format_2006$taxonOrGroup)) # replace dot by space
benthos_long_format_2006$taxonOrGroup <-(iconv(benthos_long_format_2006$taxonOrGroup, "ASCII", "UTF-8", sub="")) # encoding
benthos_long_format_2006$taxonOrGroup <- tolower(benthos_long_format_2006$taxonOrGroup) # lower case




# adjust based on knowledge of Cesar Cordeiro

benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "ventricaria ventricosa")] <- "valonia ventricosa"

# broader groups
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "anemona")] <- "actiniaria"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "leathery")] <- "leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "spirobidae - polycchaete")] <- "spirorbinae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "briozoa")] <- "bryozoa"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "bryozoan")] <- "bryozoa"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "hidrozoan")] <- "hydrozoa"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "outro hydrozoa")] <- "hydrozoa"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "poliqueta")] <- "polychaeta"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "polichaeta")] <- "polychaeta"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup %in% c("ascidea colonial" ,                  
                                                                             "ascidian",
                                                                             "outra ascidia"))] <- "ascidiacea"

# octocoral and anthozoa
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "outro anthozoa")] <- "anthozoa"
benthos_long_format_2006$taxonOrGroup[grep("octocoral",benthos_long_format_2006$taxonOrGroup)] <- "octocorallia" # "alcyonaria" nao eh aceito
# sponge
benthos_long_format_2006$taxonOrGroup[grep("sponge",benthos_long_format_2006$taxonOrGroup)] <- "porifera"
# echinoderms
benthos_long_format_2006$taxonOrGroup[grep("ourigo",benthos_long_format_2006$taxonOrGroup)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
benthos_long_format_2006$taxonOrGroup[grep("sea urchin",benthos_long_format_2006$taxonOrGroup)] <- "echinoidea"
benthos_long_format_2006$taxonOrGroup[grep("outro echinoderma",benthos_long_format_2006$taxonOrGroup)] <- "echinodermata"
benthos_long_format_2006$taxonOrGroup[grep("crinside",benthos_long_format_2006$taxonOrGroup)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
benthos_long_format_2006$taxonOrGroup[grep("estrela",benthos_long_format_2006$taxonOrGroup)] <- "asteroidea"


### melhor nao indicar grupo morfo-anatomico (MAG) como taxonOrGroup. Esse MAG nao tem compativel pra inseir no DwC/OBIS
### Vou indicar o nivel taxonomico compativel com o que tiver e o resto deixamos como estava 
### Os grupos morfo-anatomicos podem ser adicionados com merge de tabela referencia depois

# cca and caa 
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "crostose coralline algae")] <- "corallinales" # "crustose coralline algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "cianobacterias")] <- "cyanobacteria"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup %in% c("amphiroideae", # "amphiroideae"
                                                                             "jania amphiroa", # "amphiroideae"
                                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "filamentous")] <- "filamentous algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "green filamentous algae")] <- "chlorophyta" # "filamentous algae"

# algae
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup %in% c("fleshy algae", # usando o taxaOrGroup podemos manter nessa categoria e ficaria sem scientificName
                                                                             "foliaceous algae",
                                                                             "foliose",
                                                                             "frondose algae", 
                                                                             "unknown foliose"))] <- "foliose algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "calcareous turf")] <- "corallinales" # "calcareous articulate algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "corticated")] <- "corticated algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "unknown corticated")] <- "corticated algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "sargassum sp")] <- "sargassum" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "jania sp")] <- "jania" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "amphiroa sp")] <- "amphiroa" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "schizoporella sp")] <- "schizoporella" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "siderastrea spp")] <- "siderastrea" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "siderastrea spp ")] <- "siderastrea" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "mussismilia spp")] <- "mussismilia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "millepora sp")] <- "millepora" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "laurencia sp")] <- "laurencia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "digenia sp")] <- "digenia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "digenia sp ")] <- "digenia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "galaxaura sp")] <- "galaxaura" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "agaricia sp")] <- "agaricia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "codium spp")] <- "codium" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "porites sp")] <- "porites" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "padina sp")] <- "padina" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "leptogorgia sp")] <- "leptogorgia" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "chaetomorpha sp")] <- "chaetomorpha" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "caulerpa sp")] <- "caulerpa" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "dictyota sp")] <- "dictyota" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "parazoanthus cf axinellae")] <- "parazoanthus" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "zoanthus sp ")] <- "zoanthus" # leathery algae"
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "palythoa sp ")] <- "palythoa" # leathery algae"

# neospogondes
benthos_long_format_2006$taxonOrGroup[grep("neospongodes", benthos_long_format_2006$taxonOrGroup)] <- "neospongodes atlantica" # leathery algae"


# the group "cianobacterias calcareous turf" from Francini et al. (time series) divided into 2 groups
cianobacterias_calcareous_turf <- benthos_long_format_2006[which(benthos_long_format_2006$taxonOrGroup == "cianobacterias calcareous turf"),]
cianobacterias_calcareous_turf$taxonOrGroup[which(cianobacterias_calcareous_turf$taxonOrGroup == "cianobacterias calcareous turf")] <- "cyanobacteria" # transform the other
benthos_long_format_2006$taxonOrGroup[which(benthos_long_format_2006$taxonOrGroup == "cianobacterias calcareous turf")] <- "filamentous algae"
benthos_long_format_2006<-rbind (benthos_long_format_2006, cianobacterias_calcareous_turf) # bind one new group




unique(benthos_long_format_2006$taxonOrGroup)[order(unique(benthos_long_format_2006$taxonOrGroup))]




# remove plot data
benthos_long_format_2006<-benthos_long_format_2006 [which(benthos_long_format_2006$taxonOrGroup %in% 
                                                            c("sand","rock") != T),]



# matching with worms
worms_record <- lapply (unique(benthos_long_format_2006$taxonOrGroup), function (i) 
  
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
benthos_long_format_2006$scientificNameAccepted<-(df_worms_record$scientificname [match (benthos_long_format_2006$taxonOrGroup, 
                                                                                     tolower(df_worms_record$scientificname))])

# rank
benthos_long_format_2006$taxonRank<-(df_worms_record$rank [match (benthos_long_format_2006$taxonOrGroup, 
                                                                         tolower(df_worms_record$scientificname))])


# ID
benthos_long_format_2006$scientificNameID<-(df_worms_record$lsid [match (benthos_long_format_2006$taxonOrGroup, 
                                                                         tolower(df_worms_record$scientificname))])

# kingdom
benthos_long_format_2006$kingdom <-(df_worms_record$kingdom [match (benthos_long_format_2006$taxonOrGroup, 
                                                                    tolower(df_worms_record$scientificname))])

# phylum
benthos_long_format_2006$phylum <-(df_worms_record$phylum [match (benthos_long_format_2006$taxonOrGroup, 
                                                                    tolower(df_worms_record$scientificname))])

# class
benthos_long_format_2006$class <-(df_worms_record$class [match (benthos_long_format_2006$taxonOrGroup, 
                                                                tolower(df_worms_record$scientificname))])
# order
benthos_long_format_2006$order <-(df_worms_record$order [match (benthos_long_format_2006$taxonOrGroup, 
                                                                tolower(df_worms_record$scientificname))])

# family
benthos_long_format_2006$family <-(df_worms_record$family [match (benthos_long_format_2006$taxonOrGroup, 
                                                                  tolower(df_worms_record$scientificname))])

# genus
benthos_long_format_2006$genus <-(df_worms_record$genus [match (benthos_long_format_2006$taxonOrGroup, 
                                                                  tolower(df_worms_record$scientificname))])




# ------------------------------------------------------------------------
# creating eventID



# geographic location
benthos_long_format_2006$higherGeography <- "BrazilianCoast"




# creating parentEventids
benthos_long_format_2006$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format_2006$higherGeography,
           sep=""),
    benthos_long_format_2006$site,sep=":"),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  sep="_")



# creating eventids
benthos_long_format_2006$eventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format_2006$higherGeography,
           sep=""),
    benthos_long_format_2006$site,sep=":"),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  benthos_long_format_2006$quadrat,
  benthos_long_format_2006$quadratID,
  sep="_")



# occurrenceID
benthos_long_format_2006$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:RonaldoFranciniFilho-AbrolhosBank:", 
           benthos_long_format_2006$higherGeography,
           sep=""),
    benthos_long_format_2006$site,sep=":"),
  benthos_long_format_2006$locality,
  benthos_long_format_2006$eventDate,
  benthos_long_format_2006$quadrat,
  benthos_long_format_2006$quadratID,
  paste ("occ",seq(1,nrow(benthos_long_format_2006)),sep=""),
  sep="_")




# year
benthos_long_format_2006$year<-benthos_long_format_2006$eventDate

# method
benthos_long_format_2006$samplingProtocol <- "Photoquadrats - 1 x 1m" # Fixed photo-quadrats

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
benthos_long_format_2006$site<- tolower (benthos_long_format_2006$site)
benthos_long_format_2006$locality<- tolower (benthos_long_format_2006$locality)



# licence
benthos_long_format_2006$licence <- "CC BY-NC"
# language
benthos_long_format_2006$language <- "en"

# citation
benthos_long_format_2006$bibliographicCitation <- "Francini-Filho RB, Coni EOC, Meirelles PM, Amado-Filho GM, Thompson FL, et al. (2013) Dynamics of Coral Reef Benthic Assemblages of the Abrolhos
Bank, Eastern Brazil: Inferences on Natural and Anthropogenic Drivers. PLoS ONE 8(1): e54260. doi:10.1371/journal.pone.0054260"


# eventRemarks
benthos_long_format_2006$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curators Andre Luza and Cesar Cordeiro to have the complete dataset with verbatimIdentification"

# remove these MAGs
benthos_long_format_2006 <- benthos_long_format_2006 [which(is.na(benthos_long_format_2006$scientificNameAccepted) !=T),]

# ------------------------------------------------------------------------
# Formatted according to DwC







DF_eMOF_2006 <- benthos_long_format_2006 [,c("eventID", 
                                             "occurrenceID",
                                             "measurementValue", 
                                             "measurementType",
                                             "measurementUnit",
                                             "eventRemarks")]

DF_occ_2006 <- benthos_long_format_2006 [,c("eventID", 
                                            "occurrenceID",
                                            "basisOfRecord",
                                            "verbatimIdentification",
                                            "scientificNameAccepted",
                                            "taxonRank",
                                            "scientificNameID",
                                            "kingdom",
                                            "phylum",
                                            "class",
                                            "order",
                                            "family",
                                            "genus",
                                            "recordedBy", "organismQuantityType", "occurrenceStatus",
                                            "licence",
                                            "language")]


# aggregate data by eventIDs to have event_core
event_core_2006 <- data.frame (group_by(benthos_long_format_2006, eventID,higherGeography,site,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = unique(samplingEffort),
                                      sampleSizeValue =unique(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
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


write.csv(DF_occ, file =here("DwC_output",
                               "RFrancini_timeSeries_abrolhos",
                               "DF_occ_benthos.csv"))
write.csv(DF_eMOF, file =here("DwC_output",
                                "RFrancini_timeSeries_abrolhos",
                                "DF_eMOF_benthos.csv"))

write.csv(event_core, file =here("DwC_output",
                                   "RFrancini_timeSeries_abrolhos",
                                   "event_core_benthos.csv"))

# end
rm(list=ls())
