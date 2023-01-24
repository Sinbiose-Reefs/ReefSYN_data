
source("R/functions.R") # function for coordinate transformation


require(here); require(openxlsx); require(dplyr); require(reshape); require(worrms)



# ------------------------------------------------------
# ORGANIZE THE BENTIC DATASET (PELD ILOC)



# ----------------------------------- #
# benthic time series (Cordeiro C) in the oceanic islands
# ----------------------------------- #



# occurrence data
benthos_DF_eMOF <- read.csv(here ("Data",
                                  "PELD_iloc_benthos",
                               "DF_eMOF.txt"),sep=",",encoding= "UTF-8")



# event core
benthos_event_core <-  read.csv(here ("Data", 
                                      "PELD_iloc_benthos",
                                   "event_core.txt"),sep=",", encoding= "UTF-8")




# identify these sites are in the islands
benthos_event_core$higherGeography <- "BrazilianOceanicIslands"
# tartarugas in Trindade & Rocas
benthos_event_core[grep ("Trindade_Tartarugas",benthos_event_core$eventID),"locality"] <- "tartarugas_trindade"
benthos_event_core$locality<-gsub ("praia_do_", "",benthos_event_core$locality)



# ADJUSTING SITE NAMES
benthos_event_core$locality<-tolower(iconv(benthos_event_core$locality, "UTF-8", "ASCII//TRANSLIT", sub=""))
benthos_event_core$locality<-gsub (" ","_",benthos_event_core$locality)
# locations
benthos_event_core$island<-tolower(iconv(benthos_event_core$island, "UTF-8", "ASCII//TRANSLIT", sub=""))
benthos_event_core$island<-gsub (" ","_",benthos_event_core$island)

# create year
benthos_event_core$year<-substr(benthos_event_core$eventDate,1,4) # only year


# load occ id table
benthos_DF_occ2 <- read.csv(here ("Data", 
                                  "PELD_iloc_benthos",
                                  "DF_occ2.txt"),sep=",",encoding= "UTF-8")



# --------------------------
# ADJUSTING SCIENTIFIC NAMES

# adjusting spp names
benthos_DF_occ2$taxonOrGroup <-  (gsub("\\."," ",benthos_DF_occ2$scientificName))
benthos_DF_occ2$taxonOrGroup <-(iconv(benthos_DF_occ2$taxonOrGroup, "UTF-8", "ASCII//TRANSLIT", sub=""))
benthos_DF_occ2$taxonOrGroup <- tolower(benthos_DF_occ2$taxonOrGroup)

# adjust based on knowledge of Cesar Cordeiro
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "aiolochoria crassa")] <- "aiolochroia crassa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "meandrina braziliensis")] <- "meandrina brasiliensis"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "millepora")] <- "millepora sp"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "montastrea cavernosa")] <- "montastraea cavernosa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "mussismilia")] <- "mussismilia spp"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "neospongodes atlbntica")] <- "neospongodes atlantica"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "siderastrea spp ")] <- "siderastrea spp"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "siderastrea")] <- "siderastrea spp"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "siderastrea sp")] <- "siderastrea spp"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "ventricaria ventricosa")] <- "valonia ventricosa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "zooanthus sociatus")] <- "zoanthus sociatus"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "zoanthid")] <- "zoantharia"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "zoanthus sp ")] <- "zoantharia"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "palythoa")] <- "palythoa sp "
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "padina")] <- "padina sp"

# broader groups
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "leathery")] <- "leathery algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "spirobidae - polycchaete")] <- "spirorbidae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "briozoa")] <- "bryozoa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "bryozoan")] <- "bryozoa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "hidrozoan")] <- "hydrozoa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "outro hydrozoa")] <- "hydrozoa"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "poliqueta")] <- "polychaeta"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "polichaeta")] <- "polychaeta"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"
# octocoral and anthozoa
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "outro anthozoa")] <- "anthozoa"
benthos_DF_occ2$taxonOrGroup[grep("octocoral",benthos_DF_occ2$taxonOrGroup)] <- "alcyonaria"
# sponge
benthos_DF_occ2$taxonOrGroup[grep("sponge",benthos_DF_occ2$taxonOrGroup)] <- "porifera"
# echinorms
benthos_DF_occ2$taxonOrGroup[grep("ourigo",benthos_DF_occ2$taxonOrGroup)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
benthos_DF_occ2$taxonOrGroup[grep("sea urchin",benthos_DF_occ2$taxonOrGroup)] <- "echinoidea"
benthos_DF_occ2$taxonOrGroup[grep("outro echinoderma",benthos_DF_occ2$taxonOrGroup)] <- "echinodermata"
benthos_DF_occ2$taxonOrGroup[grep("crinside",benthos_DF_occ2$taxonOrGroup)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
benthos_DF_occ2$taxonOrGroup[grep("estrela",benthos_DF_occ2$taxonOrGroup)] <- "asteroidea"

# cca and caa
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "crostose coralline algae")] <- "crustose coralline algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "cianobacterias")] <- "cyanobacteria"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup %in% c("amphiroa", 
                                                             "amphiroa sp", 
                                                             "amphiroideae", 
                                                             "jania amphiroa", 
                                                             "jania sp",
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "filamentous")] <- "filamentous algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "turf")] <- "filamentous algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "green filamentous algae")] <- "filamentous algae"
# algae
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup %in% c("fleshy algae",
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "calcareous turf")] <- "calcareous articulate algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "corticated")] <- "corticated algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "unknown corticated")] <- "corticated algae"
benthos_DF_occ2$taxonOrGroup[which(benthos_DF_occ2$taxonOrGroup == "sargassum sp")] <- "leathery algae"

# check unique taxa
unique(benthos_DF_occ2$taxonOrGroup)[order(unique(benthos_DF_occ2$taxonOrGroup))]

# remove components of sampling
# remove plot data
#benthos_DF_occ2<-benthos_DF_occ2 [which(benthos_DF_occ2$taxonOrGroup %in% 
#                                                            c("unknown","sand gravel") != T),]


benthos_DF_occ2<-benthos_DF_occ2 [which(benthos_DF_occ2$taxonOrGroup %in% c("sand gravel", "unknown" ) != T),]


# match with worms
worms_record <- lapply (unique(benthos_DF_occ2$taxonOrGroup), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))


# valid name WoRMS 
benthos_DF_occ2$scientificName <- (df_worms_record$scientificname [match (benthos_DF_occ2$taxonOrGroup,
                                                                             tolower (df_worms_record$scientificname))])

# valid name WoRMS 
benthos_DF_occ2$scientificNameAccepted <- (df_worms_record$scientificname [match (benthos_DF_occ2$taxonOrGroup,
                                                                                     tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
benthos_DF_occ2$taxonRank <- (df_worms_record$rank [match (benthos_DF_occ2$taxonOrGroup,
                                                              tolower (df_worms_record$scientificname))])


# match & bind
# or taxonID
benthos_DF_occ2$scientificNameID<-(df_worms_record$lsid [match (benthos_DF_occ2$taxonOrGroup,
                                                                   tolower (df_worms_record$scientificname))])
# kingdom
benthos_DF_occ2$kingdom<-(df_worms_record$kingdom [match (benthos_DF_occ2$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])

# phylum
benthos_DF_occ2$phylum <-(df_worms_record$phylum [match (benthos_DF_occ2$taxonOrGroup,
                                                            tolower (df_worms_record$scientificname))])

# class
benthos_DF_occ2$class<-(df_worms_record$class [match (benthos_DF_occ2$taxonOrGroup,
                                                         tolower (df_worms_record$scientificname))])

# order
benthos_DF_occ2$order <-(df_worms_record$order [match (benthos_DF_occ2$taxonOrGroup,
                                                          tolower (df_worms_record$scientificname))])

# family
benthos_DF_occ2$family<-(df_worms_record$family [match (benthos_DF_occ2$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])

# genus
benthos_DF_occ2$genus <-(df_worms_record$genus [match (benthos_DF_occ2$taxonOrGroup,
                                                        tolower (df_worms_record$scientificname))])



# remove these MAGs
benthos_DF_occ2 <- benthos_DF_occ2 [which(is.na(benthos_DF_occ2$scientificNameAccepted) !=T),]
# remove taxon or group
benthos_DF_occ2<- benthos_DF_occ2[,-which(colnames(benthos_DF_occ2) == "taxonOrGroup")]


# eventRemarks
benthos_DF_eMOF$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curator Cesar Cordeiro to have the complete dataset with verbatimIdentification"




# save
write.table(benthos_DF_occ2, file =here("DwC_output",
                               "PELD_iloc_benthos",
                               "DF_occ.txt"),sep=",",
            quote = FALSE,
            row.names = F)
write.table(benthos_DF_eMOF, file =here("DwC_output",
                                "PELD_iloc_benthos",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE,
            row.names = F)

write.table(benthos_event_core, file =here("DwC_output",
                                   "PELD_iloc_benthos",
                                   "event_core.txt"),sep=",",
            quote = FALSE,
            row.names = F)

# end
rm(list=ls())
