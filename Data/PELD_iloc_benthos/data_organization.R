
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
benthos_event_core$higherGeographyID <- "BrazilianIslands"
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
benthos_event_core$eventYear<-substr(benthos_event_core$eventDate,1,4) # only year


# load occ id table
benthos_DF_occ2 <- read.csv(here ("Data", 
                                  "PELD_iloc_benthos",
                                  "DF_occ2.txt"),sep=",",encoding= "UTF-8")



# --------------------------
# ADJUSTING SCIENTIFIC NAMES

# adjusting spp names
benthos_DF_occ2$scientificName <-  (gsub("\\."," ",benthos_DF_occ2$scientificName))
benthos_DF_occ2$scientificName <-(iconv(benthos_DF_occ2$scientificName, "UTF-8", "ASCII//TRANSLIT", sub=""))
benthos_DF_occ2$scientificName <- tolower(benthos_DF_occ2$scientificName)

# adjust based on knowledge of Cesar Cordeiro
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "aiolochoria crassa")] <- "aiolochroia crassa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "meandrina braziliensis")] <- "meandrina brasiliensis"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "millepora")] <- "millepora sp"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "montastrea cavernosa")] <- "montastraea cavernosa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "mussismilia")] <- "mussismilia spp"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "neospongodes atlbntica")] <- "neospongodes atlantica"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "siderastrea spp ")] <- "siderastrea spp"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "siderastrea")] <- "siderastrea spp"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "siderastrea sp")] <- "siderastrea spp"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "ventricaria ventricosa")] <- "valonia ventricosa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "zooanthus sociatus")] <- "zoanthus sociatus"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "zoanthid")] <- "zoantharia"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "zoanthus sp ")] <- "zoantharia"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "palythoa")] <- "palythoa sp "
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "padina")] <- "padina sp"

# broader groups
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "leathery")] <- "leathery algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "spirobidae - polycchaete")] <- "spirorbidae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "briozoa")] <- "bryozoa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "bryozoan")] <- "bryozoa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "hidrozoan")] <- "hydrozoa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "outro hydrozoa")] <- "hydrozoa"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "poliqueta")] <- "polychaeta"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "polichaeta")] <- "polychaeta"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"
# octocoral and anthozoa
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "outro anthozoa")] <- "anthozoa"
benthos_DF_occ2$scientificName[grep("octocoral",benthos_DF_occ2$scientificName)] <- "alcyonaria"
# sponge
benthos_DF_occ2$scientificName[grep("sponge",benthos_DF_occ2$scientificName)] <- "porifera"
# echinorms
benthos_DF_occ2$scientificName[grep("ourigo",benthos_DF_occ2$scientificName)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
benthos_DF_occ2$scientificName[grep("sea urchin",benthos_DF_occ2$scientificName)] <- "echinoidea"
benthos_DF_occ2$scientificName[grep("outro echinoderma",benthos_DF_occ2$scientificName)] <- "echinodermata"
benthos_DF_occ2$scientificName[grep("crinside",benthos_DF_occ2$scientificName)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
benthos_DF_occ2$scientificName[grep("estrela",benthos_DF_occ2$scientificName)] <- "asteroidea"

# cca and caa
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "crostose coralline algae")] <- "crustose coralline algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "cianobacterias")] <- "cyanobacteria"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName %in% c("amphiroa", 
                                                             "amphiroa sp", 
                                                             "amphiroideae", 
                                                             "jania amphiroa", 
                                                             "jania sp",
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "filamentous")] <- "filamentous algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "turf")] <- "filamentous algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "green filamentous algae")] <- "filamentous algae"
# algae
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName %in% c("fleshy algae",
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "calcareous turf")] <- "calcareous articulate algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "corticated")] <- "corticated algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "unknown corticated")] <- "corticated algae"
benthos_DF_occ2$scientificName[which(benthos_DF_occ2$scientificName == "sargassum sp")] <- "leathery algae"

# check unique taxa
unique(benthos_DF_occ2$scientificName)[order(unique(benthos_DF_occ2$scientificName))]

# remove components of sampling
# remove plot data
#benthos_DF_occ2<-benthos_DF_occ2 [which(benthos_DF_occ2$scientificName %in% 
#                                                            c("unknown","sand gravel") != T),]







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
