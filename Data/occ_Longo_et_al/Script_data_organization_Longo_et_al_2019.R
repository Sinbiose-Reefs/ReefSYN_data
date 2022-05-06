# ------------------------------------------
# Organizing data (video plots and photoquadrats)

require(openxlsx); require(here); require(worrms); require(dplyr)

# load packages and functions
source("R/functions.R")

### dados dos peixes (Longo et al)
L.peixes <- read.csv(here("Data","occ_Longo_et_al",
                          "Data_Trophic_Interactions_WAtlantic_GLongo_clean.csv"),
                   header = T)




# -----------------------------------------------------------------------------------
# ADJUSTING SITE NAMES







## adequar os nomes das localidades e sitios da base de Longo, de acordo com Aued
# localidades
L.peixes$location [which (L.peixes$location == "alagoas")] <- "costa_corais"
L.peixes$location [which (L.peixes$location == "pernambuco")] <- "costa_corais"
L.peixes$location [which (L.peixes$location == "atol_das_rocas")] <- "rocas"
L.peixes$location [which (L.peixes$location == "guarapari")] <- "espirito_santo"
L.peixes$location [which (L.peixes$location == "arraial_do_cabo")] <- "arraial"
L.peixes$location [which (L.peixes$location == "sao_paulo")] <- "ilhabela"
L.peixes$location [which (L.peixes$location == "parcel_manoel_luis")] <- "manuel_luis"

# separar as localidades do rio grande do norte
L.peixes$location [which (L.peixes$site == "batente_das_agulhas")] <- "rgnor_natal"
L.peixes$location [which (L.peixes$site == "pedra_do_silva")] <- "rgnor_natal"
L.peixes$location [which (L.peixes$site == "barreirinha")] <- "rgnor_sul"
L.peixes$location [which (L.peixes$site == "cabeco_do_leandro")] <- "rgnor_sul"
L.peixes$location [which (L.peixes$site == "maracajau")] <- "rgnor_parrachos"
L.peixes$location [which (L.peixes$site == "parrachos")] <- "rgnor_parrachos"

# separar os sitios de santa catarina
L.peixes$location [which (L.peixes$site == "deserta")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "saco_d'agua")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "baia_da_tartaruga")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "saco_do_vidal")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "engenho")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "xavier")] <- "ilhasc_sul"

### organizar os sitios
L.peixes$site [which(L.peixes$site == "anacris")] <- "ana_cristina"
L.peixes$site [which(L.peixes$site == "parrachos")] <- 'parrachos_de_rio_do_fogo'
L.peixes$site [which(L.peixes$site == "rocas")] <- 'piscina_das_rocas'
L.peixes$site [which(L.peixes$site == "chapeiroes")] <- 'chapeirao'
L.peixes$site [which(L.peixes$site == "ilha_escalvada")] <- 'escalvada'
L.peixes$site [which(L.peixes$site == "ilha_rasa")] <- 'ilhas_rasas'
L.peixes$site [which(L.peixes$site == "porcos")] <- 'porcos_oeste'
L.peixes$site [which(L.peixes$site == "saco_do_sombrio_")] <- 'saco_do_sombrio'
L.peixes$site [which(L.peixes$site == "cabras")] <- 'ilha_das_cabras'
L.peixes$site [which(L.peixes$site == "ponta_do_diogo")] <- 'saco_do_diogo'
L.peixes$site [which(L.peixes$site == "xavier")] <- 'xavier_ponta_sul'
L.peixes$site [which(L.peixes$site == "deserta")] <- 'deserta_norte'
L.peixes$site [which(L.peixes$site == "saco_d'agua")] <- 'saco_dagua'
L.peixes$site [which(L.peixes$site == "barra_gales")] <- 'barra_da_gale'


# adjusting site names
L.peixes$site<-(iconv(L.peixes$site, "ASCII", "UTF-8", sub=""))
L.peixes$site <- tolower(L.peixes$site)
unique(L.peixes$site )[order(unique(L.peixes$site ))]

## modificar a profundidade
L.peixes$depthCategorical <- ifelse (L.peixes$depth_m >= 8, "deep","shallow") #   (categorical)
L.peixes$depthInMeters <- L.peixes$depth_m  # continuous




# -----------------------------------------------------------------------------------
# ADJUSTING REGION






# open benthos data to match region
# aued
occ_Aued_et_al <- read.xlsx(here("Data","occ_Aued_et_al",
                                 "Compiled_quadrats_allsites_Updated_ALLuza_v1.xlsx"),
                            sheet = 1, colNames = TRUE,detectDates=F)

## add region
L.peixes$region <- occ_Aued_et_al$region [match(L.peixes$location, occ_Aued_et_al$locality)]
# mismatches
L.peixes$region [which(L.peixes$site == "cabeco_do_leandro")] <- "ne_reefs"
L.peixes$region [which(L.peixes$site == "barreirinha")] <- "ne_reefs"
unique(L.peixes$site [is.na(L.peixes$region)])
# check sites from Caribe and North America
L.peixes$region [is.na(L.peixes$region)] <- "caribbean_northern_reefs"




# -----------------------------------------------------------------------------------
# ADJUSTING DATES




L.peixes$eventDate <- as.Date (L.peixes$date) # date
L.peixes$eventYear <- format(L.peixes$eventDate,"%Y")
L.peixes$eventMonth <- format(L.peixes$eventDate,"%m")





# -----------------------------------------------------------------------------------
# ADJUSTING SPECIES NAMES







## OBTER A ID DE TODAS AS ESPECIES DE PEIXES ENCONTRADAS POR Longo
todas_sp_Longo <-  (L.peixes$species_code)

# corresponder siglas de Longo com nomes completos de Quimbayo
traits_peixes <- read.csv(here("Data","trait_Quimbayo_et_al","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                         h=T,sep=";")

split_names_JPQ <- lapply (strsplit(traits_peixes$Name," ",fixed=F), substr, 1,3)
split_names_JPQ <- lapply (split_names_JPQ,tolower)
siglas_JPQ <- unlist(lapply (split_names_JPQ, function (i) paste(i[1],i[2],sep="_")))

## inserir uma tabela em Longo, com o nome completo das spp
L.peixes$scientificName <- traits_peixes$Name [match(todas_sp_Longo,siglas_JPQ)]

# encontrar quais especies estao em longo, mas nao estao em Morais
unique (todas_sp_Longo [which(todas_sp_Longo %in% siglas_JPQ == F)])


## adjusting spp.
# based on abbreviations


L.peixes$scientificName [which(L.peixes$species_code == "spa_sp")] <- "Sparisoma_sp"
L.peixes$scientificName [which(L.peixes$species_code == "ocy_cry")] <- "Ocyurus_chrysurus"
L.peixes$scientificName [which(L.peixes$species_code == "ni")] <- "Not_identified"
L.peixes$scientificName [which(L.peixes$species_code == "kyp_sp")] <- "Kyphosus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "euc_lef")] <- "Eucinostomus_lefroyi"
L.peixes$scientificName [which(L.peixes$species_code == "lut_ale")] <- "Lutjanus_alexandrei"
L.peixes$scientificName [which(L.peixes$species_code == "lut_sp")] <- "Lutjanus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "aca_sp")] <- "Acanthurus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "car_sp")] <- "Caranx_sp1"
L.peixes$scientificName [which(L.peixes$species_code == "acanthuridae")] <- "Not_identified"
L.peixes$scientificName [which(L.peixes$species_code == "dio_his")] <- "Diodon_hystrix"
L.peixes$scientificName [which(L.peixes$species_code == "hal_sp")] <- "Halichoeres_sp"
L.peixes$scientificName [which(L.peixes$species_code == "sca_sp")] <- "Scarus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "blenideo")] <- "Not_identified"
L.peixes$scientificName [which(L.peixes$species_code == "das_mar")] <- "Hypanus_marianae"
L.peixes$scientificName [which(L.peixes$species_code == "lab_sp")] <- "Labrisomus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "mal_sp")] <- "Malacoctenus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "bot_sp")] <- "Bothus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "carangideo")] <- "Caranx_sp2"
L.peixes$scientificName [which(L.peixes$species_code == "syn_sp")] <- "Synodus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "manjuba")] <- "Anchoviella_lepidentostole"
L.peixes$scientificName [which(L.peixes$species_code == "sphyraena_borealis?")] <- "Sphyraena_borealis"
L.peixes$scientificName [which(L.peixes$species_code == "myc_sp")] <- "Mycteroperca_sp"
L.peixes$scientificName [which(L.peixes$species_code == "scomberomorus")] <- "Scomberomorus_sp"
L.peixes$scientificName [which(L.peixes$species_code == "sca_coel")] <- "Scarus_coelestinus"
L.peixes$scientificName [which(L.peixes$species_code == "carangidae")] <- "Caranx_sp3"
L.peixes$scientificName [which(L.peixes$species_code == "epi_niv")] <- "Epinephelus_niveatus"
L.peixes$scientificName [which(L.peixes$species_code == "epi_cru")] <- "Epinephelus_cruentatus"
L.peixes$scientificName [which(L.peixes$species_code == "neg_bre")] <- "Negaprion_brevirostris"
L.peixes$scientificName [which(L.peixes$species_code == "gin_cir")] <- "Ginglymostoma_cirratum"
L.peixes$scientificName [which(L.peixes$species_code == "par_sp")] <- "Parablennius_sp"
L.peixes$scientificName [which(L.peixes$species_code == "aet_nar")] <- "Aetobatus_narinari"             
L.peixes$scientificName [which(L.peixes$species_code == "sco_sp")] <- "Scorpaena_sp"
L.peixes$scientificName [which(L.peixes$species_code == "hae_sp")] <- "Haemulon_sp"
L.peixes$scientificName [which(L.peixes$species_code == "das_ame")] <- "Hypanus_americana"            
L.peixes$scientificName [which(L.peixes$species_code == "sph_sp")] <- "Sphoeroides_sp"
L.peixes$scientificName [which(L.peixes$species_code ==  "car_plu")] <- "Caranx_plumbeus"     
L.peixes$scientificName [which(L.peixes$species_code ==  "lut_moh")] <- "Lutjanus_mohogani"             
L.peixes$scientificName [which(L.peixes$species_code == "mir_jac")] <- "Myripristis_jacobus"
L.peixes$scientificName [which(L.peixes$species_code == "pem_sco")] <- "Pempheris_schomburgkii"
L.peixes$scientificName [which(L.peixes$species_code == "das_sp")] <- "Dasyatis_sp"


# replace space by "_"
L.peixes$scientificName <- gsub (" ","_",L.peixes$scientificName) # mix of sep






# -----------------------------------------------------------------------------------
# ADJUSTING COORDINATES




# match geography, based on this
# https://github.com/KellyInagaki/Inagaki_etal_trophicinteractions/blob/master/Acanthuridae_feedingpressure.txt
coord_longo <-  read.csv(here("Data","occ_Longo_et_al","Coordinates.csv"), 
                          sep=";")

# coordinates not in data
match_data_coord <- (coord_longo [match(L.peixes$site, coord_longo$name_data),])

# extract coordeinates
L.peixes_coord <- cbind (L.peixes,
                  match_data_coord [,c("Long_DD", "Lat_DD")])

# not matched
not_matched <- L.peixes_coord [is.na(L.peixes_coord$Lat_DD),]

# get from Aued et al. (same sites, different names)
match_aued <- occ_Aued_et_al [which(occ_Aued_et_al$Sites%in% not_matched$site),]

# bind coordeinates that match
not_matched$Long_DD <- match_aued [match (not_matched$site,match_aued$Sites),"decimalLongitude"]
not_matched$Lat_DD <- match_aued [match (not_matched$site,match_aued$Sites),"decimalLatitude"]


# bind in data of Longo

L.peixes_coord [is.na(L.peixes_coord$Lat_DD),"Lat_DD"] <- not_matched$Lat_DD 
L.peixes_coord [is.na(L.peixes_coord$Long_DD),"Long_DD"] <- not_matched$Long_DD




# ---------------------------------------------------------------------
# further check taxonomic issues (mostly taxonomic updates)




L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Platybelone_argalus")] <- "Platybelone_argalus_argalus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Nicholsina_usta_collettei")] <- "Nicholsina_collettei"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Labrisomus_kalisherae")] <- "Gobioclinus_kalisherae"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Diplodus_argenteus_argenteus")] <- "Diplodus_argenteus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Eucinostomus_lefroyi")] <- "Ulaema_lefroyi"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Haemulon_plumieri")] <- "Haemulon_plumierii"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Hypanus_americana")] <- "Hypanus_americanus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Dasyatis_americana")] <- "Hypanus_americanus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Caranx_plumbeus")] <- "Carcharhinus_plumbeus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Lutjanus_mohogani")] <- "Lutjanus_mahogani"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Epinephelus_niveatus")] <- "Hyporthodus_niveatus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName == "Epinephelus_cruentatus")] <- "Cephalopholis_cruentata"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus_spinosus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Coryphopterus_spb" )] <-  "Coryphopterus_spp"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Dasyatis_americana" )] <-  "Hypanus americanus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Diplodus_argenteus_argenteus" )] <-  "Diplodus_argenteus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Emblemariopsis_signifera" )] <-  "Emblemariopsis_signifer"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Kyphosus_incisor" )] <-  "Kyphosus_vaigiensis"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Kyphosus_bigibbus" )] <-  "Kyphosus_sp"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Malacoctenus_sp1" )] <-  "Malacoctenus_brunoi"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Malacoctenus_sp2" )] <-  "Malacoctenus_lianae"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Malacoctenus_sp3" )] <-  "Malacoctenus lianae"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Nicholsina_usta_usta" )] <-  "Nicholsina_usta_usta"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Nicholsina_usta_collettei" )] <-  "Nicholsina_usta"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Anthias_salmopuntatus" )] <- "Choranthias_salmopunctatus"
L.peixes_coord$scientificName [which(L.peixes_coord$scientificName ==  "Emblemariosis_sp" )] <- "Emblemariopsis_sp"

# and finally replace "_" by " "
L.peixes_coord$scientificName <- gsub ("_"," ",L.peixes$scientificName)
L.peixes_coord$scientificName <- tolower (L.peixes_coord$scientificName)


# matching with worms
worms_record <- lapply (unique(L.peixes_coord$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))
# match
L.peixes_coord$scientificNameID<-(df_worms_record$lsid [match (L.peixes_coord$scientificName, tolower(df_worms_record$scientificname))])
L.peixes_coord$kingdom<-(df_worms_record$kingdom [match (L.peixes_coord$scientificName,tolower(df_worms_record$scientificname))])
L.peixes_coord$class<-(df_worms_record$class [match (L.peixes_coord$scientificName,tolower(df_worms_record$scientificname))])
L.peixes_coord$family<-(df_worms_record$family [match (L.peixes_coord$scientificName,tolower(df_worms_record$scientificname))])




# -----------------------------------------------------------------------------------
# ADJUSTING DATES






# open a csv with identity of videoplots, organized by ALLuza in excel (due to the weird format of eventIDs)
sites_plots_longo <- read.xlsx(here("Data","occ_Longo_et_al",
                                 "Data_Trophic_Interactions_WAtlantic_GLongo_clean_ListSites_Videos_ALL.xlsx"),
                                 sheet = 1, colNames = TRUE,detectDates=F)


# bind into the data
L.peixes_coord <- cbind (L.peixes_coord, 
                         video_id = sites_plots_longo$video_id)

# set years in missing data (rocas 2012)
# check here https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12806
L.peixes_coord [which(L.peixes_coord$location == "rocas"),"eventYear"] <- "2012"
L.peixes_coord [which(L.peixes_coord$location == "rocas"),"eventMonth"] <- "01"
# NA in barra da gale (2012)
L.peixes_coord [which(L.peixes_coord$site == "barra_da_gale"),"eventYear"]  <- "2012"





# -----------------------------------------------------------------------------------
# CREATING IDS







# creating parentIDs
L.peixes_coord$parentEventID <- paste (paste ("BR:SISBIOTA-MAR:",
                                              L.peixes_coord$location,sep=""), 
                                       L.peixes_coord$site, 
                                       L.peixes_coord$eventYear,
                                 sep="_")

# creating eventIds
L.peixes_coord$eventID <- paste (paste ("BR:SISBIOTA-MAR:",
                                        L.peixes_coord$location,sep=""), 
                                 L.peixes_coord$site, 
                                 L.peixes_coord$eventYear,
                                 L.peixes_coord$video_id,
                             sep="_")

# creating occurrenceIDs
L.peixes_coord$occurrenceID <- paste (paste ("BR:SISBIOTA-MAR:",
                                    L.peixes_coord$location,sep=""), 
                             L.peixes_coord$site, 
                             L.peixes_coord$eventYear,
                             L.peixes_coord$video_id,
                             paste ("occ",seq(1,nrow(L.peixes_coord)),sep=""),
                             sep="_")

# remove sites from Caribe and North America 
L.peixes_coord<-L.peixes_coord[which(L.peixes_coord$location %in% c("north_carolina_usa",
                                                                    "georgia_usa",
                                                                    "central_florida",
                                                                    "florida_keys",
                                                                    "mexico",
                                                                    "belize",            
                                                                    "curacao") == F),]





# -----------------------------------------------------------------------------------
# DWC descriptors





# method
L.peixes_coord$samplingProtocol <- "video plot"
# samplingEffort per parentEventID
table_effort <-do.call(rbind, lapply (unique(L.peixes_coord$parentEventID), function (i)

  data.frame (parentEventID = i,
            nVideosID = length(unique(L.peixes_coord [which(L.peixes_coord$parentEventID == i),
                                                      "video_id"])))
))    

# match to fill
L.peixes_coord$samplingEffort <- table_effort [match (L.peixes_coord$parentEventID,
                                                      table_effort$parentEventID),
                                "nVideosID"]

# counts don't match with those provided in the supp information of the paper
# check here https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12806
# # 5 to 40 in the paper (must consider that sites outside BR coast are not included here): 
range(rowSums(table(L.peixes_coord$parentEventID,L.peixes_coord$video_id)>0,na.rm=T))
range(L.peixes_coord$samplingEffort)
# sampleSizeValue
L.peixes_coord$sampleSizeValue <- 2*1
# sampleSizeUnit
L.peixes_coord$sampleSizeUnit <- "squared meters"
# country and code
L.peixes_coord$Country <- "Brazil"
L.peixes_coord$countryCode <- "BR"
# basisOfRecord
L.peixes_coord$basisOfRecord <- "HumanObservation"
# occurrenceStatus
L.peixes_coord$occurrenceStatus <- "presence"
# geodeticDatum
L.peixes_coord$geodeticDatum <- "decimal degrees"
# geographic location
L.peixes_coord$higherGeographyID <- ifelse (L.peixes_coord$location %in% c("rocas",
                                                                            "noronha",
                                                                            "trindade"),
                                                "BrazilianIslands",
                                                "BrazilianCoast")

# recordedBy
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "diver")] <- "recordedBy"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "Long_DD")] <- "decimalLongitude"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "Lat_DD")] <- "decimalLatitude"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "location")] <- "locationID"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "site")] <- "locality"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "depth_m")] <- "minimumDepthinMeters"
L.peixes_coord$maximumDepthinMeters <- L.peixes_coord$minimumDepthinMeters
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "beginning_time")] <- "begginingObservationTime"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "ending_time")] <- "endingObservationTime"





# --------------------------------------------------------------------------
# ADJUSTING MEASUREMENT TYPES





# split # bites,  size, activity, and time data
bites <- L.peixes_coord[,which(colnames(L.peixes_coord) %in% c("body_size_cm",
                                                               "activity",
                                                               "observation_time") == F)] 
# measurementType
bites$measurementType <- "foraging behavior"
# organismQuantityType
bites$organismQuantityType <- "number of bites"
# measurementUnit
bites$measurementUnit <- "counts"
# measurementValue
colnames(bites)[which(colnames(bites) == "number_of_bites")] <- "measurementValue"

# size
size <- L.peixes_coord[,which(colnames(L.peixes_coord) %in% c("number_of_bites",
                                                               "activity",
                                                               "observation_time") == F)] 
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "centimeters"
# measurementValue
colnames(size)[which(colnames(size) == "body_size_cm")] <- "measurementValue"

# activity
activity <- L.peixes_coord[,which(colnames(L.peixes_coord) %in% c("number_of_bites",
                                                              "body_size_cm",
                                                              "observation_time") == F)] 
# measurementType
activity$measurementType <- "activity"
# organismQuantityType
activity$organismQuantityType <- "activity"
# measurementUnit
activity$measurementUnit <- "unitless"
# measurementValue
colnames(activity)[which(colnames(activity) == "activity")] <- "measurementValue"

# time
time <- L.peixes_coord[,which(colnames(L.peixes_coord) %in% c("number_of_bites",
                                                                  "body_size_cm",
                                                                  "activity") == F)] 
# measurementType
time$measurementType <- "time"
# organismQuantityType
time$organismQuantityType <- "time"
# measurementUnit
time$measurementUnit <- "second"
# measurementValue
colnames(time)[which(colnames(time) == "observation_time")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (bites,
                     size,
                     activity,
                     time)
# measurementType
dados_bind$measurementTypeID <- "http://vocab.nerc.ac.uk/collection/P14/current/GVAR0044/"






# -----------------------------------------------------------------------------------
#  Formatted according to DwC








DF_eMOF <- dados_bind [,c("eventID", "occurrenceID","scientificName","scientificNameID",
                          "kingdom","class","family",
                          "measurementValue", "measurementType","measurementUnit",
                          "measurementTypeID",
                          "begginingObservationTime",
                          "endingObservationTime")]

DF_occ <- dados_bind [,c("eventID", "occurrenceID","scientificName","scientificNameID",
                                            "kingdom","class","family",
                                            "recordedBy", "organismQuantityType",
                                            "basisOfRecord")]

# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeographyID,locationID,locality) %>% 
                            
                            summarise(eventYear = mean(as.numeric(eventYear)),
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




# save xlsx
# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)


# save
# txt format
write.table(DF_occ, file =here("DwC_output",
                               "GLongo_spatialData",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)



write.table(DF_eMOF, file =here("DwC_output",
                                "GLongo_spatialData",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)



write.table(event_core, file =here("DwC_output",
                                   "GLongo_spatialData",
                                   "event_core.txt"),sep=",",
            quote = FALSE)

