

# ------------------------------------------
# Organizing data (video plots and photoquadrats)

require(openxlsx); require(here); require(worrms); require(dplyr)

# load packages and functions
source("R/functions.R")

### dados dos peixes (Longo et al)
L.peixes <- read.csv(here("Data","occ_Longo_et_al",
                          "Data_Trophic_Interactions_WAtlantic_GLongo_clean.csv"),
                   header = T)



unique(L.peixes [which(L.peixes$species_code == "cha_oce"), "diver"])
unique(L.peixes [which(L.peixes$species_code == "cha_oce"), "diver"])


# -----------------------------------------------------------------------------------
# ADJUSTING SITE NAMES



L.peixes$verbatimSite <- L.peixes$location
L.peixes$verbatimLocality <- L.peixes$site



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
L.peixes$region <- occ_Aued_et_al$region [match(L.peixes$location, 
                                                occ_Aued_et_al$locality)]
# mismatches
L.peixes$region [which(L.peixes$site == "cabeco_do_leandro")] <- "ne_reefs"
L.peixes$region [which(L.peixes$site == "barreirinha")] <- "ne_reefs"
unique(L.peixes$site [is.na(L.peixes$region)])
# check sites from Caribe and North America
L.peixes$region [is.na(L.peixes$region)] <- "Caribbean_NorthAmerica"


# -----------------------------------------------------------------------------------
# ADJUSTING DATES




L.peixes$eventDate <- as.Date (L.peixes$date) # date
L.peixes$year <- format(L.peixes$eventDate,"%Y")
L.peixes$month <- format(L.peixes$eventDate,"%m")
L.peixes$day <- format(L.peixes$eventDate,"%d")


# eventTime
L.peixes$eventTime <- L.peixes$time_of_day
L.peixes$eventTime <- ifelse (L.peixes$eventTime == "", NA, L.peixes$eventTime)
L.peixes$eventTime <- gsub ("-", "Z/", L.peixes$eventTime)
L.peixes$eventTime <- gsub ("h", "Z", L.peixes$eventTime)
L.peixes$eventTime <- gsub ("H", "Z", L.peixes$eventTime)
L.peixes$eventTime <- gsub ("pm", "Z", L.peixes$eventTime)
L.peixes$eventTime <- gsub ("am", "Z", L.peixes$eventTime)
L.peixes$eventTime <- gsub ("m", "Z", L.peixes$eventTime)
# paste zero if last character is not Z
L.peixes$eventTime <- ifelse (substr ((L.peixes$eventTime), nchar(L.peixes$eventTime),nchar(L.peixes$eventTime)) == "Z",
        L.peixes$eventTime,
        paste0 (L.peixes$eventTime,"Z"))
# wrong hour as NA
L.peixes$eventTime [which(L.peixes$eventTime == "00/01/1900Z")] <- NA

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


# verbatimIdentification
L.peixes$verbatimIdentification <- L.peixes$species_code

## inserir uma tabela em Longo, com o nome completo das spp
L.peixes$namesToSearch <- traits_peixes$Name [match(todas_sp_Longo,siglas_JPQ)]

# encontrar quais especies estao em longo, mas nao estao em Morais
unique (todas_sp_Longo [which(todas_sp_Longo %in% siglas_JPQ == F)])


## adjusting spp.
# based on abbreviations

L.peixes$namesToSearch [which(L.peixes$species_code == "spa_sp")] <- "Sparisoma_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "ocy_cry")] <- "Ocyurus_chrysurus"
L.peixes$namesToSearch [which(L.peixes$species_code == "ni")] <- "Not_identified"
L.peixes$namesToSearch [which(L.peixes$species_code == "kyp_sp")] <- "Kyphosus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "euc_lef")] <- "Eucinostomus_lefroyi"
L.peixes$namesToSearch [which(L.peixes$species_code == "lut_ale")] <- "Lutjanus_alexandrei"
L.peixes$namesToSearch [which(L.peixes$species_code == "lut_sp")] <- "Lutjanus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "aca_sp")] <- "Acanthurus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "car_sp")] <- "Caranx_sp1"
L.peixes$namesToSearch [which(L.peixes$species_code == "acanthuridae")] <- "Not_identified"
L.peixes$namesToSearch [which(L.peixes$species_code == "dio_his")] <- "Diodon_hystrix"
L.peixes$namesToSearch [which(L.peixes$species_code == "hal_sp")] <- "Halichoeres_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "sca_sp")] <- "Scarus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "blenideo")] <- "Not_identified"
L.peixes$namesToSearch [which(L.peixes$species_code == "das_mar")] <- "Hypanus_marianae"
L.peixes$namesToSearch [which(L.peixes$species_code == "lab_sp")] <- "Labrisomus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "mal_sp")] <- "Malacoctenus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "bot_sp")] <- "Bothus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "carangideo")] <- "Caranx_sp2"
L.peixes$namesToSearch [which(L.peixes$species_code == "syn_sp")] <- "Synodus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "manjuba")] <- "Anchoviella_lepidentostole"
L.peixes$namesToSearch [which(L.peixes$species_code == "sphyraena_borealis?")] <- "Sphyraena_borealis"
L.peixes$namesToSearch [which(L.peixes$species_code == "myc_sp")] <- "Mycteroperca_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "scomberomorus")] <- "Scomberomorus_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "sca_coel")] <- "Scarus_coelestinus"
L.peixes$namesToSearch [which(L.peixes$species_code == "carangidae")] <- "Caranx_sp3"
L.peixes$namesToSearch [which(L.peixes$species_code == "epi_niv")] <- "Epinephelus_niveatus"
L.peixes$namesToSearch [which(L.peixes$species_code == "epi_cru")] <- "Epinephelus_cruentatus"
L.peixes$namesToSearch [which(L.peixes$species_code == "neg_bre")] <- "Negaprion_brevirostris"
L.peixes$namesToSearch [which(L.peixes$species_code == "gin_cir")] <- "Ginglymostoma_cirratum"
L.peixes$namesToSearch [which(L.peixes$species_code == "par_sp")] <- "Parablennius_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "aet_nar")] <- "Aetobatus_narinari"             
L.peixes$namesToSearch [which(L.peixes$species_code == "sco_sp")] <- "Scorpaena_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "hae_sp")] <- "Haemulon_sp"
L.peixes$namesToSearch [which(L.peixes$species_code == "das_ame")] <- "Hypanus_americanus"            
L.peixes$namesToSearch [which(L.peixes$species_code == "sph_sp")] <- "Sphoeroides_sp"
L.peixes$namesToSearch [which(L.peixes$species_code ==  "car_plu")] <- "Carcharhinus_plumbeus"     
L.peixes$namesToSearch [which(L.peixes$species_code ==  "lut_moh")] <- "Lutjanus_mahogoni"             
L.peixes$namesToSearch [which(L.peixes$species_code == "mir_jac")] <- "Myripristis_jacobus"
L.peixes$namesToSearch [which(L.peixes$species_code == "pem_sco")] <- "Pempheris_schomburgkii"
L.peixes$namesToSearch [which(L.peixes$species_code == "das_sp")] <- "Dasyatis_sp"


# replace space by "_"
L.peixes$namesToSearch <- gsub (" ","_",L.peixes$namesToSearch) # mix of sep


# wrong matching
L.peixes$namesToSearch [which(L.peixes$namesToSearch  == "Chaenopsis_ocellata")] <- "Chaetodon_ocellatus"



# -----------------------------------------------------------------------------------
# ADJUSTING COORDINATES




# match geography, based on this
# https://github.com/KellyInagaki/Inagaki_etal_trophicinteractions/blob/master/Acanthuridae_feedingpressure.txt
coord_longo <-  read.csv(here("Data","occ_Longo_et_al","Coordinates.csv"), 
                          sep=";")


# coordinates not in data
match_data_coord <- (coord_longo [match(L.peixes$site, coord_longo$name_data),])


# extract coordinates
L.peixes_coord <- cbind (L.peixes,
                  match_data_coord [,c("Long_DD", "Lat_DD")])


# not matched
not_matched <- unique(L.peixes_coord [is.na(L.peixes_coord$Lat_DD),"verbatimLocality"])

# the mean coordinates of oostpunt
L.peixes_coord[grep("oost", L.peixes_coord$site),"Lat_DD"] <- apply (coord_longo [grep ("Oos",coord_longo$Site),c("Lat_DD", "Long_DD")],2,mean)[1]
L.peixes_coord[grep("oost", L.peixes_coord$site),"Long_DD"] <- apply (coord_longo [grep ("Oos",coord_longo$Site),c("Lat_DD", "Long_DD")],2,mean)[2]
L.peixes_coord[grep("oost", L.peixes_coord$site),c("Lat_DD","Long_DD")]
# the value for station rock
L.peixes_coord[grep("station", L.peixes_coord$site),"Lat_DD"] <- coord_longo [grep ("Station",coord_longo$Site),c("Lat_DD", "Long_DD")][1]
L.peixes_coord[grep("station", L.peixes_coord$site),"Long_DD"] <- coord_longo [grep ("Station",coord_longo$Site),c("Lat_DD", "Long_DD")][2]


# get from Aued et al. (same sites, different names)
match_aued <- occ_Aued_et_al [which(occ_Aued_et_al$site %in% not_matched),]
match_aued <- aggregate (match_aued, by=list(match_aued$site),FUN=mean)[,c("Group.1", "decimalLatitude", "decimalLongitude")]

# bind coordinates that matched
L.peixes_coord [which(L.peixes_coord$site %in% match_aued$Group.1[1]),c("Lat_DD", "Long_DD")] <- match_aued[1,c("decimalLatitude","decimalLongitude")]
L.peixes_coord [which(L.peixes_coord$site %in% match_aued$Group.1[2]),c("Lat_DD", "Long_DD")] <- match_aued[2,c("decimalLatitude","decimalLongitude")]


# bind in data of Longo
# saco do vidal (coordinates from Morais et al. 2017)
L.peixes_coord[which(L.peixes_coord$site == "saco_do_vidal"),"Lat_DD"] <- -27.298217
L.peixes_coord[which(L.peixes_coord$site == "saco_do_vidal"),"Long_DD"] <- -48.361041
# ilha do meio (coordinates from Morais et al. 2017)
L.peixes_coord[which(L.peixes_coord$site == "ilha_do_meio"),"Lat_DD"] <- -8.761792
L.peixes_coord[which(L.peixes_coord$site == "ilha_do_meio"),"Long_DD"] <- -35.087711
# saco_dagua (coordinates from Morais et al. 2017)
L.peixes_coord[which(L.peixes_coord$site == "saco_dagua"),"Lat_DD"] <- -27.277044
L.peixes_coord[which(L.peixes_coord$site == "saco_dagua"),"Long_DD"] <- -48.368482
# engenho (coordinates from Morais et al. 2017)
L.peixes_coord[which(L.peixes_coord$site == "engenho"),"Lat_DD"] <- -27.290608
L.peixes_coord[which(L.peixes_coord$site == "engenho"),"Long_DD"] <- -48.367013
# bt (coordinates from Morais et al. 2017)
L.peixes_coord[which(L.peixes_coord$site == "baia_da_tartaruga"),"Lat_DD"] <- -27.290608
L.peixes_coord[which(L.peixes_coord$site == "baia_da_tartaruga"),"Long_DD"] <- -48.363806
# marie_pumpoin  (coordinates from GO LONGO) 
L.peixes_coord[which(L.peixes_coord$site == "marie_pumpoin"),"Lat_DD"] <- 12.033333
L.peixes_coord[which(L.peixes_coord$site == "marie_pumpoin"),"Long_DD"] <- -68.8 
# barra da gale (coordinates from GO LONGO)
L.peixes_coord[which(L.peixes_coord$site == "barra_da_gale"),"Lat_DD"] <- -9.0327
L.peixes_coord[which(L.peixes_coord$site == "barra_da_gale"),"Long_DD"] <- -35.1927
# igrejinha é = perua preta -> -35.0887	-8.7252
L.peixes_coord[which(L.peixes_coord$site == "igrejinha"),"site"] <- "perua_preta"
# perua preta (antes igraeginha)
L.peixes_coord[which(L.peixes_coord$site == "perua_preta"),"Lat_DD"] <- -8.7252
L.peixes_coord[which(L.peixes_coord$site == "perua_preta"),"Long_DD"] <- -35.0887
# pirambu
coord_to_convert <- c("08 45 31.65", "35 5 8.01")
coord_to_convert<-angle2dec(coord_to_convert)*-1
L.peixes_coord[which(L.peixes_coord$site == "pirambu"),"Lat_DD"] <- coord_to_convert[1]
L.peixes_coord[which(L.peixes_coord$site == "pirambu"),"Long_DD"] <- coord_to_convert[2]

# ---------------------------------------------------------------------
# further check taxonomic issues (mostly taxonomic updates)



L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Platybelone_argalus")] <- "Platybelone_argalus_argalus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Nicholsina_usta_collettei")] <- "Nicholsina_usta"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Labrisomus_kalisherae")] <- "Gobioclinus_kalisherae"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Diplodus_argenteus_argenteus")] <- "Diplodus_argenteus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Eucinostomus_lefroyi")] <- "Ulaema_lefroyi"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Haemulon_plumieri")] <- "Haemulon_plumierii"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Hypanus_americana")] <- "Hypanus_americanus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Dasyatis_americana")] <- "Hypanus_americanus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Caranx_plumbeus")] <- "Carcharhinus_plumbeus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Lutjanus_mohogani")] <- "Lutjanus_mahogoni"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Epinephelus_niveatus")] <- "Hyporthodus_niveatus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch == "Epinephelus_cruentatus")] <- "Cephalopholis_cruentata"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Chilomycterus_spinosus_mauretanicus" )] <-  "Chilomycterus_spinosus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Coryphopterus_spb" )] <-  "Coryphopterus_spp"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Dasyatis_americana" )] <-  "Hypanus americanus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Diplodus_argenteus_argenteus" )] <-  "Diplodus_argenteus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Emblemariopsis_signifera" )] <-  "Emblemariopsis_signifer"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Kyphosus_incisor" )] <-  "Kyphosus_vaigiensis"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Kyphosus_bigibbus" )] <-  "Kyphosus_sp"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Malacoctenus_sp1" )] <-  "Malacoctenus_brunoi"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Malacoctenus_sp2" )] <-  "Malacoctenus_lianae"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Malacoctenus_sp3" )] <-  "Malacoctenus lianae"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Nicholsina_usta_usta" )] <-  "Nicholsina_usta_usta"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Nicholsina_usta_collettei" )] <-  "Nicholsina_usta"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Anthias_salmopuntatus" )] <- "Choranthias_salmopunctatus"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Emblemariosis_sp" )] <- "Emblemariopsis_sp"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Caranx2" )] <- "Caranx_sp2"
L.peixes_coord$namesToSearch [which(L.peixes_coord$namesToSearch ==  "Caranx3" )] <- "Caranx_sp3"


# and finally replace "_" by " "
L.peixes_coord$namesToSearch <- gsub ("_"," ",L.peixes$namesToSearch)
L.peixes_coord$namesToSearch <- tolower (L.peixes_coord$namesToSearch)

# non identified species
L.peixes_coord$identificationQualifier <- ifelse (sapply (strsplit (L.peixes_coord$namesToSearch, " "), "[", 2) %in% c("sp","sp1", "sp2", "sp3"),
                                              "sp",
                                              NA)

# species to search
L.peixes_coord$namesToSearch [which(L.peixes_coord$identificationQualifier == "sp")] <- gsub (" sp*.",
                                                                                          "",
                                                                                          L.peixes_coord$namesToSearch [which(L.peixes_coord$identificationQualifier == "sp")])





# matching with worms
worms_record <- lapply (unique(L.peixes_coord$namesToSearch), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)



# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# match
L.peixes_coord$scientificNameAccepted<-(df_worms_record$scientificname [match (L.peixes_coord$namesToSearch, tolower(df_worms_record$scientificname))])
# taxon rank of the identified level
L.peixes_coord$taxonRank <- (df_worms_record$rank [match (L.peixes_coord$namesToSearch,tolower (df_worms_record$scientificname))])
L.peixes_coord$scientificNameID<-(df_worms_record$lsid [match (L.peixes_coord$namesToSearch, tolower(df_worms_record$scientificname))])
L.peixes_coord$kingdom<-(df_worms_record$kingdom [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])
L.peixes_coord$phylum<-(df_worms_record$phylum [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])
L.peixes_coord$class<-(df_worms_record$class [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])
L.peixes_coord$order<-(df_worms_record$order [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])
L.peixes_coord$family<-(df_worms_record$family [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])
L.peixes_coord$genus<-(df_worms_record$genus [match (L.peixes_coord$namesToSearch,tolower(df_worms_record$scientificname))])



unique(L.peixes_coord [is.na(L.peixes_coord$scientificNameAccepted),"namesToSearch"])



# taxonomic updates
# species
L.peixes_coord$scientificNameAccepted[grep ("multilineata", L.peixes_coord$scientificNameAccepted)] <- "Azurina multilineata"
L.peixes_coord$scientificNameAccepted[grep ("bartholomaei", L.peixes_coord$scientificNameAccepted)] <- "Caranx bartholomaei"
L.peixes_coord$scientificNameAccepted[grep ("polygonius", L.peixes_coord$scientificNameAccepted)] <- "Acanthostracion polygonium"
L.peixes_coord$scientificNameAccepted[grep ("Hypanus americanus", L.peixes_coord$scientificNameAccepted)] <- "Hypanus berthalutzae"
L.peixes_coord$scientificNameAccepted[grep ("Serranus atricauda", L.peixes_coord$scientificNameAccepted)] <- "Serranus flaviventris"
L.peixes_coord$scientificNameAccepted[grep ("Haemulon steindachneri", L.peixes_coord$scientificNameAccepted)] <- "Haemulon atlanticus"
L.peixes_coord$scientificNameAccepted[grep ("Sphoeroides spengleri", L.peixes_coord$scientificNameAccepted)] <- "Sphoeroides camila"

# genus
L.peixes_coord$genus[grep ("multilineata", L.peixes_coord$scientificNameAccepted)] <- "Azurina"
L.peixes_coord$genus[grep ("bartholomaei", L.peixes_coord$scientificNameAccepted)] <- "Caranx"
L.peixes_coord$genus[grep ("Mycteroperca marginata", L.peixes_coord$scientificNameAccepted)] <- "Mycteroperca"


unique(L.peixes_coord$scientificNameAccepted)[order(unique(L.peixes_coord$scientificNameAccepted))]


# adjust family
L.peixes_coord$family[which(L.peixes_coord$family == "Scaridae")] <- "Labridae"


# L.peixes_coord[which(L.peixes_coord$scientificNameAccepted == "Sphyraena borealis"),"identificationQualifier"]  <- "cf."# 
# L.peixes_coord[which(L.peixes_coord$scientificNameAccepted == "Ophioblennius macclurei"),]
# L.peixes_coord[which(L.peixes_coord$scientificNameAccepted == "Malacoctenus triangulatus"),]



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
L.peixes_coord [which(L.peixes_coord$location == "rocas"),"year"] <- "2012"
L.peixes_coord [which(L.peixes_coord$location == "rocas"),"month"] <- "01"
# NA in barra da gale (2012)
L.peixes_coord [which(L.peixes_coord$site == "barra_da_gale"),"year"]  <- "2012"




# geographic location
L.peixes_coord$higherGeography <- ifelse (L.peixes_coord$location %in% c("rocas",
                                                                         "noronha",
                                                                         "trindade"),
                                          "BrazilianOceanicIslands",
                                          ifelse (L.peixes_coord$location %in% c("north_carolina_usa",
                                                                                 "georgia_usa",
                                                                                 "central_florida",
                                                                                 "florida_keys",
                                                                                 "mexico",
                                                                                 "belize",
                                                                                 "curacao"),
                                                  "Caribbean_NorthAmerica",
                                                  "BrazilianCoast"))

# check
unique(L.peixes_coord$site [which(L.peixes_coord$higherGeography == "Caribbean_NorthAmerica")])
unique(L.peixes_coord$site [which(L.peixes_coord$higherGeography == "BrazilianOceanicIslands")])


# adjusting sites and localities
L.peixes_coord$locality  <- L.peixes_coord$site
L.peixes_coord$site <- L.peixes_coord$location


# -----------------------------------------------------------------------------------
# CREATING IDS




L.peixes_coord$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:SISBIOTA-MAR-video-plots:", 
           L.peixes_coord$higherGeography,
           sep=""),
    L.peixes_coord$site,sep=":"),
  L.peixes_coord$locality,
  L.peixes_coord$year,
  sep="_")



# creating eventIds
L.peixes_coord$eventID <- paste (
                                paste ( 
                                  paste ("BR:ReefSYN:SISBIOTA-MAR-video-plots:", 
                                         L.peixes_coord$higherGeography,
                                         sep=""),
                                  L.peixes_coord$site,sep=":"),
                                L.peixes_coord$locality,
                                L.peixes_coord$year,
                                 L.peixes_coord$video_id,
                             sep="_")




# creating occurrenceIDs
L.peixes_coord$occurrenceID <- paste (
                              paste ( 
                                paste ("BR:ReefSYN:SISBIOTA-MAR-video-plots:", 
                                       L.peixes_coord$higherGeography,
                                       sep=""),
                                L.peixes_coord$site,sep=":"),
                              L.peixes_coord$locality,
                              L.peixes_coord$year,
                             L.peixes_coord$video_id,
                             paste ("occ",seq(1,nrow(L.peixes_coord)),sep=""),
                             sep="_")







# -----------------------------------------------------------------------------------
# DWC descriptors





# method
L.peixes_coord$samplingProtocol <- "video plot - 2 x 1m"

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
range(rowSums(table(L.peixes_coord$parentEventID,L.peixes_coord$video_id)>0,na.rm=T))
range(L.peixes_coord$samplingEffort)

# sampleSizeValue
L.peixes_coord$sampleSizeValue <- 2*1
# sampleSizeUnit
L.peixes_coord$sampleSizeUnit <- "squared meters"

# country and code
L.peixes_coord$Country <- NA
USA_sites <- c("station_rock","sw","sr","ms","bp", "l4","fs15","fs17","bl","cm","sf","a51","cr","mo","pr")
mexico_sites <- c("moc", "jar", "bar", "sab", "cha","par")
belize_sites <- c("cbc1","cbsw", "cbc3","cbc4","cur")
curacao_sites <- c("oostpunt","oo1","oo2", "snake_bay", "water_factory", "west_punt","marie_pumpoin")

# define countries
L.peixes_coord [which(L.peixes_coord$locality %in% USA_sites),"Country"] <- "United States"
L.peixes_coord [which(L.peixes_coord$locality %in% mexico_sites),"Country"] <- "Mexico"
L.peixes_coord [which(L.peixes_coord$locality %in% belize_sites),"Country"] <- "Belize"
L.peixes_coord [which(L.peixes_coord$locality %in% curacao_sites),"Country"] <- "Curacao"
L.peixes_coord [is.na(L.peixes_coord$Country),"Country"]<-"Brazil"
# check
# table(L.peixes_coord$Country, L.peixes_coord$site)



# remove sites from other regions
L.peixes_coord <- L.peixes_coord [which( L.peixes_coord$higherGeography != "Caribbean_NorthAmerica"),]


# basisOfRecord
L.peixes_coord$basisOfRecord <- "HumanObservation"

# occurrenceStatus
L.peixes_coord$occurrenceStatus <- "presence"

# geodeticDatum
L.peixes_coord$geodeticDatum <- "decimal degrees"


# recordedBy
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "diver")] <- "recordedBy"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "Long_DD")] <- "decimalLongitude"
colnames(L.peixes_coord)[which(colnames(L.peixes_coord) == "Lat_DD")] <- "decimalLatitude"
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
# measurementRemarks
bites$measurementRemarks <- "Missing data (NA) indicate detections without interaction with the benthos"

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
# measurementRemarks
size$measurementRemarks <- "Missing data (NA) indicate fish missing size estimates"

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
# measurementRemarks
activity$measurementRemarks <- "Missing data (NA) indicate missing activity record"
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
# measurementRemarks
time$measurementRemarks <- "Missing data (NA) indicate missing activity record"
# measurementValue
colnames(time)[which(colnames(time) == "observation_time")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (bites,
                     size,
                     activity,
                     time)


# measurementType
dados_bind$measurementTypeID <- "http://vocab.nerc.ac.uk/collection/P14/current/GVAR0044/"




# remove not identified fish
dados_bind <- dados_bind [which(is.na(dados_bind$scientificNameAccepted) !=T),]




# ----------------------------------------------------------------------------------------------
# adjust recordBy


dados_bind <- dados_bind %>% 
  mutate(recordedBy = plyr::mapvalues(recordedBy, 
                                      from = c("Thiago", "tc_mendes", "mendes_tc","thiago","thiago_mendes"  ,
                                               "Renato", "morais_ra","renato","ra_morais","renato_morais",
                                               "Gui","GOL","guilherme","guilherme_longo",
                                               "ide", "anaide" ,"anaide_aued",
                                               "ju",
                                               "luisa", "lu","luisa_fontoura",
                                               "juan", "quimbayo_jp","jp_quimbayo","juan_quimbayo",
                                               "davi",
                                               "edson",
                                               "renata","r_mazzei","renata_mazzei" ,
                                               "anderson_batista" , "batista_a",
                                               "cordeiro_camm", "cesar","camm_cordeiro" ,"cesar_cordeiro",
                                               "barbosa_m","mc_barbosa",
                                               "giglio_vj",
                                               "NCR",
                                               "JB",
                                               "GSG",
                                               "LE","l_eggertsen",    
                                               "KYI",
                                               "EAV",
                                               "MCP",
                                               "marina",
                                               "diego","diego_barneche",
                                               "roberta",
                                               "max","max_levy",
                                               "r_noguchi","ramon_noguchi",
                                               "cel_ferreira",
                                               "cgw_ferreira",
                                               "gugaw_ferreira",
                                               "gabriel_ferreira",
                                               "jl_gasparini",
                                               "jp_krajewski",
                                               "hudson_pinheiro",
                                               "ana_liedke",
                                               "sergio_floeter",
                                               "mb_lucena",
                                               "cbp_eirado-silva" ,
                                               NA,
                                               "go_correal"  ,     "gabriel_correal",
                                               "bertran_feitoza",
                                               "eduardo_godoy" ,   
                                               "ca_rangel",
                                               "claudio_sampaio",
                                               "thiony_simon",
                                               "tiago_albuquerque" ,
                                               "anchieta_nunes",
                                               "daniel_dinslaken"   ,
                                               "osmar_luiz",
                                               "marcelo_silveira"  , 
                                               "andrea_dalben" ,
                                               "alexandre_siqueira" ,
                                               "athila_bertoncini",
                                               "otavio_schlickmann",
                                               "lucas_nunes",
                                               "thiago_fiuza",
                                               "debora_ferrari",
                                               "angela_canterle"
                                      ),
                                      to = c("Thiago C Mendes","Thiago C Mendes","Thiago C Mendes","Thiago C Mendes","Thiago C Mendes",
                                             "Renato A Morais","Renato A Morais","Renato A Morais","Renato A Morais","Renato A Morais",
                                             "Guilherme O Longo","Guilherme O Longo","Guilherme O Longo","Guilherme O Longo",
                                             "Anaide W Aued","Anaide W Aued","Anaide W Aued",
                                             "Júlia Correia", 
                                             "Luísa Fontoura","Luísa Fontoura","Luísa Fontoura",
                                             "Juan P Quimbayo","Juan P Quimbayo","Juan P Quimbayo","Juan P Quimbayo",
                                             "Davi V Candido", 
                                             "Edson Faria Jr",
                                             "Renata CB Mazzei","Renata CB Mazzei","Renata CB Mazzei",
                                             "Anderson Batista","Anderson Batista",
                                             "Cesar AMM Cordeiro","Cesar AMM Cordeiro","Cesar AMM Cordeiro","Cesar AMM Cordeiro",
                                             "Moyses C Barbosa","Moyses C Barbosa",
                                             "Vinícius Giglio",
                                             "Natalia C Roos",
                                             "Jéssica Bleuel",
                                             "Gabriel Santos Garcia",
                                             "Linda Eggertsen","Linda Eggertsen",
                                             "Kelly Y Inagaki",
                                             "Edson A Vieira",
                                             "Maria Carolina Pacheco",
                                             "Marina N Sissini",
                                             "Diego R Barneche","Diego R Barneche",
                                             "Roberta Bonaldo",
                                             "Max Levy","Max Levy",
                                             "Ramon Noguchi","Ramon Noguchi",
                                             "Carlos EL Ferreira",
                                             "Carlos GW Ferreira",
                                             "Carlos GW Ferreira",
                                             "Gabriel Ferreira",
                                             "João L Gasparini",
                                             "João P Krajewski",
                                             "Hudson Pinheiro",
                                             "Ana MR Liedke",
                                             "Sérgio R Floeter",
                                             "Marcos B Lucena",
                                             "Clara BP Eirado-Silva" ,
                                             NA,
                                             "Gabriel O Correal"  ,   "Gabriel O Correal" ,  
                                             "Bertran Feitoza",
                                             "Eduardo Godoy",   
                                             "Carlos Rangel",
                                             "Claudio LS Sampaio",
                                             "Thiony Simon",
                                             "Tiago Albuquerque" ,
                                             "Anchieta Nunes",
                                             "Daniel Dinslaken"   ,
                                             "Osmar Luiz",
                                             "Marcelo Silveira"  , 
                                             "Andrea Dalben" ,
                                             "Alexandre C Siqueira",
                                             "Athila Bertoncini",
                                             "Otavio SR Cardoso",
                                             "Lucas T Nunes",
                                             "Thiago MJ Fiuza",
                                             "Débora S Ferrari",
                                             "Angela M Canterle")
  )
  )

# missing ids
dados_bind[which(dados_bind$recordedBy == ""),"recordedBy"] <- NA


# -----------------------------------------------------------------------------------
#  Formatted according to DwC








DF_eMOF <- dados_bind [,c("eventID", "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "measurementTypeID",
                          "measurementRemarks",
                          "begginingObservationTime",
                          "endingObservationTime")]

rownames(DF_eMOF)<-seq(1,nrow(DF_eMOF))

DF_occ <- dados_bind [,c("eventID", 
                         "occurrenceID",
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
                         "basisOfRecord")]

rownames(DF_occ)<-seq(1,nrow(DF_occ))

# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,verbatimLocality,site,locality) %>% 
                            
                            summarise(year = mean(as.numeric(year)),
                                      eventDate = mean(eventDate),
                                      eventTime = unique(eventTime),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country))
)




# save xlsx
# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)



# save
# txt format
write.csv(DF_occ, file =here("DwC_output",
                               "GLongo_spatialData",
                               "DF_occ.csv"))
write.csv(DF_eMOF, file =here("DwC_output",
                                "GLongo_spatialData",
                                "DF_eMOF.csv"))
write.csv(event_core, file =here("DwC_output",
                                   "GLongo_spatialData",
                                   "event_core.csv"))

rm(list=ls())
