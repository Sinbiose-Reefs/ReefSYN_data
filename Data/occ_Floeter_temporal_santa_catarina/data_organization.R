
# load Floeter et al. data

# SC time series
require(here); require(openxlsx); require(worrms); require(dplyr)



# fish data
sc_time_series <- read.csv(here ("Data",
                                 "occ_Floeter_temporal_santa_catarina",
                                 "censos_sc.csv"),
                           sep=";")

# location
sc_location <- read.xlsx(here ("Data",
                                 "occ_Floeter_temporal_santa_catarina",
                                 "location.xlsx"),
                           sep=";")




# ----------------------------------------------------------------------------
# ADJUSTING MEASUREMENT VALUE





# split abundance and size data
abundance <- sc_time_series[,which(colnames(sc_time_series) != "total_lenght")] # abundance
# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abundance")] <- "measurementValue"

# size
size <- sc_time_series[,which(colnames(sc_time_series) != "abundance")]
size$total_lenght <- as.numeric(gsub (",", ".",size$total_lenght))
# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "total_lenght")] <- "measurementValue"

# bind edited data
dados_bind <- rbind (abundance,
                     size)

# method
dados_bind$samplingProtocol <- "Underwater visual survey - 20 x 2m"
# effort
dados_bind$samplingEffort <- 1#"one observer per transect"
# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS)
dados_bind$sampleSizeValue <- 40 # area
# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"
# recordedBy
colnames(dados_bind)[which(colnames(dados_bind) == "observer")] <- "recordedBy"
# depth
colnames(dados_bind)[which(colnames(dados_bind) == "depth")] <- "depthInMeters"
# into number
dados_bind$depthInMeters<- gsub(",",".",dados_bind$depthInMeters)
dados_bind$depthInMeters <- as.numeric((dados_bind$depthInMeters))
# set min and max
colnames(dados_bind)[which(colnames(dados_bind) == "depthInMeters")] <- "minimumDepthinMeters"
dados_bind$maximumDepthinMeters <- dados_bind$minimumDepthinMeters
# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# coordinates
dados_bind$decimalLongitude<- gsub(",",".",dados_bind$longitude)# long
dados_bind$decimalLongitude <- as.numeric((dados_bind$decimalLongitude))
dados_bind$decimalLatitude<- gsub(",",".",dados_bind$latitude) #lat
dados_bind$decimalLatitude <- as.numeric((dados_bind$decimalLatitude))

# bind coordinates from moleques_do_sul (missing in data shared by SFloeter)
# coordinates based on Quimbayo et al. 2022
# https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.3966
unique(dados_bind[grep ("moleques",dados_bind$location),"longitude"])
unique(dados_bind[grep ("moleques",dados_bind$location),"latitude"])

# bind
dados_bind[grep ("moleques",dados_bind$location),"decimalLatitude"] <- -27.844774
dados_bind[grep ("moleques",dados_bind$location),"decimalLongitude"] <- -48.432854	



# ----------------------------------------------------------------------------
# ADJUSTING COLNAMES, DATES



# scientificName
colnames(dados_bind)[which(colnames(dados_bind) == "species")] <- "scientificName"
# locality
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "locality"
# more general == site
colnames(dados_bind)[which(colnames(dados_bind) == "location")] <- "site"

# editing month
dados_bind$month [which(dados_bind$month == "december")] <- 12
dados_bind$month [which(dados_bind$month == "april")] <- 04
dados_bind$month [which(dados_bind$month == "march")] <- 03
dados_bind$month [which(dados_bind$month == "january")] <- 01
dados_bind$month [which(dados_bind$month == "february")] <- 02
dados_bind$month [which(dados_bind$month == "november")] <- 11
dados_bind$month [which(dados_bind$month == "may")] <- 05
dados_bind$month [which(dados_bind$month == "july")] <- 07
dados_bind$month [which(dados_bind$month == "august")] <- 08
dados_bind$month [which(dados_bind$month == "september")] <- 09
dados_bind$month [which(dados_bind$month == "october")] <- 10

# adjust day
dados_bind$day <- ifelse(dados_bind$day < 10, 
                         paste0("0", dados_bind$day),
                         dados_bind$day)

# eventDate
dados_bind$eventDate <- as.Date (paste(dados_bind$year, 
               dados_bind$month,
               dados_bind$day,sep="-"))



# country and code
dados_bind$Country <- "Brazil"
dados_bind$countryCode <- "BR"
# basisOfRecord
dados_bind$basisOfRecord <- "HumanObservation"
# geodeticDatum
dados_bind$geodeticDatum <- "decimal degrees"
# geographic location
dados_bind$higherGeography <- "BrazilianCoast"





# ----------------------------------------------------------------------------
# ADJUSTING SITES




# verbatimLocality
dados_bind$verbatimLocality<- dados_bind$locality


# adjusting species scientific name
# replacing "_" by " "
dados_bind$locality <-(iconv(dados_bind$locality, "ASCII", "UTF-8", sub=""))
dados_bind$locality <- tolower(dados_bind$locality)


# adjust
dados_bind$locality[which(dados_bind$locality == "saco_da_agua")] <- "saco_dagua"
dados_bind$locality[which(dados_bind$locality == "saco_do_capim")] <- "capim"
dados_bind$locality[which(dados_bind$locality == "saco_do_engenho")] <- "engenho"
unique(dados_bind$locality )[order(unique(dados_bind$locality ))]
unique(dados_bind$site )[order(unique(dados_bind$site))]
# adjust site
dados_bind$site[which(dados_bind$site == "caixa_da\xe7o_beach")] <- "caixadaco_beach"





# ----------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAME



# replace "_"
dados_bind$verbatimIdentification <- gsub ("_", " ", dados_bind$scientificName)
dados_bind$species_to_search <- gsub ("_", " ", dados_bind$scientificName)


# solve a couple of problems in identification
dados_bind$species_to_search [grep ("decapteru macarellus", dados_bind$species_to_search)] <- "decapterus macarellus"

# remove stegastes partitus (comment Sergio: Temos amostras do Caribe nesse datapaper? Ste partitus é só do Caribe. Tem um registro só em SC, mas não é para estar em nenhuma amostra, double check it)

dados_bind <- dados_bind [-grep("partitus",dados_bind$species_to_search),]

# non identified species
dados_bind$identificationQualifier <- ifelse (sapply (strsplit (dados_bind$verbatimIdentification, " "), "[[", 2) == "sp",
        "sp",
        NA)

# species to search
dados_bind$species_to_search [which(dados_bind$identificationQualifier == "sp")] <- gsub (" sp",
                                                                                          "",
                                                                                          dados_bind$species_to_search [which(dados_bind$identificationQualifier == "sp")])

# matching with worms
worms_record <- lapply (unique(dados_bind$species_to_search), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))


# valid name WORMS
dados_bind$scientificNameAccepted <- (df_worms_record$scientificname [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])

# id
dados_bind$scientificNameID<-(df_worms_record$lsid [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
dados_bind$taxonRank <- (df_worms_record$rank [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])

# kingdom
dados_bind$kingdom <-(df_worms_record$kingdom [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])
# phylum
dados_bind$phylum <-(df_worms_record$phylum [match (dados_bind$species_to_search,
                                                      tolower (df_worms_record$scientificname))])


# class
dados_bind$class<-(df_worms_record$class [match (dados_bind$species_to_search,
                                                 tolower(df_worms_record$scientificname))])
# order
dados_bind$order<-(df_worms_record$order [match (dados_bind$species_to_search,
                                                 tolower(df_worms_record$scientificname))])

# family
dados_bind$family<-(df_worms_record$family [match (dados_bind$species_to_search,
                                                   tolower(df_worms_record$scientificname))])

# genus
dados_bind$genus<-(df_worms_record$genus [match (dados_bind$species_to_search,
                                                   tolower(df_worms_record$scientificname))])



# taxonomic updates
# species
dados_bind$scientificNameAccepted[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina multilineata"
dados_bind$scientificNameAccepted[grep ("bartholomaei", dados_bind$scientificNameAccepted)] <- "Caranx bartholomaei"
dados_bind$scientificNameAccepted[grep ("polygonius", dados_bind$scientificNameAccepted)] <- "Acanthostracion polygonium"
dados_bind$scientificNameAccepted[grep ("Dasyatis americana", dados_bind$scientificNameAccepted)] <- "Hypanus berthalutzae"
dados_bind$scientificNameAccepted[grep ("Haemulon steindachneri", dados_bind$scientificNameAccepted)] <- "Haemulon atlanticus"
dados_bind$scientificNameAccepted[grep ("Sphoeroides spengleri", dados_bind$scientificNameAccepted)] <- "Sphoeroides camila"


# genus
dados_bind$genus[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina"
dados_bind$genus[grep ("bartholomaei", dados_bind$scientificNameAccepted)] <- "Caranx"
dados_bind$genus[grep ("Hypanus berthalutzae", dados_bind$scientificNameAccepted)] <- "Hypanus"


# adjust family
dados_bind$family[which(dados_bind$family == "Scaridae")] <- "Labridae"



# ----------------------------------------------------------------------------
# CREATING IDS







# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                                 dados_bind$higherGeography,
                                                 sep=""),
                                          dados_bind$site,sep=":"), 
                                           dados_bind$locality, 
                                          dados_bind$year,
                              sep="_")



# creating eventIds
dados_bind$eventID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                           dados_bind$higherGeography,
                                           sep=""),
                                    dados_bind$site,sep=":"),  
                                    dados_bind$locality, 
                                    dados_bind$year,
                                    substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                        sep="_")




# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste (paste ("BR:ReefSYN:SC-TIME-SERIES:", 
                                                dados_bind$higherGeography,
                                                sep=""),
                                         dados_bind$site,sep=":"),  
                                  dados_bind$locality, 
                                  dados_bind$year,
                                  substr (dados_bind$transect_id,6,nchar(dados_bind$transect_id)),
                                  paste ("occ",seq(1,
                                                   nrow(dados_bind %>% 
                                                          filter (measurementType == "abundance"))),sep=""),
                             sep="_")




# licence
dados_bind$licence <- "CC BY-NC"

# language
dados_bind$language <- "en"

# eventRemarks
dados_bind$eventRemarks <- "Just published by Quimbayo et al. in Ecology"

dados_bind$bibliographicCitation <- "Quimbayo, J. P., Nunes, L. T., Silva, F. C., Anderson, A. B., Barneche, D. R., Canterle, A. M., Cord, I., Dalben, A., Ferrari, D. S., Fontoura, L., Fiuza, T. M. J., Liedke, A. M. R., Longo, G. O., Morais, R. A., Siqueira, A. C., & Floeter, S. R. (2023). TimeFISH: Long-term assessment of reef fish assemblages in a transition zone in the Southwestern Atlantic. Ecology, 104(3), [e3966]. https://doi.org/10.1002/ecy.3966"

# -------------------------------------------------------


# ajust recordedBy



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

# ----------------------------------------------------------------------------
#  Formatted according to DwC




DF_eMOF <- dados_bind [,c("eventID", 
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "eventRemarks")]



DF_occ <- dados_bind [,c("eventID", 
                         "occurrenceID",
                         "basisOfRecord",
                         "verbatimIdentification",
                         "scientificNameID",
                         "scientificName",
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

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = mean(year),
                                      eventDate = mean(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
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



# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)



# save
# write to txt format
write.csv(DF_occ, file =here("DwC_output",
                               "SC_time_series",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "SC_time_series",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "SC_time_series",
                                   "event_core.csv"))

## end
rm(list=ls())
