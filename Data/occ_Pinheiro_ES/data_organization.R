
# ToDo:
# PS: Stil did not work on the spreadsheet of site variables (work for the future)


# load Pinheiro et al.  dataset
# ES - Fish
require(here); require(openxlsx); require(worrms); require(dplyr); require(reshape)


# fish data
ES_data_raw <- read.xlsx(here ("Data",
                                 "occ_Pinheiro_ES",
                                 "Litoral Sul ES.xlsx"),
                           sheet=1,
                           detectDates =T)

# following whatsapp messages of Hudson, 'ponto1' is the site, 'ponto2' and 'area' depict locality
# Message 1: POnto 1 é o arquipélago ou ilha ou naufrágio, maior escala, e ponto 2 é o local especifico dentro do arquipélago
# Message 2: Ponto 1 seria Site, e Ponto 2 locality
#### difference of ponto 1 and ponto 2: Isso, acho que só muda em relação as Ilhas rasas, que tem duas ilhas, a de dentro e a de fora
# Message 3: vendo aqui, area das ilhas seria locality tb
# Message 4: subarea seria microhabitat ou porção no costão ou recife artificial. No costão seria como raso, meio, interface.

# copy of the dataset to edit
ES_data <- ES_data_raw

# remove the last columns as they are parameters derived from counts and size estimates
ES_data <- ES_data[,-which(colnames (ES_data) %in% c(
                                                        "tamanho.medio.(cm)",
                                                        "FL.and.SL",
                                                        "A",
                                                        "B",
                                                        "biom_censo",
                                                        "Total.Biomass",
                                                        "abund_m2",
                                                        "biom_m2",
                                                        "peso_total_formula",
                                                        "Trophic.Guild",
                                                        "Family",
                                                        "Order",
                                                        "Ameaça",
                                                        "Captura"))]


# -------------------------------------------------------------------

# split abundance and size data

# first need to melt data to define 'size' as a variable (rows) not a descriptor (cols)
# I will use melt function of reshape package

abundance_size <- melt (ES_data, id.vars = c("seq_censo",
                                  "saida",
                                  "data",
                                  "municipio",
                                  "Ponto",
                                  "Ponto.1", 
                                  "Habitat",
                                  "GR_Bentos",
                                  "observador",
                                  "mergulho",
                                  "censo",
                                  "ambiente",
                                  "amb.resumido",
                                  "Região",
                                  "Area",
                                  "prof..(m)",
                                  "classe.prof..(m)",
                                  "rugosidade",
                                  "seq_sp",
                                  "especie",
                                  "observador",
                                 "observação",
                                   "abund_censo"))

# filter abundance
abundance_size <- abundance_size[,-21] %>%
  filter (is.na(value) != T) 

# event remark
abundance_size$measurementRemarks <- "Authors used categories of total length"

# The original count (abund_censo) will differ from the 'value' because the melt divides the number of individuals across size categories
# abundance_size$value == abundance_size$abund_censo

# abundance dataset
abundance <- abundance_size [,which(colnames(abundance_size) %in% c("variable", "abund_censo") != T)]

# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "value")] <- "measurementValue"



# size
# remove abundance to make 'melt' easier
size <- abundance_size [,-which(colnames(abundance_size) %in% c("value", "abund_censo"))]

# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "variable")] <- "measurementValue"


# check if all is ok
table(size$measurementValue == abundance_size$variable)
table(abundance$measurementValue == abundance_size$value)

# bind edited data
dados_bind <- rbind (abundance,
                     size)



# ------------------------------------------ 
# edit other variables

# adjust municipality
dados_bind$municipality <- tolower (dados_bind$municipio)
dados_bind$municipality[which(dados_bind$municipality == "marataízes")] <- "marataizes"

# define sites and localities
dados_bind$site <- dados_bind$municipality

# paste locality
# it was the combination of municipality and point
dados_bind$locality <- paste (dados_bind$municipality,
                              tolower (dados_bind$Ponto),
                                  sep = "_")

# change colnames
colnames (dados_bind)[which(colnames (dados_bind) == "Ponto")] <- "verbatimLocality"

# scientific names
colnames (dados_bind)[which(colnames (dados_bind) == "especie")] <- "verbatimIdentification"
dados_bind$verbatimIdentification<- tolower (dados_bind$verbatimIdentification)

# date
colnames (dados_bind)[which(colnames (dados_bind) == "data")] <- "eventDate"

# adjust dates
dados_bind$year <- format (dados_bind$eventDate, "%Y")
dados_bind$month <- format (dados_bind$eventDate, "%m")
dados_bind$day <- format (dados_bind$eventDate, "%d")

# observers
colnames (dados_bind)[which(colnames (dados_bind) == "observador")] <- "recordedBy"

# ----------------------------------------------------------------------------
# ADJUSTING Depth

depth <- strsplit(dados_bind$`classe.prof..(m)`, "-")
depth <- do.call(rbind.data.frame,depth) # melt
colnames(depth) <- c("minimumDepthinMeters", "maximumDepthinMeters")

# bind to the main dataset
dados_bind <- cbind (dados_bind,
                         depth)

dados_bind$minimumDepthinMeters <- as.numeric(dados_bind$minimumDepthinMeters)
dados_bind$maximumDepthinMeters <- as.numeric(dados_bind$maximumDepthinMeters)

# ------------------------------------------------------------------------------
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
                                               "hudson_pinheiro", "hudson",
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
                                               "thiony_simon","thiony",
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
                                             "Hudson Pinheiro","Hudson Pinheiro",
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
                                             "Thiony Simon","Thiony Simon",
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


# method
dados_bind$samplingProtocol <- "Underwater visual survey - 20 x 2m"

# effort
dados_bind$samplingEffort <- 1#"one observer per transect"

# sampleSizeValue (based on Minte-Vera et al. 2008 MEPS)
dados_bind$sampleSizeValue <- 40 # area

# sampleSizeUnit
dados_bind$sampleSizeUnit <- "squared meters"


# country and code
dados_bind$Country <- "Brazil"
dados_bind$countryCode <- "BR"
# basisOfRecord
dados_bind$basisOfRecord <- "HumanObservation"
# geodeticDatum
dados_bind$geodeticDatum <- "decimal degrees"
# geographic location
dados_bind$higherGeography <- "BrazilianCoast"


# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# coordinates
# extracted using google earth.
# overlaping the picture of page 20 of this document on the google  earth map
# https://hudsonpinheiro.files.wordpress.com/2015/11/diagnc3b3stico-ambiental-do-litoral-sul-do-es-voz-da-natureza.pdf

coords <- read.xlsx(here ("Data",
                          "occ_Pinheiro_ES",
                          "coords_pts.xlsx"),
                    sheet=1,
                    detectDates =T)

# bind lat and long
dados_bind$decimalLatitude <- coords$decimalLatitude [match (dados_bind$locality,coords$locality)]
dados_bind$decimalLongitude <- coords$decimalLongitude [match (dados_bind$locality,coords$locality)]


# ----------------------------------------------------------------------------
# taxonmic checking 
# remove NAs
dados_bind <- dados_bind [which(is.na(dados_bind$verbatimIdentification)!= T),]

# species to search
species_to_search <- unique(dados_bind$verbatimIdentification)
species_to_search<-  (species_to_search[order(species_to_search)])

# matching with worms
worms_record <- lapply (species_to_search, function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# data to match
data_to_match <- data.frame(originalname=species_to_search,
                            lsid=df_worms_record$lsid,
                            scientificname=df_worms_record$scientificname,
                            scientificNameID = df_worms_record$lsid,
                            taxonRank=df_worms_record$rank,
                            kingdom=df_worms_record$kingdom,
                            phylum=df_worms_record$phylum,
                            class=df_worms_record$class,
                            order=df_worms_record$order,
                            family=df_worms_record$family,
                            genus=df_worms_record$genus)

# valid name WORMS
dados_bind$scientificNameAccepted <- (data_to_match$scientificname [match (dados_bind$verbatimIdentification,
                                                                           (data_to_match$originalname))])

# id
dados_bind$scientificNameID<-(data_to_match$lsid [match (dados_bind$verbatimIdentification,
                                                       (data_to_match$originalname))])

# taxon rank of the identified level
dados_bind$taxonRank <- (data_to_match$taxonRank [match (dados_bind$verbatimIdentification,
                                                      (data_to_match$originalname))])

# kingdom
dados_bind$kingdom <-(data_to_match$kingdom [match (dados_bind$verbatimIdentification,
                                                      (data_to_match$originalname))])

# phylum
dados_bind$phylum <-(data_to_match$phylum [match (dados_bind$verbatimIdentification,
                                                      (data_to_match$originalname))])

# class
dados_bind$class<-(data_to_match$class [match (dados_bind$verbatimIdentification,
                                                 data_to_match$originalname)])
# order
dados_bind$order<-(data_to_match$order [match (dados_bind$verbatimIdentification,
                                                 (data_to_match$originalname))])

# family
dados_bind$family<-(data_to_match$family [match (dados_bind$verbatimIdentification,
                                                (data_to_match$originalname))])

# genus
dados_bind$genus<-(data_to_match$genus [match (dados_bind$verbatimIdentification,
                                                 (data_to_match$originalname))])

# taxonomic updates
# species
dados_bind$scientificNameAccepted[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina multilineata"
dados_bind$scientificNameAccepted[grep ("polygonius", dados_bind$scientificNameAccepted)] <- "Acanthostracion polygonium"
dados_bind$scientificNameAccepted[grep ("Dasyatis centroura", dados_bind$scientificNameAccepted)] <- "Bathytoshia centroura"
dados_bind$scientificNameAccepted[grep ("Haemulon plumieri", dados_bind$scientificNameAccepted)] <- "Haemulon plumierii"
dados_bind$scientificNameAccepted[grep ("Labrisomus kalisherae", dados_bind$scientificNameAccepted)] <- "Goblioclinus kalisherae"
dados_bind$scientificNameAccepted[grep ("Haemulon steindachneri", dados_bind$scientificNameAccepted)] <- "Haemulon atlanticus"
dados_bind$scientificNameAccepted[grep ("Pempheris schomburgki", dados_bind$scientificNameAccepted)] <- "Pempheris schomburgkii"
dados_bind$scientificNameAccepted[grep ("Malacoctenus delalandei", dados_bind$scientificNameAccepted)] <- "Malacoctenus delalandii"
dados_bind$scientificNameAccepted[grep ("Sphoeroides spengleri", dados_bind$scientificNameAccepted)] <- "Sphoeroides camila"

# genus
dados_bind$genus[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina"
dados_bind$genus[grep ("Bathytoshia centroura", dados_bind$scientificNameAccepted)] <- "Bathytoshia"
dados_bind$genus[grep ("Goblioclinus kalisherae", dados_bind$scientificNameAccepted)] <- "Goblioclinus"

# adjust family
dados_bind$family[which(dados_bind$family == "Scaridae")] <- "Labridae"


dados_bind[is.na(dados_bind$class),]
dados_bind[is.na(dados_bind$scientificNameAccepted),]


# ----------------------------------------------------------------------------
# CREATING IDS


# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste (paste ("BR:ReefSYN:SouthernEspiritoSanto-ES:", 
                                                 dados_bind$higherGeography,
                                                 sep=""),
                                          dados_bind$site,sep=":"), 
                                           dados_bind$locality, 
                                          dados_bind$year,
                              sep="_")


# creating eventIds
dados_bind$eventID <- paste (paste (paste ("BR:ReefSYN:SouthernEspiritoSanto-ES:", 
                                           dados_bind$higherGeography,
                                           sep=""),
                                    dados_bind$site,sep=":"),  
                                    dados_bind$locality, 
                                    dados_bind$year,
                                    dados_bind$seq_censo, # check this
                        sep="_")


# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste (paste ("BR:ReefSYN:SouthernEspiritoSanto-ES:", 
                                                dados_bind$higherGeography,
                                                sep=""),
                                         dados_bind$site,sep=":"),  
                                  dados_bind$locality, 
                                  dados_bind$year,
                                  dados_bind$seq_censo,# check this
                             
                                  paste ("occ",seq(1,
                                                   nrow(dados_bind %>% 
                                                          filter (measurementType == "abundance"))),sep=""),
                             
                             sep="_")


# licence
dados_bind$licence <- "CC BY-NC"

# language
dados_bind$language <- "en"


# edit habitat (reef type)
dados_bind$habitat <- dados_bind$Habitat
dados_bind$habitat <- recode(dados_bind$habitat, 
       "Recifes rochosos" = "Rocky reefs",
       "Recifes biogênicos" = "Biogenic reefs",
       "Fundo biogênico" = "Biogenic bottom",
       "Fundo de algas" = "Seaweed bottom")



# sites into locations
colnames(dados_bind)[which(colnames(dados_bind) == "site")] <- "location"


# ----------------------------------------------------------------------------
#  Formatted according to DwC




DF_eMOF <- dados_bind [,c("eventID", "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit",
                          "measurementRemarks"
                          #"eventRemarks"
                          )]



DF_occ <- dados_bind [,c("eventID", 
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
                         "language"#,
                         #"bibliographicCitation"
                         )]



# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,location,verbatimLocality,locality) %>% 
                            
                            summarise(year = unique(year),
                                      eventDate = unique(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      habitat = unique(habitat),
                                      decimalLongitude = mean(decimalLongitude,na.rm=T),
                                      decimalLatitude =mean(decimalLatitude,na.rm=T),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)



# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)



# save
dir.create(here ("DwC_output", "Pinheiro_ES"))

# write to txt format
write.csv(DF_occ, file =here("DwC_output",
                               "Pinheiro_ES",
                               "DF_occ.csv"),fileEncoding = "latin1")

write.csv(DF_eMOF, file =here("DwC_output",
                                "Pinheiro_ES",
                                "DF_eMOF.csv"),fileEncoding = "latin1")


write.csv(event_core, file =here("DwC_output",
                                   "Pinheiro_ES",
                                   "event_core.csv"),fileEncoding = "latin1")


## end
rm(list=ls())
