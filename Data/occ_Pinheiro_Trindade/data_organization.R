
# -------------------------------------------
 
# martin vaz and trindade
# fish

# snapshots prior peld-iloc

# ToDo:
# PS: Still did not work on the spreadsheet of site variables (work for the future)


# load Pinheiro, Joyeaux et al.  dataset
require(here); require(openxlsx); require(worrms); require(dplyr); require(reshape)


# fish data
TMVaz_data_raw <- read.xlsx(here ("Data",
                                 "occ_Pinheiro_Trindade",
                                 "Base Trindade 2007 e 2009.xlsx"),
                           sheet="Base",
                           detectDates =T)

# following whatsapp messages of Hudson, 'ponto1' is the site, 'ponto2' and 'area' depict locality
# Message 1: POnto 1 é o arquipélago ou ilha ou naufrágio, maior escala, e ponto 2 é o local especifico dentro do arquipélago
# Message 2: Ponto 1 seria Site, e Ponto 2 locality
#### difference of ponto 1 and ponto 2: Isso, acho que só muda em relação as Ilhas rasas, que tem duas ilhas, a de dentro e a de fora
# Message 3: vendo aqui, area das ilhas seria locality tb
# Message 4: subarea seria microhabitat ou porção no costão ou recife artificial. No costão seria como raso, meio, interface.

# copy of the dataset to edit
TMVaz_data <- TMVaz_data_raw

# change colnames
colnames(TMVaz_data)[which(colnames(TMVaz_data) == "quantidade")] <- "abundance"
colnames(TMVaz_data)[which(colnames(TMVaz_data) == "TL")] <- "total length"
colnames(TMVaz_data)[which(colnames(TMVaz_data) == "espécie")] <- "verbatimIdentification"
colnames(TMVaz_data)[which(colnames(TMVaz_data) == "Ano")] <- "year"
colnames(TMVaz_data)[which(colnames(TMVaz_data) == "Habitat")] <- "habitat"
TMVaz_data$habitat <- recode(TMVaz_data$habitat, 
                             "Costão" = "Rocky shore",
                             "costão" = "Rocky shore",
                             "Patch" = "Reef patch",
                             "Plato de tras" = "Fringing reef (back)", # check
                             "Plato da borda" = "Fringing reef (crest)") # check

# remove some columns we don't need
TMVaz_data <- TMVaz_data[,-which(colnames(TMVaz_data) %in% c("G.trofico","complexidade","familia",
                                                             "A","B", "peso.especiefico","Peso.total", "X21",
                                                             "conversão"))]

# abundance dataset
abundance <- TMVaz_data [,which(colnames(TMVaz_data) %in% c("total length") != T)]

# measurementType
abundance$measurementType <- "abundance"
# organismQuantityType
abundance$organismQuantityType <- "abundance"
# measurementUnit
abundance$measurementUnit <- "individuals"
# measurementValue
colnames(abundance)[which(colnames(abundance) == "abundance")] <- "measurementValue"



# size
# remove abundance to make 'melt' easier
size <- TMVaz_data [,-which(colnames(TMVaz_data) %in% c("abundance"))]

# measurementType
size$measurementType <- "total length"
# organismQuantityType
size$organismQuantityType <- "total length"
# measurementUnit
size$measurementUnit <- "cm"
# measurementValue
colnames(size)[which(colnames(size) == "total length")] <- "measurementValue"



# bind edited data
dados_bind <- rbind (abundance,
                     size)


# ----------------------------------------------------------------------------
# ADJUSTING Depth
dados_bind$minimumDepthinMeters <- dados_bind$profundidade
dados_bind$maximumDepthinMeters <- dados_bind$profundidade

# ------------------------------------------ 
# edit other variables

# define sites and localities
dados_bind$site <- "NA"

# paste locality
# it was the combination of municipality and point
dados_bind$verbatimLocality <- dados_bind$local
dados_bind$locality <- tolower (dados_bind$local)

# recode sites
dados_bind$site <- recode(dados_bind$locality,
        "lixo"               = "trindade",
        "martin vaz (oeste)"  = "martin vaz",
        "calheta"            = "trindade",
        "tartarugas"      =  "trindade",
        "parcel"            =  "trindade" ,
        "cabritas"           =  "trindade" ,
        "racha"              =  "trindade" ,
        "parcel"             =  "trindade" ,
        "tunel"              =  "trindade" ,
        "ponta norte"        =  "trindade" ,
        "orelhas"           =  "trindade" ,
        "cabritas"           =  "trindade" ,
        "farol"            =  "trindade" ,
        "eme" = "trindade")

# recode localities
dados_bind$locality <- recode(dados_bind$locality,
                          "lixo"               = "lixo",
                          "martin vaz (oeste)"  = "martin_vaz_oeste",
                          "calheta"            = "calheta",
                          "tartarugas"      =  "tartarugas_trindade",
                          "parcel"            =  "parcel" ,
                          "cabritas"           =  "praia_das_cabritas" ,
                          "racha"              =  "racha" ,
                          "parcel"             =  "parcel" ,
                          "tunel"              =  "tunel" ,
                          "ponta norte"        =  "ponta_norte" ,
                          "orelhas"           =  "orelhas" ,
                          "cabritas"           =  "praia_das_cabritas" ,
                          "farol"            =  "farol" ,
                          "eme" = "eme")


# method
dados_bind$samplingProtocol <- "Underwater visual survey - 20 x 2m"

# effort
dados_bind$samplingEffort <- 1#"one observer per transect"

# sampleSizeValue
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
dados_bind$higherGeography <- "BrazilianOceanicIslands"


# occurrenceStatus
dados_bind$occurrenceStatus <- "presence"

# coordinates are missing

# ----------------------------------------------------------------------------

# adjust scientific names
dados_bind$verbatimIdentification<- tolower (dados_bind$verbatimIdentification)


# recode abbreviations
dados_bind$verbatimIdentification<- recode (dados_bind$verbatimIdentification, 
  "abu sax"    = "Abudefduf saxatilis",
  "aca bah"    = "Acanthurus bahianus",
  "aca coe"    = "Acanthurus coeruleus",
  "aca pol"    = "Acanthostracion polygonium",
  "aca qua"    = "Acanthostracion quadricornis",
  "alu scr"    = "Aluterus scriptus",
  "amb pin"    = "Amblycirrhitus pinos",
  "ani sur"    = "Anisotremus surinamensis",
  "apo ame"    = "Apogon americanus",
  "bal vet" = "Balistes vetula",
  "blenniidae" = "Blenniidae",
  "bod pul"    = "Bodianus pulchellus",
  "bod ruf"    = "Bodianus rufus",
  "can fig"    = "Canthigaster figueiredoi",
  "can mac"    = "Canthidermis maculata",
  "can pul"    = "Cantherhines pullus",
  "can suf"    = "Canthidermis sufflamen",
  "car cry"    = "Caranx crysos",
  "car lat"    = "Caranx latus",
  "car lug"  = "Caranx lugubris",
  "car rub"= "Caranx ruber",
  "cep ful"    = "Cephalopholis fulva",
  "cha str"    = "Chaetodon striatus",
  "chr mul"    = "Chromis multilineata",
  "clupeidae"  = "Clupeidae",
  "cory sp"    = "Coryphopterus sp.",
  "dac vol"    = "Dactylopterus volitans",
  "dio hol"    = "Diodon holocanthus",
  "dip arg"    = "Diplodus argenteus",
  "ech cat"= "Echidna catenata",
  "ela pri"   = "Elacatinus pridisi",
  "enc car"    = "Enchelycore carychroa",
  "enc nig"    = "Enchelycore nigricans",
  "ent sp"     = "Entomacrodon sp.",
  "epi ads"    = "Epinephelus adscensionis",
  "gna tho"= "Gnatholepis thompsoni",
  "gym mil"    = "Gymnothorax miliaris",
  "gym mor"    = "Gymnothorax moringa", # OU "Gymnothorax mordax"
  "gyn cir"    = "Gynglimostoma cirratum",
  "hal bra" = "Halichoeres brasiliensis",
  "hal pen"   = "Halichoeres penrosei", 
  "hal poe"= "Halichoeres poeyi",
  "hal sp"= "Halichoeres sp.",
  "hal sp "= "Halichoeres sp.",
  "hem bra"    ="Hemiramphus brasiliensis",
  "het cru"    = "Heteropriacanthus cruentatus",
  "hol ads"    = "Holocentrus adscensionis",
  "hol tri"    = "Holacanthus tricolor",
  "kyph sp"    = "Kyphosus sp.",
  "lab nuq"= "Labrisomus nuchipinnis",
  "lagosta"   = "Panulirus sp.",
  "mal plu"    = "Malacanthus plumieri",
  "mal sp"     = "Malacanthus sp.",
  "mel nig"    = "Melichthys niger",
  "menephorus" = "Menephorus punticulatus", # hybrid (adjust after) otherwise worms will report two (wrong) matches
  "Menephorus" = "Menephorus punticulatus", # hybrid (adjust after)
  "mic chr"    = "Microspathodon chrysurus",
  "mul mar"    = "Mulloidichthys martinicus",
  "myc int"    = "Mycteroperca interstitialis",
  "myc vem"= "Mycteroperca venenosa",
  "myr bre"  = "Myrichthys breviceps",
  "myr jac" = "Myripristis jacobus",
  "oph atl"    = "Ophioblennius atlanticus",
  "opi aur"    = "Opistognathus aurifrons",
  "par bai"    = "Paradiplogramus bairdi",
  "par fur"    = "Paranthias furcifer",
  "pla arg"    = "Platybelone argalus",
  "pro bra"    = "Prognathodes brasiliensis",
  "pse mac"    = "Pseudupeneus maculatus",
  "ryp sap" = "Rypticus saponaceus",
  "sar bul"= "Sargocentron bullisi",
  "sco plu"    = "Scorpaena plumieri",
  "ser riv"    = "Seriola rivoliana",
  "spa amp"= "Sparisoma amplum",
  "spa axi"    = "Sparisoma axillare",
  "spa pit"= "Scarinae", # still need to check
  "sph bar"    = "Sphyraena barracuda",
  "sph spe"    = "Sphoeroides spengleri",
  "ste pic"    = "Stegastes pictus",
  "ste tri"   = "Stegastes trindadensis",
  "syn syn"    = "Synodus synodus",
  "tha nor" = "Thalassoma noronhanum"
)  

# taxonmic checking 
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
dados_bind$scientificNameAccepted[grep ("Sphoeroides spengleri", dados_bind$scientificNameAccepted)] <- "Sphoeroides camila"

# genus
dados_bind$genus[grep ("multilineata", dados_bind$scientificNameAccepted)] <- "Azurina"
dados_bind$genus[grep ("Bathytoshia centroura", dados_bind$scientificNameAccepted)] <- "Bathytoshia"
dados_bind$genus[grep ("Goblioclinus kalisherae", dados_bind$scientificNameAccepted)] <- "Goblioclinus"


# adjust family
dados_bind$family[which(dados_bind$family == "Scaridae")] <- "Labridae"



# bind identification remarks
# Sergio: Menephorus é um nome utilizado para o híbrido entre 
# Sergio: Deixar ele na base de dados pois é um cara interessante sim de ter nas planilhas, e colocar assim: Menephorus (hybrid)

dados_bind[is.na(dados_bind$class),"verbatimIdentification"] <- "Menephorus (hybrid)"
dados_bind$identificationRemarks <- NA
dados_bind [is.na(dados_bind$class),"identificationRemarks"] <- "Menephorus is an hybrid between Cephalopholis fulva and Paranthias furcifer"
dados_bind [is.na(dados_bind$class),"taxonRank"] <- "Hybrid"
dados_bind [is.na(dados_bind$class),"kingdom"] <- "Animalia"
dados_bind [is.na(dados_bind$class),"phylum"] <- "Chordata"
dados_bind [is.na(dados_bind$class),"class"] <- "Teleostei"

# remove panulirus sp. (Malacostraca)
dados_bind <- dados_bind %>% 
  filter (class != "Malacostraca")




# ----------------------------------------------------------------------------
# ADJUSTING GEOGRAPHIC COORDINATES



# coordinates (gather coordinates from PELD Dataset)
PELD_coords <- read.csv (here ("\\.","Pos_Doc_Sinbiose", "ReefSYN_data", "DwC_output","PELD_iloc_fish","event_core.csv"))
PELD_coords_trindade <- PELD_coords %>% 
                      
  filter (island == "trindade")  
# aggregate at locality level
PELD_coords_trindade <- data.frame (decimalLatitude = tapply(PELD_coords_trindade$decimalLatitude,
               PELD_coords_trindade$locality,"mean"),
            decimalLongitude = tapply(PELD_coords_trindade$decimalLongitude,
                                      PELD_coords_trindade$locality,"mean"))

# match
dados_bind <- cbind (dados_bind,
                    PELD_coords_trindade [match (dados_bind$locality,
                   rownames(PELD_coords_trindade)),])

dados_bind$georeferenceRemarks <-  "Coordinates gathered from PELD dataset, Dataset IV"


# ----------------------------------------------------------------------------
# CREATING IDS

# IDs
# creating parentIDs
dados_bind$parentEventID <- paste (paste (paste ("BR:ReefSYN:TrindadeMartinVaz:", 
                                                 dados_bind$higherGeography,
                                                 sep=""),
                                          dados_bind$site,sep=":"), 
                                           dados_bind$locality, 
                                          dados_bind$year,
                              sep="_")


# creating eventIds
dados_bind$eventID <- paste (paste (paste ("BR:ReefSYN:TrindadeMartinVaz:", 
                                           dados_bind$higherGeography,
                                           sep=""),
                                    dados_bind$site,sep=":"),  
                                    dados_bind$locality, 
                                    dados_bind$year,
                                    dados_bind$seq_censo, # check this
                        sep="_")


# creating occurrenceIDs
dados_bind$occurrenceID <- paste (paste (paste ("BR:ReefSYN:TrindadeMartinVaz:", 
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

# eventRemarks
dados_bind$bibliographicCitation <- "Pinheiro H, Camilato JLV, Joyeaux J-C. (2009) New records of fishes for Trindade-Martin Vaz oceanic insular complex, Brazil. Zootaxa, 2298. DOI: 10.11646/zootaxa.2298.1.3"

# recordedBy
dados_bind$recordedBy <- "Hudson Pinheiro | Jean-Christophe Joyeaux"


# ----------------------------------------------------------------------------
#  Formatted according to DwC




DF_eMOF <- dados_bind [,c("eventID", "occurrenceID",
                          "measurementValue", 
                          "measurementType",
                          "measurementUnit"#,
                          #"measurementRemarks"
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
                         "identificationRemarks",
                         "recordedBy",
                         "organismQuantityType", 
                         "occurrenceStatus",
                         "licence",
                         "language",
                         "bibliographicCitation"
                         )]



# aggregate data by eventIDs to have event_core

event_core <- data.frame (group_by(dados_bind, eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = unique(year),
                                      #eventDate = unique(eventDate),
                                      minimumDepthinMeters = mean(minimumDepthinMeters),
                                      maximumDepthinMeters = mean(maximumDepthinMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = mean(samplingEffort),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      habitat = unique(habitat),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      georeferenceRemarks = unique(georeferenceRemarks),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)



# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)



# save
dir.create(here ("DwC_output", "Pinheiro_TrindadeMVaz"))

# write to txt format
write.csv(DF_occ, file =here("DwC_output",
                               "Pinheiro_TrindadeMVaz",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "Pinheiro_TrindadeMVaz",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "Pinheiro_TrindadeMVaz",
                                   "event_core.csv"))


## end
rm(list=ls())
