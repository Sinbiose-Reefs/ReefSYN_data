

## codes for formatting benthic data of Aued et al. 2018, 2019
## Data also available at DRYAD repository: https://doi.org/10.5061/dryad.f5s90

require(here); require(dplyr); require(worrms); require (reshape)
## load benthic dataset from Aued et al. 2018 (PloSOne)

bentos_amostras <- read.csv (here ("Data", 
                                   "occ_Aued_et_al",
                                   "compiled_quadrats_allsites.csv"),
                             h=T, fill=T,
                             fileEncoding = "Latin1", check.names = T)

# ALLuza modified in excel because it was too difficult to separate events from video ids
bentos_variaveis <- openxlsx::read.xlsx(here("Data", "occ_Aued_et_al",
                                   "Compiled_quadrats_allsites_Updated_ALLuza_v1.xlsx"))

# ----------------------------------------------------------------------------------------------
# ORGANIZING DATES, YEARS ...




### extract data and other info from the original samples ID
# amostras_bentos <- do.call(rbind, strsplit (as.character (bentos$Samples),"/",fixed=T))

# transforming data into long format
# 8 = the first col with taxon data in the original data of Aued et al.
#bentos_long_format <- lapply (seq(8, ncol(bentos_amostras)), function (i) {
#  
#  subset_data <- bentos_variaveis [,c(1:13)] ## sampling descriptors
#  
#  subset_data <- cbind(subset_data, ## bind into the descriptors the
#                       sp=colnames(bentos_amostras)[i], # taxon and its
#                       bentos_amostras[,i]) ## relative cover
#  
#  
#  
#  
#  
#  bentos_long <- melt (subset_data,id=subset_data$modifiedSamples#colnames(subset_data)[1:14]) # transf to  long format
#
#}
#)

bentos_long_format <- bentos_variaveis# do.call (rbind, bentos_long_format) # melt the list
#bentos_long_format <- bentos_long_format [, -grep("variable", colnames(bentos_long_format))] # rm the just created col

# verbatim dates
bentos_long_format$verbatimEventDate <- bentos_long_format$data

## adjusting dates
bentos_long_format$Data <- as.character(bentos_long_format$data)

## set "2010" for missing dates (checked with Anaide)
novas_datas <- ifelse (nchar (bentos_long_format$Data) > 1  & nchar (bentos_long_format$Data) < 6,
                       paste (bentos_long_format$Data,"2010",sep="-"), # missing year == 2010 
                       bentos_long_format$Data)##

## dates in format YYYY-mm-dd
novas_datas <- paste (substr (novas_datas,7,10), 
                      substr (novas_datas,4,5), 
                      substr (novas_datas,1,2),sep="-")

### bind in the dataset the different dates
bentos_long_format$eventDate <- as.Date(novas_datas, format="%Y-%m-%d")
bentos_long_format$day <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%d")
bentos_long_format$month <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%m")
bentos_long_format$year <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%Y")
# check year
bentos_long_format [is.na(bentos_long_format$year),]


# fill the year with remaining data

require(dplyr)
year_to_input <- bentos_long_format %>% 
  
  group_by(locality) %>%
  
  summarise (year = min(as.numeric(year),na.rm=T))

# match
year_to_input_match <- year_to_input$year [match (bentos_long_format [is.na(bentos_long_format$year),"locality"],
       year_to_input$locality)]

bentos_long_format [is.na(bentos_long_format$year),"year"] <- year_to_input_match


# ----------------------------------------------------------------------------------------------
# ORGANIZING DEPTHS, REGIONS, SITES



## define whether samples were in deep or shallow
bentos_long_format$verbatimDepth <- bentos_long_format$modifiedDepth

# adjusting depth
bentos_long_format$eventDepth <- as.factor(bentos_long_format$modifiedDepth) # modified to intervals
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "1-7m")] <- "shallow"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "4-7m")] <- "shallow"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "8-12m")] <- "deep"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "8-15m")] <- "deep"

## define whether samples were in oceanic islands,  Southeastern or Northeastern regions
bentos_long_format$Region <- as.factor (bentos_long_format$locality)
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) %in% c("noronha","rocas","trindade"))] <- "oc_isl"
# nao tem sao pedro e sao paulo EM RELACAO A MORAIS ET AL. 2017
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "abrolhos")] <- "ne_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "btds_santos")] <- "ne_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "costa_corais")] <- "ne_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "manuel_luis")] <- "ne_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "rgnor_parrachos")] <- "ne_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "rgnor_norte")] <- "ne_reefs"

## NAO TEM CEARA E RG_NORTE SUL EM RELACAO A MORAIS ET AL. 2017
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "alcatrazes")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "abrolhos")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "arraial")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "espirito_santo")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "ilhabela")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "ilhasc_norte")] <- "se_reefs"
levels (bentos_long_format$Region)[which(levels (bentos_long_format$Region) == "ilhasc_sul")] <- "se_reefs"

## adjust site names (following Morais et al. 2017)
# verbatimLocality
bentos_long_format$verbatimLocality <- bentos_long_format$site

# to lower 
bentos_long_format$Sites <- tolower (bentos_long_format$site)

# ajustar os sitios de rio grande do norte, de acordo com G Longo
bentos_long_format$locality[which(bentos_long_format$locality == "rgnor_norte")] <- "rgnor_natal"

# tartarugas in rocas and trindade (only rocas in this dataset)
# unique(bentos_long_format [which(bentos_long_format$Locality == "trindade"),"Sites"])
# removing locality names from location name
bentos_long_format$Sites<-gsub ("arvoredo_", "",bentos_long_format$Sites)
bentos_long_format$Sites<-gsub ("_noronha", "",bentos_long_format$Sites)
# unique(bentos_long_format$Sites)[order(unique(bentos_long_format$Sites))]






# ----------------------------------------------------------------------------------------------
# ADJUSTING COORDINATES


# verbatimLatitude and Longitude!
#bentos_long_format$verbatimLatitude <- bentos_long_format$Lat
#bentos_long_format$verbatimLongitude <- bentos_long_format$Lon

## coordinates to spatial points
# adjusting longitude of one coordinate on land
## "saco dagua" which falls within the continent
bentos_long_format [grep("saco_dagua",bentos_long_format$Sites),"decimalLatitude"] <- as.numeric(-27.274033)
bentos_long_format [grep("saco_dagua",bentos_long_format$Sites),"decimalLongitude"] <- as.numeric(-48.367183)







# ----------------------------------------------------------------------------------------------
# SOME DATES WERE MISSING. NOW WE'RE IMPUTING BASED ON INFORMATION GATHERED WITH THE AUTHORS






### define an ID for each event (first try to define one)
bentos_long_format$eventID <- paste (bentos_long_format$Region,
                                     bentos_long_format$site,
                                     bentos_long_format$Sites,
                                     bentos_long_format$eventDepth,
                                     bentos_long_format$year,sep=".")


# change colnames
#colnames(bentos_long_format) <- c("verbatimSamples", "modifiedSamples", "depth","data",
#                                  "recordedBy", "device", "photoquadrat", 
#                                  "locality", "verbatimSite", "reefType","decimalLongitude", "decimalLatitude",
#                                  "modifiedDepth", "verbatimIdentification", "measurementValue",
#                                  "verbatimEventDate","verbatimEventDay","verbatimEventMonth", 
#                                  "verbatimEventYear", "verbatimDepth", "region",
#                                  "eventID", "occurrenceID", "parentEventID", )
#





# ----------------------------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES BASED ON THE KNOWLEDGE OF CESAR A.M.M. CORDEIRO



# taxonOrGroup, for uncertain groups
###  CEsar: nem tudo aqui (em scientificName) eh nome cientifico e alguns nem tem como incluir num taxon, podemos deixar no script como taxonOrGroup apenas 
### pra organizar e esta coluna sumiria na planilha final do DwC, deixando soh scientificName (apos validar no WoRMS) e o verbatimIdentification



bentos_long_format$verbatimIdentification <- bentos_long_format$scientificName
bentos_long_format$taxonOrGroup  <- bentos_long_format$verbatimIdentification
bentos_long_format$taxonOrGroup <-  (gsub("\\."," ",bentos_long_format$taxonOrGroup)) # replace dot by space
bentos_long_format$taxonOrGroup <-(iconv(bentos_long_format$taxonOrGroup, "ASCII", "UTF-8", sub="")) # encoding
bentos_long_format$taxonOrGroup <- tolower(bentos_long_format$taxonOrGroup) # lower case




# adjust based on knowledge of Cesar Cordeiro

bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "ventricaria ventricosa")] <- "valonia ventricosa"

# broader groups
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "anemona")] <- "actiniaria"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "leathery")] <- "leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "spirobidae - polycchaete")] <- "spirorbinae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "briozoa")] <- "bryozoa"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "bryozoan")] <- "bryozoa"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "hidrozoan")] <- "hydrozoa"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "outro hydrozoa")] <- "hydrozoa"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "poliqueta")] <- "polychaeta"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "polichaeta")] <- "polychaeta"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"

# octocoral and anthozoa
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "outro anthozoa")] <- "anthozoa"
bentos_long_format$taxonOrGroup[grep("octocoral",bentos_long_format$taxonOrGroup)] <- "octocorallia" # "alcyonaria" nao eh aceito
# sponge
bentos_long_format$taxonOrGroup[grep("sponge",bentos_long_format$taxonOrGroup)] <- "porifera"
# echinoderms
bentos_long_format$taxonOrGroup[grep("ourigo",bentos_long_format$taxonOrGroup)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
bentos_long_format$taxonOrGroup[grep("sea urchin",bentos_long_format$taxonOrGroup)] <- "echinoidea"
bentos_long_format$taxonOrGroup[grep("outro echinoderma",bentos_long_format$taxonOrGroup)] <- "echinodermata"
bentos_long_format$taxonOrGroup[grep("crinside",bentos_long_format$taxonOrGroup)] <- "crinoidea"# crinoidea (crinside devido à conversao pra encoding utf 8)
bentos_long_format$taxonOrGroup[grep("estrela",bentos_long_format$taxonOrGroup)] <- "asteroidea"


### melhor nao indicar grupo morfo-anatomico (MAG) como taxonOrGroup. Esse MAG nao tem compativel pra inseir no DwC/OBIS
### Vou indicar o nivel taxonomico compativel com o que tiver e o resto deixamos como estava 
### Os grupos morfo-anatomicos podem ser adicionados com merge de tabela referencia depois

# cca and caa 
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "crostose coralline algae")] <- "corallinales" # "crustose coralline algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "cianobacterias")] <- "cyanobacteria"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup %in% c("amphiroideae", # "amphiroideae"
                                                             "jania amphiroa", # "amphiroideae"
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "filamentous")] <- "filamentous algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "green filamentous algae")] <- "chlorophyta" # "filamentous algae"

# algae
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup %in% c("fleshy algae", # usando o taxaOrGroup podemos manter nessa categoria e ficaria sem scientificName
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "calcareous turf")] <- "corallinales" # "calcareous articulate algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "corticated")] <- "corticated algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "unknown corticated")] <- "corticated algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "sargassum sp")] <- "sargassum" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "jania sp")] <- "jania" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "amphiroa sp")] <- "amphiroa" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "schizoporella sp")] <- "schizoporella" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "siderastrea spp")] <- "siderastrea" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "mussismilia spp")] <- "mussismilia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "millepora sp")] <- "millepora" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "laurencia sp")] <- "laurencia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "digenia sp")] <- "digenia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "digenia sp ")] <- "digenia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "galaxaura sp")] <- "galaxaura" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "agaricia sp")] <- "agaricia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "codium spp")] <- "codium" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "porites sp")] <- "porites" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "padina sp")] <- "padina" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "leptogorgia sp")] <- "leptogorgia" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "chaetomorpha sp")] <- "chaetomorpha" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "caulerpa sp")] <- "caulerpa" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "dictyota sp")] <- "dictyota" # leathery algae"
bentos_long_format$taxonOrGroup[which(bentos_long_format$taxonOrGroup == "parazoanthus cf axinellae")] <- "parazoanthus" # leathery algae"



unique(bentos_long_format$taxonOrGroup)[order(unique(bentos_long_format$taxonOrGroup))]


# matching scientificNames with worms
# remove plot data (because worms may find some equivalent, weird name for them)

bentos_long_format<-bentos_long_format [which(bentos_long_format$taxonOrGroup %in% c("sombra","quadrado","areia e cascalho","desconhecido" ) != T),]

# match with worms
worms_record <- lapply (unique(bentos_long_format$taxonOrGroup), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)



# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# valid name WoRMS 
bentos_long_format$scientificName <- (df_worms_record$scientificname [match (bentos_long_format$taxonOrGroup,
                                                                   tolower (df_worms_record$scientificname))])

# valid name WoRMS 
bentos_long_format$scientificNameAccepted <- (df_worms_record$scientificname [match (bentos_long_format$taxonOrGroup,
                                                                             tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
bentos_long_format$taxonRank <- (df_worms_record$rank [match (bentos_long_format$taxonOrGroup,
                                                              tolower (df_worms_record$scientificname))])


# match & bind
# or taxonID
bentos_long_format$scientificNameID<-(df_worms_record$lsid [match (bentos_long_format$taxonOrGroup,
                                                                   tolower (df_worms_record$scientificname))])
# kingdom
bentos_long_format$kingdom<-(df_worms_record$kingdom [match (bentos_long_format$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])

# phylum
bentos_long_format$phylum <-(df_worms_record$phylum [match (bentos_long_format$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])

# class
bentos_long_format$class<-(df_worms_record$class [match (bentos_long_format$taxonOrGroup,
                                                         tolower (df_worms_record$scientificname))])

# order
bentos_long_format$order <-(df_worms_record$order [match (bentos_long_format$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])

# family
bentos_long_format$family<-(df_worms_record$family [match (bentos_long_format$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])

# genus
bentos_long_format$genus <-(df_worms_record$genus [match (bentos_long_format$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])



# ----------------------------------------------------------------------------------------------
# CREATING EVENTIDS, PARENTIDS, OCCURRENCEIDS
# PREPARING TO DWC FORMAT





# se formos seguir a logica do REEFSyn como guarda-chuva, temos que lembrar de inserir o BR:REEFSyn:SISBIOTA-MAR: 
# BrazilianOceanicIslands -> temos alguns monitoramentos em ilhas costeiras


# adjusting colname of sites

site is locality

bentos_long_format$site == bentos_long_format$Sites

(bentos_long_format$locality)


# site (less local)
bentos_long_format$verbatimLocality <- bentos_long_format$Sites
bentos_long_format$verbatimSite <- bentos_long_format$locality
bentos_long_format$site <- bentos_long_format$verbatimSite
# locality (more local)
bentos_long_format$locality <- bentos_long_format$verbatimLocality


# geographic location
bentos_long_format$higherGeography <- ifelse (bentos_long_format$site %in% c("rocas",
                                                                                     "noronha",
                                                                                     "trindade"),
                                                "BrazilianOceanicIslands", 
                                                "BrazilianCoast")


## substituir a ID antiga pela nova com os anos ajustados (new eventID)
bentos_long_format$eventID <- paste (
  paste ( 
      paste ("BR:ReefSYN:SISBIOTA-MAR:", 
             bentos_long_format$higherGeography,
             sep=""),
         bentos_long_format$site,sep=":"),
  bentos_long_format$locality,
  bentos_long_format$year,
  bentos_long_format$photoquadrat,
  sep="_")




# occurrenceID
bentos_long_format$occurrenceID <- paste (
  paste ( 
    paste ("BR:ReefSYN:SISBIOTA-MAR:", 
           bentos_long_format$higherGeography,
           sep=""),
    bentos_long_format$site,sep=":"),
  bentos_long_format$locality,
  bentos_long_format$year,
              bentos_long_format$photoquadrat,
                  paste ("occ",seq(1,nrow(bentos_long_format)),sep=""),
          sep="_")


# creating parentEventids
bentos_long_format$parentEventID <- paste (
  paste ( 
    paste ("BR:ReefSYN:SISBIOTA-MAR:", 
           bentos_long_format$higherGeography,
           sep=""),
    bentos_long_format$site,sep=":"),
  bentos_long_format$locality,
  bentos_long_format$year,
  sep="_")


# method
bentos_long_format$samplingProtocol <- "Photoquadrats - 2 x 1m"

# samplingEffort
bentos_long_format$samplingEffort <- 5 # 5 quadrats per plot, 25 * 25 cm

# sampleSizeValue
bentos_long_format$sampleSizeValue <- 0.25*0.25 # subsamples # the scale we're presenting data

# sampleSizeUnit
bentos_long_format$sampleSizeUnit <- "squared meters"


# country and code
bentos_long_format$Country <- "Brazil"
bentos_long_format$countryCode <- "BR"


# basisOfRecord
bentos_long_format$basisOfRecord <- "HumanObservation"

# OccurrenceStatus
bentos_long_format$occurrenceStatus <- "presence"

# organismQuantityType
bentos_long_format$organismQuantityType <- "Relative cover"

# measurementType
bentos_long_format$measurementType <- "Relative cover"

# measurementUnit
bentos_long_format$measurementUnit <- "dimensionless"

# geodeticDatum
bentos_long_format$geodeticDatum <- "decimal degrees"

# depth
# range of 1-7 m, and 8-15 meters
bentos_long_format$minimumDepthinMeters <- NA
bentos_long_format$maximumDepthinMeters <- NA
bentos_long_format$minimumDepthinMeters[which(bentos_long_format$eventDepth %in% c("shallow"))] <- 1
bentos_long_format$maximumDepthinMeters[which(bentos_long_format$eventDepth %in% c("shallow"))] <- 7
bentos_long_format$minimumDepthinMeters[which(bentos_long_format$eventDepth %in% c("deep"))] <- 8
bentos_long_format$maximumDepthinMeters[which(bentos_long_format$eventDepth %in% c("deep"))] <- 15


# habitat
bentos_long_format$habitat <- bentos_long_format$reefType
bentos_long_format$habitat <- paste (bentos_long_format$habitat, "-reef", sep = "")


# licence
bentos_long_format$licence <- "CC BY-NC"
# language
bentos_long_format$language <- "en"
# citation
bentos_long_format$bibliographicCitation <- "Aued, Anaide Wrublevski et al. (2019), Data from: Large-scale patterns of benthic marine communities in the Brazilian Province, Dryad, Dataset, https://doi.org/10.5061/dryad.f5s90"

# eventRemarks
bentos_long_format$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curators Andre Luza and Cesar Cordeiro to have the complete dataset with verbatimIdentification"

# remove these MAGs
bentos_long_format <- bentos_long_format [which(is.na(bentos_long_format$scientificNameAccepted) !=T),]





# ----------------------------------------------------------------------------------------------
# DWC FORMAT





# Formatted according to DwC
# measurement or facts
DF_eMOF <- bentos_long_format [,c("eventID", 
                                  "occurrenceID",
                                  "measurementValue", 
                                  "measurementType",
                                  "measurementUnit",
                                  "eventRemarks")]

# occurrence
DF_occ <- bentos_long_format [,c("eventID", "occurrenceID",
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
                                 "recordedBy", "organismQuantityType",
                                 "occurrenceStatus",
                                 "licence",
                                 "language",
                                 "bibliographicCitation"
                                 )]

# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(bentos_long_format[,-4], eventID,higherGeography,site,verbatimLocality,locality) %>% 
                            
                            summarise(year = mean(as.numeric(year)),
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

# save
# csv format
write.csv(DF_occ, file =here("DwC_output",
                               "AAued_spatialData",
                               "DF_occ.csv"))
write.csv(DF_eMOF, file =here("DwC_output",
                                "AAued_spatialData",
                                "DF_eMOF.csv"))
write.csv(event_core, file =here("DwC_output",
                                   "AAued_spatialData",
                                   "event_core.csv"))

# end
rm(list=ls())
