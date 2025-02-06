
# ------------------------------------------
# Abrolhos & other sites

# load packages and functions
require(here); require (openxlsx); require(reshape); require(dplyr); require(worrms)


# open  data to match region
occ_Francini_et_al <- read.xlsx(here("Data","occ_Francini_et_al",
                                 "Compiled_quadrats_benthic_versao2.0_ate30m.xlsx"),
                            sheet = 1, colNames = TRUE,detectDates=F)


# remove data from Aued et al. 2018 (no year)
occ_Francini_et_al <- occ_Francini_et_al[which(is.na(occ_Francini_et_al$YEAR)!= T),]



# remove data from Abrolhos 2008 - already in the time series
occ_Francini_et_al <- occ_Francini_et_al[-which(occ_Francini_et_al$REGION == "ABROLHOS"),]




# ----------------------------------------------------------------------------------------------
# DATA TO LONG FORMAT





# colnames site information/environment
target_cols_sites <- c("REGION","REEF","SITE","HAB",           
      "MONTH","YEAR","DEPTH","LAT","LONG","DIST_OFFSHORE","DIST_100M_ISO",  
      "KD_medio","NSST_media","PAR_media","Salinity.Mean",     
      "Nitrate.Mean.Surface")



# extract taxa cover data
cover_data <- occ_Francini_et_al[,which(colnames(occ_Francini_et_al) %in% 
                                          target_cols_sites ==F)]


# repeat site covs as much as the number of taxa
var_data <- occ_Francini_et_al[,which(colnames(occ_Francini_et_al) %in% target_cols_sites ==T)]
var_data<- lapply (seq(1,ncol(cover_data)), function (i)
  var_data
)

# melt this cov_data
var_data<- do.call(rbind,var_data)
 



# ----------------------------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES 





# melt taxa cover 
cover_data<-melt(cover_data)
colnames(cover_data) <- c("scientificName", "cover")
cover_data$scientificName <- as.character(cover_data$scientificName)

# verbatimIdentification
cover_data$verbatimIdentification <- cover_data$scientificName


# adjusting taxa to search 

cover_data$taxonOrGroup <- cover_data$verbatimIdentification


# edit abbreviations
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MUSHAR")] <- "mussismilia harttii"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MUSHIS")] <- "mussismilia hispida"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MUSBRA")] <- "mussismilia braziliensis"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MUSLEP")] <- "mussismilia leptophylla"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "AGFRAG")] <- "agaricia fragilis"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "AGAHUM")] <- "agaricia humilis"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "SID.SP")] <- "siderastrea"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MONCAV")] <- "montastraea cavernosa"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MADDEC")] <- "madracis decactis"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "FAVGRA")] <- "favia gravida"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MEABRA")] <- "meandrina brasiliensis"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "PORBRA")] <- "porites branneri"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "PORAST")] <- "porites astreoides"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "ASTPHY")] <- "astrangia phyllangia"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "SCOWEL")] <- "scolymia wellsi"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "STEMEC")] <- "stephanocoenia intersepta" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "ASTSOL")] <- "astrangia solitaria" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MILLEPORA.SP")] <- "millepora"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "PALYTHOA")] <- "palythoa" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "OTHER_ZOA")] <- "zoantharia" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "SPONGE")] <- "sponge" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "TURF")] <- "calcareous turf" # Calcareous.turf in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "MACROALGAE")] <- "leathery algae" # not found in Aued et al. # imputed by ALL
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "CYANOB")] <- "cyanobacteria"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "CCA")] <- "crustose coralline algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "OCTOCORAL")] <-"octocorallia" # "alcyonaria" nao eh aceito
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "CORAL")] <- "scleractinia" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "BRIOZOA")] <- "bryozoa" # not found in Aued et al.
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "ASCIDIAN")] <- "ascidiacea" # not found in Aued et al.




# FURTHER ADJUSTING SCIENTIFIC NAMES  BASED IN OTHER BENTHIC DATASETS (KNOWLEDGE OF CESAR CORDEIRO)

# adjust based on knowledge of Cesar Cordeiro

cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "ventricaria ventricosa")] <- "valonia ventricosa"

# broader groups
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "anemona")] <- "actiniaria"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "leathery")] <- "leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "spirobidae - polycchaete")] <- "spirorbinae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "briozoa")] <- "bryozoa"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "bryozoan")] <- "bryozoa"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "hidrozoan")] <- "hydrozoa"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "outro hydrozoa")] <- "hydrozoa"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "poliqueta")] <- "polychaeta"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "polichaeta")] <- "polychaeta"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup %in% c("ascidea colonial" ,                  
                                                                             "ascidian",
                                                                             "outra ascidia"))] <- "ascidiacea"

# octocoral and anthozoa
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "outro anthozoa")] <- "anthozoa"
cover_data$taxonOrGroup[grep("octocoral",cover_data$taxonOrGroup)] <- "octocorallia" # "alcyonaria" nao eh aceito
# sponge
cover_data$taxonOrGroup[grep("sponge",cover_data$taxonOrGroup)] <- "porifera"
# echinoderms
cover_data$taxonOrGroup[grep("ourigo",cover_data$taxonOrGroup)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
cover_data$taxonOrGroup[grep("sea urchin",cover_data$taxonOrGroup)] <- "echinoidea"
cover_data$taxonOrGroup[grep("outro echinoderma",cover_data$taxonOrGroup)] <- "echinodermata"
cover_data$taxonOrGroup[grep("crinside",cover_data$taxonOrGroup)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
cover_data$taxonOrGroup[grep("estrela",cover_data$taxonOrGroup)] <- "asteroidea"


### melhor nao indicar grupo morfo-anatomico (MAG) como taxonOrGroup. Esse MAG nao tem compativel pra inseir no DwC/OBIS
### Vou indicar o nivel taxonomico compativel com o que tiver e o resto deixamos como estava 
### Os grupos morfo-anatomicos podem ser adicionados com merge de tabela referencia depois

# cca and caa 
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "crostose coralline algae")] <- "corallinales" # "crustose coralline algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "cianobacterias")] <- "cyanobacteria"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup %in% c("amphiroideae", # "amphiroideae"
                                                                             "jania amphiroa", # "amphiroideae"
                                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "filamentous")] <- "filamentous algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "green filamentous algae")] <- "chlorophyta" # "filamentous algae"

# algae
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup %in% c("fleshy algae", # usando o taxaOrGroup podemos manter nessa categoria e ficaria sem scientificName
                                                                             "foliaceous algae",
                                                                             "foliose",
                                                                             "frondose algae", 
                                                                             "unknown foliose"))] <- "foliose algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "calcareous turf")] <- "corallinales" # "calcareous articulate algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "corticated")] <- "corticated algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "unknown corticated")] <- "corticated algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "sargassum sp")] <- "sargassum" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "jania sp")] <- "jania" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "amphiroa sp")] <- "amphiroa" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "schizoporella sp")] <- "schizoporella" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "siderastrea spp")] <- "siderastrea" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "mussismilia spp")] <- "mussismilia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "millepora sp")] <- "millepora" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "laurencia sp")] <- "laurencia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "digenia sp")] <- "digenia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "digenia sp ")] <- "digenia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "galaxaura sp")] <- "galaxaura" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "agaricia sp")] <- "agaricia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "codium spp")] <- "codium" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "porites sp")] <- "porites" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "padina sp")] <- "padina" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "leptogorgia sp")] <- "leptogorgia" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "chaetomorpha sp")] <- "chaetomorpha" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "caulerpa sp")] <- "caulerpa" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "dictyota sp")] <- "dictyota" # leathery algae"
cover_data$taxonOrGroup[which(cover_data$taxonOrGroup == "parazoanthus cf axinellae")] <- "parazoanthus" # leathery algae"


# bind data
francini_bind_data <- cbind(var_data,
                            cover_data)


# remove plot data
francini_bind_data <-francini_bind_data[which(francini_bind_data$taxonOrGroup %in% 
                                                c("Sand","Rock") != T),]




# validation of species name (worms) ----------------------------------
# matching with worms


worms_record <- lapply (unique(francini_bind_data$taxonOrGroup), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# match
# scientific name
francini_bind_data$scientificNameAccepted<-(df_worms_record$scientificname [match (francini_bind_data$taxonOrGroup,
                                                                               tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
francini_bind_data$taxonRank <- (df_worms_record$rank [match (francini_bind_data$taxonOrGroup,
                                                              tolower (df_worms_record$scientificname))])


# aphid
francini_bind_data$scientificNameID<-(df_worms_record$lsid [match (francini_bind_data$taxonOrGroup,
                                                                   tolower (df_worms_record$scientificname))])


# kingdom
francini_bind_data$kingdom<-(df_worms_record$kingdom [match (francini_bind_data$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])

# phylum
francini_bind_data$phylum<-(df_worms_record$phylum [match (francini_bind_data$taxonOrGroup,
                                                             tolower (df_worms_record$scientificname))])


# class
francini_bind_data$class<-(df_worms_record$class [match (francini_bind_data$taxonOrGroup,
                                                         tolower (df_worms_record$scientificname))])

# order
francini_bind_data$order <-(df_worms_record$order [match (francini_bind_data$taxonOrGroup,
                                                         tolower (df_worms_record$scientificname))])

# family
francini_bind_data$family<-(df_worms_record$family [match (francini_bind_data$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])

# genus
francini_bind_data$genus <-(df_worms_record$genus [match (francini_bind_data$taxonOrGroup,
                                                           tolower (df_worms_record$scientificname))])



# ----------------------------------------------------------------------------------------------
# ADJUSTING SITES, DATES, REGION, DEPTH







# designate REGION,  SITE and REEF as original data
# colnames(francini_bind_data) [which(colnames(francini_bind_data) == "REGION")] <- "verbatimRegion"
colnames(francini_bind_data) [which(colnames(francini_bind_data) == "SITE")] <- "verbatimLocality"
colnames(francini_bind_data) [which(colnames(francini_bind_data) == "REEF")] <- "verbatimSite"

# localities (following SISBIOTA-MAR names)
francini_bind_data$site <- francini_bind_data$verbatimSite
francini_bind_data$site [which(francini_bind_data$site == "ABROLHOS")] <- "abrolhos"
francini_bind_data$site [which(francini_bind_data$site == "ASPSP")] <- "stpauls_rocks"
francini_bind_data$site [which(francini_bind_data$site == "ES")] <- "espirito_santo"
francini_bind_data$site [which(francini_bind_data$site == "NORONHA")] <- "noronha"
francini_bind_data$site [which(francini_bind_data$site == "PARAIBA")] <- "paraiba"
francini_bind_data$site [which(francini_bind_data$site == "RIODEJANEIRO")] <- "rio_de_janeiro"
francini_bind_data$site [which(francini_bind_data$site == "RJ")] <- "rio_de_janeiro"
francini_bind_data$site [which(francini_bind_data$site == "TRINDADE")] <- "trindade"

# baseado em outros datasests (e.g., Longo et al. 2019)
francini_bind_data$site [which(francini_bind_data$site == "SP")] <- "ilhabela"
francini_bind_data$site [which(francini_bind_data$site == "ALAGOAS")] <- "costa_corais"
francini_bind_data$site [which(francini_bind_data$site == "BAHIA")] <- "btds_santos"
francini_bind_data$site [which(francini_bind_data$site == "MARANHAO")] <- "manuel_luis"
francini_bind_data$site [which(francini_bind_data$site == "RIOGRANDEDONORTE")] <- "rgnor_natal"
francini_bind_data$site [which(francini_bind_data$site == "ROCAS")] <- "rocas"
francini_bind_data$site [which(francini_bind_data$site == "LAJEDESANTOS")] <- "laje_santos"

# adjust sites (mix of reef and sites)
# francini_bind_data$reef <- francini_bind_data$verbatimReef
francini_bind_data$locality <- tolower (francini_bind_data$verbatimLocality)
francini_bind_data$locality[which(francini_bind_data$locality == "laj dois irmaos")] <- "laje_dois_irmaos"
francini_bind_data$locality[which(francini_bind_data$locality == "lajedesantos")] <- "laje_santos"
francini_bind_data$locality[which(francini_bind_data$locality == "ilhagrande")] <- "ilha_grande"

# small corections
francini_bind_data$locality <- gsub(" ","_",tolower (francini_bind_data$locality))
francini_bind_data$locality[which(francini_bind_data$locality == "martim_vaz_")] <- "martim_vaz"

# adjust dates
francini_bind_data$year <- francini_bind_data$YEAR # year
francini_bind_data$month <- francini_bind_data$MONTH # month
francini_bind_data$month[which(francini_bind_data$month == "FEV")] <- "02"
francini_bind_data$month[which(francini_bind_data$month == "SET")] <- "09"
francini_bind_data$month[which(francini_bind_data$month == "SET ")] <- "09"
francini_bind_data$month[which(francini_bind_data$month == "MAI")] <- "05"
francini_bind_data$month[which(francini_bind_data$month == "NOV")] <- "11"
francini_bind_data$month[which(francini_bind_data$month == "JAN")] <- "01"
francini_bind_data$month[which(francini_bind_data$month == "JUL")] <- "07"
francini_bind_data$month[which(francini_bind_data$month == "MAR")] <- "03"
francini_bind_data$month[which(francini_bind_data$month == "MAR ")] <- "03"
francini_bind_data$month[which(francini_bind_data$month == "OUT")] <- "10"


# create dates
francini_bind_data$eventDate <- paste (francini_bind_data$year,
                                       francini_bind_data$month,
                                       sep="-")


# depthInMeters
francini_bind_data$depthInMeters <- francini_bind_data$DEPTH


# define geographic region (oceanic islands,  SE, NE, S)
francini_bind_data$region <- francini_bind_data$site
francini_bind_data$region [which(francini_bind_data$region == "noronha")] <- "oc_isl"
francini_bind_data$region [which(francini_bind_data$region == "trindade")] <- "oc_isl"
francini_bind_data$region [which(francini_bind_data$region == "stpauls_rocks")] <- "oc_isl"

# nao tem sao pedro e sao paulo EM RELACAO A MORAIS ET AL. 2017
francini_bind_data$region [which(francini_bind_data$region == "abrolhos")] <- "ne_reefs"
francini_bind_data$region [which(francini_bind_data$region == "paraiba")] <- "ne_reefs"

## NAO TEM CEARA E RG_NORTE SUL EM RELACAO A MORAIS ET AL. 2017
francini_bind_data$region [which(francini_bind_data$region == "espirito_santo")] <- "se_reefs"
francini_bind_data$region [which(francini_bind_data$region == "ilha_grande")] <- "se_reefs"
francini_bind_data$region [which(francini_bind_data$region == "laje_santos")] <- "se_reefs"

# chance colnames
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "LAT")] <- "decimalLatitude" 
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "LONG")] <- "decimalLongitude" 
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "HAB")] <- "habitat" 

# change habitats
# adjusting habitat (based on Francini-Filho et al. 2013)
francini_bind_data$habitat <- tolower(francini_bind_data$habitat)
francini_bind_data$habitat[which(francini_bind_data$habitat == "tp")] <- "pinnacles_top"
francini_bind_data$habitat[which(francini_bind_data$habitat == "pa")] <- "pinnacles_wall"








# ----------------------------------------------------------------------------------------------
# CREATING EVENTIDS, PARENTIDS, OCCURRENCEIDS
# PREPARING TO DWC FORMAT





# geographic location
francini_bind_data$higherGeography <- ifelse (francini_bind_data$site %in% c("noronha",
                                                                                   "trindade",
                                                                                   "stpauls_rocks"),
                                                "BrazilianOceanicIslands",
                                                "BrazilianCoast")




# creating parentIDs
francini_bind_data$parentEventID <- paste (paste ( 
                                          paste ("BR:ReefSYN:RonaldoFrancini-Filho-USP:", 
                                                 francini_bind_data$higherGeography,
                                                 sep=""),
                                          francini_bind_data$site,sep=":"),
                                           francini_bind_data$locality, 
                                           francini_bind_data$year,
                              sep="_")




# creating eventID
francini_bind_data$eventID <- paste (paste ( 
  paste ("BR:ReefSYN:RonaldoFrancini-Filho-USP:", 
         francini_bind_data$higherGeography,
         sep=""),
  francini_bind_data$site,sep=":"), 
                                          francini_bind_data$locality, 
                                          francini_bind_data$year,
                                          sep="_")





# creating occurrenceID
francini_bind_data$occurrenceID <- paste (paste ( 
  paste ("BR:ReefSYN:RonaldoFrancini-Filho-USP:", 
         francini_bind_data$higherGeography,
         sep=""),
  francini_bind_data$site,sep=":"), 
                                     francini_bind_data$locality, 
                                     francini_bind_data$year,
                                     paste ("occ",seq(1,nrow(francini_bind_data)),sep=""),
                                     sep="_")



# years per site (only bay has two years of sampling)
table(

    rowSums(table (francini_bind_data$locality,francini_bind_data$year)>0)

)





# years per locality (only bay has two years of sampling)
table(
  
  rowSums(table (francini_bind_data$site,francini_bind_data$year)>0)
  
)





# which years and sites
years_sites <- table (francini_bind_data$site,francini_bind_data$locality,francini_bind_data$year)

# years_sites [,,c("2008","2011")]

# darwin core format
# measurementType
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "cover")] <- "measurementValue"

# 0 - 1
francini_bind_data$measurementValue <- francini_bind_data$measurementValue

# method
francini_bind_data$samplingProtocol <- "Photoquadrats - 0.66 x 0.75 m"
# samplingEffort

# only the range mentioned
# not sure yet how many per site
francini_bind_data$samplingEffort <- "5 - 33" # 5 to 33 (Following Santana et al 2023)

# eventRemarks
francini_bind_data$eventRemarks <- "Only the range of # plots across sites provided"
# sampleSizeValue
francini_bind_data$sampleSizeValue <- 0.66*0.75 # 5 quadrats, 66 * 75 cm

# sampleSizeUnit
francini_bind_data$sampleSizeUnit <- "squared meters"

# country and code
francini_bind_data$Country <- "Brazil"
francini_bind_data$countryCode <- "BR"

# basisOfRecord
francini_bind_data$basisOfRecord <- "HumanObservation"

# occurrenceStatus
francini_bind_data$occurrenceStatus <- "presence"

# recordedBy
francini_bind_data$recordedBy <- "Ronaldo Francini-Filho | Erika FC Santana | Anaide W Aued"

# organismQuantityType
francini_bind_data$organismQuantityType <- "Percentage cover"

# measurementType
francini_bind_data$measurementType <- "Percentage cover"

# measurementUnit
francini_bind_data$measurementUnit <- "dimensionless"

# geodeticDatum
francini_bind_data$geodeticDatum <- "decimal degrees"

# set min and max
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "depthInMeters")] <- "minimumDepthInMeters"
francini_bind_data$maximumDepthInMeters <- francini_bind_data$minimumDepthInMeters


# licence
francini_bind_data$licence <- "CC BY"

# language
francini_bind_data$language <- "en"


# eventRemarks
francini_bind_data$eventRemarks <- "Bare substrate, sediment, lost information (shade, quadrat, tape), morpho-anatomical benthic groups and turf were not included in the data because they do not represent taxonomical entities in which DwC standards are based. This implies in a measurementValue which does not add up to 1. Please contact the data curators Andre Luza and Cesar Cordeiro to have the complete dataset with verbatimIdentification"



# remove these MAGs
francini_bind_data <- francini_bind_data [which(is.na(francini_bind_data$scientificNameAccepted) !=T),]


# sites into locations
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "site")] <- "location"



# check whether the sum of one event ID reaches 100
# for one ID
test_id <- unique(francini_bind_data$eventID)

test <- (francini_bind_data[which(francini_bind_data$eventID == test_id[1]) ,]) %>%
  
  group_by(eventID, scientificName) %>%
  summarize(cover = sum(measurementValue)) # 

hist(test$cover)

test <- francini_bind_data %>%
  
  group_by(eventID) %>%
  summarize(cover = sum(measurementValue)) # 

range(test$cover)

test %>%
  filter (cover >1)


# for all event IDs
data_all <- francini_bind_data %>% # Problem on the join with many-to-many relationship
  
  group_by(eventID, scientificName) %>%
  summarize(total = sum(measurementValue)) %>% 
  ungroup()

data_all %>%
  filter (total >1)

hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units



# ----------------------------------------------------------------------------------------------
# DWC FORMAT




# Formatted according to DwC
DF_eMOF <- francini_bind_data [,c("eventID",
                                  "occurrenceID",
                                  "measurementValue", 
                                  "measurementType",
                                  "measurementUnit",
                                  "eventRemarks")]

DF_occ <- francini_bind_data [,c("eventID", 
                                 "occurrenceID",
                                 "basisOfRecord",
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
                                 "language")]


# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(francini_bind_data, eventID,higherGeography,location,verbatimLocality,locality) %>% 
                            
                            summarise(year = unique(year),
                                      eventDate = unique(eventDate),
                                      minimumDepthInMeters = mean(minimumDepthInMeters),
                                      maximumDepthInMeters = mean(maximumDepthInMeters),
                                      samplingProtocol = unique(samplingProtocol),
                                      samplingEffort = unique(samplingEffort),
                                      sampleSizeValue = mean(sampleSizeValue),
                                      sampleSizeUnit = unique(sampleSizeUnit),
                                      eventRemarks = unique(eventRemarks),
                                      decimalLongitude = mean(decimalLongitude),
                                      decimalLatitude = mean(decimalLatitude),
                                      geodeticDatum = unique(geodeticDatum),
                                      Country = unique(Country),
                                      countryCode = unique(countryCode))
)



data_all <- left_join(DF_eMOF, event_core) %>% 
  left_join(., DF_occ) %>% # Problem on the join with many-to-many relationship
  
  group_by(eventID, scientificNameAccepted) %>%
  summarize(total = sum(measurementValue)) %>% 
  ungroup()

hist(data_all$total) # Problem: the sum is not equal to 100 for most sampling units




# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)

# save RData
# txt format
write.csv(DF_occ, file =here("DwC_output",
                               "XIII",
                               "DF_occ.csv"))

write.csv(DF_eMOF, file =here("DwC_output",
                                "XIII",
                                "DF_eMOF.csv"))


write.csv(event_core, file =here("DwC_output",
                                   "XIII",
                                   "event_core.csv"))
rm(list=ls())
