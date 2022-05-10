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

# edit abbreviations
cover_data$scientificName[which(cover_data$scientificName == "MUSHAR")] <- "mussismilia harttii"
cover_data$scientificName[which(cover_data$scientificName == "MUSHIS")] <- "mussismilia hispida"
cover_data$scientificName[which(cover_data$scientificName == "MUSBRA")] <- "mussismilia braziliensis"
cover_data$scientificName[which(cover_data$scientificName == "MUSLEP")] <- "mussismilia leptophylla"
cover_data$scientificName[which(cover_data$scientificName == "AGFRAG")] <- "agaricia fragilis"
cover_data$scientificName[which(cover_data$scientificName == "AGAHUM")] <- "agaricia humilis"
cover_data$scientificName[which(cover_data$scientificName == "SID.SP")] <- "siderastrea spp"
cover_data$scientificName[which(cover_data$scientificName == "MONCAV")] <- "montastraea cavernosa"
cover_data$scientificName[which(cover_data$scientificName == "MADDEC")] <- "madracis decactis"
cover_data$scientificName[which(cover_data$scientificName == "FAVGRA")] <- "favia gravida"
cover_data$scientificName[which(cover_data$scientificName == "MEABRA")] <- "meandrina brasiliensis"
cover_data$scientificName[which(cover_data$scientificName == "PORBRA")] <- "porites branneri"
cover_data$scientificName[which(cover_data$scientificName == "PORAST")] <- "porites astreoides"
cover_data$scientificName[which(cover_data$scientificName == "ASTPHY")] <- "astrangia phyllangia"
cover_data$scientificName[which(cover_data$scientificName == "SCOWEL")] <- "scolymia wellsi"
cover_data$scientificName[which(cover_data$scientificName == "STEMEC")] <- "stephanocoenia intersepta" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "ASTSOL")] <- "astrangia solitaria" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "MILLEPORA.SP")] <- "millepora sp"
cover_data$scientificName[which(cover_data$scientificName == "PALYTHOA")] <- "palythoa sp" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "OTHER_ZOA")] <- "other zoanthid" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "SPONGE")] <- "sponge" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "TURF")] <- "calcareous turf" # Calcareous.turf in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "MACROALGAE")] <- "leathery algae" # not found in Aued et al. # imputed by ALL
cover_data$scientificName[which(cover_data$scientificName == "CYANOB")] <- "cyanobacteria"
cover_data$scientificName[which(cover_data$scientificName == "CCA")] <- "crustose coralline algae"
cover_data$scientificName[which(cover_data$scientificName == "OCTOCORAL")] <- "alcyonaria" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "CORAL")] <- "scleractinia" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "BRIOZOA")] <- "bryozoa" # not found in Aued et al.
cover_data$scientificName[which(cover_data$scientificName == "ASCIDIAN")] <- "ascidiacea" # not found in Aued et al.






# ADJUSTING SCIENTIFIC NAMES  BASED IN OTHER BENTHIC DATASETS (KNOWLEDGE OF CESAR CORDEIRO)






# adjusting spp names
cover_data$scientificName <-  (gsub("\\."," ",cover_data$scientificName))
cover_data$scientificName<-(iconv(cover_data$scientificName, "ASCII", "UTF-8", sub=""))
cover_data$scientificName <- tolower(cover_data$scientificName)

# adjust based on knowledge of Cesar Cordeiro
cover_data$scientificName[which(cover_data$scientificName == "aiolochoria crassa")] <- "aiolochroia crassa"
cover_data$scientificName[which(cover_data$scientificName == "meandrina braziliensis")] <- "meandrina brasiliensis"
cover_data$scientificName[which(cover_data$scientificName == "millepora")] <- "millepora sp"
cover_data$scientificName[which(cover_data$scientificName == "montastrea cavernosa")] <- "montastraea cavernosa"
cover_data$scientificName[which(cover_data$scientificName == "mussismilia")] <- "mussismilia spp"
cover_data$scientificName[which(cover_data$scientificName == "neospongodes atlbntica")] <- "neospongodes atlantica"
cover_data$scientificName[which(cover_data$scientificName == "siderastrea spp ")] <- "siderastrea spp"
cover_data$scientificName[which(cover_data$scientificName == "siderastrea")] <- "siderastrea spp"
cover_data$scientificName[which(cover_data$scientificName == "siderastrea sp")] <- "siderastrea spp"
cover_data$scientificName[which(cover_data$scientificName == "ventricaria ventricosa")] <- "valonia ventricosa"
cover_data$scientificName[which(cover_data$scientificName == "zooanthus sociatus")] <- "zoanthus sociatus"
cover_data$scientificName[which(cover_data$scientificName == "zoanthid")] <- "zoantharia"
cover_data$scientificName[which(cover_data$scientificName == "zoanthus sp ")] <- "zoantharia"
cover_data$scientificName[which(cover_data$scientificName == "palythoa")] <- "palythoa sp "
cover_data$scientificName[which(cover_data$scientificName == "padina")] <- "padina sp"

# broader groups
cover_data$scientificName[which(cover_data$scientificName == "leathery")] <- "leathery algae"
cover_data$scientificName[which(cover_data$scientificName == "spirobidae - polycchaete")] <- "spirorbidae"
cover_data$scientificName[which(cover_data$scientificName == "briozoa")] <- "bryozoa"
cover_data$scientificName[which(cover_data$scientificName == "bryozoan")] <- "bryozoa"
cover_data$scientificName[which(cover_data$scientificName == "hidrozoan")] <- "hydrozoa"
cover_data$scientificName[which(cover_data$scientificName == "outro hydrozoa")] <- "hydrozoa"
cover_data$scientificName[which(cover_data$scientificName == "poliqueta")] <- "polychaeta"
cover_data$scientificName[which(cover_data$scientificName == "polichaeta")] <- "polychaeta"
cover_data$scientificName[which(cover_data$scientificName %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"
# octocoral and anthozoa
cover_data$scientificName[which(cover_data$scientificName == "outro anthozoa")] <- "anthozoa"
cover_data$scientificName[grep("octocoral",cover_data$scientificName)] <- "alcyonaria"
# sponge
cover_data$scientificName[grep("sponge",cover_data$scientificName)] <- "porifera"
# echinorms
cover_data$scientificName[grep("ourigo",cover_data$scientificName)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
cover_data$scientificName[grep("sea urchin",cover_data$scientificName)] <- "echinoidea"
cover_data$scientificName[grep("outro echinoderma",cover_data$scientificName)] <- "echinodermata"
cover_data$scientificName[grep("crinside",cover_data$scientificName)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
cover_data$scientificName[grep("estrela",cover_data$scientificName)] <- "asteroidea"

# cca and caa
cover_data$scientificName[which(cover_data$scientificName == "crostose coralline algae")] <- "crustose coralline algae"
cover_data$scientificName[which(cover_data$scientificName == "cianobacterias")] <- "cyanobacteria"
cover_data$scientificName[which(cover_data$scientificName %in% c("amphiroa", 
                                                             "amphiroa sp", 
                                                             "amphiroideae", 
                                                             "jania amphiroa", 
                                                             "jania sp",
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
cover_data$scientificName[which(cover_data$scientificName == "filamentous")] <- "filamentous algae"
cover_data$scientificName[which(cover_data$scientificName == "green filamentous algae")] <- "filamentous algae"
cover_data$scientificName[which(cover_data$scientificName == "cianobacterias calcareous turf")] <- "cyanobacteria" # transform the other
cover_data$scientificName[which(cover_data$scientificName == "turf")] <- "filamentous algae"
# algae
cover_data$scientificName[which(cover_data$scientificName %in% c("fleshy algae",
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
cover_data$scientificName[which(cover_data$scientificName == "calcareous turf")] <- "calcareous articulate algae"
cover_data$scientificName[which(cover_data$scientificName == "corticated")] <- "corticated algae"
cover_data$scientificName[which(cover_data$scientificName == "unknown corticated")] <- "corticated algae"
cover_data$scientificName[which(cover_data$scientificName == "sargassum sp")] <- "leathery algae"


# bind data
francini_bind_data <- cbind(var_data,
                            cover_data)


# remove plot data
francini_bind_data <-francini_bind_data[which(francini_bind_data$scientificName %in% 
                                                c("sand","rock") != T),]


# validation of species name (worms)
# matching with worms


worms_record <- lapply (unique(francini_bind_data$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# match
francini_bind_data$scientificNameOBIS<-(df_worms_record$scientificname [match (francini_bind_data$scientificName,tolower (df_worms_record$scientificname))])
francini_bind_data$scientificNameID<-(df_worms_record$lsid [match (francini_bind_data$scientificName,tolower (df_worms_record$scientificname))])
francini_bind_data$kingdom<-(df_worms_record$kingdom [match (francini_bind_data$scientificName,tolower (df_worms_record$scientificname))])
francini_bind_data$class<-(df_worms_record$class [match (francini_bind_data$scientificName,tolower (df_worms_record$scientificname))])
francini_bind_data$family<-(df_worms_record$family [match (francini_bind_data$scientificName,tolower (df_worms_record$scientificname))])






# ----------------------------------------------------------------------------------------------
# ADJUSTING SITES, DATES, REGION, DEPTH







# designate REGION,  SITE and REEF as original data
colnames(francini_bind_data) [which(colnames(francini_bind_data) == "REGION")] <- "verbatimRegion"
colnames(francini_bind_data) [which(colnames(francini_bind_data) == "SITE")] <- "verbatimLocality"
colnames(francini_bind_data) [which(colnames(francini_bind_data) == "REEF")] <- "verbatimReef"

# localities
francini_bind_data$locality <- francini_bind_data$verbatimRegion
francini_bind_data$locality [which(francini_bind_data$locality == "ABROLHOS")] <- "abrolhos"
francini_bind_data$locality [which(francini_bind_data$locality == "ASPSP")] <- "stpauls_rocks"
francini_bind_data$locality [which(francini_bind_data$locality == "ES")] <- "espirito_santo"
francini_bind_data$locality [which(francini_bind_data$locality == "NORONHA")] <- "noronha"
francini_bind_data$locality [which(francini_bind_data$locality == "PARAIBA")] <- "paraiba"
francini_bind_data$locality [which(francini_bind_data$locality == "RIODEJANEIRO")] <- "rio_de_janeiro"
francini_bind_data$locality [which(francini_bind_data$locality == "RJ")] <- "rio_de_janeiro"

# baseado em outros datasests (e.g., Longo et al. 2019)
francini_bind_data$locality [which(francini_bind_data$locality == "SP")] <- "ilhabela"
francini_bind_data$locality [which(francini_bind_data$locality == "TRINDADE")] <- "trindade"
francini_bind_data$locality [which(francini_bind_data$locality == "ALAGOAS")] <- "costa_corais"
francini_bind_data$locality [which(francini_bind_data$locality == "BAHIA")] <- "btds_santos"
francini_bind_data$locality [which(francini_bind_data$locality == "MARANHAO")] <- "manuel_luis"
francini_bind_data$locality [which(francini_bind_data$locality == "RIOGRANDEDONORTE")] <- "rgnor"
francini_bind_data$locality [which(francini_bind_data$locality == "ROCAS")] <- "rocas"

# adjust sites (mix of reef and sites)
francini_bind_data$reef <- francini_bind_data$verbatimReef
francini_bind_data$site <- francini_bind_data$verbatimLocality
francini_bind_data$locality[which(francini_bind_data$reef == "RGNOR_NORTE")] <- "rgnor_natal"
francini_bind_data$locality[which(francini_bind_data$reef == "rgnor_parrachos")] <- "rgnor_parrachos"
francini_bind_data$locality[which(francini_bind_data$reef == "ILHASC_NORTE")] <- "ilhasc_norte"
francini_bind_data$locality[which(francini_bind_data$reef == "ILHASC_SUL")] <- "ilhasc_sul"
francini_bind_data$locality[which(francini_bind_data$reef == "arraial")] <- "arraial"
francini_bind_data$locality[which(francini_bind_data$reef == "LAJEDESANTOS")] <- "laje_santos"
francini_bind_data$locality[which(francini_bind_data$site == "ILHAGRANDE")] <- "ilha_grande"

# small corections
francini_bind_data$site <- gsub(" ","_",tolower (francini_bind_data$site))
francini_bind_data$site[which(francini_bind_data$site == "martim_vaz_")] <- "martim_vaz"

# adjust dates
francini_bind_data$eventYear <- francini_bind_data$YEAR # year
francini_bind_data$eventMonth <- francini_bind_data$MONTH # month
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "FEV")] <- "02"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "SET")] <- "09"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "SET ")] <- "09"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "MAI")] <- "05"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "NOV")] <- "11"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "JAN")] <- "01"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "JUL")] <- "07"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "MAR")] <- "03"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "MAR ")] <- "03"
francini_bind_data$eventMonth[which(francini_bind_data$eventMonth == "OUT")] <- "10"

# create dates
francini_bind_data$eventDate <- paste (francini_bind_data$eventYear,
                                       francini_bind_data$eventMonth,
                                       sep="-")

# depthInMeters
francini_bind_data$depthInMeters <- francini_bind_data$DEPTH

# define geographic region (oceanic islands,  SE, NE, S)
francini_bind_data$region <- francini_bind_data$locality
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
# edit reefs (mix of sites and reefs)
francini_bind_data$site[which(francini_bind_data$site == "mverde")] <- "mato_verde"
francini_bind_data$site[which(francini_bind_data$site == "pnorte")] <- "portinho_norte"
francini_bind_data$site[which(francini_bind_data$reef == "ITACOLOMIS")] <- "itacolomis"
francini_bind_data$site[which(francini_bind_data$reef == "ARQUIPEL")] <- "arquipel"
francini_bind_data$site[which(francini_bind_data$reef == "SGOMES")] <- "sgomes"
francini_bind_data$site[which(francini_bind_data$reef == "PAREDES")] <- "paredes"
francini_bind_data$site[which(francini_bind_data$reef == "PAB")] <- "pab"
francini_bind_data$site[which(francini_bind_data$site == "tim1")] <- "timbebas"
francini_bind_data$site[which(francini_bind_data$site == "tim2")] <- "timbebas"
francini_bind_data$site[which(francini_bind_data$site == "tim3")] <- "timbebas"
data.frame (unique(francini_bind_data$site)[order(unique(francini_bind_data$site))])

# chance colnames
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "LAT")] <- "decimalLatitude" 
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "LONG")] <- "decimalLongitude" 
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "HAB")] <- "habitat" 

# change habitats
# adjusting habitat (based on Francini-Filho et al. 2013)
francini_bind_data$habitat <- tolower(francini_bind_data$habitat)
francini_bind_data$habitat[which(francini_bind_data$habitat == "tp")] <- "pinnacles_top"
francini_bind_data$habitat[which(francini_bind_data$habitat == "pa")] <- "pinnacles_wall"

# change colnames of sites and localities
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "locality")] <- "locationID" 
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "site")] <- "locality" 







# ----------------------------------------------------------------------------------------------
# CREATING EVENTIDS, PARENTIDS, OCCURRENCEIDS
# PREPARING TO DWC FORMAT









# creating parentIDs
francini_bind_data$parentEventID <- paste (paste ("BR:RFF-USP:",
                                                  francini_bind_data$locationID,sep=""), 
                                           francini_bind_data$locality, 
                                           francini_bind_data$eventYear,
                              sep="_")




# creating eventID
francini_bind_data$eventID <- paste (paste ("BR:RFF-USP:",
                                                 francini_bind_data$locationID,sep=""), 
                                          francini_bind_data$locality, 
                                          francini_bind_data$eventYear,
                                          sep="_")



# creating occurrenceID
francini_bind_data$occurrenceID <- paste (paste ("BR:RFF-USP:",
                                            francini_bind_data$locationID,sep=""), 
                                     francini_bind_data$locality, 
                                     francini_bind_data$eventYear,
                                     paste ("occ",seq(1,nrow(francini_bind_data)),sep=""),
                                     sep="_")



# years per site (only bay has two years of sampling)
table(

    rowSums(table (francini_bind_data$locality,francini_bind_data$eventYear)>0)

)





# years per locality (only bay has two years of sampling)
table(
  
  rowSums(table (francini_bind_data$locationID,francini_bind_data$eventYear)>0)
  
)





# which years and sites
years_sites <- table (francini_bind_data$locationID,francini_bind_data$locality,francini_bind_data$eventYear)
# years_sites [,,c("2008","2011")]

# darwin core format
# measurementType
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "cover")] <- "measurementValue"

# method
francini_bind_data$samplingProtocol <- "photoquadrats"
# samplingEffort



# CHECK THIS ONCE AGAIN!!!
francini_bind_data$samplingEffort <- 5 # 5 to 33 (Following Santana et al unpublished data)





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
francini_bind_data$recordedBy <- NA
# organismQuantityType
francini_bind_data$organismQuantityType <- "Percentage cover"
# measurementType
francini_bind_data$measurementType <- "Percentage cover"
# measurementUnit
francini_bind_data$measurementUnit <- "dimensionless"
# geodeticDatum
francini_bind_data$geodeticDatum <- "decimal degrees"
# set min and max
colnames(francini_bind_data)[which(colnames(francini_bind_data) == "depthInMeters")] <- "minimumDepthinMeters"
francini_bind_data$maximumDepthinMeters <- francini_bind_data$minimumDepthinMeters

# geographic location
francini_bind_data$higherGeographyID <- ifelse (francini_bind_data$locality %in% c("noronha",
                                                                                   "trindade",
                                                                                   "stpauls_rocks"),
                                                "BrazilianIslands",
                                                "BrazilianCoast")








# ----------------------------------------------------------------------------------------------
# DWC FORMAT








# Formatted according to DwC
DF_eMOF <- francini_bind_data [,c("eventID", "occurrenceID",
                                  "verbatimIdentification",
                                  "scientificName","scientificNameID",
                                  "scientificNameOBIS","kingdom","class","family",
                                  "measurementValue", "measurementType","measurementUnit")]
DF_occ <- francini_bind_data [,c("eventID", "occurrenceID","basisOfRecord",
                                 "verbatimIdentification",
                                 "scientificName","scientificNameID",
                                 "scientificNameOBIS","kingdom","class","family",
                                 "recordedBy", "organismQuantityType", "occurrenceStatus")]

# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(francini_bind_data, eventID,higherGeographyID,locationID,verbatimLocality,locality) %>% 
                            
                            summarise(eventYear = unique(eventYear),
                                      eventDate = unique(eventDate),
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

# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)

# save RData
# txt format
write.table(DF_occ, file =here("DwC_output",
                               "RFrancini_spatialData",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)
write.table(DF_eMOF, file =here("DwC_output",
                                "RFrancini_spatialData",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)

write.table(event_core, file =here("DwC_output",
                                   "RFrancini_spatialData",
                                   "event_core.txt"),sep=",",
            quote = FALSE)

