## codigos para formatar os dados de bentos de Aued et al. 2018
## dados disponivel na PlosOne

require(here); require(dplyr); require(worrms); require (reshape)

## benthic dataset from Aued et al. 2018 (PloSOne)
bentos_amostras <- read.csv (here ("Data","occ_Aued_et_al",
                                   "compiled_quadrats_allsites.csv"),
                             h=T,fill=T)

# ALLuza modified in excel because it was too difficult to separate events from video ids
## variaveis modificadas, mas os dados de bentos ficaram num formato errado - pegar da planilha antiga
bentos_variaveis <- read.csv (here("Data","occ_Aued_et_al",
                                   "Modified_compiled_quadrats_allsites.csv"), 
                            h=T,fill=T)




# ----------------------------------------------------------------------------------------------
# ORGANIZING DATES, YEARS ...




### extract data and other info from the original samples ID
# amostras_bentos <- do.call(rbind, strsplit (as.character (bentos$Samples),"/",fixed=T))

# transforming data into long format
# 8 = the first col with taxon data in the original data of Aued et al.
bentos_long_format <- lapply (seq(8,ncol(bentos_amostras)), function (i) {
  
  subset_data <- bentos_variaveis [,c(1:13)] ## sampling descriptors
  
  subset_data <- cbind(subset_data, ## bind into the descriptors the
                       sp=colnames(bentos_amostras)[i], # taxon and its
                       bentos_amostras[,i]) ## relative cover
  
  bentos_long <- melt (subset_data,id=colnames(subset_data)[1:14]) # transf no formato long

}
)
bentos_long_format <- do.call (rbind, bentos_long_format) # melt the list
bentos_long_format <- bentos_long_format [,-grep("variable",colnames(bentos_long_format))] # rm the just created col

## adjusting dates
bentos_long_format$Data <- as.character(bentos_long_format$Data)

## set "2010" for missing dates (check again with Anaide)
novas_datas <- ifelse (nchar (bentos_long_format$Data) > 1  & nchar (bentos_long_format$Data) < 6,
                       paste (bentos_long_format$Data,"2010",sep="-"), # se falta ano/em branco, coloca 1999
                       bentos_long_format$Data)## se nao mantenha igual

## dates in format YYYY-mm-dd
novas_datas <- paste (substr (novas_datas,7,10), 
                      substr (novas_datas,4,5), 
                      substr (novas_datas,1,2),sep="-")

### bind in the dataset the different dates
bentos_long_format$eventDate <- as.Date(novas_datas, format="%Y-%m-%d")
bentos_long_format$eventDay <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%d")
bentos_long_format$eventMonth <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%m")
bentos_long_format$eventYear <- format(as.Date(novas_datas, format="%Y-%m-%d"),"%Y")

## return 1999 as Not available
# bentos_long_format$eventYear [which(bentos_long_format$eventYear == "1999")] <- "NA"
#plot(bentos_long_format$Lon , bentos_long_format$Lat,xlim=c(-70,10))





# ----------------------------------------------------------------------------------------------
# ORGANIZING DEPTHS, REGIONS, SITES






## define whether samples were in deep or shallow
bentos_long_format$eventDepth <- as.factor(bentos_long_format$Depth.1)
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "1-7m")] <- "shallow"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "4-7m")] <- "shallow"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "8-12m")] <- "deep"
levels (bentos_long_format$eventDepth) [which(levels (bentos_long_format$eventDepth) == "8-15m")] <- "deep"

## define whether samples were in oceanic islands,  Southeastern or Northeastern regions
bentos_long_format$Region <- as.factor (bentos_long_format$Locality)
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
bentos_long_format$verbatimLocality <- bentos_long_format$Sites
# to lower 
bentos_long_format$Sites <- tolower (bentos_long_format$Sites)
bentos_long_format$Sites[which((bentos_long_format$Sites) == "farrilhoes")] <- "farilhoes"
bentos_long_format$Sites[which( (bentos_long_format$Sites) == "orelha")] <- "orelhas"

# ajustar os sitios de rio grande do norte, de acordo com G Longo
bentos_long_format$Locality[which(bentos_long_format$Locality == "rgnor_norte")] <- "rgnor_natal"

# tartaruras in rocas and trindade (only rocas in this dataset)
# unique(bentos_long_format [which(bentos_long_format$Locality == "trindade"),"Sites"])
# removing locality names from location name
bentos_long_format$Sites<-gsub ("arvoredo_", "",bentos_long_format$Sites)
bentos_long_format$Sites<-gsub ("_noronha", "",bentos_long_format$Sites)
# unique(bentos_long_format$Sites)[order(unique(bentos_long_format$Sites))]






# ----------------------------------------------------------------------------------------------
# ADJUSTING COORDINATES





## coordinates to spatial points
# adjusting longitude of one coordinate on land
## "saco dagua" which falls within the continent
bentos_long_format [grep("saco_dagua",bentos_long_format$Sites),"Lat"] <- as.numeric(-27.274033)
bentos_long_format [grep("saco_dagua",bentos_long_format$Sites),"Lon"] <- as.numeric(-48.367183)







# ----------------------------------------------------------------------------------------------
# SOME DATES WERE MISSING. NOW WE'RE IMPUTING BASED ON INFORMATION GATHERED WITH THE AUTHORS






### define an ID for each event (first try to define one)
bentos_long_format$eventID <- paste (bentos_long_format$Region,
                                     bentos_long_format$Locality,
                                     bentos_long_format$Sites,
                                     bentos_long_format$eventDepth,
                                     bentos_long_format$eventYear,sep=".")


bentos_long_format[bentos_long_format$Sites == "anequim",]

## adjust missing dates following supporting information of Aued et al. 2018
unique(bentos_long_format [which(is.na(bentos_long_format$eventYear)),"eventID"]) # check missing ones
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.noronha.conceicao.deep.NA"),"eventMonth"] <- 10
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.noronha.conceicao.deep.NA"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.noronha.conceicao.shallow.NA"),"eventMonth"] <- 10
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.noronha.conceicao.shallow.NA"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.rgnor_natal.pedra_do_silva.deep.NA"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.rgnor_natal.pedra_do_silva.deep.NA"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.btds_santos.farol_da_barra.shallow.NA"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.btds_santos.farol_da_barra.shallow.NA"),"eventYear"] <- 2012

## replace 1999 by the correct year following Aued et al. 2018
# unique(bentos_long_format [which(bentos_long_format$eventYear == "1999"),"eventID"]) ## conferir os NAs
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.abrolhos.portinho_norte.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "ne_reefs.abrolhos.portinho_norte.shallow.1999"),"eventYear"] <- 2010
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.ilha_das_cabras.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.ilha_das_cabras.shallow.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.ilha_das_cabras.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.ilha_das_cabras.deep.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.alcatrazes.portinho_sudoeste.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.alcatrazes.portinho_sudoeste.shallow.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.alcatrazes.portinho_sudoeste.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.alcatrazes.portinho_sudoeste.deep.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_diogo.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_diogo.shallow.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_diogo.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_diogo.deep.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_sombrio.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_sombrio.deep.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_sombrio.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.ilhabela.saco_do_sombrio.shallow.1999"),"eventYear"] <- 2013
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.ancoras.shallow.1999"),"eventMonth"] <- 01
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.ancoras.shallow.1999"),"eventYear"] <- 2012
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.podes_crer.shallow.1999"),"eventMonth"] <- 01
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.podes_crer.shallow.1999"),"eventYear"] <- 2012
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.piscina_das_rocas.shallow.1999"),"eventMonth"] <- 01
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.piscina_das_rocas.shallow.1999"),"eventYear"] <- 2012
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.salao.deep.1999"),"eventMonth"] <- 01
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.salao.deep.1999"),"eventYear"] <- 2012
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.tartarugas.shallow.1999"),"eventMonth"] <- 01
bentos_long_format [which(bentos_long_format$eventID == "oc_isl.rocas.tartarugas.shallow.1999"),"eventYear"] <- 2012
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.anequim.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.anequim.shallow.1999"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.anequim.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.anequim.deep.1999"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.cardeiros.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.cardeiros.deep.1999"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.cardeiros.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.cardeiros.shallow.1999"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.porcos_oeste.shallow.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.porcos_oeste.shallow.1999"),"eventYear"] <- 2011
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.porcos_oeste.deep.1999"),"eventMonth"] <- 03
bentos_long_format [which(bentos_long_format$eventID == "se_reefs.arraial.porcos_oeste.deep.1999"),"eventYear"] <- 2011

# change colnames
colnames(bentos_long_format) <- c("verbatimSamples", "modifiedSamples", "depth","data",
                                  "recordedBy", "device", "photoquadrat", 
                                  "locality", "sites", "reefType","decimalLongitude", "decimalLatitude",
                                  "modifiedDepth", "scientificName", "measurementValue",
                                  "eventDate", "eventDay", "eventMonth", "eventYear","eventDepth", "region",
                                  "verbatimLocality",
                                  "eventID")






# ----------------------------------------------------------------------------------------------
# ADJUSTING SCIENTIFIC NAMES BASED ON THE KNOWLEDGE OF CESAR A.M.M. CORDEIRO

### nem tudo aqui eh nome cientifico e alguns nem tem como incluir num taxon, podemos deixar no script como taxonOrGroup apenas 
### pra organizar e esta coluna sumiria na planilha final do DwC, deixando soh scientificName (apos validar no WoRMS) e o verbatimIdentification



bentos_long_format$verbatimIdentification <- bentos_long_format$scientificName
bentos_long_format$scientificName <-  (gsub("\\."," ",bentos_long_format$scientificName)) # replace dot by space
bentos_long_format$scientificName <-(iconv(bentos_long_format$scientificName, "ASCII", "UTF-8", sub="")) # encoding
bentos_long_format$scientificName <- tolower(bentos_long_format$scientificName) # lower case


# adjust based on knowledge of Cesar Cordeiro

bentos_long_format$scientificName[which(bentos_long_format$scientificName == "ventricaria ventricosa")] <- "valonia ventricosa"

# broader groups
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "anemona")] <- "actiniaria"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "leathery")] <- "leathery algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "spirobidae - polycchaete")] <- "spirorbinae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "briozoa")] <- "bryozoa"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "bryozoan")] <- "bryozoa"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "hidrozoan")] <- "hydrozoa"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "outro hydrozoa")] <- "hydrozoa"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "poliqueta")] <- "polychaeta"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "polichaeta")] <- "polychaeta"
bentos_long_format$scientificName[which(bentos_long_format$scientificName %in% c("ascidea colonial" ,                  
                                                             "ascidian",
                                                             "outra ascidia"))] <- "ascidiacea"

# octocoral and anthozoa
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "outro anthozoa")] <- "anthozoa"
bentos_long_format$scientificName[grep("octocoral",bentos_long_format$scientificName)] <- "octocorallia" # "alcyonaria" nao eh aceito
# sponge
bentos_long_format$scientificName[grep("sponge",bentos_long_format$scientificName)] <- "porifera"
# echinoderms
bentos_long_format$scientificName[grep("ourigo",bentos_long_format$scientificName)] <- "echinoidea" # ourico (ourigo deviaod à conversao pra encoding utf 8)
bentos_long_format$scientificName[grep("sea urchin",bentos_long_format$scientificName)] <- "echinoidea"
bentos_long_format$scientificName[grep("outro echinoderma",bentos_long_format$scientificName)] <- "echinodermata"
bentos_long_format$scientificName[grep("crinside",bentos_long_format$scientificName)] <- "crinoidea"# crinoidea (crinside deviaod à conversao pra encoding utf 8)
bentos_long_format$scientificName[grep("estrela",bentos_long_format$scientificName)] <- "asteroidea"


### melhor nao indicar grupo morfo-anatomico (MAG) como scientificName. Esse MAG nao tem compativel pra inseir no DwC/OBIS
### Vou indicar o nivel taxonomico compativel com o que tiver e o resto deixamos como estava 
### Os grupos morfo-anatomicos podem ser adicionados com merge de tabela referencia depois

# cca and caa 
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "crostose coralline algae")] <- "corallinales" # "crustose coralline algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "cianobacterias")] <- "cyanobacteria"
bentos_long_format$scientificName[which(bentos_long_format$scientificName %in% c("amphiroa", # melhor deixar o genero mesmo
                                                             "amphiroa sp", # melhor deixar o genero mesmo
                                                             "amphiroideae", # "amphiroideae"
                                                             "jania amphiroa", # "amphiroideae"
                                                             "jania sp", # melhor deixar o genero mesmo
                                                             "unknown articulated coralline algae"))] <- "amphiroideae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "filamentous")] <- "filamentous algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "green filamentous algae")] <- "chlorophyta" # "filamentous algae"

# algae
bentos_long_format$scientificName[which(bentos_long_format$scientificName %in% c("fleshy algae", # usando o taxaOrGroup podemos manter nessa categoria e ficaria sem scientificName
                                                             "foliaceous algae",
                                                             "foliose",
                                                             "frondose algae", 
                                                             "unknown foliose"))] <- "foliose algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "calcareous turf")] <- "corallinales" # "calcareous articulate algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "corticated")] <- "corticated algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "unknown corticated")] <- "corticated algae"
bentos_long_format$scientificName[which(bentos_long_format$scientificName == "sargassum sp")] <- "sargassum" # leathery algae"

unique(bentos_long_format$scientificName)[order(unique(bentos_long_format$scientificName))]


# matching scientificNames with worms

# remove plot data
bentos_long_format<-bentos_long_format [which(bentos_long_format$scientificName %in% c("sombra","quadrado","areia e cascalho","desconhecido" ) != T),]

# match with worms
worms_record <- lapply (unique(bentos_long_format$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))




# valid name OBIS -> o OBIS eh soh o repositorio, o WoRMS que faz a validacao mesmo entao nao faz muito sentido
# o termo do DwC pro scientificName apos a validacao pode ser scientificNameAccepted. Nesse caso os grupos morfo-anatomicos do bentos
# nao vao interferir no DwC mas vao continuar lah. Apesar do termo nao estar no DwC, o proprio WoRMS usa scientificNameAccepted.

# valid name WoRMS 
bentos_long_format$scientificNameOBIS <- (df_worms_record$scientificname [match (bentos_long_format$scientificName,
                                                                   tolower (df_worms_record$scientificname))])

# match & bind
bentos_long_format$scientificNameID<-(df_worms_record$lsid [match (bentos_long_format$scientificName,
                                                                   tolower (df_worms_record$scientificname))])
# kingdom
bentos_long_format$kingdom<-(df_worms_record$kingdom [match (bentos_long_format$scientificName,
                                                             tolower (df_worms_record$scientificname))])
# class
bentos_long_format$class<-(df_worms_record$class [match (bentos_long_format$scientificName,
                                                         tolower (df_worms_record$scientificname))])
# family
bentos_long_format$family<-(df_worms_record$family [match (bentos_long_format$scientificName,
                                                           tolower (df_worms_record$scientificname))])






# ----------------------------------------------------------------------------------------------
# CREATING EVENTIDS, PARENTIDS, OCCURRENCEIDS
# PREPARING TO DWC FORMAT





# se formos seguir a logica do REEFSyn como guarda-chuva, temos que lembrar de inserir o BR:REEFSyn:SISBIOTA-MAR: 
# BrazilianOceanicIslands -> temos alguns monitoramentos em ilhas costeiras


## substituir a ID antiga pela nova com os anos ajustados (new eventID)
bentos_long_format$eventID <- paste (
  paste ("BR:SISBIOTA-MAR:", 
         bentos_long_format$locality,sep=""),
  bentos_long_format$sites,
  bentos_long_format$eventYear,
  bentos_long_format$photoquadrat,
  sep="_")


# occurrenceID
bentos_long_format$occurrenceID <- paste (
                  paste ("BR:SISBIOTA-MAR:", 
                  bentos_long_format$locality,sep=""),
              bentos_long_format$sites,
              bentos_long_format$eventYear,
              bentos_long_format$photoquadrat,
                  paste ("occ",seq(1,nrow(bentos_long_format)),sep=""),
          sep="_")

# creating parentEventids
bentos_long_format$parentEventID <- paste (
  paste ("BR:SISBIOTA-MAR:", 
         bentos_long_format$locality,sep=""),
  bentos_long_format$site,
  bentos_long_format$eventYear,
  sep="_")

# method
bentos_long_format$samplingProtocol <- "photoquadrats"
# samplingEffort
bentos_long_format$samplingEffort <- 5 # 5 quadrats, 25 * 25 cm
# sampleSizeValue
bentos_long_format$sampleSizeValue <- 0.25*0.25
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
bentos_long_format$minimumDepthinMeters[which(bentos_long_format$eventDepth == "shallow")] <- 1
bentos_long_format$maximumDepthinMeters[which(bentos_long_format$eventDepth == "shallow")] <- 7
bentos_long_format$minimumDepthinMeters[which(bentos_long_format$eventDepth == "deep")] <- 8
bentos_long_format$maximumDepthinMeters[which(bentos_long_format$eventDepth == "deep")] <- 15

# adjusting colname of sites
# locationID (less local)
colnames(bentos_long_format)[which(colnames(bentos_long_format) == "locality")] <- "locationID"
# locality (more local)
colnames(bentos_long_format)[which(colnames(bentos_long_format) == "sites")] <- "locality"
# geographic location
bentos_long_format$higherGeographyID <- ifelse (bentos_long_format$locationID %in% c("rocas",
                                                                                   "noronha",
                                                                                   "trindade"),
                                                "BrazilianIslands", 
                                                "BrazilianCoast")


# ----------------------------------------------------------------------------------------------
# DWC FORMAT





# Formatted according to DwC
# measurement or facts
DF_eMOF <- bentos_long_format [,c("eventID", "occurrenceID",
                                  "verbatimIdentification","scientificName",
                                  "scientificNameID","scientificNameOBIS",
                                  "kingdom","class","family",
                                  "measurementValue", "measurementType","measurementUnit")]
# occurrence
DF_occ <- bentos_long_format [,c("eventID", "occurrenceID","basisOfRecord",
                                 "verbatimIdentification",
                                 "scientificName","scientificNameID",
                                 "scientificNameOBIS",
                                 "kingdom","class","family",
                                 "recordedBy", "organismQuantityType", "occurrenceStatus")]

# aggregate data by eventIDs to have event_core
event_core <- data.frame (group_by(bentos_long_format, eventID,higherGeographyID,locationID,verbatimLocality,locality) %>% 
                            
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
# make a list with files in DwC
output <- list (DF_occ = DF_occ,
                DF_eMOF = DF_eMOF,
                event_core=event_core)
# save
# txt format
write.table(DF_occ, file =here("DwC_output",
                               "AAued_spatialData",
                               "DF_occ.txt"),sep=",",
            quote = FALSE)
write.table(DF_eMOF, file =here("DwC_output",
                                "AAued_spatialData",
                                "DF_eMOF.txt"),sep=",",
            quote = FALSE)

write.table(event_core, file =here("DwC_output",
                                   "AAued_spatialData",
                                   "event_core.txt"),sep=",",
            quote = FALSE)

