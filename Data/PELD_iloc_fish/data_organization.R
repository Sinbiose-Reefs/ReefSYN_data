
source("R/functions.R") # function for coordinate transformation


require(here); require(openxlsx); require(dplyr); require(reshape); require(worrms)



# ------------------------------------------------------
# ORGANIZE THE FISH DATASET (PELD ILOC)



# ----------------------------------- #
# fish time series (J Quimbayo) in the oceanic islands
# ----------------------------------- #



# occurrence data
fish_DF_eMOF <- read.csv(here ("Data",
                                  "PELD_iloc_fish",
                               "DF_eMOF.txt"),sep=",",
                         encoding= "UTF-8")



# event core
fish_event_core <-  read.csv(here ("Data", 
                                      "PELD_iloc_fish",
                                   "event_core.txt"),sep=",", 
                             encoding= "UTF-8")




# identify these sites are in the islands
fish_event_core$higherGeography <- ifelse (fish_event_core$island == "Ascension", 
                                           "UnitedKingdomIslands",
                                           "BrazilianOceanicIslands")
# remove ascension
fish_event_core <- fish_event_core [which(fish_event_core$island != "Ascension"),]


# tartarugas in Trindade & Rocas
fish_event_core[grep ("Trindade_Tartarugas",fish_event_core$eventID),"locality"] <- "tartarugas_trindade"
fish_event_core$locality<-tolower(iconv(fish_event_core$locality, "UTF-8", "ASCII//TRANSLIT", sub=""))
fish_event_core$locality<-gsub (" ","_",fish_event_core$locality)

# adjusts
fish_event_core$locality[which(fish_event_core$locality == "sao_pedro__fora")] <- "sao_pedro_fora"
fish_event_core$locality<-gsub ("praia_do_", "",fish_event_core$locality)
fish_event_core$locality<-gsub ("praia_da_", "",fish_event_core$locality)

# create year
fish_event_core$year<-substr(fish_event_core$eventDate,1,4) # only year

# load occ id table
fish_DF_occ2 <- read.csv(here ("Data", 
                                  "PELD_iloc_fish",
                                  "DF_occ.txt"),sep=",",encoding= "UTF-8")



# --------------------------
# ADJUSTING SCIENTIFIC NAMES
fish_DF_occ2$scientificName <- tolower (fish_DF_occ2$scientificName)

# comments from Lucas Nunes (email to ALLuza)
# - Anisotremus virginicus não ocorre nas ilhas, trocar para Anisotremus surinamensis; OK
# - Existem nomes das espécies que estão errados, por exemplo: "Acanthostracion polygonius" o certo sería: "Acanthostracion polygonium", "Carangoides bartholomaei" -> "Caranx bartholomaei"; "hybrid_cep_ful_par_fur" -> " "Menaphorus punticulatus". Enfim, vale dar uma conferida em tudo e seguir o catalog of fishes. Não entendi muito bem a diferença entre as duas colunas com o nome de espécies, não tive acesso aos metadados.
# - Também existem alguns updates de taxonomia que merecem uma atenção, por exemplo, "Chromis multilineata" agora é "Azurina multilineata", "Ophioblennius" nas ilhas Brasileiras é "Ophioblennius trinitatis".

# correcting
fish_DF_occ2$scientificName[grep("anisotremus virginicus", fish_DF_occ2$scientificName)] <- "anisotremus surinamensis"
fish_DF_occ2$scientificName[grep("^ophioblennius$", fish_DF_occ2$scientificName)] <- "ophioblennius trinitatis"
fish_DF_occ2$scientificName[grep("menaphorus", fish_DF_occ2$scientificName)] <- "menephorus punticulatus"


# match with worms
worms_record <- lapply (unique(fish_DF_occ2$scientificName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record))

# valid name WoRMS 
fish_DF_occ2$scientificNameAccepted <- (df_worms_record$scientificname [match (fish_DF_occ2$scientificName,
                                                                                tolower (df_worms_record$scientificname))])

# taxon rank of the identified level
fish_DF_occ2$taxonRank <- (df_worms_record$rank [match (fish_DF_occ2$scientificName,
                                                           tolower (df_worms_record$scientificname))])


# match & bind
# or taxonID
fish_DF_occ2$scientificNameID<-(df_worms_record$lsid [match (fish_DF_occ2$scientificName,
                                                                tolower (df_worms_record$scientificname))])
# kingdom
fish_DF_occ2$kingdom<-(df_worms_record$kingdom [match (fish_DF_occ2$scientificName,
                                                          tolower (df_worms_record$scientificname))])

# phylum
fish_DF_occ2$phylum <-(df_worms_record$phylum [match (fish_DF_occ2$scientificName,
                                                         tolower (df_worms_record$scientificname))])

# class
fish_DF_occ2$class<-(df_worms_record$class [match (fish_DF_occ2$scientificName,
                                                      tolower (df_worms_record$scientificname))])

# order
fish_DF_occ2$order <-(df_worms_record$order [match (fish_DF_occ2$scientificName,
                                                       tolower (df_worms_record$scientificname))])

# family
fish_DF_occ2$family<-(df_worms_record$family [match (fish_DF_occ2$scientificName,
                                                        tolower (df_worms_record$scientificname))])

# genus
fish_DF_occ2$genus <-(df_worms_record$genus [match (fish_DF_occ2$scientificName,
                                                     tolower (df_worms_record$scientificname))])

# taxonomic updates
# species
fish_DF_occ2$scientificNameAccepted[grep ("multilineata", fish_DF_occ2$scientificNameAccepted)] <- "Azurina multilineata"
fish_DF_occ2$scientificNameAccepted[grep ("bartholomaei", fish_DF_occ2$scientificNameAccepted)] <- "Caranx bartholomaei"
fish_DF_occ2$scientificNameAccepted[grep ("polygonius", fish_DF_occ2$scientificNameAccepted)] <- "Acanthostracion polygonium"

# genus
fish_DF_occ2$genus[grep ("multilineata", fish_DF_occ2$scientificNameAccepted)] <- "Azurina"
fish_DF_occ2$genus[grep ("bartholomaei", fish_DF_occ2$scientificNameAccepted)] <- "Caranx"



# adjust site names
fish_event_core$island<-tolower (fish_event_core$island)
fish_event_core$locality <- tolower (fish_event_core$locality)


# save
write.csv(fish_DF_occ2, file =here("DwC_output",
                               "PELD_iloc_fish",
                               "DF_occ.csv"),#sep=",",#col.names = TRUE,
            quote = FALSE)
write.csv(fish_DF_eMOF, file =here("DwC_output",
                                "PELD_iloc_fish",
                                "DF_eMOF.csv"),#sep=",",col.names = TRUE,
            quote = FALSE)

write.csv(fish_event_core, file =here("DwC_output",
                                   "PELD_iloc_fish",
                                   "event_core.csv"),#sep=",",
            #col.names = TRUE,
            quote = T
          )

# end
rm(list=ls())
