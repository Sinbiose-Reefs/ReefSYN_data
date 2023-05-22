### teste datasets
library(readr)

## AAued_spatialData (OK)

DF_eMOF <- read_csv("DwC_output/AAued_spatialData/DF_eMOF.csv") 
DF_occ <- read_csv("DwC_output/AAued_spatialData/DF_occ.csv")
event_core <- read_csv("DwC_output/AAued_spatialData/event_core.csv")

lapply(event_core, unique) 
lapply(DF_occ, unique) # recordedBy nomes nao padronizados --> ALL: Nomes ajustados

# $recordedBy
# "Thiago" "Renato" "Gui"    "ide"    "ju"     "lu"     "juan"   "anaide"
# "davi"   "edson"  "renata"

# ADD RESOURCE CREATORS - TODOS? 

# DATES
event_core %>% 
  distinct(eventDate) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% summary()
data.frame()

DF_occ %>% 
  mutate(recordedBy = plyr::mapvalues(recordedBy, 
                                      from = c("Thiago", "tc_mendes", "mendes_tc","thiago",
                                               "Renato", "morais_ra","renato",
                                               "Gui","GOL","guilherme",
                                               "ide", "anaide" ,
                                               "ju","luisa"
                                               "lu",
                                               "juan", "quimbayo_jp",
                                               "davi",
                                               "edson",
                                               "renata",
                                               "batista_a",
                                               "cordeiro_camm", "cesar",
                                               "barbosa_m",
                                               "giglio_vj",
                                               "NCR",
                                               "JB",
                                               "GSG",
                                               "LE",
                                               "KYI",
                                               "EAV",
                                               "MCP",
                                               "marina",
                                               "diego",
                                               "roberta",
                                               "davi",
                                               "max" ),
                                      "cel_ferreira"     "cgw_ferreira"     "r_noguchi"        "tc_mendes"
                                      "camm_cordeiro"    "mc_barbosa"       "jp_quimbayo"      "mb_lucena"
                                      "r_mazzei"         "l_eggertsen"      "cbp_eirado-silva" NA
                                      "go_correal"       "ra_morais"
                                      "ramon_noguchi"      "gugaw_ferreira"     "cel_ferreira"       "bertran_feitoza"
                                      "eduardo_godoy"      "ca_rangel"          "thiago_mendes"      "jp_quimbayo"
                                      "renata_mazzei"      "renato_morais"      "sergio_floeter"     "ana_liedke"
                                      "jl_gasparini"       "jp_krajewski"       "hudson_pinheiro"    "gabriel_ferreira"
                                      "anaide_aued"        "claudio_sampaio"    "cesar_cordeiro"     "thiony_simon"
                                      "diego_barneche"     "anderson_batista"   "tiago_albuquerque"  "anchieta_nunes"
                                      "daniel_dinslaken"   NA                   "gabriel_correal"    "osmar_luiz"
                                      "marcelo_silveira"   "andrea_dalben"      "alexandre_siqueira" "max_levy"
                                      "guilherme_longo"    "luisa_fontoura"     "athila_bertoncini"
                                      to = c("Thiago C Mendes","Thiago C Mendes","Thiago C Mendes",
                                             "Renato A Morais","Renato A Morais",
                                             "Guilherme O Longo",
                                             "Anaide W Aued","Anaide W Aued",
                                             "ju", "lu",
                                             "Juan P Quimbayo","Juan P Quimbayo",
                                             "davi", "edson",
                                             "Renata Mazzei",
                                             "batista_a",
                                             "cordeiro_camm",
                                             "barbosa_m",
                                             "giglio_vj",
                                             "Natalia Roos"))
         
         [1] "Hudson Pinheiro"    "Carlos EL Ferreira" "Renato Morais"      "João P Krajewski"
         [5] "Sérgio R Floeter"   "Ramon Noguchi"      "Gabriel Ferreira"   "Eduardo Godoy"
         [9] "Bertran Feitoza"    "Carlos Rangel"      "Moyses C Barbosa"   "Ana Liedke"
         [13] "Marcos B Lucena"    "Juan P Quimbayo"    "Anaide W Aued"      "Vinícius Giglio"
         [17] "Thiago C Mendes"    "Renata CB Mazzei"   "João L Gasparini"   "Cesar AMM Cordeiro"
         [21] "Thiony Simon"       "Helder Guabiroba"   "Lucas Nunes"        "Anderson Batista"
         [25] "Linda Eggertsen"    "Luísa Fontoura"     "Caio Pimentel"      "Natália Roos"
         [29] "Isadora Cord"
         
         lapply(DF_eMOF, unique) 
         
         # remover primeira coluna de todos
         summary(AA_DF_eMOF)
         summary(AA_DF_occ)
         summary(AA_event_core)
         
         AA_DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         AA_DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         AA_event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(AA_DF_eMOF$eventID), unique(AA_DF_occ$eventID))
         setdiff(unique(AA_DF_eMOF$eventID), unique(AA_event_core$eventID))
         setdiff(unique(AA_DF_occ$eventID), unique(AA_event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(AA_DF_eMOF$occurrenceID), unique(AA_DF_occ$occurrenceID))
         setdiff(unique(AA_DF_eMOF$occurrenceID), unique(AA_event_core$occurrenceID))
         setdiff(unique(AA_DF_occ$occurrenceID), unique(AA_event_core$occurrenceID))
         
         # tipos de cobertura
         AA_DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         rm(list = ls())
         
         ##############################################################################
         ##############################################################################
         
         
         ## Alcatrazes (ERRO conferir spp) --> ALL: feito: nomes ajustados, e spp conferida com Juan


         DF_eMOF <- read_csv("DwC_output/Alcatrazes_time_series/DF_eMOF.csv") 
         DF_occ <- read_csv("DwC_output/Alcatrazes_time_series/DF_occ.csv")
         event_core <- read_csv("DwC_output/Alcatrazes_time_series/event_core.csv")
         
         lapply(event_core, unique) 
         lapply(DF_occ, unique) # recordedBy nomes nao padronizados
         # $recordedBy
         # [1] "morais_ra"     "batista_a"     "tc_mendes"     "cordeiro_camm" "barbosa_m"    
         # [6] "quimbayo_jp"   "mendes_tc"     "giglio_vj"
         lapply(DF_eMOF, unique) # recordedBy nomes nao padronizados --> ALL: resolvido
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ) --> ALL: Resolvido
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         # Chromis limbaughi errado... provavelmente Stegastes pictus --> ALL: Chromis limbata, conferido com Juan 
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         
         ## Glongo Ross (ERRO)
         
         # fish
         DF_eMOF <- read_csv("DwC_output/GLongo_NRoss_spatialData/DF_eMOF_fish.csv") 
         DF_occ <- read_csv("DwC_output/GLongo_NRoss_spatialData/DF_occ_fish.csv")
         event_core <- read_csv("DwC_output/GLongo_NRoss_spatialData/event_core_fish.csv")
         
         lapply(event_core, unique) # locality -> trocar barra por underline --> ALL: resolvido
         lapply(DF_occ, unique) # recordedBy soh com siglas
         # $recordedBy
         # [1] "NCR" "GOL" "JB"  "GSG" "LE"  "KYI" "EAV" "MCP" --> ALL: resolvido: nomes ajustados
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ) --> ALL: resolvido
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         

         # Trachinotus ovatus errado... --> ALL: Kelly Inagaki falou que está correto
         # Eunostomus argenteus --> ALL: Kelly --> Eucinostomus argenteus
         # Sparisoma
         # Myrichtys ocellatus
         # Rypticus saponaceous --> ALL: Rypticus saponaceus
         # Haemulon crysargirum
         # Chromis scotti checar
         # Acanthostracion polygonia
         # Dasyatis marianae
         # Sardinella brasiliensis?? ver se mudou em Natal tb

	 # ALL : respostas da KElly INagaki pelo whatsapp

	 # Trachinotus ovatus (tá errado?)
         # Eucinostomus argenteus (corrigido)
         # Sparisoma (é o gênero; talvez tbm encontre como Sparisoma sp)
         # Myrichthys ocellatus (corrigido)
         # Rypticus saponaceous (tá correto) 
         # Brachygenys chrysargyreum (corrigido; antes tava Haemulon crysargirum)
         # Chromis scotti checar (de onde veio esse? não achei na planilha. Essa espécie não ocorre no brasil) ----> Gui Longo: Chromis scotti acontece na margem setentrional do Brasil, já tem registro na PB também
         # Acanthostracion polygonium (corrigido; antes tava Acanthostracion polygonia)
         # Hypanus marianae (corrigido; antes tava Dasyatis marianae)
         # Sardinella brasiliensis (correto)



         rm(list = ls())


         
         #######################################
         # benthos (OK)
         DF_eMOF <- read_csv("DwC_output/GLongo_NRoss_spatialData/DF_eMOF_benthos.csv") 
         DF_occ <- read_csv("DwC_output/GLongo_NRoss_spatialData/DF_occ_benthos.csv")
         event_core <- read_csv("DwC_output/GLongo_NRoss_spatialData/event_core_benthos.csv")
         
         lapply(event_core, unique)
         lapply(DF_occ, unique) 
         # $recordedBy
         # [1] "Natalia Roos" --> ALL: Natalia C Roos
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ) --> ALL: resolvido
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         
         ##############################################################################
         ##############################################################################
         
         ## Glongo Spatial (ERRO - conferir spp)
         
         # fish
         DF_eMOF <- read_csv("DwC_output/GLongo_spatialData/DF_eMOF.csv") 
         DF_occ <- read_csv("DwC_output/GLongo_spatialData/DF_occ.csv")
         event_core <- read_csv("DwC_output/GLongo_spatialData/event_core.csv")
         
         lapply(event_core, unique)
         lapply(DF_occ, unique) # recordedBy nao padronizado ----> ALL: resolvido
         # $recordedBy
         # [1] "juan"      "anaide"    "marina"    NA          "renato"    "guilherme" "thiago"   
         # [8] "diego"     "roberta"   "davi"      "edson"     "cesar"     "luisa"     "max" 


         lapply(DF_eMOF, unique) # measurementValue nao padronizado ou sem legenda ---> como assim? Tem o Type and Unit associados?
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()

         # descobrir o q esse NA significa, provavelmente occ sem interacao. --> ALL: sim, confirmei com o Gui.
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head


         # checar sem data
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         # Sphyraena borealis -----> ALL: na verbatimIdentification está "sphyraena_borealis?" --> Gui confirmou que está correto
         # Serranus atricauda -> Serranus flaviventris -----> ALL: feito
         # Sardinella brasiliensis?? ver se mudou em Natal tb
         # Ophioblennius macclurei
         # Malacoctenus triangulatus
         # Malacoctenus delalandii
         # Epinephelus marginatus
         # Chilomycterus spinosus spinosus
         # Arcos rhodospilus
         


	--->	ALL: resposta do Gui
	# Sphyraena borealis✔️
         # Serranus atricauda -> Serranus flaviventris✔️
         # Sardinella brasiliensis?? ver se mudou em Natal tb
         # Ophioblennius macclurei
         # Malacoctenus triangulatus✔️
         # Malacoctenus delalandii✔️
         # Epinepheluus marginatus VIROU MYCTEROPERCA MARGINATA ---> ALL: corrigido
         # Chilomycterus spinosus ✔️
         # Arcos rhodospilus


         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## PELD_iloc_benthos (ERRO - 48 uniqueID no event_core que nao existem no occ) -> cobertura 100% areia ou outra categoria que nao eh biologica
         # nome ilhas ou site com acento: island ("s~ao_pedro_e_s~ao_paulo"), locality ("laje_dois_irm~aos", "cemit'erio")

         # ------> ALL: removi os ids faltantes

         # 
         DF_eMOF <- read_csv("DwC_output/PELD_iloc_benthos/DF_eMOF.csv") 
         DF_occ <- read_csv("DwC_output/PELD_iloc_benthos/DF_occ.csv")
         event_core <- read_csv("DwC_output/PELD_iloc_benthos/event_core.csv")
         
         lapply(event_core, unique) # nome locality ("laje_dois_irm~aos", "cemit'erio") e island ("s~ao_pedro_e_s~ao_paulo") com acento ---> ALL: aqui nao aparece acento

         # $island
         # [1] "fernando_de_noronha"     "atol_das_rocas"          "s~ao_pedro_e_s~ao_paulo" ---> ALL: aqui consta "fernando_de_noronha"   "atol_das_rocas"        "sao_pedro_e_sao_paulo" "trindade" 
         # [4] "trindade" 


         # [3] "são pedro e são paulo" -----> ALL: stpauls_rocks


         lapply(DF_occ, unique)
         lapply(DF_eMOF, unique)

         # $analyzedBy
         # [1] "Julia B Zamoner"   "Manoela Birolli"   "Vitor Picolotto"   "Diana Vergara"    
         # [5] "Carolina Medeiros"
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         
         setdiff(unique(benthos_event_core$eventID), unique(benthos_DF_occ2$eventID))
         
         filter(benthos_event_core, eventID %in% setdiff(unique(benthos_event_core$eventID), unique(benthos_DF_occ2$eventID)))
         
         lapply(benthos_event_core, unique)
         
         # tem valores no eMOF q nao tem no occ
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ) ----> ALL: feito
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         # tem valores no eMOF q nao tem no occ
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## PELD_iloc_fish (ERRO)
         
         # 
         DF_eMOF <- read_csv("DwC_output/PELD_iloc_fish/DF_eMOF.csv") 
         DF_occ <- read_csv("DwC_output/PELD_iloc_fish/DF_occ.csv")
         event_core <- read_csv("DwC_output/PELD_iloc_fish/event_core.csv")
         
         lapply(event_core, unique) # nome locality e island com acento
         # $island
         # [1] "trindade"              "noronha"               "rocas"                

         # [4] "são pedro e são paulo" -----> ALL: stpauls_rocks
         
         # locality
         # "conceic~ao", "t'unel", "farilh~oes", "b'oia", "naufr'agio", "sal~ao", "laje_dois_irm~aos" --> ALL: aqui aparece tudo sem acento
         


	lapply(DF_occ, unique) # ainda tem dados de Ascensao / recordedBy "João P Krajewski", "Vinícius Giglio", "Natália Roos",  "Luísa Fontoura"  

	-------> Dados de Ascensao removidos

         # $recordedBy
         # [1] "Hudson Pinheiro"    "Carlos EL Ferreira" "Renato Morais"      "João P Krajewski"  
         # [5] "Sérgio R Floeter"   "Ramon Noguchi"      "Gabriel Ferreira"   "Eduardo Godoy"     
         # [9] "Bertran Feitoza"    "Carlos Rangel"      "Moyses C Barbosa"   "Ana Liedke"        
         # [13] "Marcos B Lucena"    "Juan P Quimbayo"    "Anaide W Aued"      "Vinícius Giglio"   
         # [17] "Thiago C Mendes"    "Renata CB Mazzei"   "João L Gasparini"   "Cesar AMM Cordeiro"
         # [21] "Thiony Simon"       "Helder Guabiroba"   "Lucas Nunes"        "Anderson Batista"  
         # [25] "Linda Eggertsen"    "Luísa Fontoura"     "Caio Pimentel"      "Natália Roos"      
         # [29] "Isadora Cord" 

         -------> ALL: Nomes dos observadores ajustados



         lapply(DF_eMOF, unique) # ainda tem dados de Ascensao ----> ALL: removido
         
         dplyr::filter(event_core, eventID == "BR:PELD-ILOC:Ascension_Guano_Jetty_2015_24")
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()

         # 1 Hal bra de Trindade sem tamanho

         lapply(DF_eMOF, unique)
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         # 81 q nao tem no event_core
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         # mesmos 81 q nao tem no event_core
         
         
         # occurrenceID (nao precisa ter no event_core, soh no occ) -----------> ALL: resolvido
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))

         # tem valores no eMOF q nao tem no occ -------->  ALL: resolvido
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         # tirar NA
         filter(DF_occ, is.na(scientificNameAccepted)) %>% distinct(scientificName)
         # Menaphorus punticulatus ---> ALL: creio estar OK agora. Encontro 'Menephorus'

         # tirar dados de Ascencao?? ----> ALL: feito
         # Chilomycterus spinosus mauretanicus => Chilomycterus mauretanicus
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## RFrancini_spatialData (OK)
         
         # 
         DF_eMOF <- read_csv("DwC_output/RFrancini_spatialData/DF_eMOF.csv") 
         DF_occ <- read_csv("DwC_output/RFrancini_spatialData/DF_occ.csv")
         event_core <- read_csv("DwC_output/RFrancini_spatialData/event_core.csv")
         
         lapply(event_core, unique)
         # $site
         # [1] "espirito_santo" "laje_santos"    "paraiba"        "rio_de_janeiro" "noronha"       
         # [6] "stpauls_rocks"  "trindade"  
         lapply(DF_occ, unique)  
         # $recordedBy
         # [1] "R Francini-Filho, E Santana, A Aued"
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         # 81 q nao tem no event_core
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         # mesmos 81 q nao tem no event_core
         
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         # tem valores no eMOF q nao tem no occ
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## RFrancini_timeSeries_abrolhos (ERRO)
         
         # fish
         DF_eMOF <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/DF_eMOF_fish.csv") 
         DF_occ <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/DF_occ_fish.csv")
         event_core <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/event_core_fish.csv")
         
         lapply(event_core, unique) #
         # $site
         # [1] "ARQUIPELAGO"         "ITACOLOMIS"          "PARCEL DOS ABROLHOS" "PAREDES"            
         # [5] "TIMBEBAS"     
         lapply(DF_occ, unique) # recordedBy nao padronizado --- ALL: nomes ajustados

         # $recordedBy
         # [1] "RON"     "BEL"     "ROD"     "BRU"     "ERIC"    "CAP"     "GRA"     "RIC"    
         # [9] "RLM"     "ITA"     "SARA"    "CAIO"    "XAL"     "CHALIE"  "CAI"     "COS"    
         # [17] "ERI"     "CAM"     "ERICKA"  "MAGRA"   "RODRI"   "MOU"     "MOURA"   NA       
         # [25] "ERICA"   "CAMI"    "NO"      "ROM"     "EDU"     "PEDRO"   "PED"     "ROR"    
         # [33] "REIS"    "MAR"     "CAMILO"  "MAROCCI" "DEM"     "Daniel"  "Aline"   "Gibran" 
         # [41] "Nicole"  "Magra"   "ALINE"   "HUDSON"  "GIBRAN"  "NICOLE"  "SARTOR" 
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         # Microgobius carri -> checar --> ALL: R Francini-Filho confirmou que está correto
         # Ctenogobius saepepallens -> checar --> ALL: R Francini-Filho confirmou que está correto
         # Hypanus berthalutzea -> Hypanus berthalutzae--> ALL: Ajustado
         
         rm(list = ls())
         
         
         #######################################
         # benthos (OK)
         DF_eMOF <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/DF_eMOF_benthos.csv") 
         DF_occ <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/DF_occ_benthos.csv")
         event_core <- read_csv("DwC_output/RFrancini_timeSeries_abrolhos/event_core_benthos.csv")
         
         lapply(event_core, unique) # site nao padronizado
         # $site
         # [1] "amp ita"    "itacolomis" "paredes"    "sgomes"     "timbebas"   "arquipel"  
         # [7] "pab"  
         lapply(DF_occ, unique)
         # $recordedBy
         # [1] "Ronaldo Francini-Filho"
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## RJ_time_series (ERRO)
         
         DF_eMOF <- read_csv("DwC_output/RJ_time_series/DF_eMOF.csv")
         DF_occ <- read_csv("DwC_output/RJ_time_series/DF_occ.csv")
         event_core <- read_csv("DwC_output/RJ_time_series/event_core.csv")
         
         lapply(event_core, unique)
         lapply(DF_occ, unique) # recordedBy sigla_sobrenome -----> ALL: resolvido
         # $recordedBy
         # [1] "cel_ferreira"     "cgw_ferreira"     "r_noguchi"        "tc_mendes"       
         # [5] "camm_cordeiro"    "mc_barbosa"       "jp_quimbayo"      "mb_lucena"       
         # [9] "r_mazzei"         "l_eggertsen"      "cbp_eirado-silva" NA                
         # [13] "go_correal"       "ra_morais"    
         lapply(DF_eMOF, unique)
         
         # remover primeira coluna de todos
         summary(DF_eMOF)

         DF_eMOF %>% data.frame() %>% head

         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()

         # ver o q eh NA aqui ----> ALL: vendo com o Thiago
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head


         # NA datas ---> ALL: resolvido
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)   --------> ALL: resolvido
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         filter(DF_occ, is.na(scientificNameAccepted)) %>% data.frame()


         # verificar verbatimIdentification #N/A -> transeco vazio? -------> ALL: sao esses codes: "che_myd" "bat_spp" --> creio que #N/A não seja peixe
	 ---> resposta do THiago: che_myd é tartaruga / 
	
	

         # Scorpaenodes caribbaeus
         # Nicholsina usta usta -> Nicholsina usta
         # Malacoctenus triangulatus
         # Diplodus argenteus argenteus
         # Chilomycterus spinosus spinosus


         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## RMorais_spatialData
         
         DF_eMOF <- read_csv("DwC_output/RMorais_spatialData/DF_eMOF.csv")
         DF_occ <- read_csv("DwC_output/RMorais_spatialData/DF_occ.csv")
         event_core <- read_csv("DwC_output/RMorais_spatialData/event_core.csv")
         

         lapply(event_core, unique) # "stpauls_rocks"
         # $site
         # [1] "abrolhos"        "alcatrazes"      "arraial"         "btds_santos"    
         # [5] "ceara"           "costa_corais"    "espirito_santo"  "ilha_grande"    
         # [9] "ilhabela"        "ilhasc_norte"    "ilhasc_sul"      "laje_santos"    
         # [13] "manuel_luis"     "rgnor_natal"     "rgnor_parrachos" "rgnor_sul"      
         # [17] "noronha"         "rocas"           "stpauls_rocks"   "trindade"

 
         lapply(DF_occ, unique) # recordedBy nome_sobrenome  ------> ALL: nomes ajustados
         # $recordedBy
         # [1] "ramon_noguchi"      "gugaw_ferreira"     "cel_ferreira"       "bertran_feitoza"   
         # [5] "eduardo_godoy"      "ca_rangel"          "thiago_mendes"      "jp_quimbayo"       
         # [9] "renata_mazzei"      "renato_morais"      "sergio_floeter"     "ana_liedke"        
         # [13] "jl_gasparini"       "jp_krajewski"       "hudson_pinheiro"    "gabriel_ferreira"  
         # [17] "anaide_aued"        "claudio_sampaio"    "cesar_cordeiro"     "thiony_simon"      
         # [21] "diego_barneche"     "anderson_batista"   "tiago_albuquerque"  "anchieta_nunes"    
         # [25] "daniel_dinslaken"   NA                   "gabriel_correal"    "osmar_luiz"        
         # [29] "marcelo_silveira"   "andrea_dalben"      "alexandre_siqueira" "max_levy"          
         # [33] "guilherme_longo"    "luisa_fontoura"     "athila_bertoncini" 
         lapply(DF_eMOF, unique)
         
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         # measurementUncertainty remover
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         # NA datas
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         filter(DF_occ, is.na(scientificNameAccepted)) %>% data.frame() %>% distinct(verbatimIdentification)
         # menaphorus.punticulatus, sparisoma.spp, entomacordus.sp, ogcocephalus.vespertilio
         # Sphyraena borealis?
         # Sparisoma viride
         # Serranus atricauda
         # Scorpaenodes caribbaeus
         # Scorpaena brachyptera
         # Scomberomorus maculatus
         # Sardinella brasiliensis?
         # Prognathodes marcellae
         # Platybelone argalus argalus -> Platybelone argalus
         # Nicholsina collettei
         # Muraena melanotis
         # Microgobius carri
         # Halichoeres rubrovirens
         
         rm(list = ls())
         
         
         ##############################################################################
         ##############################################################################
         
         ## SC_time_series
         
         DF_eMOF <- read_csv("DwC_output/SC_time_series/DF_eMOF.csv")
         DF_occ <- read_csv("DwC_output/SC_time_series/DF_occ.csv")
         event_core <- read_csv("DwC_output/SC_time_series/event_core.csv")
         
         lapply(event_core, unique) # site "caixa_da\xe7o_beach" -------- > ALL : ajustado
         lapply(DF_occ, unique) # recordedBy nome_sobrenome   -------- > ALL : ajustado
         # $recordedBy
         # [1] "diego_barneche"     "marcelo_silveira"   "sergio_floeter"     "andrea_dalben"     
         # [5] "daniel_dinslaken"   "anderson_batista"   "juan_quimbayo"      "renato_morais"     
         # [9] "max_levy"           "guilherme_longo"    "athila_bertoncini"  "luisa_fontoura"    
         # [13] "alexandre_siqueira" "ana_liedke"         "otavio_schlickmann" "lucas_nunes"       
         # [17] "thiago_fiuza"       "debora_ferrari"     "angela_canterle"  
         lapply(DF_eMOF, unique)
         
         
         # remover primeira coluna de todos
         summary(DF_eMOF)
         DF_eMOF %>% data.frame() %>% head
         filter(DF_eMOF, is.na(measurementValue)) %>% data.frame()
         # measurementUncertainty remover
         
         summary(DF_occ)
         DF_occ %>% data.frame() %>% head
         # NA coords
         
         summary(event_core)
         event_core %>% data.frame() %>% head
         
         
         DF_eMOF %>% 
           select(-`...1`) %>% 
           distinct()
         
         DF_occ %>% 
           select(-`...1`) %>% 
           distinct() 
         
         event_core %>% 
           select(-`...1`) %>% 
           distinct()
         
         # uniqueID
         setdiff(unique(DF_eMOF$eventID), unique(DF_occ$eventID))
         setdiff(unique(DF_occ$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_eMOF$eventID), unique(event_core$eventID))
         setdiff(unique(event_core$eventID), unique(DF_eMOF$eventID))
         setdiff(unique(DF_occ$eventID), unique(event_core$eventID))
         
         # occurrenceID (nao precisa ter no event_core, soh no occ)
         setdiff(unique(DF_eMOF$occurrenceID), unique(DF_occ$occurrenceID))
         setdiff(unique(DF_occ$occurrenceID), unique(DF_eMOF$occurrenceID))
         
         # especies
         DF_occ %>% 
           select(scientificNameAccepted) %>% 
           distinct() %>% 
           arrange(scientificNameAccepted) %>% 
           data.frame() 
         
         filter(DF_occ, is.na(scientificNameAccepted)) %>% 
           data.frame() %>% 
           distinct(verbatimIdentification)
         # parablennius sp # ------> ALL: aparentemente é isso mesmo, tb consta em Quimbayo et al. 2023 TimeFISH (Ecology) : Sérgio tb confirmou
         
         rm(list = ls())
         