

# function to load and edit the table of data
function_load_data <- function (dir, file) { 
  
  # load data
  # eMOF
  DF_eMOF <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                  "ReefSYN_data", 
                                  "DwC_output", 
                                  dir,
                                  #interesting_dirs[4], 
                                  file[1]),
                       #interesting_files[[4]][1]),
                       h=T,row.names = NULL)
  
  # event Core
  DF_event_core <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                        "ReefSYN_data", 
                                        "DwC_output", 
                                        #interesting_dirs[4], 
                                        dir,
                                        file[3]),
                             #interesting_files[[4]][3]),
                             h=T,row.names=NULL)
  
  # island == site to PELD dataset
  if (sum(colnames (DF_event_core) == "island")>0) {
    
    
    colnames (DF_event_core) [which(colnames (DF_event_core) == "island")] <- "site"
    
  } else (colnames (DF_event_core) )
  
  # occurrence -- find the species
  DF_occ <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                 "ReefSYN_data", 
                                 "DwC_output", 
                                 dir,
                                 file[2]),
                      # interesting_dirs[4], 
                      # interesting_files[[4]][2]),
                      h=T,row.names = NULL)
  
  # match to find location and site
  # matchings DwcDatasets
  DF_eMOF <- cbind ( DF_eMOF,
                     
                     # bind site information
                     DF_event_core [match (DF_eMOF$eventID, 
                                           DF_event_core$eventID), 
                                    c ("eventID",
                                       'higherGeography',
                                       "site",
                                       "locality",
                                       "year",
                                       "sampleSizeValue",
                                       "sampleSizeUnit",
                                       "decimalLatitude",
                                       "decimalLongitude" )]
  )
  
  # matchings DwcDatasets
  DF_eMOF <- cbind ( DF_eMOF,
                     
                     # bind site information
                     scientificNameAccepted=DF_occ [match (DF_eMOF$occurrenceID, 
                                                           DF_occ$occurrenceID), 
                                                    c ("scientificNameAccepted" )]
  )
  
  return (DF_eMOF)  
  
}
