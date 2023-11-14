# - - - - - - - - - - - - - - - - - - - - - - -  - - 

# Function to format data into an array and then long format structure


formatting_fish_data <- function (directory,
                                  file_DF_eMOF,
                                  file_DF_occ,
                                  file_DF_event_core) {

      
        
        # load data
        # eMOF
        DF_eMOF <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                        "ReefSYN_data", 
                                        "DwC_output", 
                                        directory,
                                        file_DF_eMOF),
                             h=T,row.names = NULL)
        
        # use only number of individuals
        DF_eMOF <- DF_eMOF %>%
          filter (measurementType == "abundance")
        
        # event Core
        DF_event_core <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                              "ReefSYN_data", 
                                              "DwC_output", 
                                              directory,
                                              file_DF_event_core),
                                   h=T,row.names=NULL)
        
        # match to find location and site
        # island == site to PELD dataset
        if (sum(colnames (DF_event_core) == "island")>0) {
          
          
          colnames (DF_event_core) [which(colnames (DF_event_core) == "island")] <- "site"
          
        } else (colnames (DF_event_core) )
        
        
        # occurrence -- find the species
        DF_occ <- read.csv (here::here("\\.","Pos_Doc_Sinbiose", 
                                       "ReefSYN_data", 
                                       "DwC_output", 
                                       directory,
                                       file_DF_occ),
                            h=T,row.names = NULL)
        
        
        # matchings DwcDatasets
        data <- cbind ( DF_eMOF,
                           
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
        data <- cbind ( data,
                           
                           # bind spp identity
                        scientificNameAccepted = DF_occ [match (DF_eMOF$occurrenceID,
                                                                DF_occ$occurrenceID
                                                                 ), 
                                                          
                                                             "scientificNameAccepted"]
        )
        
        
        
      # vector of sites
      sites <- unique(data$locality)
      
      # only species level
      species <- unique (data$scientificNameAccepted) 
      species <- species[which(lapply (strsplit(species," "),length)>1)]
      data <- data [which(data$scientificNameAccepted %in% species),]
      
      # data into an array
      inds_per_year <- tapply (as.numeric(data$measurementValue),
              list(data$scientificNameAccepted, 
                     data$year,
                     data$locality),
             sum)
      
      # keep only species with > 1 detection
      inds_per_year <- inds_per_year [which(apply(inds_per_year,1,sum,na.rm=T)>0),,]
      
      
      # transects per year
      transects_per_year <- tapply (data$eventID,
                                    list(data$year,
                                         data$locality),
                                    FUN = function(x) length(unique(x)))
      
      # NA means no sampling event
      transects_per_year[is.na(transects_per_year)]<-0
      
      # check
      # head(inds_per_year [,,1])
      # transects_per_year
      # head(inds_per_year[,1:3,1])
      
      # knowing the occasions with transect but no detection of a focal species
      ## let's do with for to keep the array structure
      
      for (s in 1:dim(inds_per_year)[3]){ # for each site
        for (t in 1:dim(inds_per_year)[2]) { # for each year
        # check the presence of transects
          
        inds_per_year[,t,s] <- ifelse (
          
              is.na(inds_per_year [,t,s]) & transects_per_year [t,s] > 0, # if no detection (still NAs) but prsence of transect
            
              0, # insert zeros
            
              inds_per_year [,t,s] # otherwise keep NAs or 1s
              
              ) 
        }
      }
      
      # any abundance >= 1 into 1 (detection)
      inds_per_year [inds_per_year>=1] <- 1
      
      # melt
      long_format_DF<-melt(inds_per_year,as.is=T)
      colnames (long_format_DF) <- c("species", "year", "locality", "detection")
      
      # bind region name
      long_format_DF$higherGeography <- DF_event_core$higherGeography [match (
                                                                              long_format_DF$locality,
                                                                              DF_event_core$locality)]
      # bind site name
      long_format_DF$site <- DF_event_core$site [match (
                                                                      long_format_DF$locality,
                                                                      DF_event_core$locality)]
                                                                    
      # and dataset
      long_format_DF$dataset <- directory
      
      
      # bind effort
      long_transects_per_year <- melt(transects_per_year,as.is=T)
      colnames(long_transects_per_year) <- c("year", "locality", "Nevents")
      
      # bind
      long_format_DF <- cbind (long_format_DF,
                               
                               "Nevents"=long_transects_per_year [match (
                                 
                                 paste (long_format_DF$year,
                                        long_format_DF$locality,sep="_"),
                                 paste (long_transects_per_year$year,
                                        long_transects_per_year$locality,sep="_")
                                 
                               ), "Nevents" ])
      
      
      
      # check if that worked? i.e., number of sampling events always higher than detection
      table (long_format_DF$Nevents >= long_format_DF$detection)
      
      
      # bind lat/long
      long_format_DF<-cbind (long_format_DF,
                             
                             DF_event_core [match (long_format_DF$locality,
                                                    DF_event_core$locality),
                                             c("decimalLongitude","decimalLatitude")]
      )
      
      # return
      return (long_format_DF)

}
