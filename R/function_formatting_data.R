# - - - - - - - - - - - - - - - - - - - - - - -  - - 

# Function to format data into an array structure


# =========================================================
# PELD
# function 1: formatting fish data (incidence data)


#data <- benthos_TS_data
#years <- seq (2000,2020)

formatting_fish_data_PELD <- function (data,years) {

      # create the pivot tables, per year
      # years of time series
      #years <- seq(min(unique(data$eventDate),na.rm=T),
      #               max(unique(data$eventDate),na.rm=T)
      #)
      
      # vector of sites
      sites <- unique(data$locality)
      
      # species
      species <- unique (data$scientificName) 
      
      # unique occasions (eventIDs) per year and site
      occasions <- lapply (years, function (year)
        lapply (sites, function (site)
          
          
          # eventDate is the  year (date is not available) 
          length(unique(data [which(data$eventDate == year & 
                                    data$locality == site),"eventID"])
          )))
      
      # find the maximum of eventIDs for data imputing
      max_occasions<- max(unlist(occasions)) # number of transects per site
      
      # pivot table
      tabs_per_year <- lapply (species, function (species)
        
        lapply (years, function (year)
          
          lapply (sites, function (site)
            
            cast (formula = locality ~ eventID,
                  value = "measurementValue",
                  fun.aggregate = sum,
                  data = data[which(data$scientificName == species & 
                                              data$eventDate == year & 
                                              data$locality == site),]) 
          )
        )
      )
      
      # fill occasions without sampling (NA)
      imputed_occasions <- lapply (tabs_per_year, function (species) # each species
        
        lapply (species, function (year)  # each year
          
          lapply (year, function (site) { # each site
            
            # which occasions to input (diff between observed and max occasions)    
            imput_occasions <- matrix (NA, 
                                       nrow = nrow (site),
                                       ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
            imputed_occasions<- (cbind (site[,-1],
                                        imput_occasions)) # max occasion + 1 (the locality id)
            colnames(imputed_occasions) <- seq(1,max_occasions)
            ; # return
            imputed_occasions
            
          }
          )
        )
      )
      
      # melt the first two levels of the list
      matrix_year_spp <- lapply (imputed_occasions, function (species) # for each species
        lapply (species, function (year) # for each year
          
          # melt
          do.call(rbind,year)
        )
      )
      
      # list into array [site,transect,year, species]
      array_sites_occs_years <-   array(unlist(matrix_year_spp), 
                                        dim = c(length(sites), 
                                                max_occasions,
                                                length(years),
                                                length(species)),
                                        dimnames = list (sites,
                                                         seq(1,max_occasions),
                                                         years,
                                                         species
                                        ))
      
      # check if the array is in the correct order
      # array_sites_occs_years[5,,14,1] == imputed_occasions[[1]][[14]][[5]]
      
      # transforming counts into detection/non-detection data
      array_sites_occs_years [which(array_sites_occs_years >0)]<- 1
      
      # finding zeros (transects with record of any species)
      # pivot table
      tabs_per_year_zeros <- lapply (years, function (year)
        
        lapply (sites, function (site)
          
          
          # eventDate is the  year (date is not available) 
          cast (formula = locality ~ eventID,
                value = "measurementValue",
                fun.aggregate = sum,
                data = data[which(data$eventDate == year & 
                                  data$locality == site),]) 
        )
      )
      
      # fill occasions without sampling (NA)
      imputed_occasions_zeros <- lapply (tabs_per_year_zeros, function (year)  # each year
        
        lapply (year, function (site) { # each site
          
          # which occasions to input (diff between observed and max occasions)    
          imput_occasions <- matrix (NA, 
                                     nrow = nrow (site),
                                     ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
          imputed_occasions<- (cbind (site[,-1],
                                      imput_occasions)) # max occasion + 1 (the locality id)
          colnames(imputed_occasions) <- seq(1,max_occasions)
          ; # return
          imputed_occasions
          
        }
        )
      )
      
      # melt the first two levels of the list
      matrix_year_spp_zeros <- lapply (imputed_occasions_zeros, function (year) # for each year
        
        # melt
        do.call(rbind,year)
      )
      
      # list into array [site,transect,year, species]
      array_sites_occs_years_zeros <-   array(unlist(matrix_year_spp_zeros), 
                                              dim = c(length(sites), 
                                                      max_occasions,
                                                      length(years)),
                                              dimnames = list (sites,
                                                               seq(1,max_occasions),
                                                               years
                                              ))
      
      # the occasion has or not a transect?
      array_sites_occs_years_zeros [which(array_sites_occs_years_zeros >=0)]<- 0 
      
      # knowing the occasions with transect but no detection of a focal species
      ## let's do with for to keep the array's structure
      
      for (s in 1:dim(array_sites_occs_years)[4]){ # for each species
        for (t in 1:dim(array_sites_occs_years)[3]) { # for each year
          # check the presence of transects
          
          
          array_sites_occs_years[,,t,s] <- ifelse (
            is.na(array_sites_occs_years [,,t,s]) & array_sites_occs_years_zeros [,,t] ==0, # if no detection (still NAs) but prsence of transect
            0, # insert zeros
            array_sites_occs_years [,,t,s]) # otherwise keep NAs or 1s
        }
      }
      
      # now the presence of values in this table must resemble the values in the matrix with zeros
      # check this
      # table(is.na(array_sites_occs_years[,,2,1]) == is.na(array_sites_occs_years_zeros[,,2]) )
      # table(is.na(array_sites_occs_years[,,14,3]) == is.na(array_sites_occs_years_zeros[,,14]) )
      # table(is.na(array_sites_occs_years[,,7,3]) == is.na(array_sites_occs_years_zeros[,,7]) )
      
      
      # output to return
      
      return (array_sites_occs_years)

}

# ====================================================================
# function 2: formatting benthic data (cover)


formatting_benthic_data_PELD <- function (data,years){
      
      # create the pivot tables, per year
      # sites
      sites <- unique(data$locality)
      
      # species
      species <- unique (data$scientificName)
      
      #  observation at the scale of transect (as fish)
      # frst replace the word ""Quadrat by "Q"
      data$eventID<- gsub ("Quadrat","Q",data$eventID)
      # jpg and JPG by nothing
      data$eventID<- gsub (".jpg","",data$eventID)
      data$eventID<- gsub (".JPG","",data$eventID)
      # tarnsect id
      transect_benthos <- substr(data$eventID,
                                 nchar(data$eventID)-4,
                                 nchar(data$eventID))
      transect_benthos <- gsub (".*_","",transect_benthos)
      transect_benthos <- toupper(transect_benthos) # toupper
      transect_benthos <- gsub (" ","",transect_benthos) # remove spaces
      transect_benthos <- gsub ("\\(","",transect_benthos) # remove spaces
      transect_benthos <- gsub ("\\)","",transect_benthos) # remove spaces
      transect_benthos[-grep("Q",(transect_benthos))] <- paste("Q",(transect_benthos) [-grep("Q",(transect_benthos))],sep="") # weird syntax but it works
      
      # cbind
      data$transectID <- transect_benthos
      
      # unique occasions (eventIDs) per year and site
      occasions <- lapply (years, function (year)
        lapply (sites, function (site)
          
          # eventDate is the  year (date is not available) 
          length(unique(data [which(data$eventDate == year & 
                                    data$locality == site),"transectID"])
          )))
      
      # find the maximum of eventIDs for data imputing
      max_occasions <- max(unlist(occasions)) # number of photoquadrats per site
      
      # pivot table
      tabs_per_year <- lapply (species, function (species)
        
        lapply (years, function (year)
          
          lapply (sites, function (site)
            
            # eventDate is the  year (date is not available) 
            cast (formula = locality ~ transectID,
                  value = "measurementValue",
                  fun.aggregate = mean,
                  data = data[which(data$scientificName == species & 
                                    data$eventDate == year & 
                                    data$locality == site),]) 
          )
        )
      )
      
      # fill occasions without sampling (NA)
      imputed_occasions <- lapply (tabs_per_year, function (species) # each species
        
        lapply (species, function (year)  # each year
          
          lapply (year, function (site) { # each site
            
            # which occasions to input (diff between observed and max occasions)    
            imput_occasions <- matrix (NA, 
                                       nrow = nrow (site),
                                       ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
            imputed_occasions<- (cbind (site[,-1],
                                        imput_occasions)) # max occasion + 1 (the locality id)
            colnames(imputed_occasions) <- seq(1,max_occasions)
            ; # return
            imputed_occasions
            
          }
          )
        )
      )
      
      # melt the first two levels of the list
      matrix_year_spp <- lapply (imputed_occasions, function (species) # for each species
        lapply (species, function (year) # for each year
          
          # melt
          do.call(rbind,year)
        )
      )
      
      # list into array [site,transect,year, species]
      array_sites_occs_years <-   array(unlist(matrix_year_spp), 
                                        dim = c(length(sites), 
                                                max_occasions,
                                                length(years),
                                                length(species)),
                                        dimnames = list (sites,
                                                         seq(1,max_occasions),
                                                         years,
                                                         species
                                        ))
      
      # check if the array is in the correct order
      # array_sites_occs_years[1,,6,1] == imputed_occasions[[1]][[6]][[1]]
      
      # transforming counts into detection/non-detection data
      array_sites_occs_years [which(array_sites_occs_years >0)]<- 1
      
      # finding zeros (transects with record of any species)
      # pivot table
      tabs_per_year_zeros <- lapply (years, function (year)
        
        lapply (sites, function (site)
          
          # eventDate is the  year (date is not available)
          cast (formula = locality ~ transectID,
                value = "measurementValue",
                fun.aggregate = sum,
                data = data[which(data$eventDate == year & 
                                  data$locality == site),]) 
        )
      )
      
      # fill occasions without sampling (NA)
      imputed_occasions_zeros <- lapply (tabs_per_year_zeros, function (year)  # each year
        
        lapply (year, function (site) { # each site
          
          # which occasions to input (diff between observed and max occasions)    
          imput_occasions <- matrix (NA, 
                                     nrow = nrow (site),
                                     ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
          imputed_occasions<- (cbind (site[,-1],
                                      imput_occasions)) # max occasion + 1 (the locality id)
          colnames(imputed_occasions) <- seq(1,max_occasions)
          ; # return
          imputed_occasions
          
        }
        )
      )
      
      # melt the first two levels of the list
      matrix_year_spp_zeros <- lapply (imputed_occasions_zeros, function (year) # for each year
        
        # melt
        do.call(rbind,year)
      )
      
      
      # list into array [site,transect,year, species]
      array_sites_occs_years_zeros <-   array(unlist(matrix_year_spp_zeros), 
                                              dim = c(length(sites), 
                                                      max_occasions,
                                                      length(years)),
                                              dimnames = list (sites,
                                                               seq(1,max_occasions),
                                                               years
                                              ))
      
      # the occasion has or not a transect?
      array_sites_occs_years_zeros [which(array_sites_occs_years_zeros >=0)]<- 0 
      
      # knowing the occasions with transect but no detection of a focal species
      ## let's do with for to keep the array structure
      
      for (s in 1:dim(array_sites_occs_years)[4]){ # for each species
        for (t in 1:dim(array_sites_occs_years)[3]) { # for each year
          # check the presence of transects
          
          
          array_sites_occs_years[,,t,s] <- ifelse (
            is.na(array_sites_occs_years [,,t,s]) & array_sites_occs_years_zeros [,,t] ==0, # if no detection (still NAs) but prsence of transect
            0, # insert zeros
            array_sites_occs_years [,,t,s]) # otherwise keep NAs or 1s
        }
      }
      
      # now the presence of values in this table must resemble the values in the matrix with zeros
      # check this
      # table(is.na(array_sites_occs_years[,,2,1]) == is.na(array_sites_occs_years_zeros[,,2]) )
      # table(is.na(array_sites_occs_years[,,7,3]) == is.na(array_sites_occs_years_zeros[,,7]) )
      # table(is.na(array_sites_occs_years[,,1,3]) == is.na(array_sites_occs_years_zeros[,,1]) )
      
      return (array_sites_occs_years)
      
      # conferir se trindade e aspsp tem ate 2017

}

# =====================================================================
# formatting data
# Abrolhos time series Ronaldo Francini (RF)

#data <- benthos_TS_data_RF
#years <- seq (2000,2020)

formatting_benthic_data_RF <- function (data,years){
  
  # create the pivot tables, per year
  # years of time series
  #years <- seq(min(unique(data$eventDate),na.rm=T),
  #             max(unique(data$eventDate),na.rm=T)
  #)
  
  # sites
  sites <- unique(data$locality)
  
  # species
  species <- unique (data$scientificName)
  
  #  observation at the scale of transect (as fish)
  # frst replace the word ""Quadrat by "Q"
  # up to 2005 (diff method)
  
  first_data<- data [which(data$eventDate %in% c(2003,2004,2005)),]
  sample_id <- strsplit (substr(first_data$eventID,nchar (first_data$eventID)-13, nchar(first_data$eventID)),"_") # substr
  sample_id<-(sapply (sample_id,"[",2)) # extract sample id
  first_data <- cbind (first_data, # bind
                       sampleid=sample_id)
  
  # after 2005
  scd_data <- data [which(data$eventDate %in% seq (2006,2014)),]
  sample_id_scd <- strsplit (substr(scd_data$eventID,nchar (scd_data$eventID)-7, nchar(scd_data$eventID)),"_") # substr
  sample_id_scd<-(sapply (sample_id_scd,"[",3)) # extract sample id
  scd_data <- cbind (scd_data, # bind
                       sampleid=sample_id_scd)
  
  # bind the periods
  data <- rbind (first_data,
                 scd_data)
  
  # unique occasions (eventIDs) per year and site
  occasions <- lapply (years, function (year)
    lapply (sites, function (site)
      
      # eventDate is the  year (date is not available)
      length(unique(data [which(data$eventDate == year & 
                                data$locality == site),"sampleid"])
      )))
  
  # find the maximum of eventIDs for data imputing
  max_occasions <- max(unlist(occasions)) # number of photoquadrats per site
  
  # pivot table
  tabs_per_year <- lapply (species, function (species)
    
    lapply (years, function (year)
      
      lapply (sites, function (site)
        
        # eventDate is the  year (date is not available)
        cast (formula = locality ~ sampleid,
              value = "measurementValue",
              fun.aggregate = mean,
              data = data[which(data$scientificName == species & 
                                  data$eventDate == year & 
                                  data$locality == site),]) 
      )
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions <- lapply (tabs_per_year, function (species) # each species
    
    lapply (species, function (year)  # each year
      
      lapply (year, function (site) { # each site
        
        # which occasions to input (diff between observed and max occasions)    
        imput_occasions <- matrix (NA, 
                                   nrow = nrow (site),
                                   ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
        imputed_occasions<- (cbind (site[,-1],
                                    imput_occasions)) # max occasion + 1 (the locality id)
        colnames(imputed_occasions) <- seq(1,max_occasions)
        ; # return
        imputed_occasions
        
      }
      )
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp <- lapply (imputed_occasions, function (species) # for each species
    lapply (species, function (year) # for each year
      
      # melt
      do.call(rbind,year)
    )
  )
  
  # list into array [site,transect,year, species]
  array_sites_occs_years <-   array(unlist(matrix_year_spp), 
                                    dim = c(length(sites), 
                                            max_occasions,
                                            length(years),
                                            length(species)),
                                    dimnames = list (sites,
                                                     seq(1,max_occasions),
                                                     years,
                                                     species
                                    ))
  
  # check if the array is in the correct order
  # array_sites_occs_years[1,,6,1] == imputed_occasions[[1]][[6]][[1]]
  
  # transforming counts into detection/non-detection data
  array_sites_occs_years [which(array_sites_occs_years >0)]<- 1
  
  # finding zeros (transects with record of any species)
  # pivot table
  tabs_per_year_zeros <- lapply (years, function (year)
    
    lapply (sites, function (site)
      
      cast (formula = locality ~ sampleid,
            value = "measurementValue",
            fun.aggregate = sum,
            data = data[which(data$eventDate == year & 
                              data$locality == site),]) 
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions_zeros <- lapply (tabs_per_year_zeros, function (year)  # each year
    
    lapply (year, function (site) { # each site
      
      # which occasions to input (diff between observed and max occasions)    
      imput_occasions <- matrix (NA, 
                                 nrow = nrow (site),
                                 ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
      imputed_occasions<- (cbind (site[,-1],
                                  imput_occasions)) # max occasion + 1 (the locality id)
      colnames(imputed_occasions) <- seq(1,max_occasions)
      ; # return
      imputed_occasions
      
    }
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp_zeros <- lapply (imputed_occasions_zeros, function (year) # for each year
    
    # melt
    do.call(rbind,year)
  )
  
  
  # list into array [site,transect,year, species]
  array_sites_occs_years_zeros <-   array(unlist(matrix_year_spp_zeros), 
                                          dim = c(length(sites), 
                                                  max_occasions,
                                                  length(years)),
                                          dimnames = list (sites,
                                                           seq(1,max_occasions),
                                                           years
                                          ))
  
  # the occasion has or not a transect?
  array_sites_occs_years_zeros [which(array_sites_occs_years_zeros >=0)]<- 0 
  
  # knowing the occasions with transect but no detection of a focal species
  ## let's do with for to keep the array structure
  
  for (s in 1:dim(array_sites_occs_years)[4]){ # for each species
    for (t in 1:dim(array_sites_occs_years)[3]) { # for each year
      # check the presence of transects
      
      
      array_sites_occs_years[,,t,s] <- ifelse (
        is.na(array_sites_occs_years [,,t,s]) & array_sites_occs_years_zeros [,,t] ==0, # if no detection (still NAs) but prsence of transect
        0, # insert zeros
        array_sites_occs_years [,,t,s]) # otherwise keep NAs or 1s
    }
  }
  
  # now the presence of values in this table must resemble the values in the matrix with zeros
  # check this
  # table(is.na(array_sites_occs_years[,,2,1]) == is.na(array_sites_occs_years_zeros[,,2]) )
  # table(is.na(array_sites_occs_years[,,7,3]) == is.na(array_sites_occs_years_zeros[,,7]) )
  # table(is.na(array_sites_occs_years[,,1,3]) == is.na(array_sites_occs_years_zeros[,,1]) )
  
  return (array_sites_occs_years)
  
  
}

# formating fish data
formatting_fish_data_RF <- function (data,years){
  
  # create the pivot tables, per year
  # years of time series
  #years <- seq(min(unique(data$eventDate),na.rm=T),
  #             max(unique(data$eventDate),na.rm=T)
  #)
  
  # sites
  sites <- unique(data$locality)
  
  # species
  species <- unique (data$scientificName)
  
  #  observation at the scale of transect (as fish)
  sample_id_scd <- strsplit (substr(data$eventID,nchar (data$eventID)-18, nchar(data$eventID)),"_") # substr
  sample_id_scd<-(sapply (sample_id_scd,"[",4)) # extract sample id
  data <- cbind (data, # bind
                sampleid=sample_id_scd)
  
  # unique occasions (eventIDs) per year and site
  occasions <- lapply (years, function (year)
    lapply (sites, function (site)
      
      # eventDate is the  year (date is not available)
      length(unique(data [which(data$eventDate == year & 
                                data$locality == site),"sampleid"])
      )))
  
  # find the maximum of eventIDs for data imputing
  max_occasions <- max(unlist(occasions)) # number of photoquadrats per site
  
  # pivot table
  tabs_per_year <- lapply (species, function (species)
    
    lapply (years, function (year)
      
      lapply (sites, function (site)
        
        # eventDate is the  year (date is not available)
        cast (formula = locality ~ sampleid,
              value = "measurementValue",
              fun.aggregate = mean,
              data = data[which(data$scientificName == species & 
                                  data$eventDate == year & 
                                  data$locality == site),]) 
      )
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions <- lapply (tabs_per_year, function (species) # each species
    
    lapply (species, function (year)  # each year
      
      lapply (year, function (site) { # each site
        
        # which occasions to input (diff between observed and max occasions)    
        imput_occasions <- matrix (NA, 
                                   nrow = nrow (site),
                                   ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
        imputed_occasions<- (cbind (site[,-1],
                                    imput_occasions)) # max occasion + 1 (the locality id)
        colnames(imputed_occasions) <- seq(1,max_occasions)
        ; # return
        imputed_occasions
        
      }
      )
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp <- lapply (imputed_occasions, function (species) # for each species
    lapply (species, function (year) # for each year
      
      # melt
      do.call(rbind,year)
    )
  )
  
  # list into array [site,transect,year, species]
  array_sites_occs_years <-   array(unlist(matrix_year_spp), 
                                    dim = c(length(sites), 
                                            max_occasions,
                                            length(years),
                                            length(species)),
                                    dimnames = list (sites,
                                                     seq(1,max_occasions),
                                                     years,
                                                     species
                                    ))
  
  # check if the array is in the correct order
  # array_sites_occs_years[1,,6,1] == imputed_occasions[[1]][[6]][[1]]
  
  # transforming counts into detection/non-detection data
  array_sites_occs_years [which(array_sites_occs_years >0)]<- 1
  
  # finding zeros (transects with record of any species)
  # pivot table
  tabs_per_year_zeros <- lapply (years, function (year)
    
    lapply (sites, function (site)
      # eventDate is the  year (date is not available)
      cast (formula = locality ~ sampleid,
            value = "measurementValue",
            fun.aggregate = sum,
            data = data[which(data$eventDate == year & 
                                data$locality == site),]) 
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions_zeros <- lapply (tabs_per_year_zeros, function (year)  # each year
    
    lapply (year, function (site) { # each site
      
      # which occasions to input (diff between observed and max occasions)    
      imput_occasions <- matrix (NA, 
                                 nrow = nrow (site),
                                 ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
      imputed_occasions<- (cbind (site[,-1],
                                  imput_occasions)) # max occasion + 1 (the locality id)
      colnames(imputed_occasions) <- seq(1,max_occasions)
      ; # return
      imputed_occasions
      
    }
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp_zeros <- lapply (imputed_occasions_zeros, function (year) # for each year
    
    # melt
    do.call(rbind,year)
  )
  
  
  # list into array [site,transect,year, species]
  array_sites_occs_years_zeros <-   array(unlist(matrix_year_spp_zeros), 
                                          dim = c(length(sites), 
                                                  max_occasions,
                                                  length(years)),
                                          dimnames = list (sites,
                                                           seq(1,max_occasions),
                                                           years
                                          ))
  
  # the occasion has or not a transect?
  array_sites_occs_years_zeros [which(array_sites_occs_years_zeros >=0)]<- 0 
  
  # knowing the occasions with transect but no detection of a focal species
  ## let's do with for to keep the array structure
  
  for (s in 1:dim(array_sites_occs_years)[4]){ # for each species
    for (t in 1:dim(array_sites_occs_years)[3]) { # for each year
      # check the presence of transects
      
      
      array_sites_occs_years[,,t,s] <- ifelse (
        is.na(array_sites_occs_years [,,t,s]) & array_sites_occs_years_zeros [,,t] ==0, # if no detection (still NAs) but prsence of transect
        0, # insert zeros
        array_sites_occs_years [,,t,s]) # otherwise keep NAs or 1s
    }
  }
  
  # now the presence of values in this table must resemble the values in the matrix with zeros
  # check this
  # table(is.na(array_sites_occs_years[,,2,1]) == is.na(array_sites_occs_years_zeros[,,2]) )
  # table(is.na(array_sites_occs_years[,,7,3]) == is.na(array_sites_occs_years_zeros[,,7]) )
  # table(is.na(array_sites_occs_years[,,1,3]) == is.na(array_sites_occs_years_zeros[,,1]) )
  
  return (array_sites_occs_years)
  
  
}


# ===========================================
# Santa catarina

formatting_fish_data_SC <- function (data,years) {
  
  # create the pivot tables, per year
  # years of time series
  #years <- seq(min(unique(data$eventDate),na.rm=T),
  #               max(unique(data$eventDate),na.rm=T)
  #)
  
  # vector of sites
  sites <- unique(data$locality)
  
  # species
  species <- unique (data$scientificName)
  
  # unique occasions (eventIDs) per year and site
  occasions <- lapply (years, function (year)
    lapply (sites, function (site)
      
      length(unique(data [which(data$eventYear == year & 
                                data$locality == site),"eventID"])
      )))
  
  # find the maximum of eventIDs for data imputing
  max_occasions<- max(unlist(occasions)) # number of transects per site
  
  # pivot table
  tabs_per_year <- lapply (species, function (species)
    
    lapply (years, function (year)
      
      lapply (sites, function (site)
        
        cast (formula = locality ~ eventID,
              value = "measurementValue",
              na.rm=T,
              fun.aggregate = sum,
              data = data[which(data$scientificName == species & 
                                  data$eventYear == year & 
                                  data$locality == site),]) 
      )
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions <- lapply (tabs_per_year, function (species) # each species
    
    lapply (species, function (year)  # each year
      
      lapply (year, function (site) { # each site
        
        # which occasions to input (diff between observed and max occasions)    
        imput_occasions <- matrix (NA, 
                                   nrow = nrow (site),
                                   ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
        imputed_occasions<- (cbind (site[,-1],
                                    imput_occasions)) # max occasion + 1 (the locality id)
        colnames(imputed_occasions) <- seq(1,max_occasions)
        ; # return
        imputed_occasions
        
      }
      )
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp <- lapply (imputed_occasions, function (species) # for each species
    lapply (species, function (year) # for each year
      
      # melt
      do.call(rbind,year)
    )
  )
  
  # list into array [site,transect,year, species]
  array_sites_occs_years <-   array(unlist(matrix_year_spp), 
                                    dim = c(length(sites), 
                                            max_occasions,
                                            length(years),
                                            length(species)),
                                    dimnames = list (sites,
                                                     seq(1,max_occasions),
                                                     years,
                                                     species
                                    ))
  
  # check if the array is in the correct order
  # array_sites_occs_years[5,,14,1] == imputed_occasions[[1]][[14]][[5]]
  
  # transforming counts into detection/non-detection data
  array_sites_occs_years [which(array_sites_occs_years >0)]<- 1
  
  # finding zeros (transects with record of any species)
  # pivot table
  tabs_per_year_zeros <- lapply (years, function (year)
    
    lapply (sites, function (site)
      
      cast (formula = locality ~ eventID,
            value = "measurementValue",
            fun.aggregate = sum,
            data = data[which(data$eventYear == year & 
                                data$locality == site),]) 
    )
  )
  
  # fill occasions without sampling (NA)
  imputed_occasions_zeros <- lapply (tabs_per_year_zeros, function (year)  # each year
    
    lapply (year, function (site) { # each site
      
      # which occasions to input (diff between observed and max occasions)    
      imput_occasions <- matrix (NA, 
                                 nrow = nrow (site),
                                 ncol = max_occasions - (ncol (site)-1))# ncol minus the first (locality id) 
      imputed_occasions<- (cbind (site[,-1],
                                  imput_occasions)) # max occasion + 1 (the locality id)
      colnames(imputed_occasions) <- seq(1,max_occasions)
      ; # return
      imputed_occasions
      
    }
    )
  )
  
  # melt the first two levels of the list
  matrix_year_spp_zeros <- lapply (imputed_occasions_zeros, function (year) # for each year
    
    # melt
    do.call(rbind,year)
  )
  
  # list into array [site,transect,year, species]
  array_sites_occs_years_zeros <-   array(unlist(matrix_year_spp_zeros), 
                                          dim = c(length(sites), 
                                                  max_occasions,
                                                  length(years)),
                                          dimnames = list (sites,
                                                           seq(1,max_occasions),
                                                           years
                                          ))
  
  # the occasion has or not a transect?
  array_sites_occs_years_zeros [which(array_sites_occs_years_zeros >=0)]<- 0 
  
  # knowing the occasions with transect but no detection of a focal species
  ## let's do with for to keep the array's structure
  
  for (s in 1:dim(array_sites_occs_years)[4]){ # for each species
    for (t in 1:dim(array_sites_occs_years)[3]) { # for each year
      # check the presence of transects
      
      
      array_sites_occs_years[,,t,s] <- ifelse (
        is.na(array_sites_occs_years [,,t,s]) & array_sites_occs_years_zeros [,,t] ==0, # if no detection (still NAs) but prsence of transect
        0, # insert zeros
        array_sites_occs_years [,,t,s]) # otherwise keep NAs or 1s
    }
  }
  
  # now the presence of values in this table must resemble the values in the matrix with zeros
  # check this
  # table(is.na(array_sites_occs_years[,,2,1]) == is.na(array_sites_occs_years_zeros[,,2]) )
  # table(is.na(array_sites_occs_years[,,14,3]) == is.na(array_sites_occs_years_zeros[,,14]) )
  # table(is.na(array_sites_occs_years[,,7,3]) == is.na(array_sites_occs_years_zeros[,,7]) )
  
  
  # output to return
  
  return (array_sites_occs_years)
  
}
