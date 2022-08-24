# function
# array into long format


array_into_long_format <- function (array_data, group,db) {  
  
  
    df_long<-lapply (seq(1,length(dimnames(array_data)[[1]])), function (i) # site
  
        lapply (seq(1,length(dimnames(array_data)[[3]])), function (t) # year
    
            do.call (rbind,lapply (seq(1,length(dimnames(array_data)[[4]])), function (k) {# species
      
      
      tryCatch (
        
        data.frame ("occasions" = dimnames (array_data)[2][[1]][which(is.na(array_data[i,,t,k]) == F)],
                    "detection" = array_data[i,,t,k][ which(is.na(array_data[i,,t,k]) == F)],
                    "site" = dimnames(array_data)[[1]][i],
                    "year" = dimnames(array_data)[[3]][t],
                    "species" = dimnames(array_data)[[4]][k],
                    "group" = group,
                    "database" = db
                    
        ),
        error = function(e) return ("NULL"))
      
    } 
    
    )
    )
    )
    )
    
  # filtering our missing years and occasions
    
    df_long <- lapply (df_long, function (k)
      
      k[unlist(lapply (k,ncol))!= 1]
      
      )
    
    # melt within site
    # bind years of each site
    df_long<- lapply (df_long, function (i) 
      
      do.call (rbind, i))
    
    # finally melt sites
    df_long<-do.call(rbind,df_long)
    
  # return
    return(df_long)
  }


