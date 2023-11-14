# ---------------------------------#
#    Time series (TS) formatting   #
# ---------------------------------#

# packages
require(here); require(reshape);require(dplyr)
require(openxlsx);  require(rgdal);require(raster)


# load functions (formatting data to array)
source (here ("R", "function_formatting_data.R"))

# list of long formats (array into a long format)
source (here ("R", "function_array_to_long.R"))

# identify the place of dirs and files
interesting_dirs <- list.files (here("\\.","Pos_Doc_Sinbiose", 
                                     "ReefSYN_data", 
                                     "DwC_output")) 

# select folders
interesting_dirs <- interesting_dirs [c(2,3,7,10,13,14,16)]

# interesting files
interesting_files <- lapply (interesting_dirs, function (i) 
  
  list.files (here("\\.","Pos_Doc_Sinbiose", 
                   "ReefSYN_data", 
                   "DwC_output",
                   i))
  )
interesting_files <- lapply (interesting_files, function (i) {
  
  i [grep("benthos",i, invert=T)]
  
})



# format all datasets at once
list_datasets <- lapply (seq (1,length(interesting_dirs)), function (i) 
  
        formatting_fish_data (directory =  interesting_dirs[i],
                              file_DF_eMOF = interesting_files[[i]][1],
                              file_DF_event_core = interesting_files[[i]][3],
                              file_DF_occ = interesting_files[[i]][2])
)


# melt
TS_datasets <- do.call(rbind, list_datasets)
# order
TS_datasets <- TS_datasets %>%
  filter (is.na(detection) !=T )

TS_datasets %>% 
  filter (species == "Carcharhinus perezii") %>%
  dplyr::select (dataset)


# naive occurrence
test <- tapply(TS_datasets$detection,
       list(TS_datasets$locality,
            TS_datasets$species,
            TS_datasets$year),
       max,na.rm=T)

range(test,na.rm=T)

# effort table
eff <- tapply(TS_datasets$Nevents,
                      list(TS_datasets$locality,
                           TS_datasets$year),
                      mean,na.rm=T)

# 
plot(colSums(test [,233,],na.rm=T),type= 'b')
plot(colSums(eff,na.rm=T),type="b")

dimnames(test)


