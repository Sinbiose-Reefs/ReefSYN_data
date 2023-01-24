# ## packages to load data
# library(here)
# library(openxlsx)
# library(tidyverse)
# 
# ## packages to map
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(gridExtra)
# library(ggrepel)
# library(scatterpie)
# library(sf)
# library(rgeos)
# 
# ## wordcloud
# 
# library(tm)
# library(SnowballC)
# library(wordcloud)
# 
# # load pictures
# library(knitr)    # For knitting document and include_graphics function
# library(png)      # For grabbing the dimensions of png files
# 
# # load packages (NOAA data)
# library(rerddap)
# library(parallel)
# library(lubridate)
# library(flexdashboard)
# library(leaflet)
# library(vegan)
# library(xts)
# library(dygraphs)
# library(plotly)
# library(mapdata)
# library(RColorBrewer)
# palette(RColorBrewer::brewer.pal(8, "Set2"))
# library(reshape2)
# library(magrittr)
# library(patchwork)

# Package names
packages <- c("here", "openxlsx", "tidyverse", "rnaturalearth", "rnaturalearthdata" , "gridExtra" , 
              "ggrepel" , "scatterpie" , "sf" , "rgeos" , "tm", "SnowballC", 
              "wordcloud", "knitr", "png", "rerddap", "parallel", "lubridate",
              "flexdashboard", "leaflet", "vegan", "dygraphs", "xts", "plotly",
              "mapdata", "RColorBrewer", "reshape2", "magrittr", "patchwork")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

palette(RColorBrewer::brewer.pal(8, "Set2"))

# Remove
rm(installed_packages, packages)