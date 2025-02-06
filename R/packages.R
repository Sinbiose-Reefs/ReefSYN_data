
# Package names
packages <- c("here", "openxlsx", "tidyverse", "rnaturalearth", "rnaturalearthdata" , "gridExtra" , 
              "ggrepel" , "scatterpie" , "sf" , "rgeos" , "tm", "SnowballC", 
              "wordcloud", "tm","knitr", "png", "rerddap", "parallel", "lubridate",
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