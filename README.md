Standardized datasets of Brazilian reef diversity in space and time
================
Reef Synthesis Working Group
2025/10/28

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Here we present main details about our datapaper. We adopted the Darwin
Core Standard to organize the 16 datasets. The root R script
“RUN_TO_ORGANIZE_DATA_INTO_DwC.R” can be used to generate these
organized datasets from raw data. By running this script you will create
the folder ‘DwC_output’, which will then host the processed datasets.
The last updates are listed in the end of this file.

Folders in this project:

- **Data**: raw datasets. Each dataset folder has its own R script to
  organize data (DwC standard);

- **DwC_output**: the processed data, after running each organization
  script - folders will be numbered sequentially from I to XVI;

- **Policy**: policies of authorship and data sharing (only Portuguese);

- **R**: folder with R scripts. Draft of a Shiny App;

## Figure: Team members’ Institutes and Research Topics

<img src="README_files/figure-gfm/fig1.png" width="100%" height="100%" style="display: block; margin: auto;" />
Institutes (A) and research topics (B) explored by ReefSYN.
Abbreviations: AIMS: Australian Institute of Marine Science; CDU:
Charles Darwin University; CEBIMar: Centre for Marine Biology of the
University of São Paulo; ICMBio: Chico Mendes Institute for Biodiversity
Conservation; IO-USP: Oceanographic Institute of the University of São
Paulo; PSL: Paris Sciences et Lettres University; UENF: North Fluminense
State University; UFES: Federal University of Espírito Santo; UFF:
Fluminense Federal University; UFOPA: Federal University of Western
Pará; UFRN: Federal University of Rio Grande do Norte; UFSC: Federal
University of Santa Catarina; UFSM: Federal University of Santa Maria;
UnB: University of Brasília.

## ReefSYN Data Structure

<img src="dataStructure.png" width="75%" height="75%" style="display: block; margin: auto;" />
Structure of the datasets gathered by the ReefSYN working group, showing
the Darwin Core Standard terms included in most datasets. These terms
are part of a standardized glossary maintained by the Darwin Core Task
Group (2009).

# Data availability

These data are published under CC BY-NC license. “Policy of data sharing
and use” can be found in the ReefSYN Organization in Github (available
at
[here::here](https://github.com/Sinbiose-Reefs/reefsyn_site/blob/master/DataPolicy_SINBIOSE.pdf)).
Data embargo goes up to January 2025.

# Acknowledgements

Students and researchers that collected the data. ReefSYN was supported
by the Center for Synthesis in Biodiversity and Ecosystem Services
(SinBiose, CNPq). The authors would like to thank the contributions of
Thiago Silveira and Marina Sissini (PELD-ILOC team).

# Financial support

This project is funded by the Center for Synthesis in Biodiversity and
Ecosystem Services SinBiose
(<https://www.gov.br/cnpq/pt-br/acesso-a-informacao/acoes-e-programas/programas/sinbiose-1>)
(CNPq \#442417/2019-5, granted to MGB). The center is part of the
National Council for Scientific and Technological Development (Conselho
Nacional de Desenvolvimento Científico e Tecnológico, CNPq). Researchers
from the “Brazilian Marine Biodiversity Research Network – SISBIOTA-Mar”
(CNPq \#563276/2010-0 and FAPESC \#6308/2011-8 to SRF) and ‘‘Programa de
Monitoramento de Longa Duração das Comunidades Recifais de Ilhas
Oceânicas – PELD ILOC’’ (CNPq 441241/2016-6, to CELF), initiatives that
collected and shared their data sets used in this research. ALL received
post-doctoral fellowships from CNPq (#153024/2022-4, \#164240/2021-7,
\#151228/2021-3, \#152410/2020-1). J.P.Q. received post-doctoral
fellowship from FAPESP (2018/21380-0 and 2021/). TCM received
post-doctoral fellowships from FAPERJ (E-26/202.372/2021) and CNPq
(#102450/2022-6). GOL is grateful to a research productivity scholarship
provided by the Brazilian National Council for Scientific and
Technological Development (CNPq; 310517/2019-2) and Serrapilheira
Institute (Grant No. Serra-1708-15364) for continued research support.

## Main updates:

### 2025/06/24

1 - Folders with DwC data named appropriately (from I to XVI);  
2 - The redundancy of records among data sets was eliminated. For
instance, many records from PELD, ES, SC, …, were aggregated to the
Dataset I – SISBIOTA data set– in the past, generating the data
redundancy (identified by Juliana Fonseca-UFF). Thus, we kept the
records from their original sources;  
3 - The taxonomic updates were:  
a) *Ophioblennius atlanticus* (Only eastern Atlantic): Brazil has
*Ophioblennius trinitatis* in the coast and islands. The only exception
is Santa Catarina, which has a new species (SR Floeter pers. comm.).
Thus, the data set of Santa Catarina has just “Ophioblennius”. Details
can be found here:
<https://lbmm.ufsc.br/pdfs/Lastrucci_et_al_(2018)_Ophioblennius_JFB.pdf>;  
b) *Stegastes fuscus trindadensis* in Trindade, instead of *Stegastes
fuscus*.  

### 2025/10/28

1 - Correcting year in dataset IX;  
2 - Correction of site name in dataset V (“cabritas” to
“praia_das_cabritas”);  

ToDo list:

1)  Sum of benthic cover within eventIDs sum 1 (ie. plot scale). For
    some datasets might not be possible (XIII, for which plot ID was not
    available). Look the scripts in R/Corrections and try to solve the
    issues. PS: perhaps not a problem if one wants to work at site
    level, but need some care if one is interested in plot-level cover
    data;
2)  Is it possible to define a scale (hierarchy of locations,
    localities, etc) that integrates all data sets? From now, each data
    set has an unique hierarchy which might not match with other data
    sets. Perhaps the best way to create ‘sites’ is through the use of
    the whole hierarchy by creating combinations of unique coordinates,
    locations, localities, regions.

## The data was processed using the following versions of software and associated packages:

    ## R version 4.4.1 (2024-06-14 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: Europe/Paris
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] patchwork_1.3.0         magrittr_2.0.3          reshape2_1.4.4         
    ##  [4] mapdata_2.3.1           maps_3.4.2              plotly_4.10.4          
    ##  [7] xts_0.13.2              zoo_1.8-12              dygraphs_1.1.1.6       
    ## [10] vegan_2.6-6.1           lattice_0.22-6          permute_0.9-7          
    ## [13] leaflet_2.2.2           flexdashboard_0.6.2     rerddap_1.2.0          
    ## [16] png_0.1-8               knitr_1.48              wordcloud_2.6          
    ## [19] RColorBrewer_1.1-3      terra_1.8-10            SnowballC_0.7.1        
    ## [22] tm_0.7-15               NLP_0.3-2               sf_1.0-18              
    ## [25] scatterpie_0.2.4        ggrepel_0.9.6           gridExtra_2.3          
    ## [28] rnaturalearthdata_1.0.0 rnaturalearth_1.0.1     lubridate_1.9.3        
    ## [31] forcats_1.0.0           stringr_1.5.1           dplyr_1.1.4            
    ## [34] purrr_1.1.0             readr_2.1.5             tidyr_1.3.1            
    ## [37] tibble_3.2.1            ggplot2_3.5.2           tidyverse_2.0.0        
    ## [40] openxlsx_4.2.7.1        here_1.0.1             
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] DBI_1.2.3          rlang_1.1.3        e1071_1.7-16       compiler_4.4.1    
    ##  [5] mgcv_1.9-1         vctrs_0.6.5        httpcode_0.3.0     pkgconfig_2.0.3   
    ##  [9] fastmap_1.2.0      rmarkdown_2.28     tzdb_0.4.0         xfun_0.51         
    ## [13] cachem_1.1.0       jsonlite_1.8.9     highr_0.11         tweenr_2.0.3      
    ## [17] cluster_2.1.6      R6_2.6.1           bslib_0.8.0        stringi_1.8.4     
    ## [21] hoardr_0.5.5       jquerylib_0.1.4    Rcpp_1.0.13        Matrix_1.7-0      
    ## [25] splines_4.4.1      timechange_0.3.0   tidyselect_1.2.1   rstudioapi_0.16.0 
    ## [29] yaml_2.3.8         codetools_0.2-20   curl_5.2.3         plyr_1.8.9        
    ## [33] withr_3.0.2        evaluate_1.0.1     units_0.8-5        proxy_0.4-27      
    ## [37] polyclip_1.10-7    zip_2.3.1          xml2_1.3.6         pillar_1.11.0     
    ## [41] KernSmooth_2.23-24 ggfun_0.1.6        ncdf4_1.23         generics_0.1.4    
    ## [45] rprojroot_2.0.4    hms_1.1.3          munsell_0.5.1      scales_1.3.0      
    ## [49] class_7.3-22       glue_1.7.0         slam_0.1-55        lazyeval_0.2.2    
    ## [53] tools_4.4.1        data.table_1.17.8  fs_1.6.4           grid_4.4.1        
    ## [57] crosstalk_1.2.1    colorspace_2.1-1   nlme_3.1-164       ggforce_0.4.2     
    ## [61] cli_3.6.2          rappdirs_0.3.3     viridisLite_0.4.2  gtable_0.3.5      
    ## [65] yulab.utils_0.1.8  sass_0.4.9         digest_0.6.35      classInt_0.4-10   
    ## [69] crul_1.5.0         htmlwidgets_1.6.4  farver_2.1.2       htmltools_0.5.8.1 
    ## [73] lifecycle_1.0.4    httr_1.4.7         MASS_7.3-60.2
