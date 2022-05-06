# ---------------------------------#
# package to transit across WD & folders

require(here)

# call scripts and organize data

# create a folder to host datasets organized according to darwin core standards

dir.create ("DwC_output")
dir.create (here ("DwC_output", "AAued_spatialData"))
dir.create (here ("DwC_output", "GLongo_NRoss_spatialData"))
dir.create (here ("DwC_output", "GLongo_spatialData"))
dir.create (here ("DwC_output", "RFrancini_spatialData"))
dir.create (here ("DwC_output", "RFrancini_timeSeries_abrolhos"))
dir.create (here ("DwC_output", "RJ_time_series"))
dir.create (here ("DwC_output", "RMorais_spatialData"))
dir.create (here ("DwC_output", "SC_time_series"))
dir.create (here ("DwC_output", "PELD_iloc_benthos"))
dir.create (here ("DwC_output", "PELD_iloc_fish"))



# ---------------------

# organize data of Aued et al. 2018

source (here ("Data","occ_Aued_et_al", "org_bentos_data.R"))




# organize data of FLoeter et al. unpublished

source (here ("Data","occ_Floeter_temporal_santa_catarina", "data_organization.R"))




# organize data of Francini & Santana et al. unpublished

source (here ("Data","occ_Francini_et_al", "Script_data_organization_francini.R"))




# organize data of Francini et al Time Series of Abrolhos
# fish
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_fish.R"))

# benthos
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_benthos.R"))




# organize data of Longo et al 2019

source (here ("Data","occ_Longo_et_al", "Script_data_organization_Longo_et_al_2019.R"))




# organize data of Mendes, Ferreira et al unpublished

source (here ("Data","occ_Mendes_temporal_arraial", "data_organization.R"))





# organize data of Mendes, Ferreira et al unpublished

source (here ("Data","occ_Mendes_temporal_arraial", "data_organization.R"))





# organize data of Morais et al. 2017

source (here ("Data","occ_Morais_et_al", "organizacao_dados.R"))






# organize data of Morais et al. 2017
# fish and benethos at once

source (here ("Data","occ_RN_Norte_Longo", "data_organization.R"))




# a couple of adjustments in peld iloc datasets


# organize data of Morais et al. 2017
# fish and benethos at once

source (here ("Data","PELD_iloc_benthos", "data_organization.R"))
source (here ("Data","PELD_iloc_fish", "data_organization.R"))


