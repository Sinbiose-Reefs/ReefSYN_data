# ---------------------------------#
# package to transit across WD & folders

require(here)

# call scripts and organize data

# create a folder to host datasets organized according to darwin core standards

dir.create ("DwC_output")
# fish datasets
dir.create (here ("DwC_output", "RMorais_spatialData")) # dataset I
dir.create (here ("DwC_output", "RFrancini_timeSeries_abrolhos")) # datasets II and XII
dir.create (here ("DwC_output", "RJ_time_series")) # dataset III
dir.create (here ("DwC_output", "PELD_iloc_fish")) # dataset IV
dir.create (here ("DwC_output", "Pinheiro_TrindadeMVaz")) # dataset V
dir.create (here ("DwC_output", "SC_time_series")) # dataset VI
dir.create (here ("DwC_output", "Pinheiro_GuarapariES")) # dataset VII 
dir.create (here ("DwC_output", "Pinheiro_ES")) # dataset VIII
dir.create (here ("DwC_output", "GLongo_spatialData")) # dataset IX
dir.create (here ("DwC_output", "Alcatrazes_time_series"))  # dataset X
dir.create (here ("DwC_output", "GLongo_NRoss_spatialData")) # dataset XI and XVI

# benthic datasets
dir.create (here ("DwC_output", "RFrancini_spatialData")) # dataset XIII
dir.create (here ("DwC_output", "AAued_spatialData")) # dataset XIV
dir.create (here ("DwC_output", "PELD_iloc_benthos")) # dataset XV


# ---------------------

# fish datasets
source (here ("Data","occ_Morais_et_al", "data_organization.R")) # organize dataset I
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_fish.R")) # organize dataset II
source (here ("Data","occ_Mendes_temporal_arraial", "data_organization.R")) # organize dataset III
source (here ("Data","PELD_iloc_fish", "data_organization.R")) # organize dataset IV
source (here ("Data","occ_Pinheiro_Trindade", "data_organization.R")) # organize dataset V
source (here ("Data","occ_Floeter_temporal_santa_catarina", "data_organization.R")) # organize dataset VI
source (here ("Data","occ_Pinheiro_Guarapari", "data_organization.R")) # organize dataset VII
source (here ("Data","occ_Pinheiro_ES", "data_organization.R")) # organize dataset VIII
source (here ("Data","occ_Longo_et_al", "data_organization.R")) # organize dataset IX
source (here ("Data","occ_Quimbayo_temporal_alcatrazes", "data_organization.R")) # organize dataset X
source (here ("Data","occ_RN_Norte_Longo", "data_organization.R")) # organize dataset XI and XVI

# benthic datasets
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_benthos.R")) # organize dataset XII
source (here ("Data","occ_Francini_et_al", "data_organization.R")) # organize dataset XIII
source (here ("Data","occ_Aued_et_al", "data_organization.R")) # organize dataset XIV
source (here ("Data","PELD_iloc_benthos", "data_organization.R")) # organize dataset XV



