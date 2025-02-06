# ---------------------------------#
# package to transit across WD & folders

require(here)

# call scripts and organize data

# create a folder to host datasets organized according to darwin core standards

dir.create ("DwC_output")
# fish datasets
dir.create (here ("DwC_output", "I")) # dataset I
dir.create (here ("DwC_output", "II")) # datasets II and XII
dir.create (here ("DwC_output", "III")) # dataset III
dir.create (here ("DwC_output", "IV")) # dataset IV
dir.create (here ("DwC_output", "V")) # dataset V
dir.create (here ("DwC_output", "VI")) # dataset VI
dir.create (here ("DwC_output", "VII")) # dataset VII 
dir.create (here ("DwC_output", "VIII")) # dataset VIII
dir.create (here ("DwC_output", "IX")) # dataset IX
dir.create (here ("DwC_output", "X"))  # dataset X
dir.create (here ("DwC_output", "XI")) # dataset XI and XVI

# benthic datasets
dir.create (here ("DwC_output", "XII")) #  XII
dir.create (here ("DwC_output", "XIII")) # dataset XIII
dir.create (here ("DwC_output", "XIV")) # dataset XIV
dir.create (here ("DwC_output", "XV")) # dataset XV
dir.create (here ("DwC_output", "XVI")) # dataset XI and XVI


# ---------------------

# fish datasets
source (here ("Data","occ_Morais_et_al", "data_organization.R")) # organize dataset I
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_fish.R")) # organize dataset II
source (here ("Data","occ_Mendes_temporal_arraial", "data_organization.R")) # organize dataset III
source (here ("Data","PELD_iloc_fish", "data_organization.R")) # organize dataset IV
source (here ("Data","occ_Pinheiro_Trindade", "data_organization.R")) # organize dataset V
source (here ("Data","occ_Floeter_temporal_santa_catarina", "data_organization.R")) # organize dataset VI
source (here ("Data","occ_Pinheiro_ES", "data_organization.R")) # organize dataset VIII
source (here ("Data","occ_Longo_et_al", "data_organization.R")) # organize dataset IX
source (here ("Data","occ_Quimbayo_temporal_alcatrazes", "data_organization.R")) # organize dataset X
source (here ("Data","occ_RN_Norte_Longo", "data_organization.R")) # organize dataset XI 

# benthic datasets
source (here ("Data","occ_Francini_temporal_abrolhos", "data_organization_benthos.R")) # organize dataset XII
source (here ("Data","occ_Francini_et_al", "data_organization.R")) # organize dataset XIII
source (here ("Data","occ_Aued_et_al", "data_organization.R")) # organize dataset XIV
source (here ("Data","PELD_iloc_benthos", "data_organization.R")) # organize dataset XV

# it depends on dataset XIV
source (here ("Data","occ_Pinheiro_Guarapari", "data_organization.R")) # organize dataset VII

# all files are in the correct folders

# end ()


