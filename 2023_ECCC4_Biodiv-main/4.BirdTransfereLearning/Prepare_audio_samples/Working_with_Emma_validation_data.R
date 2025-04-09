######## Set up ########
# Libraries
suppressPackageStartupMessages({
     library(readxl)
     library(dplyr)
})

# P-drive path
p_path<-base::file.path("P:","Projets","Actif","2023_ECCC4_Biodiv")

######## Identify total number of species ########
# Data folder names
folder_names<-c(
     "Audiomoths_2023_AudioSamples",
     "Audiomoths_2024_Spring_AudioSamples",
     "Audiomoths_2024_Summer_AudioSamples")

# Loop and identify species
sp_df<-list()
for(i in 1:length(folder_names)){
     folder<-folder_names[i]
     sub_list<-list()

     # Path to data
     data<-file.path(p_path,"3-Analyses","1-Data","AudioMoth_recordings",folder)

     # List folders inside
     folders_inside<-base::list.files(path=data,include.dirs=TRUE)

     # Remove Noise, Uncertain, snippet_paths.csv and snippet_paths.xlsx
     folders_inside <- folders_inside[folders_inside != "Noise"]
     folders_inside <- folders_inside[folders_inside != "Uncertain"]
     folders_inside <- folders_inside[folders_inside != "snippet_paths.csv"]
     folders_inside <- folders_inside[folders_inside != "snippet_paths.xlsx"]

     # Loop over species and Identify species
     for(ii in 1:length(folders_inside)){
          species_folder<-folders_inside[ii]

          # Path to species data
          sp_data<-file.path(data,species_folder)

          # List folders inside
          species_files_inside<-base::list.files(path=sp_data,include.dirs=FALSE)

          # Remove "Not_processed"
          Not_processed <- "Not_processed" %in% species_files_inside
          species_files_inside <- species_files_inside[species_files_inside != "Not_processed"]

          # Correct species name
          species_name<-base::gsub("Correct ", "", species_folder)
          species_name<-base::gsub("INCORRECT ", "", species_name)

          # Create dataframe with results
          species_dataframe_results<-data.frame(
               season = folder,
               species = species_name,
               Not_processed = Not_processed,
               occrances = as.double(length(species_files_inside))
          )

          # Add to list
          sub_list[[ii]]<-species_dataframe_results
     }
     # Collapse Add to list
     sp_df[[i]]<-do.call(rbind,sub_list)
     print(paste0("Done with folder:   ",folder))
}

# Combine into dataframe
species_dataframe<-do.call(rbind,sp_df)

# Identify total number of species
total_species<-species_dataframe %>%
     dplyr::pull(species) %>%
     unique()
length(total_species)
# 136
print(total_species)
#  [1] "Aix sponsa"                 "Anas crecca"
#  [3] "Anas platyrhynchos"         "Anser caerulescens"
#  [5] "Baeolophus bicolor"         "Bombycilla cedrorum"       
#  [7] "Branta canadensis"          "Bucephala clangula"
#  [9] "Buteo jamaicensis"          "Buteo platypterus"
# [11] "Catharus guttatus"          "Certhia americana"
# [13] "Coccothraustes vespertinus" "Colaptes auratus"
# [15] "Columba livia"              "Corvus brachyrhynchos"
# [17] "Corvus corax"               "Cyanocitta cristata"
# [19] "Dryobates pubescens"        "Dryobates villosus"
# [21] "Dryocopus pileatus"         "Falco columbarius"
# [23] "Gavia immer"                "Haemorhous mexicanus"
# [25] "Haemorhous purpureus"       "Haliaeetus leucocephalus"
# [27] "Junco hyemalis"             "Larus delawarensis"
# [29] "Lophodytes cucullatus"      "Megaceryle alcyon"         
# [31] "Melanerpes carolinus"       "Meleagris gallopavo"
# [33] "Melospiza melodia"          "Molothrus ater"
# [35] "Parkesia noveboracensis"    "Passer domesticus"
# [37] "Poecile atricapillus"       "Quiscalus quiscula"
# [39] "Regulus satrapa"            "Setophaga coronata"
# [41] "Setophaga palmarum"         "Sialia sialis"
# [43] "Sitta canadensis"           "Sitta carolinensis"
# [45] "Sphyrapicus varius"         "Spinus pinus"
# [47] "Spinus tristis"             "Sturnus vulgaris"
# [49] "Turdus migratorius"         "Vireo olivaceus"
# [51] "Zenaida macroura"           "Zonotrichia albicollis"
# [53] "Acanthis flammea"           "Cardinalis cardinalis"     
# [55] "Loxia leucoptera"           "Plectrophenax nivalis"
# [57] "Spizelloides arborea"       "Zonotrichia leucophrys"
# [59] "Actitis macularius"         "Agelaius phoeniceus"
# [61] "Anas rubripes"              "Antigone canadensis"
# [63] "Archilochus colubris"       "Bonasa umbellus"
# [65] "Buteo lineatus"             "Catharus ustulatus"
# [67] "Charadrius vociferus"       "Dumetella carolinensis"
# [69] "Gallinago delicata"         "Geothlypis trichas"
# [71] "Hirundo rustica"            "Icterus galbula"
# [73] "Larus argentatus"           "Mareca americana"
# [75] "Melospiza georgiana"        "Mergus merganser"
# [77] "Mniotilta varia"            "Myiarchus crinitus"
# [79] "Pandion haliaetus"          "Passerculus sandwichensis"
# [81] "Pheucticus ludovicianus"    "Pipilo erythrophthalmus"
# [83] "Podilymbus podiceps"        "Sayornis phoebe"
# [85] "Scolopax minor"             "Seiurus aurocapilla"
# [87] "Setophaga caerulescens"     "Setophaga fusca"
# [89] "Setophaga petechia"         "Setophaga pinus"
# [91] "Setophaga virens"           "Spizella passerina"
# [93] "Strix varia"                "Tachycineta bicolor"
# [95] "Toxostoma rufum"            "Tringa melanoleuca"
# [97] "Troglodytes hiemalis"       "Vireo gilvus"
# [99] "Vireo solitarius"           "Bucephala albeola"
#[101] "Clangula hyemalis"          "Corthylio calendula"
#[103] "Leiothlypis ruficapilla"    "Ardea herodias"
#[105] "Butorides virescens"        "Cardellina canadensis"
#[107] "Cardellina pusilla"         "Cathartes aura"
#[109] "Catharus fuscescens"        "Chordeiles minor"
#[111] "Coccyzus erythropthalmus"   "Contopus virens"
#[113] "Dolichonyx oryzivorus"      "Empidonax alnorum"
#[115] "Empidonax flaviventris"     "Empidonax minimus"
#[117] "Falco sparverius"           "Geothlypis philadelphia"
#[119] "Hylocichla mustelina"       "Melospiza lincolnii"
#[121] "Passerina cyanea"           "Petrochelidon pyrrhonota"
#[123] "Piranga olivacea"           "Riparia riparia"
#[125] "Setophaga americana"        "Setophaga castanea"
#[127] "Setophaga magnolia"         "Setophaga pensylvanica"
#[129] "Setophaga ruticilla"        "Setophaga striata"
#[131] "Setophaga tigrina"          "Sturnella magna"
#[133] "Troglodytes aedon"          "Tyrannus tyrannus"
#[135] "Vireo philadelphicus"       "Leiothlypis peregrina"

######## Identify species that still need to be validated ########
# Species with 0 observations but that contains "unprocessed" data needs validation
empty_species<-species_dataframe %>%
     dplyr::filter(occrances==0) %>%
     dplyr::pull(species) %>%
     unique()
length(empty_species)
# 56

# Load xlsx Emma worked on
base::suppressMessages({
     autumn_2023 <- readxl::read_xlsx(
          path = base::file.path(
               p_path,"3-Analyses","1-Data","AudioMoth_recordings",
               "BirdNet_species_prediction_list_Emma.xlsx"),
          sheet = "2023")
})
base::suppressMessages({
     spring_2024 <- readxl::read_xlsx(
          path = base::file.path(
               p_path,"3-Analyses","1-Data","AudioMoth_recordings",
               "BirdNet_species_prediction_list_Emma.xlsx"),
          sheet = "Spring_2024")
})
base::suppressMessages({
     summer_2024 <- readxl::read_xlsx(
          path = base::file.path(
               p_path,"3-Analyses","1-Data","AudioMoth_recordings",
               "BirdNet_species_prediction_list_Emma.xlsx"),
          sheet = "Summer_2024")
})

# View unique groups
unique(autumn_2023$`Audio ID status`)
unique(spring_2024$`Audio ID status`)
unique(summer_2024$`Audio ID status`)

# Per season, identify the validated species
autumn_2023_sp_ID<-autumn_2023 %>%
     dplyr::filter(`Audio ID status`=="DONE" | `Audio ID status`=="15 SAMPLES") %>%
     dplyr::select(Species,`Audio ID status`)
autumn_2023_sp_ID$season<-"autumn_2023"
spring_2024_sp_ID<-spring_2024 %>%
     dplyr::filter(`Audio ID status`=="DONE" | `Audio ID status`=="15 SAMPLES") %>%
     dplyr::select(Specie,`Audio ID status`) %>%
     dplyr::rename(Species=Specie)
spring_2024_sp_ID$season<-"spring_2024"
summer_2024_sp_ID<-summer_2024 %>%
     dplyr::filter(`Audio ID status`=="DONE" | `Audio ID status`=="15 SAMPLES") %>%
     dplyr::select(Specie,`Audio ID status`) %>%
     dplyr::rename(Species=Specie)
summer_2024_sp_ID$season<-"summer_2024"

# Create dataframe containing information on which season was validated
validated_species_data<-dplyr::full_join(autumn_2023_sp_ID,spring_2024_sp_ID,by="Species")
validated_species_data<-dplyr::full_join(validated_species_data,summer_2024_sp_ID,by="Species")

# Create vector of species names
validated_species<-sort(unique(
     c(autumn_2023_sp_ID$Species,spring_2024_sp_ID$Species,summer_2024_sp_ID$Species)))
rm(autumn_2023_sp_ID,spring_2024_sp_ID,summer_2024_sp_ID)

# Because some species have been validated, but contain no samples, they need to be removed, and revalidated
# Find species in `validated_species` that is not within `empty_species`
validated_species <- dplyr::setdiff(validated_species, empty_species)
length(validated_species)
# 66
print(validated_species)
# [1] "Anas platyrhynchos"      "Branta canadensis"
# [3] "Bucephala albeola"       "Bucephala clangula"
# [5] "Butorides virescens"     "Cardinalis cardinalis"
# [7] "Catharus fuscescens"     "Certhia americana"
# [9] "Charadrius vociferus"    "Colaptes auratus"
#[11] "Contopus virens"         "Corthylio calendula"
#[13] "Corvus brachyrhynchos"   "Corvus corax"
#[15] "Cyanocitta cristata"     "Dolichonyx oryzivorus"  
#[17] "Dryobates pubescens"     "Dryobates villosus"
#[19] "Dryocopus pileatus"      "Dumetella carolinensis"
#[21] "Empidonax alnorum"       "Empidonax flaviventris"
#[23] "Empidonax minimus"       "Falco sparverius"
#[25] "Geothlypis philadelphia" "Geothlypis trichas"
#[27] "Haemorhous mexicanus"    "Hirundo rustica"
#[29] "Hylocichla mustelina"    "Icterus galbula"
#[31] "Junco hyemalis"          "Larus delawarensis"
#[33] "Leiothlypis ruficapilla" "Mareca americana"
#[35] "Melospiza georgiana"     "Melospiza melodia"
#[37] "Mniotilta varia"         "Pheucticus ludovicianus"
#[39] "Piranga olivacea"        "Plectrophenax nivalis"
#[41] "Podilymbus podiceps"     "Poecile atricapillus"   
#[43] "Regulus satrapa"         "Riparia riparia"
#[45] "Sayornis phoebe"         "Seiurus aurocapilla"
#[47] "Setophaga caerulescens"  "Setophaga fusca"
#[49] "Setophaga magnolia"      "Setophaga pensylvanica"
#[51] "Setophaga petechia"      "Setophaga ruticilla"
#[53] "Setophaga striata"       "Setophaga tigrina"
#[55] "Setophaga virens"        "Sitta carolinensis"
#[57] "Spinus pinus"            "Spinus tristis"
#[59] "Sturnella magna"         "Tachycineta bicolor"
#[61] "Troglodytes hiemalis"    "Turdus migratorius"
#[63] "Vireo gilvus"            "Vireo philadelphicus"   
#[65] "Vireo solitarius"        "Zonotrichia albicollis"

# Remove these species from `validated_species_data`
validated_species_data<- validated_species_data %>%
     dplyr::filter(Species %in% validated_species)
validated_species_data$processed<-NA

# Save list
save_file_location<-base::file.path(p_path,"3-Analyses","1-Data","Biodiversity","Species_already_validated.csv")
write.csv(validated_species_data,save_file_location)

# Identify species left to validate
# Find species in `total_species` that is not within `validated_species`
species_needs_validation<- dplyr::setdiff(total_species, validated_species)

# Save list
save_file_location<-base::file.path(p_path,"3-Analyses","1-Data","Biodiversity","Species_left_to_validate.csv")
species_needs_validation<-data.frame(species=species_needs_validation)
species_needs_validation$processed<-NA
write.csv(species_needs_validation,save_file_location)
