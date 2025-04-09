# Load useful paths, packages and functions
source("5.RemoteMonitoring/utils.R")

# filter the permanent IEQM sampling sites 
pep_path <- "dat/friches/placettes/PEP.gpkg"
lrs <- sf::st_layers(pep_path)
pep <- sf::st_read(pep_path, layer = lrs$name[1]) # high level metadata 
peeori <- sf::st_read(pep_path, layer = lrs$name[29]) |> #pee_ori_sond
  as_tibble()
# Note: An "origine" perturbation is an intervention that removed >75%
# of the merchantable surface area
table(peeori$origine) #CT = coupe totale
table(peeori$an_origine) #we want sites that have been cut more than 15 years ago
# the variables are measured using photointerpretation
# the column an_pro_ori tells us the year associated with the satellite photo used
peeori$age_perturb <- as.numeric(peeori$an_pro_ori) - as.numeric(peeori$an_origine)
table(peeori$age_perturb)
peeori$landsat_avail_full <- ifelse(as.numeric(peeori$an_origine) > 1972, 1, 0)
potsites <- peeori |>
  dplyr::filter(origine == "CT" & age_perturb > 15 & landsat_avail_full == 1)
# add geographic information 
potsites <- potsites |>
  dplyr::left_join(pep, by = "id_pe") |>
  sf::st_as_sf()

# load the small AOI created in QGIS to collect a set of sampling sites
aoi <- "dat/friches/aoi_friches.gpkg" |>
  sf::st_read(quiet = TRUE)
aoi$area <- sf::st_area(aoi)

# filter the sites using the aoi
potsites_aoi <- potsites |> 
  sf::st_intersection(aoi) 

# forest stands
stands <- extract_friches(aoi, "dat/friches/", only_friches = FALSE) |>
  sf::st_intersection(aoi) |>
  dplyr::filter(TYPE_TER == "TRF" & !is.na(TYPE_COUV))

# add corresponding stands to sampling sites
potsites_stands <- potsites |>
  sf::st_drop_geometry() |>
  dplyr::select(id_pe:an_perturb, type_couv:etat_str, in_etage:landsat_avail_full, latitude:longitude) |>
  dplyr::left_join(
    stands |> 
      dplyr::select(GEOC_MAJ:AN_PERTURB, ET_DOMI:STRATE, SUPERFICIE, Shape), 
    by = c("geocode" = "GEOCODE")
  ) |>
  dplyr::select(!contains(".x")) |>
  dplyr::select(!contains(".y")) |>
  sf::st_as_sf() 

# export
sf::st_write(potsites, "dat/friches/placettes/potential_sites.gpkg", append = FALSE)
sf::st_write(potsites_stands, "dat/friches/placettes/potential_sites.gpkg", "samplingsites_stands", append = TRUE)
sf::st_write(stands, "dat/stands.gpkg", append = FALSE)

