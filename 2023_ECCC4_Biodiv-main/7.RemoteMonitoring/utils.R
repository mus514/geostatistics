# This file contains paths to useful data and loads useful packages

#LIBRARIES

shhh <- function(x){suppressWarnings(suppressPackageStartupMessages(x))}

cran_packages <- c(
  "devtools"
  , "sf"
  , "raster"
  , "terra"
  , "rstac"
  , "fasterize"
  , "dplyr"
  , "tidyr"
  , "ggplot2"
  , "readr"
  , "httr"
)

toinstall_cran <- setdiff(cran_packages, rownames(installed.packages()))
install.packages(toinstall_cran)

install_forester <- !any(grepl("ForestR", rownames(installed.packages())))
if (install_forester) {
  gh_tk <- readLines("5.RemoteMonitoring/ghtoken.txt", warn = FALSE)
  Sys.setenv("GITHUB_PAT" = gh_tk)
  devtools::install_github("Habitat-RD/2023_HAB_ForesteR")
}
shhh(library(ForestR))

print("Libraries installed.")

lapply(cran_packages, function(x) {
  shhh(library(x, character.only = TRUE))
})

print("Libraries loaded.")

# FUNCTIONS

extract_friches <- function(roi, destfolder, only_friches = TRUE) {
  if (!dir.exists(destfolder)) {
    dir.create(destfolder)
  }
  # for this, we need to change the timeout option
  options(timeout = 20 * 60)
  data(tiles)
  roi_t <- tiles %>%
    sf::st_intersection(roi)
  t <- roi_t$NTS_SNRC
  # base ieqm url
  IEQM_url <- "https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Cartes_ecoforestieres_perturbations/" #nolint
  output_list <- c()
  t_downloaded <- list.files(
    destfolder, pattern = "CARTE_ECO_MAJ_", full.names = F
  )

  for (i in t) {
    tt <- strsplit(i, "")[[1]][2:4] %>%
      paste(collapse = "")

    tmp <- paste0("CARTE_ECO_MAJ_", tt, "_10.gdb")

    if (!tmp %in% t_downloaded) {
      todwld <- paste0(IEQM_url, tt, "/CARTE_ECO_MAJ_", tt, "_10.zip")
      destfile <- paste0(destfolder, "/tile_", tt, ".zip")
      download.file(todwld, destfile, method = "libcurl")
      unzip(destfile, exdir = destfolder)
    }

    tmp_plot <- sf::st_read(
      dsn = paste0(destfolder, "CARTE_ECO_MAJ_", tt, "_10.gdb"),
      layer = paste0("PEE_MAJ_", tt),
      quiet = TRUE
    )

    if (only_friches) {
      tmp_plot <- tmp_plot %>%
        dplyr::filter(ORIGINE == "FR" & !CL_HAUT %in% c("1", "2", "3", "4")) %>%
        dplyr::select(GEOCODE, ORIGINE, CL_HAUT) %>%
        sf::st_transform(crs = crs(roi)) %>%
        sf::st_intersection(st_geometry(roi))
    }

    output_list[[i]] <- tmp_plot

  } #for loop

  ieqm_friches <- do.call(rbind, output_list) %>%
    unique

  return(ieqm_friches)
}


# PATHS

wd <- getwd()

#Define the project codes
project_code <- "2023_ECCC4_Biodiv"

# Path to Analysis Folder
path_gh <- paste0("P:/Projects/", project_code)

# Path to project
path_data <- paste0(
  "P:/Projets/Actif/",
  project_code,
  "/3-Analyses/1-Data/"
)

# Path to Database
path_db <- "P:/Database/"

print("Paths to project files defined.")

# LOCAL DATA fOLDER

local_dat <- "dat/"
if (!dir.exists(local_dat)) {
  dir.create(local_dat)
}

# add to gitignore
gi <- readLines(".gitignore", warn = FALSE)
do_write <- !any(grepl("dat/", gi))
if (do_write) {
  write("\ndat/", file = ".gitignore", append = TRUE)
}

# useful functions

create_mask_landsat <- function(qa_band, prgm) {
  # landat 4 to 7
  if (prgm < 8) {
    cloud_mask <- qa_band %in% c(
      5442, # dilated cloud over land
      5506, # dilated cloud over water
      5696, # mild conf. cloud
      5760, # mild conf. cloud over water
      5896, # high conf. cloud
      7440, # high conf. cloud shadow
      7568, # water with cloud shadow
      7696, # mild conf. cloud with shadow
      7824, # mild conf. cloud with shadow over water
      7960, # high conf. cloud with shadow
      8088, # high conf. cloud with shadow over water
      13664 # high conf. snow or ice
    )
  } else {
    cloud_mask <- qa_band %in% c(
      21826, # dilated cloud over land
      21890, # dilated cloud over water
      22080, # mild conf. cloud
      22144, # mild conf. cloud over water
      22280, # high conf. cloud
      23888, # high conf. cloud shadow
      23952, # water with cloud shadow
      24088, # mild conf. cloud with shadow
      24216, # mild conf. cloud with shadow over water
      24344, # high conf. cloud with shadow
      24472, # high conf. cloud with shadow over water
      30048, # high conf. snow or ice
      54596, # High conf Cirrus
      54852, # Cirrus, mid cloud
      55052 # Cirrus, high cloud
    )
  }
  return(cloud_mask)
}

evi <- function(red, nir, blue) {
  evi_val <- 2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1))
  return(evi_val)
}

ndvi <- function(red, nir) {
  ndvi_val <- (nir - red) / (nir + red)
  return(ndvi_val)
}

sr <- function(nir, red) {
  sr_val <- nir / red
  return(sr_val)
}

gr <- function(nir, green) {
  gr_val <- nir / green
  return(gr_val)
}

savi <- function(nir, red) {
  savi_val <- (nir - red) / (nir + red + 0.5)
  return(savi_val)
}

msi <- function(swir, nir) {
  
}

veg_indices <- function(mpc_query, i, prgm) {
  qaband <- rstac::assets_url(mpc_query, "qa_pixel")[i] |>
    terra::rast() |>
    create_mask_landsat(prgm)
  sitecrop <- site |> 
    sf::st_transform(crs = crs(qaband))
  redband <- rstac::assets_url(mpc_query, "red")[i] |>
    terra::rast() |> 
    terra::mask(qaband, maskvalue = TRUE) |>
    terra::crop(sitecrop, mask = TRUE)
  blueband <- rstac::assets_url(mpc_query, "blue")[i] |>
    terra::rast() |> 
    terra::mask(qaband, maskvalue = TRUE) |>
    terra::crop(sitecrop, mask = TRUE)
  nirband <- rstac::assets_url(mpc_query, "nir08")[i] |>
    terra::rast() |> 
    terra::mask(qaband, maskvalue = TRUE) |>
    terra::crop(sitecrop, mask = TRUE)
  evi_val <- 2.5 * ((nirband - redband) / (nirband + 6 * redband - 7.5 * blueband + 1))
  # ajouter les différents indices
  return(evi_val)
}


#End of script#