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
  , "tidyverse"
  , "tidyr"
  , "ggplot2"
  , "readr"
  , "landscapemetrics"
  , "devtools"
  , "crayon"
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

#End of script#