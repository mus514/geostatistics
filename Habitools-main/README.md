# Habitools <img src="man/figures/logo.png" align="right" height="139" alt="" />

Bienvenue au paquet Habitools! Ce paquet regroupe des fonctions, 
données et autres petits programme pour faciliter et accélérer 
les analyses communes. 


<!-- badges: start -->
[![R-CMD-check](https://github.com/habitat-nature/Habitools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/habitat-nature/Habitools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


## Installation

L'installation de la version de développement du paquet Habitools
[GitHub](https://github.com/habitat-nature/Habitools) with:

``` r
# install.packages("devtools")
devtools::install_github("habitat-nature/Habitools")
```

Sinon, si vous êtes directement dans le dossier du projet on utilise `load_all()` : 

``` r
devtools::load_all()
```

## Utilisation

``` r
library(Habitools)

##### Test create_sas() #####
# Create account sas
create_sas()

##### Test set_sas() #####
# Create sas for .Renviron
set_sas(sas_duration=7200)

# Update sas for .Renviron
set_sas(sas_duration=180,update=TRUE)

##### Test query_lookup() #####
# Query lookup table
geoboundaries_France <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")

# View file path
geoboundaries_France$file_path

##### Test open_parquet() #####
# Grab parquet link
aoi_metadata <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")

# Open dataset
aoi<-open_parquet(
     aoi_metadata$file_path,
     link_expiration_time=15)

# View dataset
dplyr::glimpse(aoi)
aoi |>
     dplyr::distinct(shapeISO) |> 
     dplyr::pull()

# Filter DB and create sf object
filter_aoi<-aoi |>
     dplyr::filter(shapeISO == "CA-QB") |>
     duckdbfs::to_sf(crs = 4326)

# Plot
plot(duckdbfs::to_sf(aoi,crs=4326)$geom)
plot(filter_aoi$geom,add=TRUE,col="red")

##### Test spatial_join_parquets() #####
# Open large dataset you want to reduce spatially
http_link1 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
db1 <- open_parquet(
     http_link1$file_path,
     link_expiration_time=15)

# Open smaller dataset used for spatial join
http_link2 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
db2 <- open_parquet(
     http_link2$file_path,
     link_expiration_time=15) |>
     dplyr::filter(shapeISO == "CA-QB")

# Perform spatial join
db <- spatial_join_parquets(
     db1,
     db2,
     method="st_within",
     join_type="right") |>
     duckdbfs::to_sf(crs = 4326)

# Plot
plot(duckdbfs::to_sf(db1,crs=4326)$geom)
plot(db$geom,add=TRUE,col="red")

## Use local file for spatial join
# Open large dataset you want to reduce spatially
http_link1 <- query_lookup(country="canada",subject = "geo-boundaries-adm1-simplified")
db1 <- open_parquet(
     http_link1$file_path,
     link_expiration_time=15)

# Open smaller dataset used for spatial join from local file system
http_link2 <- example_montreal_sf # local sf object
db2 <- open_parquet(
     http_link2,
     file_type="sf")

# Perform spatial join
db <- spatial_join_parquets(
     db1,
     db2,
     method="st_intersects",
     join_type="right")|>
     duckdbfs::to_sf(crs = 4326)

# Plot
plot(duckdbfs::to_sf(db1,crs=4326)$geom)
plot(db$geom,add=TRUE,col="red")

##### Test load_raster() #####
# Grab aoi for loading raster
aoi <- example_montreal_sf

# Create template for raster from aoi
tmp_rast <- create_rast_template(aoi=aoi,crs=32198,res=20)

# Open large dataset you want to reduce spatially
http_link <- query_lookup(country="canada", subject = "utilisation-territoire", year = 2020)

# Load raster from blob
lulc <- load_raster(http_link$file_path,aoi)

# Align to grid
aligned_lulc <- align_raster(raster=lulc,temp_grid=tmp_rast,method="near")
terra::plot(lulc)

```
