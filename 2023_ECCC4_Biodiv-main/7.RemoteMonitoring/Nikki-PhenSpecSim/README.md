# ECCC4_PhenSpecSim
List of scripts needed to calculate changes in phenospectral similarity 

# R	

### 01_prep-IEQM.R

Filters IEQM forest inventory sites to ones appropriate for testing workflow
 
### 02_prep-polygon-files_IEQM.R

Prepares shapefiles needed for spectral angle analysis from IEQM forest inventory sites
 
### 03_prep-polygon-files_coulees.R

Prepares shapefiles needed for spectral angle analysis from agricultural corridors
 
### 04_calculate-spec-angle-R.R

Calculates spectral angle between target and reference sites using Sentinel2 surface reflectance layers for IEQM forest inventory sites
 
### 05_analyze-spec-angle-GEE_IEQM.R

Analyzes spectral angle data from Google Earth Engine script for IEQM forest inventory sites
 
### 06_analyze-spec-angle-GEE_coulees.R

Analyzes spectral angle data from Google Earth Engine script for agricultural corridors  
 
# GEE	

### get_s2_layers.txt

Exports cloud-free Sentinal2 data for period of interest
 
### spectral-angle-analysis_s2_IEQM.txt

Calculates spectral angle between target and reference sites using Sentinel2 surface reflectance layers for IEQM forest inventory sites
  
### spectral-angle-analysis_s2_coulees.txt

Calculates spectral angle between target and reference sites using Sentinel2 surface reflectance layers for agricultural corridors  
  
# Other

### GEE_imports.txt
  
List of imported variables needed for all GEE scripts 
  
### GEE_runTaskList.txt

Functions for the GEE console that run all unsubmitted tasks

### Run scripts in the following order:
01_prep-IEQM.R

02_prep-polygon-files_IEQM.R

03_prep-polygon-files_coulees.R

get_s2_layers.txt

spectral-angle-analysis_s2_IEQM.txt

spectral-angle-analysis_s2_coulees.txt

04_calculate-spec-angle-R.R

05_analyze-spec-angle-GEE_IEQM.R

06_analyze-spec-angle-GEE_coulees.R
  
