###############################################################################################
#
#                  AUXILIARY FUNCTIONS TO SCRIPT "BERN CITY RAYSHADER MAP"
#
#  -------------------------------------------------------------------------------------------
#
#  Description:
#  ------------
#  Auxiliary Functions for custom data loading & cropping
#  ----------------------------------------------------------------------------
#
#  Created by: Christoph von Matt (Twitter @chvonmatt / Github: @codicolus)
#  Date: 11-04-2022 (cleaned + extended version)
#
#  Copyright 2022 Christoph von Matt
#  Licence: CC-BY-NC-SA 3.0
#
#  For licence information see here:
#  https://creativecommons.org/licenses/by-nc-sa/3.0/de/
#  Full licence informations:
#  https://creativecommons.org/licenses/by-nc-sa/3.0/de/legalcode
#
#
###############################################################################################

# Libraries
# --------------------------
# --------------------------
library(raster)
library(sf)

# Variables
# --------------------------
# --------------------------
# projections (from http://epsg.io/2056)
LV95_proj4 <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

# Functions
# --------------------------
# --------------------------

# Data Import & Merge
# -------------------

# Function which Prepares Filepaths
# - if provided a folder - returns all files
get_filepaths <- function(path){
  if(is.na(path)){return(NA)}
  # check validity of files
  if(length(grep("[.]", path)) != 0){
    files <- path
  }else{
    # if directory exists
    if(dir.exists(path)){
      files <- list.files(path, full.names = TRUE)
    }else{
      warning("No files found!")
    }
  }
  return(files)
}



# This function loads the data according to specifications
# Type: 
# - either DEM or IMAGE
# Path: 
# - if NA --> custom data is loaded
# - if not NA: checks if single file (evtl. in folder) or several files
# --> if several files: tries to merge them
load_data <- function(path, type = "DEM", projection = LV95_proj4){
  
  if(!type %in% c("DEM", "IMAGE")){
    warning("Type must be either 'IMAGE' or 'DEM'!")
    return(NA)
  }
  if(is.na(path)){
    return(NA)
  }
  
  if(type == "DEM"){
    outdata <- raster(path)
  }else{
    outdata <- stack(path)
  }
  
  # set crs if necessary
  if(is.na(crs(outdata))){
    crs(outdata) <- crs(projection)
  }
  return(outdata)
}


# Function to merge (try to merge) & load several input tiles
merge_and_load <- function(paths, type = "DEM", projection = LV95_proj4){
  
  if(!type %in% c("DEM", "IMAGE")){
    warning("Type must be either 'IMAGE' or 'DEM'!")
    return(NA)
  }
  
  # looping over files (enumerated)
  for(file_nr in seq(length(paths))){
    
    # load data
    data <- load_data(paths[file_nr], type = type, projection = projection)
    # merge with existing
    if(file_nr == 1){
      data_merged <- data
    }else{
      data_merged <- raster::merge(data_merged, data)
    }
    rm(data)
  }
  return(data_merged)
}





