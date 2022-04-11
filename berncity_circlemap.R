###############################################################################################
#
#                  SCRIPT "BERN CITY RAYSHADER MAP"
#
#  --------------------------------------------------------------------------------------------
#  Description:
#  ------------
#  Script to make the Bern City-Map (Circle-Rayshader) (Map ONLY!)
#
#  Additional layouting has been done in GIMP.
#  QGIS parts have been adopted in R
#
#  Data used:
#  ----------
#  - Building Heights of the Canton of Bern
#  Link: https://opendata.swiss/de/dataset/hohenangaben-des-kantons-bern1
#  - swissIMAGE orthophotos (resolution can vary)
#  Link: https://opendata.swiss/de/dataset/swissimage-10-cm-digitale-orthophotomosaik-der-schweiz
#  - swissALTI3D (Digital Elevation Model, DEM)
#  Link: https://opendata.swiss/de/dataset/swissalti3d
#               
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
###############################################################################################

# Libraries
# -------------------------
library(raster)
library(rayshader)
library(ggplot2)
library(sf)

# auxiliary functions
source("auxiliaryfunctions_berncity.R")

# Paths
# -------------------------
# DEM & IMAGE
# can be a folder of files, or one specific file (raster-format)
# if NA (dem + image paths): default repository data will be used
# --> if 1 file in folder: reads file
# --> if several & merge = TRUE --> tries to merge tiles
# --> same for image
# NOTE: MERGE-FILES WILL BE SAVED as "merge_" + DATATYPE + ".tiff" in outfolder-directory
dem_path <- "swissALTI3D_merged.tif" # swissALTI3D-Tile(s)
image_path <- "swissIMAGE_merged.tif" # swissIMAGE-Tile(s)
merge <- FALSE

# output-folder for products
# if NA: current directory is used
outfolder_path <- NA
# write out files
write_raster <- FALSE
write_plot <- TRUE
# BUILDING HEIGHTS
# --> should be in Shapefile-Format (.shp)
# --> if NA: default repository data will be used
buildingheights_path <- NA
height_variable <- NA


# Variables
# -------------------------
# cropping to custom extent (e.g. centering city)
# --> if dem + image are not matching
# --> if FALSE: takes extent as is (ATTENTION: Tiles must be of identical extent!)
crop_to_custom <- TRUE


# Load & Prepare data
# ------------------
# get paths
dem_path <- get_filepaths(dem_path)
image_path <- get_filepaths(image_path)

# Read-in files
# ---------------
# first: DEM
# If several files + merge: merge
if((length(dem_path) > 1)&merge){
  dem <- merge_and_load(dem_path)
}else{
  # if only one file available load file
  dem <- load_data(dem_path, type = "DEM")
}

# second: IMAGE
# If several + merge: merge & save in outdir
if((length(image_path) > 1)&merge){
  image <- merge_and_load(image_path, type = "IMAGE")
}else{
  # if only one file available load file
  image <- load_data(image_path, type = "IMAGE")
}

# Save raster if desired
# NOTE: Cannot overwrite loaded files (will issue error if write_files = TRUE)
if(write_raster){
  if(!is.na(outfolder_path)){
    raster::writeRaster(dem, file.path(outfolder_path, "swissALTI3D_merged.tif"),
                        overwrite = TRUE)
    raster::writeRaster(image, file.path(outfolder_path, "swissIMAGE_merged.tif"),
                        overwrite = TRUE)
  }else{
    raster::writeRaster(dem, "swissALTI3D_merged.tif", overwrite = TRUE)
    raster::writeRaster(image, "swissIMAGE_merged.tif", overwrite = TRUE)
  }
}


# if path specified custom data else example data
buildingheights_path <- ifelse(is.na(buildingheights_path),
                               "heightdata_cantonbern/buildings_city.shp",
                               buildingheights_path)
# read shapefile
buildings <- read_sf(buildingheights_path)

# custom data may require a coordinate transformation / crs to be set
# set crs
# buildings <- buildings %>% st_set_crs()
# transform
# buildings <- buildings %>% st_transform()

# crop buildings to raster-extent
dem_ext <- extent(dem)
bb_buildings <- st_bbox(buildings)
buildings <- st_crop(buildings, dem_ext)


# OPTIONAL: CROPPING DATA TO CUSTOM EXTENT (interactive)
# -----------------------------------------------------
if(crop_to_custom){
  
  # PLOT TO GIVE AN IDEA OF A POTENTIAL CUSTOM BOUNDING BOX
  plot(image[[1]])
  plot(buildings, add = TRUE)
  
  ############################
  # --------------------------
  # SET A CUSTOM BOUNDING BOX
  # order: xmin, xmax, ymin, ymax
  custom_bbox <- c(2600000, 2601500, 1199000, 1200500) # serves as example - for original berncity map no cropping
  # --------------------------
  ############################
  
  # create rectancle extent-polygon
  custom_bbox <- list(rbind(
    c(custom_bbox[1], custom_bbox[3]),
    c(custom_bbox[1], custom_bbox[4]),
    c(custom_bbox[2], custom_bbox[4]),
    c(custom_bbox[2], custom_bbox[3]),
    c(custom_bbox[1], custom_bbox[3])
  ))
  
  # create bounding polygon with appropriate crs
  custom_bbox <- st_polygon(custom_bbox) %>% 
    st_sfc() %>% 
    st_set_crs(LV95_proj4)
  custom_bbox_spatial <- custom_bbox %>% 
    as("Spatial")
  
  # CHECK RESULT
  plot(custom_bbox_spatial, add = TRUE, border = "red", lwd = 4)
  
  # crop all data uniformly
  dem <- crop(dem, custom_bbox_spatial)
  image <- crop(image, custom_bbox_spatial)
  plot(image[[1]])
  plot(dem)
  plot(custom_bbox_spatial, add = TRUE, border = "red", lwd = 4)
  rm(custom_bbox_spatial)
  buildings <- st_crop(buildings, custom_bbox %>% st_transform(st_crs(buildings)))
  plot(buildings, add = TRUE)
}

# rasterize buildings
name <- "buildings_rasterized.tif"
height_variable <- ifelse(is.na(height_variable), "GEBHOEHE", height_variable)
if(!file.exists(name)){
  buildings_rasterized <- rasterize(buildings, dem, field = height_variable, fun = max)
  # save files if desired
  if(write_raster){
    writeRaster(buildings_rasterized, name)
  }
}else{
  buildings_rasterized <- raster(name)
}


# make a surface model (but only with rasterizted building heights - no vegetation)
# set negative values or NAs to 0
buildings_rasterized[buildings_rasterized < 0 | is.na(buildings_rasterized)] <- 0

# add heights to DEM
dem <- dem + buildings_rasterized
plot(dem)
rm(buildings, buildings_rasterized, dem_ext, custom_bbox, bb_buildings, height_variable)

# transform image to array (needs sufficient RAM)
# take only RGB channels (avoid IR channel if available)
image <- image[[1:3]]
names(image) <- c("r", "g", "b")

image_array <- as.array(image) / 255
rm(image)
image_array_contrast <- scales::rescale(image_array, to = c(0,1))

# finally: rayshade!
elev <- raster_to_matrix(dem)
plot_3d(image_array_contrast, elev, windowsize = c(1900), zscale = 2.5, shadowdepth = -100,
        zoom=0.5, phi=90,theta=90,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B",
        baseshape = "circle")

# save current view
if(write_plot){
  render_snapshot("testpath2save.png")
}

# close window
rgl::rgl.close()
