# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("rgeos", "rgdal", "tidyverse", "sf", "USAboundaries", "ggshadow", "ggnewscale", "tidyterra", "gganimate", "transformr", "maptiles", "magick"), # packages that your targets need to run
  format = "rds" # default storage format
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# Load the R scripts with your custom functions:
source("1_fetch/src/get_data.R")

# Replace the target list below with your own:
# 1_fetch
list(
  tar_target(perim,
             get_fire_perim(url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip",
                            perim_zip_path = "1_fetch/tmp/mtbs.zip", 
                            perim_tmp_path = "1_fetch/tmp", 
                            crs = 9311)),
  
  tar_target(huc,
             get_huc(file_in = "1_fetch/in/F2F2_HUC12.shp",
                     crs = 9311)),
  
  tar_target(tiles,
             get_tiles(huc, 
                       provider = "CartoDB.DarkMatterNoLabels", 
                       crop = T, 
                       verbose = T, 
                       zoom = 4))
)
