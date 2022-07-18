# Load packages required to define the pipeline:
library(targets)
suppressPackageStartupMessages(library(tidyverse))

# Set target options:
tar_option_set(
  packages = c("rgeos", "rgdal", "tidyverse", "sf", "USAboundaries", "ggshadow", "ggnewscale", "terra", "tidyterra", "gganimate", "transformr", "maptiles", "magick"), # packages that your targets need to run
  format = "rds" # default storage format
)
options(tidyverse.quiet = TRUE)

# Load functions:
source("1_fetch/src/get_data.R")
source("2_process/src/chart_data.R")
source("2_process/src/map_data.R")
source("3_visualize/src/chart_animation.R")
source("3_visualize/src/map_animation.R")

# Define parameters
sf::sf_use_s2(FALSE)
crs <- 9311
conus <- state.abb %>%
  subset(!. %in% c("AK", "HI"))

# 1_fetch
p1 <- list(
  # Download fire perimeter data from MTBS
  tar_target(perim,
             get_fire_perim(url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip",
                            perim_zip_path = "1_fetch/tmp/mtbs.zip", 
                            perim_tmp_path = "1_fetch/tmp", 
                            crs = 9311)),
  
  # Load Forests 2 Faucets 2.0 data
  tar_target(huc,
             get_huc(file_in = "1_fetch/in/F2F2_HUC12.shp",
                     crs = 9311)),
  
  # Download basemap tiles
  tar_target(basemap,
             get_basemap(file_in_for_extent = huc, 
                         file_out = "1_fetch/out/basemap.tif"),
             format = "file")
)

# 2_process
p2 <- list(
  
  # Build data for graphs
  tar_target(chart_data,
             build_chart_data(years = 1984:2020, 
                              perim = perim, 
                              huc = huc)),
  
  # Build data for map
  tar_target(map_data,
             sf2df(sf = perim, 
                   years = 1984:2020))
)

# 3_visualize
p3 <- list(
  
  # Build chart animation for graphs 
  tar_target(chart_animation_gif,
             build_graph(chart_data = chart_data, 
                         col_lines = c("#0abdc6", "#ea00d9"), 
                         file_out = "3_visualize/out/chart_animation.gif"),
             format = "file"),
  
  # Build frames for map animation
  tar_target(map_animation_pngs,
             iterate_map_years(basemap = basemap, fire_pts = map_data, 
                               start_year = 1984, end_year = 2020, 
                               col_fire = "#c94b10", col_bg = "#262626", 
                               out_image_dir = "3_visualize/out/map_frames"),
             format = "file")
  
  # Morph images into animation
  
  # Use image_append(c(map_animation_gif, chart_animation_gif)) to stitch gifs together
)

c(p1, p2, p3)