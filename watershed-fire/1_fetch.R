# Load libraries
library(rgeos)
library(rgdal)
library(tidyverse)
library(sf)
library(USAboundaries)
library(ggshadow)
library(ggnewscale)
library(tidyterra)
library(gganimate)
library(transformr)
library(maptiles)

# Parameters
sf_use_s2(FALSE)
crs <- 9311
year = 2014
conus <- state.abb %>%
  subset(!. %in% c("AK", "HI"))

url <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
perim_zip_path <- "1_fetch/tmp/mtbs.zip"
perim_tmp_path <- "1_fetch/tmp"

# Download fire perimeter
if(!file.exists(perim_zip_path)) download.file(url, perim_zip_path)
if(!file.exists(perim_tmp_path)) unzip(perim_zip_path, exdir = perim_tmp_path)

mtbs_file_path <- unzip(perim_zip_path, list = T) %>%
  select(Name) %>%
  filter(str_sub(Name, -3) == "shp") %>%
  unlist() %>%
  paste(perim_tmp_path, ., sep = "/")

perim <- st_read(mtbs_file_path) %>%
  mutate(Year = str_sub(Ig_Date, start = 1L, end = 4L),
         State = str_sub(Event_ID, start = 1L, end = 2L)) %>%
  filter(Incid_Type %in% c("Wildfire", "Wildland Fire Use"),
         State %in% conus) %>%
  select(Event_ID, Incid_Name, Ig_Date, Year, State, Incid_Type, BurnBndAc, BurnBndLat, BurnBndLon) %>%
  st_transform(crs = crs)
  
# Import huc
huc <- st_read("1_fetch/in/F2F2_HUC12.shp") %>%
  st_transform(crs = crs)

# Download map tiles
tiles <- get_tiles(huc, provider = "CartoDB.DarkMatterNoLabels", crop = T, verbose = T, zoom = 4)
