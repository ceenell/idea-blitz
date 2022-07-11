# Script to run the waterfalls viz pipeline
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'sbtools', 'geojsonR', 'sp','sf', 'waterfall', 'ggthemes', 'readr'))

source("src/functions.R")

## Global options

## Fetch data
list(
  # Read in data from Science Base
  tar_target(
    conus_sb_geojson,
    read_data_sb(sb_id = "5e8d2b5982cee42d13466001",
                 names = "conterminousUS_falls_nhd.geojson",
                 destinations = "out/conterminousUS_falls_nhd.geojson"),
    format = "file"
  ),
  # Read in web-scraped data (optional, if not using Cee's code)
  tar_target(
    wwdb_scraped_df,
    read_data_wwdb(file = "in/us_waterfalls.csv")
  )
)

## Process data


## Visualize data