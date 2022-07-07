library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'dataRetrieval',
  'sf',
  'tidyverse'
))

query_startDate <- '2022-01-01'
query_endDate <- '2022-06-30'

##### Download data #####
p1_targets <- list(

  # Identify which sites have had a field visit across the U.S.
  tar_target(p1_field_visit_sites,
             purrr::map(sprintf("%02d", 1:21), function(huc_code) {
               whatNWISsites(huc = huc_code,
                             service='meas',
                             startDate = query_startDate,
                             endDate = query_endDate,
                             siteType = 'ST') %>%
                 pull(site_no) %>% unique()
             }) %>% reduce(c) %>% unique()),

  # Download the actual field measurment data for these sites
  tar_target(p1_field_visit_data, readNWISmeas(
    siteNumbers = p1_field_visit_sites,
    startDate = query_startDate,
    endDate = query_endDate)),

  # Create a spatial object for the site locations
  tar_target(p1_field_visit_sites_sf, readNWISsite(p1_field_visit_sites) %>%
               st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326))

)

##### Process/prep data #####
p2_targets <- list(

)

##### Visualize data #####
p3_targets <- list(

)

c(p1_targets, p2_targets, p3_targets)
