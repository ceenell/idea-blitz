library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'dataRetrieval',
  'ggbeeswarm',
  'gridExtra',
  'scico',
  'sf',
  'tidycensus',
  'tidyverse'
))

source('fxns.R')

query_startDate <- '2012-06-30'
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
               st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4326)),

  # Get population data from the U.S. Census Bureau API
  tar_target(p1_conus_states, c(state.abb, 'PR')),
  tar_target(p1_place_population_sf, pull_population_data(p1_conus_states))

)

##### Process/prep data #####
p2_targets <- list(

  # Summarize information about the site visitors using the `party_nm` field
  tar_target(p2_field_visit_data_hydrotechs, parse_visitor_initials(p1_field_visit_data)),
  tar_target(p2_field_visit_site_bff, identify_site_bff(p2_field_visit_data_hydrotechs)),

  # Identify the top 5 most visited sites in the time period
  tar_target(p2_field_visit_sites_top5, identify_most_visited_sites(p1_field_visit_data)),

  # Determine the closest populated place to each site
  tar_target(p2_place_population_sf, prepare_population_data(p1_place_population_sf)),
  tar_target(p2_field_visit_sites_nearest_city, identify_nearest_place(p1_field_visit_sites_sf, p2_place_population_sf)),

  # Calculate average annual visits per site
  tar_target(p2_field_visits_yearly, calculate_avg_annual_visits(p1_field_visit_data)),

  # Add time of day category for each visit with an associated time
  tar_target(p2_field_visits_timeofday, categorize_visit_into_time_of_day(p1_field_visit_data))

)

##### Visualize data #####
p3_targets <- list(

  # Keeping visualization pieces in an RMD for now while I explore
  tar_render(p3_visual_exploration_doc,
             '3_visualize.Rmd',
             params = list(
               # Targets needed by the visualize step
               p1_field_visit_sites_sf,
               p2_field_visit_sites_top5,
               p2_field_visit_site_bff,
               p2_field_visit_sites_nearest_city,
               p2_field_visits_yearly,
               p2_field_visits_timeofday),
             packages = c('knitr', 'tidyverse'))

)

c(p1_targets, p2_targets, p3_targets)
