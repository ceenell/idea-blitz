
#' @title Parse the `party_nm` field into individual site visitor initials
#' @description this function separates the `party_nm` field into individual
#' initials and duplicates the field visit record for each initial contained
#' within the `party_nm` string.
#' @details The `party_nm` field appears to be a free text field, so while there
#' is some structure to how people enter their initials, it varies
#' widely. This function should not represent a comprehensive approach
#' to parsing this field into individual hydrotech initials, but does
#' do a good job for the most commond formats.
#' @param field_visit_data a data.frame returned by `dataRetrieval::readNWISmeas()`
#' which has at least a `party_nm` column.
#'

parse_visitor_initials <- function(field_visit_data) {

  possible_separators <- c(
    "/",
    "/[[:space:]]", # TODO: THIS IS NOT WORKING, spaces remain
    ",",
    ",[[:space:]]",  # TODO: THIS IS NOT WORKING, spaces remain
    "[[:space:]]//[[:space:]]",
    "\\+",
    "[[:space:]]\\+[[:space:]]"
  )

  field_visit_data %>%
    mutate(initials_list = str_split(party_nm, paste(possible_separators, collapse="|"))) %>%
    unnest(cols = c(initials_list)) %>%
    # Remove empty strings
    filter(!stringi::stri_isempty(initials_list)) %>%
    # The same hydrotech might write capital letters one day, but lowercase
    # the next. Let's consider them the same person for this workflow.
    mutate(hydrotech = toupper(initials_list)) %>%
    select(-initials_list)

}

#' @title For each site, identify the individual who has the most visits.
#' @description Assuming each site + initials group can represent one unique
#' site visitor, this function tallies the number of visits and returns the
#' initials that appear the most for each site. We are calling the hydrotech
#' behind the initials the site's "best friend".
#' @param field_visit_data_hydrotechs a data.frame returned by `parse_visitor_initials()`
#'
#' @return data.frame with `site_no`, `best_friend`, and `n_bff_visits` columns
#'

identify_site_bff <- function(field_visit_data_hydrotechs) {

  field_visit_data_hydrotechs %>%
    # Tally how many visits each hydrotech has made to per site
    group_by(site_no, hydrotech) %>%
    tally() %>%
    ungroup() %>%
    # Now keep only the hydrotech who made the most visits for each site
    group_by(site_no) %>%
    slice(which.max(n)) %>%
    ungroup() %>%
    select(site_no, best_friend = hydrotech, n_bff_visits = n)

}

#' @title Return the sites with the most visits
#' @description This function tallies the number of visits per site and
#' returns `top_n` site numbers representing the most visited.
#' @param field_visit_data a data.frame returned by `dataRetrieval::readNWISmeas()`
#' @param top_n a number representing how many sites should be returned,
#' defaults to 5
#'
#' @return data.frame with `site_no` and `n_total_visits` columns
#'

identify_most_visited_sites <- function(field_visit_data, top_n = 5) {

  field_visit_data %>%
    group_by(site_no) %>%
    tally() %>%
    arrange(desc(n)) %>%
    head(top_n) %>%
    select(site_no, n_total_visits = n)
}

#' @title Download population data from the U.S. Census Bureau
#' @description This function uses the `tidycensus` package to download
#' resident population data using the 'place' geographies from the Census
#' Bureau. Note that the API call used here was not returning data for
#' Puerto Rico, so we excluded it in the subsequent functions that use the
#' population data.
#'
#' @details To use this, you will need to have a local API key setup. Follow the
#' instructions at https://walker-data.com/tidycensus/articles/basic-usage.html
#' for requesting a key. You will receive the email fast, but note that you may
#' need to wait a few minutes before successfully clicking on the activate key
#' URL. Once the key is activated, run `census_api_key("YOUR KEY", install = TRUE)`
#' and then you should be able to use this function.
#'
#' @param state_cd vector of character abbreviations for states to pull place data
#'
#' @return an `sf` object with polygons for the places, plus `GEOID` (US Census
#' Bureau defined place code), `NAME` (plain text place name), `variable` (will
#' only say "POP" for population), and `value` (the resident population count) columns.
#'

pull_population_data <- function(state_cd) {
  get_estimates(
    'place',
    year = 2019, # Can't get anything newer than 2019 to work
    state = state_cd,
    variable = "POP",
    geometry = TRUE)
}

#' @title Clean the population data to use in later steps
#' @description This function applies a filter to keep only places of a certain
#' resident size and converts the polygons into centroids.
#'
#' @param all_population_data the `sf` object returned from `pull_population_data()`
#' @param min_value single numeric value indicating the minimum population a
#' place should have to remain in the dataset. Note that the default is 50k
#' residents, which means that some states (such as Vermont, where the most
#' populated city is Burlington with just under 43k) will not have any places
#' included in the final dataset.
#'
#' @return an `sf` object with points for the places, plus `GEOID` (US Census
#' Bureau defined place code), `NAME` (plain text place name), and `population`
#' (the resident population count) columns.
#'

prepare_population_data <- function(all_population_data, min_value = 50*10^3) {
  all_population_data %>%
    select(-variable, population = value) %>%
    filter(population > min_value) %>%
    st_centroid() %>%
    st_transform(crs = 4326)
}

#' @title For each site, identify the closest populated place
#' @description This function identifies the nearest place for each gage and
#' how far that place is in kilometers and miles. It also filters out sites
#' in Puerto Rico due to the limitation noted in `pull_population_data()` with
#' pulling place population data for Puerto Rico.
#'
#' @param sites_sf an `sf` object created using NWIS site metadata
#' @param places_sf the `sf` object returned from `prepare_population_data()`
#'
#' @return an `sf` object with points for gages, all site metadata columns
#' returned by `dataRetrieval::readNWISsite()`, plus `nearest_place` (US Census
#' Bureau plain text name for a location), `nearest_place_distance_km` (distance
#' in kilometers from the gage to the `nearest_place`), and `nearest_place_distance_mi`
#' (distance in miles from the gage to the `nearest_place`).
#'

identify_nearest_place <- function(sites_sf, places_sf) {
  distances_mat <- st_distance(sites_sf, places_sf)
  min_distance_per_site <- apply(distances_mat, 1, min)
  min_distance_per_site_i <- apply(distances_mat, 1, which.min)
  nearest_info <- sites_sf %>%
    mutate(nearest_place = places_sf$NAME[min_distance_per_site_i],
           nearest_place_distance_km = min_distance_per_site / 1000,
           nearest_place_distance_mi = nearest_place_distance_km / 1.609) %>%
    # REMOVE Puerto Rico for now since we can't get population data from tidycensus right now.
    filter(state_cd != '72')
  return(nearest_info)
}

#' @title For each site, tally and then average annual visits
#' @description This function calculates an average annual number
#' of visits for each site. Note that visits where `measurement_dt`
#' is `NA` are dropped entirely here. For 2012-06-30 to 2022-06-30,
#' 1% of the field visits (~ 8,500 visits) had `NA` in the
#' `measurement_dt` field and were dropped.
#'
#' @param field_visit_data the data.frame object returned by `dataRetrieval::readNWISmeas()`
#'
#' @return a data.frame with two columns, `site_no` and `avg_annual_visits`
#'

calculate_avg_annual_visits <- function(field_visit_data) {
  field_visit_data %>%
    mutate(year = lubridate::year(measurement_dt)) %>%
    group_by(site_no, year) %>%
    tally() %>%
    group_by(site_no) %>%
    summarize(avg_annual_visits = mean(n, na.rm=TRUE))
}

#' @title Categorize each field visit record into one of the 24 hours in a day
#' @description This function takes the `measurement_tm` column from the field
#' visit measurement records and categorizes into a single hour of the day between
#' 1 and 24 by rounding up (e.g "09:12" is the 10th hour of the day, while "09:00"
#' is the last value of the 9th hour of the day).
#'
#' @param field_visit_data the data.frame object returned by `dataRetrieval::readNWISmeas()`
#'
#' @return a data.frame with the same columns as `field_visit_data` plus `time_of_day`,
#' representing an integer hour of the day between 1 and 24
#'

categorize_visit_into_time_of_day <- function(field_visit_data) {
  field_visit_data %>%
    separate(measurement_tm, c('hours', 'minutes'), sep = ':') %>%
    mutate(time_of_day = ceiling(as.numeric(hours) + as.numeric(minutes)/60)) %>%
    dplyr::select(-hours, -minutes) %>%
    filter(!is.na(time_of_day) & time_of_day != 0)
}
