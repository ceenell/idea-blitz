
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

pull_population_data <- function(state_cd) {
  # https://walker-data.com/tidycensus/articles/basic-usage.html

  # Signed up for a key (takes < 1 min to get one), then I had to
  # wait 2 minutes after receiving the email before I could click
  # the activate URL and successfully activate my key. Then, I ran
  # `census_api_key("YOUR KEY", install = TRUE)` to store my key.

  get_estimates(
    # Can't get anything newer than 2019 to work
    'place',
    state = state_cd,
    variable = "POP",
    geometry = TRUE)
}
# Chose 50k as minimum because that means that there is at least one city per state
# because Burlington, VT is the lowest "most populated" state city.
prepare_population_data <- function(all_population_data, min_value = 50*10^3) {
  all_population_data %>%
    select(-variable, population = value) %>%
    filter(population > min_value) %>%
    st_centroid() %>%
    st_transform(crs = 4326)
}

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
# field_visit_data <- tar_read(p1_field_visit_data)
calculate_avg_annual_visits <- function(field_visit_data) {
  field_visit_data %>%
    # We lose a bunch of sites here because they have an NA
    # for the measurement_dt
    mutate(year = lubridate::year(measurement_dt)) %>%
    group_by(site_no, year) %>%
    tally() %>%
    group_by(site_no) %>%
    summarize(avg_annual_visits = mean(n, na.rm=TRUE))
}

categorize_visit_into_time_of_day <- function(field_visit_data) {
  field_visit_data %>%
    separate(measurement_tm, c('hours', 'seconds'), sep = ':') %>%
    mutate(time_of_day = ceiling(as.numeric(hours) + as.numeric(seconds)/60)) %>%
    dplyr::select(-hours, -seconds) %>%
    # Making assumption that "work hours" are between 7 AM and 6 PM local time
    mutate(is_afterwork = time_of_day > 18 | time_of_day < 7) %>%
    filter(!is.na(time_of_day) & time_of_day != 0)
}
