
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
#' @return data.frame with `site_no` and `best_friend` columns
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
    select(site_no, best_friend = hydrotech)

}

#' @title Return the sites with the most visits
#' @description This function tallies the number of visits per site and
#' returns `top_n` site numbers representing the most visited.
#' @param field_visit_data a data.frame returned by `dataRetrieval::readNWISmeas()`
#' @param top_n a number representing how many sites should be returned,
#' defaults to 5
#'
#' @return vector of site numbers with the most total visits
#'
identify_most_visited_sites <- function(field_visit_data, top_n = 5) {
  field_visit_data %>%
    group_by(site_no) %>%
    tally() %>%
    arrange(desc(n)) %>%
    head(top_n) %>%
    pull(site_no)
}
