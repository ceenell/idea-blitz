# Script to run the waterfalls viz pipeline
library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c('tidyverse', 'sbtools', 'geojsonR', 'sp','sf', 
                            'waterfall', 'ggthemes', 'readr','rvest', 'xml2'),
               error = NULL)

source("src/functions.R")
source('src/scrape_waterfall_data.R')

## waterfall database site
url <- 'https://www.worldwaterfalldatabase.com/country/United-States/list'

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
  
  # Scraping the waterfall database
  tar_target(
    # get urls for all database pages to scrape
    ## taking the second last list item (li) of the page navigation below the table
    ## where the integer represents total number of pages in the database
    n_pages,
    url %>%
      read_html() %>%
      html_element('ul.pagination') %>%
      html_elements('li') %>%
      tail(2) %>%
      head(1) %>%
      html_text() %>%
      as.integer()
  ),
  ## extract table header and reformat
  tar_target(
    table_header,
    c('Status', 'Waterfall','Access','Country Rating','Height','State')
  ),
  ## create a tibble of every url to scrape
  tar_target(
    page_df,
    tibble(page_num = 1:n_pages,
           page_url = sprintf('https://www.worldwaterfalldatabase.com/country/United-States/list?page=%s', page_num))
  ),
  # for each page of the database, scrape table data
  ## fall_type corresponds to html classes for each row that indicate whether the entry is confirmed, cataloged etc
  ## doing this for confirmed and cataloged entries, separately
  ## create a branch for every "page" in the database
  tar_target(
    ## pull data into a table, takes around 10 min
    confirmed_df,
    extract_tbl_contents(url = page_df$page_url,
                         fall_type = '.Confirmed-row',
                         table_head = table_header),
    pattern = map(page_df)
  ),
  tar_target(
    cataloged_df,
    extract_tbl_contents(url = page_df$page_url,
                         fall_type = '.Cataloged-row',
                         table_head = table_header),
    pattern = map(page_df)
  ),
  # now take the urls for each waterfall entry that lead to more info
  tar_target(
    confirmed_urls,
    confirmed_df %>% ungroup() %>% pull(url) %>% unique()
  ),
  tar_target(
    cataloged_urls,
    cataloged_df %>% ungroup() %>% pull(url) %>% unique()
  ),
  # and use to scrape additional stats from those pages (this takes a while)
  ## create a branch for every waterfall entry
  ## a few sites error out, so using tryCatch to work through
  tar_target(
    ## this step takes a while ~30 min
    cataloged_stats,
    tryCatch(
      add_fall_info(cataloged_urls),
      error = function(e){message(cataloged_urls)}
    ),
    pattern = map(cataloged_urls)
  ),
  tar_target(
    ## this step takes a while ~30 min
    confirmed_stats,
    tryCatch(
      add_fall_info(confirmed_urls),
      error = function(e){message(confirmed_urls)}
    ),
    pattern = map(confirmed_urls)
  ),
  tar_target(
    ## combine data
    wf_df,
    bind_rows(confirmed_df %>% left_join(confirmed_stats),
              cataloged_df %>% left_join(cataloged_stats)) 
  ),
  tar_target(
    # save
    waterfall_df_csv,
    {
      out_file <- 'out/us_waterfalls.csv'
      write_csv(wf_df, out_file)
      return(out_file)
    },
    format = 'file'
  )
)

## Process data


## Visualize data