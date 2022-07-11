extract_tbl_contents <- function(url, fall_type = '.Confirmed-row', table_head){
  
  ## locate data table
  fall_rows <- url %>%
    read_html() %>%
    # locate the table
    html_element('div .tableWrapper') %>% 
    # select rows by fall type
    html_elements(fall_type) 
  
  ## table isn't written as a standard html table so need to 
  ## work to pull out the desired content from cells
  fall_data <- fall_rows %>%
    # extract row cells
    html_elements('.tableCell') %>%
    # convert to text
    html_text2()%>%
    # clean text
    str_replace_all("[\r\n]" , "") %>%
    str_squish()
  
  ## also pull the link for the fall-specific page
  fall_a <- fall_rows %>%
    # extract row cells
    html_element('.tableCell.sortColumn') %>%
    html_element('a') %>%
    html_attr('href')
  fall_links <- fall_a[-1]
  
  # populate data frame
  # fall_data returns a list of values
  wf_df <- tibble(
    var = rep(table_head, length(fall_data)/length(table_head)),
    val = fall_data) %>%
    # create a grouping var that represents each row
    mutate(fall_id = floor((row_number()-0.1)/length(table_head))) %>%
    # convert to wide to match site format
    pivot_wider(names_from = 'var', values_from = 'val') %>%
    mutate(Status = fall_type) %>%
    mutate(url = fall_links)

  return(wf_df)
  

}

add_fall_info <- function(url){

   page_tables <- url %>%
    read_html() %>%
    html_table() 
   
   ratings <- page_tables[[1]]
   geo_data <- page_tables[[2]]
   stats <- page_tables[[3]]
   
  ratings %>%
    bind_rows(geo_data) %>%
    rename(name = X1, value = X2) %>%
    bind_rows(stats %>% select(name = X1, value = X3)) %>%
    pivot_wider() %>%
    mutate(url = url)

}