# Functions to create the waterfalls viz

# Description Read in data from science base
read_data_sb <- function(sb_id, names, destinations){
  item_file_download(sb_id = sb_id,
                     names = names,
                     destinations = destinations,
                     overwrite_file = T)
  
  return(destinations)
}

# Description Read in data manually from in/ directory
read_data_wwdb <- function(file){
  wwdb_df <- read_csv(file = file, show_col_types = F)
}