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


# Description Clean up the SB data
clean_sb_data <- function(usgs_sb_spdf){
  usgs_sb_df <- as.data.frame(usgs_sb_spdf)
  
  # Remove weird duplicate record for Black Slate Falls 13098 (two diff sources)
  usgs_sb_df <- usgs_sb_df %>% filter(feature.id != "e40ee991-8a58-11e6-b586-ecf4bb62914c")
  
  # Change the characteristics string in json format so that we don't lose missing data rows
  usgs_sb_df$characteristics <- ifelse(is.na(usgs_sb_df$characteristics), 
                                        '[ { \"metadata status\": "missing" } ]', usgs_sb_df$characteristics)
  
  return(usgs_sb_df)
}

# Description Spatially join state name with waterfall data from SB 
spatially_join_waterfalls <- function(usgs_sb_sf, usa_states_sp){
  # Transform with same projection as states shapefile
  usgs_sb_sf <- st_transform(x = usgs_sb_sf, st_crs(usa_states_sp))
  
  # Spatially join in state name to the waterfalls data
  usgs_sb_join_sf <- st_join(usgs_sb_sf, usa_states_sp["name"], left = T)
  
  # Clean up data
  usgs_sb_join_sf <- usgs_sb_join_sf %>%
    rename(name = name.x,
           state = name.y,
           coords.x = coords.x1,
           coords.y = coords.x2) %>%
    select(-characteristics, -source, 
           -nhdplusv2.address, -nhdhr.address, -spatial.certainty)
  
  return(usgs_sb_join_sf)
}

# Description Extract metadata for SB data and clean up for graphing
metadata_json_export <- function(usgs_sb_join_sf, usgs_sb_df){
  # Characteristics
  characteristics <- map_dfr(usgs_sb_df$characteristics, fromJSON)
  characteristics <- characteristics %>%
    select(-source)
  
  # Source data - challenging because some sources saved them as chars, some as integers
  source_char <- map_dfr(usgs_sb_df$source[1:11660], fromJSON)
  source_num <- map_dfr(usgs_sb_df$source[11661:nrow(usgs_sb_df)],fromJSON)
  source_num$id <- as.character(source_num$id)
  source <- bind_rows(source_char, source_num)
  source <- source %>% select(dataset, id)
  
  # Merge with waterfalls data
  usgs_sb_join_sf <- bind_cols(usgs_sb_join_sf, characteristics, source)
  
  # Make naming consistent for unnamed waterfalls
  table(usgs_sb_join_sf$name[grep("nname", usgs_sb_join_sf$name)])
  usgs_metadata_sf <- usgs_sb_join_sf %>%
    mutate(name = replace(name, grep("nname",name),"Unnamed Waterfall"))
  
  # Remove rapids and historical waterfalls
  usgs_metadata_sf <- usgs_metadata_sf %>%
    filter(! fall.type %in% c("historical waterfalls", "segmented rapids", "wide rapids", "rapids",
                              "historical rapids"))

  # Standardize drop data
  usgs_metadata_sf <- usgs_metadata_sf %>%
    #filter(is.na(`metadata status` )) %>%
    mutate(drop_ft = #first test, is there data on total height in feet?
             ifelse(!is.na(`total height feet`), `total height feet`,
                    #second, is there data on height under "not specified" (different datasets)
                    no = ifelse(!is.na(`not specified height feet`), `not specified height feet`,
                                #third, is there data on height by largest drop?
                                no = ifelse(!is.na(`tallest drop feet`), `tallest drop feet`, 
                                            NA))), # put in NA if not otherwise specified
           state = replace(state, feature.id == "e40eea77-8a58-11e6-b586-ecf4bb62914c", "Michigan"))
  
  # Remove all the World Waterfall Database (WWDB) data to not duplicate scraped data
  usgs_metadata_sf <- usgs_metadata_sf %>%
    filter(!is.na(name)) %>%
    filter(dataset != "World Waterfall Database") %>%
    filter(!is.na(state))
  
  return(usgs_metadata_sf)
}

# Description Clean and ready the web-scraped data
metadata_scraped_export <- function(wwdb_scraped_df){
  wwdb_metadata_df <- wwdb_scraped_df %>%
    select(Waterfall, State, Height, Location, 'Total Height', 'Tallest Drop', 'Num of Drops', 'Stream')
  
  # Remove Alaska and Hawaii
  #wwdb_metadata_df <- wwdb_metadata_df %>% filter(! State %in% c("Hawaii", "Alaska"))
  
  # Rename to match SB data release
  wwdb_metadata_df <- wwdb_metadata_df %>%
    rename(name = Waterfall,
           state = State,
           stream.name = Stream)
  
  # Extract height 
  wwdb_metadata_df <- wwdb_metadata_df %>%
    mutate(drop_m_char = (gsub("([0-9]+).*$", "\\1", `Total Height`)))
  
  # And if it's not in height, maybe at least in tallest drop
  wwdb_metadata_df <- wwdb_metadata_df %>%
    mutate(drop_m_char = coalesce(drop_m_char, (gsub("([0-9]+).*$", "\\1", `Tallest Drop`))))
  
  # And if it's not in height, maybe at least in tallest drop
  wwdb_metadata_df <- wwdb_metadata_df %>%
    mutate(drop_m_char = coalesce(drop_m_char, str_match(Height, "/\\s*(.*?)\\s*meters")[,2]))
    
  # And convert to ft
  wwdb_metadata_df <- wwdb_metadata_df %>%
    mutate(drop_ft = as.numeric(drop_m_char) * 3.28084)
         
}


# Remove missing values for graphing and create cumulative drop height for graphing
prep_for_graphing <- function(waterfalls_df){
  # Remove the missing values for drop heights
  waterfalls_graphing_df <- waterfalls_df %>%
    filter(! is.na(drop_ft))
  waterfalls_graphing_df <- waterfalls_graphing_df %>%
    filter(drop_ft > 0)
  
  # Add in cumulative heights for graphing (ordered by drop height)
  waterfalls_graphing_df <- waterfalls_graphing_df %>%
    group_by(state) %>%
    arrange((drop_ft), desc(id)) %>%
    mutate(drop_cum = cumsum(drop_ft))
  
  # Add in order that they should be graphed by
  waterfalls_graphing_df <- waterfalls_graphing_df %>%
    group_by(state) %>%
    arrange(desc(drop_cum)) %>%
    mutate(waterfall_number = row_number())
}

# Making main waterfall graph
plot_waterfalls <- function(waterfalls_graphing_df, out_file){
  ggplot(data = waterfalls_graphing_df, 
         aes(x = waterfall_number, y = drop_cum, group = state))+
    geom_step(color = "#BDCCD4")+
    geom_step(data = waterfalls_graphing_df %>% filter(state %in% c("Washington")), color = "#0069B5")+
    theme_tufte()+
    ylab("Waterfall Drop (ft)")+
    xlab("# of Waterfalls")
  
  ggsave(filename = out_file,
         width = 1600, 
         height = 1600, 
         dpi = 300, units = "px")
}