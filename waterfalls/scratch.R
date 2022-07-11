#' # Let's go chasing waterfalls!
#' 
#' 
#' 
#' ## Header
#' 
library(ezknitr)
library(tidyverse)
library(sbtools)
library(geojsonR)
library(sp)
library(sf)
library(waterfall)
library(ggthemes)

library(USAboundaries) #install.packages("USAboundariesData", repos = "https://ropensci.r-universe.dev", type = "source")



#' ## Fetch data
#' 
#' CONUS dataset
item_file_download(sb_id = "5e8d2b5982cee42d13466001",
                   names = "conterminousUS_falls_nhd.geojson",
                   destinations = "1_fetch/out/conterminousUS_falls_nhd.geojson",
                   overwrite_file = T)
#' Hawaii dataset
#' 
item_file_download(sb_id = "5c769dc4e4b0fe48cb4b1596",
                   names = "hi_falls.geojson",
                   destinations = "1_fetch/out/hi_falls.geojson",
                   overwrite_file = T)

#' Manually downloaded data
#' 
mn_kml <- sf::st_read("1_fetch/in/MNWaterfall.kml")

#' ## Process data
#' 
#' CONUS dataset - read in .geojson format
#' 
conus_geojson <- geojsonio::geojson_read("1_fetch/out/conterminousUS_falls_nhd.geojson", 
                                         what = "sp")



#' So that we don't lose any data, fill in a missing value for metadata


conus_geojson$characteristics <- ifelse(is.na(conus_geojson$characteristics), 
                                        '[ { \"metadata status\": "missing" } ]', conus_geojson$characteristics)




#' View waterfall locations
#' 
plot(conus_geojson)

#' Convert to df
#' 
conus_df <- as.data.frame(conus_geojson)


#' Remove weird duplicate record for Black Slate Falls 13098 (two diff sources)
#' 
conus_df <- conus_df %>% filter(feature.id != "e40ee991-8a58-11e6-b586-ecf4bb62914c")


#' Convert to sf class
conus_df_sf <- conus_df %>% 
  st_as_sf(coords = c("verifiedx", "verifiedy"), crs = conus_geojson@proj4string)



#' get USA states, filter out Puerto Rico, Alaska, and Hawaii for now
states_df <- USAboundaries::us_states(resolution = "high") %>% 
  filter(!state_abbr %in% c("PR", "AK", "HI"))
plot(states_df$geometry)
plot(conus_df_sf$geometry, add = T)

#' pull in state name to waterfall locations
#test <- sp::over(x = conus_df_sf, y = states_df)
conus_df_sf <- st_transform(x = conus_df_sf, st_crs(states_df))
waterfalls <- st_join(conus_df_sf, states_df["name"], left = T)

waterfalls <- waterfalls %>%
  rename(name = name.x,
         state = name.y) %>%
  select(-characteristics, -source, 
         -nhdplusv2.address, -nhdhr.address, -spatial.certainty)

#' Parse the json string with metadata
characteristics <- purrr::map_dfr(conus_df_sf$characteristics, 
                                  jsonlite::fromJSON)
characteristics <- characteristics %>%
  select(-source)


#' Parse the json string with source information
#' 
source_char <- purrr::map_dfr(conus_df_sf$source[1:11660],
                              jsonlite::fromJSON)
source_num <- purrr::map_dfr(conus_df_sf$source[11661:nrow(conus_df_sf)],
                              jsonlite::fromJSON)
source_num$id <- as.character(source_num$id)
source <- bind_rows(source_char, source_num)

source <- source %>% select(dataset, id)



#' Bind back together
waterfalls <- bind_cols(waterfalls, characteristics, source)


#' Make naming consistent for unnamed waterfalls
#' 
table(waterfalls$name[grep("nname", waterfalls$name)])
waterfalls <- waterfalls %>%
  mutate(name = replace(name, grep("nname",name),"Unnamed"))


#' Check for duplicates
#' 
waterfall_dup_check <- waterfalls %>%
  group_by(fall.type, name, stream.name, state) %>%
  filter(n() > 1)

#' Take out rapids
#' 
table(waterfalls$fall.type)

#' Keep records of historical waterfalls separately
#' 
historical_waterfalls <- waterfalls %>%
  filter(fall.type == "historical waterfall")


#' Remove rapids and historical waterfalls
#' 
waterfalls <- waterfalls %>%
  filter(! fall.type %in% c("historical waterfalls", "segmented rapids", "wide rapids", "rapids",
                            "historical rapids"))
table(waterfalls$fall.type)

#' Standardize drop data
#' 
waterfall_data <- waterfalls %>%
  #filter(is.na(`metadata status` )) %>%
  mutate(drop_ft = #first test, is there data on total height in feet?
           ifelse(!is.na(`total height feet`), `total height feet`,
                  #second, is there data on height under "not specified" (different datasets)
                          no = ifelse(!is.na(`not specified height feet`), `not specified height feet`,
                                      #third, is there data on height by largest drop?
                                      no = ifelse(!is.na(`tallest drop feet`), `tallest drop feet`, 
                                                  NA))), # put in NA if not otherwise specified
        state = replace(state, feature.id == "e40eea77-8a58-11e6-b586-ecf4bb62914c", "Michigan"))

waterfall_data_cleaned <- waterfall_data %>% filter(drop_ft > 0)
summary(waterfall_data$drop_ft)
summary(waterfall_data_cleaned$drop_ft)



state_data <- waterfall_data %>%
  group_by(state) %>%
  summarise(mean = mean(drop_ft, na.rm = T),
            median = median(drop_ft, na.rm = T),
            max = max(drop_ft, na.rm = T),
            sum = sum(drop_ft, na.rm = T),
            count = length(drop_ft))


#ggplot(data = waterfall_data, aes(y = drop_ft, x = ))
plot_data_AZ <- waterfall_data %>% select(state, drop_ft, fall.type) %>% filter(state == "Arizona") %>% filter(!is.na(drop_ft)) %>%
  arrange(drop_ft)
plot_data_WY <- waterfall_data %>% select(state, drop_ft) %>% filter(state == "Wyoming") %>% filter(!is.na(drop_ft)) %>%
  arrange(drop_ft) 
waterfalls::waterfall(plot_data_AZ) 

ggplot(data = state_data, aes(x = state, y = sum))+
  geom_bar(stat = "identity")

ggplot(data = waterfall_data_cleaned, aes(x = state, y = drop_ft))+
  geom_bar(stat = "identity", aes(fill = fall.type))

ggplot(data = state_data, aes(x = state, y = count))+
  geom_bar(stat = "identity")




#' Manually make the plot with just three example states to start
#' 

#waterfall_data_cleaned <- waterfall_data_cleaned %>% filter(drop_ft > 20)

state_data_graphed <- waterfall_data_cleaned %>%
  group_by(state) %>%
  summarise(mean = mean(drop_ft, na.rm = T),
            median = median(drop_ft, na.rm = T),
            max = max(drop_ft, na.rm = T),
            sum = sum(drop_ft, na.rm = T),
            count = length(drop_ft))

waterfall_graph_df <- waterfall_data_cleaned %>% 
  group_by(state) %>%
  arrange((drop_ft), desc(id)) %>%
  mutate(drop_cum = cumsum(drop_ft))


waterfall_graph_df <- waterfall_graph_df  %>%
  group_by(state) %>%
  arrange(desc(drop_ft), id) %>%
  mutate(waterfall_number = row_number())

highlight_states <- c("Oregon", "Montana", "New Mexico", "California", "North Carolina")

# Main plot
ggplot(data = waterfall_graph_df %>% filter(state != "Washington"), 
       aes(x = waterfall_number, y = drop_cum, group = state))+
  geom_step(color = "#BDCCD4")+
  geom_step(data = waterfall_graph_df %>% filter(state %in% highlight_states), color = "#0069B5")+
  theme_tufte()+
  ylab("Waterfall Drop (ft)")+
  xlab("# of Waterfalls")

#ggsave(file = "3_visualize/out/main.png", width = 1600, height = 900, units = "px", dpi = 300)

# Inset map
ggplot(data = waterfall_graph_df %>% filter(state != "Washington"), 
       aes(x = waterfall_number, y = drop_cum, group = state))+
  geom_step(color = "#BDCCD4")+
  geom_step(data = waterfall_graph_df %>% filter(state %in% highlight_states), color = "#0069B5")+
  geom_step(data = waterfall_graph_df %>% filter(state == "Washington"), color = "#E68A1B")+
  theme_void()+
  ylab("Waterfall Drop (ft)")+
  xlab("# of Waterfalls")

