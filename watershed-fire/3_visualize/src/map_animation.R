# Build static map for given year
build_map <- function(basemap, fire_pts, year, col_fire){
  # Load basemap
  basemap <- rast(basemap)
  
  # Filter fire points to given year
  fire_pts_year <- fire_pts %>%
    filter(Year == year)
  
  # Fiter fire points to all years prior to given year
  fire_pts_past <- fire_pts %>%
    filter(Year < year)
  
  ggplot()+
    # Plot basemap
    geom_spatraster_rgb(data = basemap) +
    
    # Plot dark glowpoints for previously burned areas
    geom_glowpoint(data = fire_pts_past,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc),
                   alpha = .1,
                   color = "#45220f",
                   shadowcolour = "#45220f",
                   shadowalpha = .05,
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.1, 1)) +
    new_scale("size") +
    
    # Plot glowpoints for current year
    geom_glowpoint(data = fire_pts_year,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc),
                   alpha = .8,
                   color = col_fire,
                   shadowcolour = col_fire,
                   shadowalpha = .1,
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.1, 1.5)) +
    new_scale("size") +
    geom_glowpoint(data = fire_pts_year,
                   #aes(geometry = geometry, size = GIS_ACRES),
                   aes(x = lon, y = lat, size = BurnBndAc), 
                   alpha = .6,
                   shadowalpha = .05,
                   color = "#ffffff",
                   #stat = "sf_coordinates",
                   show.legend = FALSE) +
    scale_size(range = c(.01, .7)) +
    
    # Styling
    theme_void() +
    
    # Print year text
    geom_text(aes(x= -Inf, y = -Inf, hjust = -0.5, vjust = -1.2,
                  label = ifelse(year %% 1 == 0, year, "")),
              size = 10, color = "gray70", fontface = "bold")
}

# Iterate building of maps over a range of years
iterate_map_years <- function(basemap, fire_pts, start_year, end_year, col_fire, col_bg, out_image_dir){
  for(i in seq(from = start_year, to = end_year, by = 0.5)){
    build_map(basemap = basemap, fire_pts = fire_pts, year = i, col_fire = col_fire) %>%
      ggsave(filename = file.path(out_image_dir, paste0("map_", i, ".png")), 
             bg = col_bg, height = 4, width = 6, units = "in", dpi = 300)
  }
  
  return(out_image_dir)
}