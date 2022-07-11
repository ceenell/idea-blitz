
# Static map
build_map <- function(basemap, fire_pts, year, col_fire){
  fire_pts_year <- fire_pts %>%
    filter(Year == year)
  
  fire_pts_past <- fire_pts %>%
    filter(Year < 2014)
  
  ggplot()+
    geom_spatraster_rgb(data = basemap) +
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
    theme_void() +
    geom_text(aes(x= -Inf, y = -Inf, hjust = -0.5, vjust = -1.2,label = year),
              size = 10, color = "gray70", fontface = "bold")
}
