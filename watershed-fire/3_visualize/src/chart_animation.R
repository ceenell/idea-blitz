build_graph <- function(chart_data, col_lines, file_out){
  # Build charts
  animation <- ggplot() +
    # Plot line graph
    geom_glowline(data = chart_data, aes(x = Year, y = value, color = name)) +
    # Plot points with alternated data (so entire lines are static)
    geom_glowpoint(data = chart_data %>%
                     rename(Year2 = Year), 
                   aes(x = Year2, y = value, color = name), size = 2) +
    # Styling
    facet_wrap(~ name_f, ncol = 1, scales = "free_y") +
    scale_color_manual(values = col_lines) +
    ylab(NULL) +
    theme(plot.background = element_rect(fill = "#262626", color = NA),
          panel.background = element_rect(fill = "#262626", color = NA),
          strip.background = element_blank(),
          strip.text = element_text(color = "gray70", size = 10, face = "bold"),
          strip.placement = "outside",
          panel.spacing = unit(1/8, "in", data = NULL),
          legend.position = "none",
          axis.title.x = element_text(color = "gray70", size = 10),
          panel.grid = element_line(color = "gray40"),
          axis.text = element_text(color = "gray40")) +
    transition_time(Year2)
  
  animate(animation, fps = 10, nframes = 100, renderer = magick_renderer()) %>%
    image_write_gif(path = file_out)
}