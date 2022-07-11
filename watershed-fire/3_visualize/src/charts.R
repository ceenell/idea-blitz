# Build graphs
build_graph <- function(chart_data, year, color){
  # Data for vlines
  vline_data <- data.frame(x = rep(year, 4),
                           y = rep(c(-Inf, Inf), times = 2),
                           name = rep(unique(chart_data$name), each = 2))
  # Build charts
  ggplot() +
    # Plot line graph
    geom_glowline(data = chart_data, aes(x = Year, y = value, color = name)) +
    #geom_glowpoint(data = filter(chart_data, Year == 1999), aes(x = Year, y = value)) +
    #geom_vline(xintercept = year) +
    # Plot vlines
    geom_glowline(data = vline_data, aes(x = x, y = y), color = "#c94b10") +
    # Styling
    facet_wrap(~ name_f, ncol = 1, scales = "free_y") +
    scale_color_manual(values = color) +
    ylab(NULL) +
    theme(plot.background = element_rect(fill = "#262626"),
          panel.background = element_rect(fill = "#262626"),
          strip.background = element_blank(),
          strip.text = element_text(color = "gray70", size = 10, face = "bold"),
          strip.placement = "outside",
          panel.spacing = unit(1/4, "in", data = NULL),
          legend.position = "none",
          axis.title.x = element_text(color = "gray70", size = 10),
          panel.grid = element_line(color = "gray40"),
          axis.text = element_text(color = "gray40"))
}