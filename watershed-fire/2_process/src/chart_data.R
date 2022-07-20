# Get data points for graph for a given year
data_by_year <- function(perim, huc, year){
  # Filter fire perim by year
  perim_year <- perim %>%
    filter(Year == year)
  
  # Filter only hucs that supply surface water
  huc_intersection <- huc %>%
    filter(SUM_POP > 0) %>%
    mutate(area_ws = st_area(.)) %>%
    st_intersection(perim_year) %>%
    mutate(area_int = st_area(.)) %>%
    mutate(prop_burned = area_int/area_ws)
  
  out <- data.frame(Year = year,
                    Mean_ws_prop_affected_pc = mean(huc_intersection$prop_burned) %>%
                      as.numeric()*100,
                    Population_affected_mil = sum(huc_intersection$SUM_POP)/1000000)
  
  return(out)
}

# Get data points for all years specified and prep for graphing
build_chart_data <- function(years, perim, huc){
  lapply(years, FUN = function(X) data_by_year(perim, huc, X)) %>%
    bind_rows() %>%
    pivot_longer(-one_of("Year")) %>%
    mutate(name = recode(name, "Mean_ws_prop_affected_pc" = "Proportion of US water supply watersheds affected by wildfire (%)",
                         "Population_affected_mil" = "Consumers of water from affected watersheds (millions of people)")) %>%
    mutate(name_f = factor(name, levels = c("Proportion of US water supply watersheds affected by wildfire (%)",
                                            "Consumers of water from affected watersheds (millions of people)")))
}