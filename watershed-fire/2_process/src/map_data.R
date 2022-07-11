sf2df <- function(sf, years){
  st_centroid(sf) %>%
    st_geometry() %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    setNames(c("lon","lat")) %>%
    bind_cols(st_set_geometry(sf, NULL)) %>%
    filter(Year %in% 2013:2016)
}