make_edges <- function(name){
  depth <- str_count(name, '\\w+')
  edge_df <- NULL
  for (i in depth:2){
    edge <- tibble(to = word(name, 1, i, sep = '\\.'), 
                   from = word(name, 1, i-1, sep = '\\.'))
    edge_df <- bind_rows(edge, edge_df)
  }
  return(edge_df)
}