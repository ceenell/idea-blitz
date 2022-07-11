library(targets)

tar_option_set(packages = c(
  'tidyverse',
  'readxl',
  'igraph',
  'ggraph'
))

#source('src/create_taxonomy.R')

# toxin producing genera of algae - either named in the fact sheet or on wikipedia
harmful_algae <- c('Microcystis', 'Raphidiopis', 'Aphanizomenon', 'Chrysosporum', 
                   'Cuspidothrix', 'Dolichospermum', 'Oscillatoria', 'Phormidium',
                   'Planktothrix','Streptomyces',
                   'Anabaena')

list(
  ## read in raw data
  tar_target(
    raw_sp,
    read_excel('in/NGWOS Ecology Data KSummers/Species Identification and Enumeration/rawdata_392_1_algae_01202022.xls',
               na = c("", "."))
  ),
  ## filter to Starved Rock site, minor data cleaning
  tar_target(
    raw_filt,
    raw_sp %>%
      rename(cells = algal_cell_concentration_cells_per_ml_, 
             cells_percent = relative_algal_cell_concentration_percent,
             biovolume = total_biovolume_cubic_um_per_ml_,
             biovolume_percent = relative_total_biovolume_percent) %>%
      mutate(species = gsub('. ', '', species),
             taxa_name = gsub('.NA', '', paste(division, class_, order_, family, genus, sep = '.')),
             genus = ifelse(genus == 'Colonial', NA, genus)) %>%
      select(sample_id, site, sample_date, 
             taxa_name, division, class_, order_, family, genus, species, 
             cells, cells_percent, biovolume, biovolume_percent) %>%
      filter(site == 'USGS 05553700 ILLINOIS RIVER AT STARVED ROCK, IL') 
  ),
  tar_target(
    ## aggregate to genus
    genus_data,
    raw_filt %>%
      group_by(sample_id, site, sample_date, taxa_name, division, class_, order_, family, genus) %>%
      summarize(across(c(contains('cells'), contains('biovolume')), ~sum(.x))) %>%
      mutate(genus = ifelse(is.na(genus), 'unknown', genus))
  ),
  tar_target(
    ## total by genus x sample, retain division
    genus_total,
    genus_data %>%
      group_by(sample_id, sample_date, taxa_name, division, genus)%>%
      summarize(across(c(contains('cells'), contains('biovolume')), ~sum(.x))) 
  ),
  tar_target(
    ## total by division x sample
    division_total,
    genus_data %>%
      group_by(sample_id, sample_date, taxa_name, division)%>%
      summarize(across(c(contains('cells'), contains('biovolume')), ~sum(.x))) 
  ),
  # create complete taxonomy for each terminal taxa in data
  tar_target(
    ## each taxonomic level that appears in data
    taxa_full,
    raw_filt %>%
      distinct(taxa_name, division, class_, order_, family, genus, species) 
    ),
  tar_target(
    ## list of terminal taxa
    terminal_taxa,
    taxa_full %>% pull(taxa_name) %>% unique()
  )
  ## create hierachical data structure to represent taxonomy
  ### requires that community data are quantified at each taxonomic level
  #tar_target(
  #  # network verticies
  #  vert_all,
  #  taxa_full %>%
  #    mutate(taxa_end = stringr::word(taxa_name, -1, sep = '\\.')) %>%
  #    left_join(taxa_total) %>%
  #    select(name = taxa_name, taxa_end, total)
  #),
  #tar_target(
  #  ## network edges
  #  ### for each level to a terminal taxa, find total cells/mL
  #  edge_all,
  #  make_edges(terminal_taxa),
  #  pattern = map(terminal_taxa)
  #),
  #tar_target(
  #  ## create network object
  #  net_all,
  #  graph_from_data_frame(edge_all, #%>% filter(to %in% terminal_taxa) %>% filter(from %in% terminal_taxa), 
  #                        vertices = vert_all)
  #  ## error about mismatch nodes needs to be addressed
  #),
  #tar_target(
  #  bubble_all,
  #  # plot aggregate composition across samples
  #  ggraph(net_all, layout = 'circlepack', weight=total) + 
  #    geom_node_circle(aes(fill = as.factor(depth)), color = NA) +
  #    theme_void() + 
  #    theme(legend.position="FALSE") +
  #    coord_fixed() +
  #    scale_fill_scico_d(palette="bukavu", end = 0.45, direction = -1)+
  #    ggtitle("All samples") +
  #    theme_bubbles()
  #),
  #tar_target(
  #  bubble_all_png,
  #  ggsave(bubble_all, 'out/bubble_all.png', with = 1000, height = 1000)
  #)
)
