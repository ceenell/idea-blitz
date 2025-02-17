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
  tar_target(
    toxins,
    read_excel('in/NGWOS Ecology Data KSummers/CyanoToxinResults.2021.xlsx') %>%
      filter(`Site Name` == "Illinois River at Starved Rock, IL") %>%
      # create variable to match with toxin data
      # some sample times are encoded in sample names to match
      mutate(sample_id = case_when(
        `Sample ID` == 'ILSR-Scum'  ~ 'ILSR 1530',
        `Sample ID` == 'ILBR' & `Date Collected` == '6/16' ~ 'ILBR 1115',
        `Sample ID` == 'ILSR' & `Date Collected` == '6/16' ~ 'ILSR 1200',
        `Sample ID` == 'ILSRM' & `Date Collected` == '6/16' ~ 'ILSRM 1630',
        `Sample ID` == '5553700' & `Date Collected` == '06/23' ~ 'Starved Rock 1600',
        `Sample Date` == '*7/12/2021' ~ 'Starved Rock 1055',
        `Sample ID` == 'ILBR' & `Date Collected` == '44439' ~ 'ILSRBR', ## not sure about this one, could be flipped with the next
        `Sample ID` == 'ILSR' & `Date Collected` == '44439' ~ 'IL Grab @ sensors',
        `Sample ID` == 'ILSRM' & `Date Collected` == '44440' & Time == 1000 ~ 'ILSR grab at sensors', # uncertain
        `Sample ID` == 'ILSRM' & `Date Collected` == '44440' & Time ==  1030 ~ 'ILSRM'
        
      ))%>%
      select(sample_id, MIB = `MIB\r\n(ng/L)`, geosmin = `Geosmin (ng/L)`, contains('Total'))
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
    genus_sample_total,
    genus_data %>%
      group_by(sample_id, sample_date, taxa_name, division, genus)%>%
      summarize(across(c(contains('cells'), contains('biovolume')), ~sum(.x))) %>%
      left_join(toxins)
  ),
  tar_target(
    ## total by genus x sample, retain division
    genus_total,
    genus_data %>%
      group_by(taxa_name, division, genus)%>%
      summarize(across(c(contains('cells'), contains('biovolume')), ~sum(.x))) %>%
      mutate(cells_percent = 100*(cells/4505364),
             biovolume_percent = 100*(biovolume/92723920))
  ),
  tar_target(
    genus_total_csv,
    {
      out_file <- 'out/genus_total.csv'
      write_csv(genus_total, out_file)
      return(out_file)
    },
    format = 'file'
  ),
  tar_target(
    genus_sample_total_csv,
    {
      out_file <- 'out/genus_sample_total.csv'
      write_csv(genus_sample_total, out_file)
      return(out_file)
    },
    format = 'file'
  ),
  tar_target(
    ## total by division x sample
    division_total,
    genus_data %>%
      ungroup() %>%
      group_by(division)%>%
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
