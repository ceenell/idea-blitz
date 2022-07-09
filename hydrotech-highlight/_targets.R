library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  'tidyverse'
))

##### Download data #####
p1_targets <- list(

)

##### Process/prep data #####
p2_targets <- list(

)

##### Visualize data #####
p3_targets <- list(

)

c(p1_targets, p2_targets, p3_targets)
