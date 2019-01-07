#' @title  MCSim input data
#' @author Eric Sokol (sokole@gmail.com)
#' @description  input data for MCSims for constant starting points

rm(list=ls())
gc()

options(stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# required packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make sure working dir has script and input data files in same place
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# try to change dir -- assuming you're in the github parent dir
try({setwd('./collaboration_with_Ben')})

# check for input files
if(!grepl('collaboration_with_Ben',getwd())){
  stop('Make sure you have your working directory set properly')
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arguments common to all sims -- two sets of clumped sites
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# -- fixed across simulations -- metacommunity characteristics
# niche positions, niche breadths, and relative abundances for 6 species
my_J.t0_scenario_1 <- data.frame(
  spp1 = c(47, 47, 47,
           47, 47, 47,
           1, 0, 1,
           0, 1, 0),
  spp2 = c(1, 0, 1,
           47, 47, 47,
           47, 47, 47,
           0, 1, 0),
  spp3 = c(5, 5, 5,
           5, 5, 5,
           4, 6, 4,
           6, 4, 6),
  spp4 = c(47, 47, 47,
           0, 1, 0,
           1, 0, 1,
           47, 47, 47),
  spp5 = c(0, 1, 0,
           1, 0, 1,
           47, 47, 47,
           47, 47, 47))
write_csv(my_J.t0_scenario_1, 'INDATA_J_t0_scenario_1.csv')

my_J.t0_scenario_2 <- data.frame(
  spp1 = c(0, 1, 0,
           47, 47, 47,
           47, 47, 47,
           1, 0, 1),
  spp2 = c(0, 1, 0,
           1, 0, 1,
           47, 47, 47,
           47, 47, 47),
  spp3 = c(6, 4, 6,
           5, 5, 5,
           5, 5, 5,
           4, 6, 4),
  spp4 = c(47, 47, 47,
           47, 47, 47,
           0, 1, 0,
           1, 0, 1),
  spp5 = c(47, 47, 47,
           0, 1, 0,
           1, 0, 1,
           47, 47, 47))
write_csv(my_J.t0_scenario_2, 'INDATA_J_t0_scenario_2.csv')


rowSums(my_J.t0_scenario_1)
rowSums(my_J.t0_scenario_2)


my_spp_trait_data <- data.frame(
  spp_id = paste0('spp', 1:5), 
  niche_position = c(.2, -.5, 0, -.15, .5),
  niche_breadth = c(.15, .15, .3, .15, .15))
write_csv(my_spp_trait_data, 'INDATA_spp_traits.csv')

# -- fixed across simulations -- landscape characteristics
# defining site data for sim landscape
site_data <- data.frame(
  x = c(1, 2, 3, 
        1, 2, 3, 
        11, 12, 13,
        11, 12, 13),
  y = c(1, 2, 1, 
        11, 12, 11,
        1, 2, 1,
        11, 12, 11),
  Ef = c(.1, .2, .3,
         -.1, -.2, -.3,
         -.5, .5, -.9,
         .2, .3, .2
         ),
  m = rep(.1, 12),
  JL = 100)
write_csv(site_data, 'INDATA_site_info.csv')

#END