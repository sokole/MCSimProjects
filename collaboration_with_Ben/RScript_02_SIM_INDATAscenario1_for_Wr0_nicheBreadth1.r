#' @title  simple MCSim scenarios for Ben
#' @author Eric Sokol (sokole@gmail.com)
#' @description  Distinct scenarios -- 
#'   --> 1. species sorting with no dispersal (everything everywhere but environment selects)
#'   2. species sorting with strong dispersal limitation
#'   3. neutral no dispersal limitation
#'   4. neutral with dispersal limitation

rm(list=ls())
gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# required packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# use devtools package to install MCSim from github
# devtools::install_github('sokole/MCSim@dev')
library(MCSim)

library(tidyverse)
library(vegan)
library(labdsv)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make sure working dir has script and input data files in same place
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# try to change dir -- assuming you're in the github parent dir
try({setwd('./collaboration_with_Ben')})

# check for input files
if(!any(grepl('INDATA', list.files('.')))){
  stop('Make sure you have your working directory set properly and the input data are available in your working direcotry')
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# user provided vars
# ~~~~~~~~~~~~~~~~~~~~~~
n_sim_reps <- 5 #number of replicate sims
my_n.timestep <- 200 #number of time steps

my_W.r <- 0 #dispersal kernel slope, 0 = no limitation
my_niche_breadth_multiplier <- 1 #increases neutrality by increasing niche bredth, numbers > 2 should be approx neutral



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arguments common to all sims -- two sets of clumped sites
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# naming scheme for scneario used to label output files
my_scenario_name <- paste0('Wr',my_W.r,'_nicheBreadth',my_niche_breadth_multiplier,'_timesteps',my_n.timestep)

# default is no invasion
# my_nu <- 0.001 #metacommunity invasion probability
# my_speciation.limit <- 5 #set limit to number of potential invaders
my_nu <- 0 #no invasion from outside metacommunity
my_speciation.limit <- 0 #closed system with no invaders



# -- fixed across simulations -- metacommunity characteristics
# niche positions, niche breadths, and relative abundances for 6 species

# read in a site by species table of species counts for t0
my_J.t0 <- read_csv('INDATA_J_t0_scenario_1.csv')

# read in a table of species trait characteristics
my_spp_trait_data <- read_csv('INDATA_spp_traits.csv') 

# assign trait values for sim input, modify niche breadths based on multiplier
my_niche_positions <-  my_spp_trait_data$niche_position
my_niche_breadths <- my_spp_trait_data$niche_breadth * my_niche_breadth_multiplier

# -- fixed across simulations -- landscape characteristics
# read in table of site data
site_data <- read_csv('INDATA_site_info.csv')


#############
# send function landscape list and interval duration list, along with other metaSim parameters that will be fixed across all simulations

my_landscape <- MCSim::make.landscape(site_data)

############
############
############
############
# looping for sim reps

for(i_sim_rep in 1:n_sim_reps){
  
  # -- call the wrapper function
  my_sim_result <- MCSim::metasim(
    scenario.ID = my_scenario_name,
    landscape = my_landscape,
    J.t0 = my_J.t0,
    trait.Ef = my_niche_positions,
    trait.Ef.sd = my_niche_breadths,
    W.r = my_W.r,
    nu = my_nu,
    speciation.limit = my_speciation.limit,
    n.timestep = my_n.timestep,
    save.sim = TRUE
  )
  
  # convert long format species count data to wide
  data_wide <- my_sim_result$J.long %>% group_by(timestep, site) %>% spread(spp, count)
  print(data_wide)
  
  # hellinger transform abundance data
  data_wide_hell <- data_wide[, my_spp_trait_data$spp_id] %>% 
    vegan::decostand('hellinger') 
  
  # reduce dimensionality using pco
  mod.pco <- data_wide_hell %>%
    dist() %>%
    labdsv::pco()
  
  # return the pco scores for each site x timestep
  data_wide$pco1 <- mod.pco$points[,1]
  data_wide$pco2 <- mod.pco$points[,2]
  
  ###################################################################################
  # If you want to make some plots to visualize sim output, here is some code...
  ################################################
  
  # # data for plotting results
  # data_wide_plot <- data.frame(timestep = data_wide$timestep, site = data_wide$site, data_wide_hell)
  # data_wide_plot$pco1 <- mod.pco$points[,1]
  # data_wide_plot$pco2 <- mod.pco$points[,2]

  # # plot dispersal kernel
  # file_name <- paste0('SIM_OUTPUT/Fig_disp_kern_',my_scenario_name,'.pdf')
  # pdf(file_name)
  # plot_standardized_disp_kernel(my_W.r, landscape = my_landscape)
  # dev.off()
  
  # # plot sites
  # file_name <- paste0('SIM_OUTPUT/Fig_landscape_',my_scenario_name,'.pdf')
  # pdf(file_name)
  # site_data %>%
  #   ggplot(aes(x, y,
  #              size = JL,
  #              color = Ef)) +
  #   geom_point() +
  #   scale_color_gradient2(
  #     low = "red",
  #     mid = 'gray',
  #     high = "darkblue",
  #     midpoint = 0)
  # dev.off()
  
  # # plot results
  # file_name <- paste0('SIM_OUTPUT/Fig_comm_TS_',my_sim_result$sim.result.name,'.pdf')
  # pdf(file_name)
  # data_long_plot <- data_wide_plot %>%
  #   tidyr::gather(., 'spp', 'count_hell', -timestep, -site) %>%
  #   mutate(
  #     var_type = case_when(
  #       grepl('pco',spp) ~ 'pco_score',
  #       TRUE ~ 'spp_count'
  #     ),
  #     line_size = case_when(
  #       grepl('pco',spp) ~ 2,
  #       TRUE ~ 1
  #     )
  #   )
  # 
  # data_long_plot %>% ggplot(aes(timestep, count_hell,
  #                               color = spp,
  #                               linetype = var_type,
  #                               size = line_size)) +
  #   geom_line() +
  #   facet_wrap(~ as.factor(site))
  # dev.off()
  
  ###########################
  # END plotting code
  ##########################################################
  
  #write out sim result as csv
  file_name <- paste0('SIM_OUTPUT/RESULT_JT_',my_sim_result$sim.result.name,'.csv')
  write_csv(
    data_wide,
    path = file_name)
  
} # END loop for sim reps
############
############
############
############

# END