rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

set.seed(1234)

# use devtools package to install MCSim from github
# devtools::install_github('sokole/MCSim@dev')
library(MCSim)

library(tidyverse)

# read in point coordinates
my_xy_coordinates <- readr::read_csv('analysis_files/map_xy_cluster_with_outliers.csv')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# user provided vars
# ~~~~~~~~~~~~~~~~~~~~~~

# landscape params
my_subsurface_m <- .05
my_surface_m <- .10

# for trait trade-offs
my_dispersal_bias <- 0.75 #calculated as m * (1 + dispersal_bias OR 1 - dispersal_bias) depending if good disperser or bad disperser

my_niche_breadth <- 0.3 #filtering strong enough for tradeoffs -- subsurface has filter selecting for "resistant" taxa, surface selects for both equally
# my_niche_breadth <- 2 #neutral eventually
# my_niche_breadth <- 20 #neutral


# disturbance characteristics
my_surface_mortality_percent <- .50
my_subsurface_mortality_percent <- 0

my_subsurface_m_immigration_disturbance <- my_subsurface_m #does not change
my_surface_m_immigration_disturbance <- .9

disturbance_extent <- .25
disturbance_duration <- 3

# recovery_duration <- 6
recovery_duration <- 50

#
total_timesteps <- 200
n_cycles <- round((total_timesteps-recovery_duration)/(recovery_duration + disturbance_duration), 0)

scenario_description <- paste0(
  '_m-sub-', my_subsurface_m,
  '_m-surf-', my_surface_m,
  '_disp-bias-', my_dispersal_bias,
  '_niche-width-', my_niche_breadth,
  '_distmor-',my_surface_mortality_percent, 
  '_distext-',disturbance_extent, 
  '_distdur-', disturbance_duration,
  '_recovdur-', recovery_duration,
  '_ncycles-', n_cycles,
  '__',Sys.time()%>%format('%Y-%m-%d_%H%M%S'))

scenario_short_name <- paste0(
  '_sigma-', my_niche_breadth,
  '_distext-',disturbance_extent, 
  '_distmor-',my_surface_mortality_percent, 
  '_recovdur-', recovery_duration)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arguments common to all sims -- two sets of clumped sites
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

my_W.r <- 300 #dispersal kernel slope, 0 = no limitation
# my_niche_breadth_multiplier <- 1 #increases neutrality by increasing niche bredth, numbers > 2 should be approx neutral

# default is no invasion
# my_nu <- 0.001 #metacommunity invasion probability
# my_speciation.limit <- 5 #set limit to number of potential invaders
my_nu <- 0 #no invasion from outside metacommunity
my_speciation.limit <- 0 #closed system with no invaders

my_subsurface_JL <- 100
my_surface_JL <- 100

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in data to parameterize simulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d_comm_raw <- read_csv('analysis_files/d_comm_raw.csv')
d_site_niche <- read_csv('analysis_files/d_site_niche.csv')
d_niche <- read_csv('analysis_files/d_niche.csv')


d_traits <- read_csv('SEMS_traitmatrix_20181203.csv') %>% 
  dplyr::select(taxa, Trait_code)

d_traits <- d_traits %>% filter(Trait_code != 3)

taxa_list <- intersect(d_traits$taxa, names(d_comm_raw))
d_comm_raw <- d_comm_raw[,taxa_list]

d_traits <- d_traits %>% left_join(d_niche)
   

###############################################################
# For the taxa x trait matrix, there is a trait code category which classifies taxa as either resilient (trait_code=1), resistant (trait_code=2), or both (trait_code=3) based on whether the taxa has traits that confer these strategies. The first 5 traits (Long adult lifespan:Strong Occurence in Drift) confer resilience to drying whereas the last 4 traits (Dessication resistance forms:Low rheophily) confer resistance to drying.

# trait_codes and parameters
# 1 -- resilient -- high dispersal
# 2 -- resistant -- can use refugia
# 3 -- both (increased immigration and can use refugia)

# site niche_positions -- surface = 0, benthic = -.5
# site immigration pressures -- surface = higher m, benthic = low m
# trait scores -- benthic refuge users = -.25, surface dwellers = .25
# niche breadth -- variable 

d_traits <- d_traits %>%
  mutate(
    my_niche_positions = case_when(
      Trait_code %in% c(2, 3) ~ -.25 ,#resistant -- can use benthic refugia
      Trait_code == 1 ~ .25),
    my_niche_breadths = my_niche_breadth,
    my_dispersal_traits = case_when(
      Trait_code %in% c(1, 3) ~ 1 + my_dispersal_bias,
      Trait_code == 2 ~ 1 - my_dispersal_bias))

# d_traits %>% ggplot(aes(as.factor(Trait_code), my_niche_breadths, col = Trait_code)) +
#   geom_boxplot()
# 
# d_traits %>% ggplot(aes(as.factor(Trait_code), my_niche_positions, col = Trait_code)) +
#   geom_boxplot()

####################################################
# -- make starting metacommunity for time t = 0
################

# make JL_t0 tables for surface and subsurface
# - map empirical observed data to the simulated landscape 
my_row_indexes <- sample(1:nrow(d_comm_raw), size = nrow(my_xy_coordinates), replace = TRUE)

my_JL_t0_surface <- d_comm_raw[my_row_indexes,]
my_JL_t0_subsurface <- my_JL_t0_surface

my_JL_t0 <- bind_rows(my_JL_t0_surface, my_JL_t0_subsurface)

####################################
# -- Assembly landscape info
#########################

# -- map disturbance onto sites
my_xy_coordinates$disturbance_class <- 'none'

# -- first, identify disturbed outlier sites
rows_2_edit <- which(my_xy_coordinates$site_type == 'outlier')
my_xy_coordinates$disturbance_class[
  sample(
    x = rows_2_edit, 
    size = round(length(rows_2_edit)*disturbance_extent, 0))] <- 'disturbed'

# -- second, identify the same proportion of cluster sites as disturbed
rows_2_edit <- which(my_xy_coordinates$site_type == 'cluster')
my_xy_coordinates$disturbance_class[
  sample(
    x = rows_2_edit, 
    size = round(length(rows_2_edit)*disturbance_extent, 0))] <- 'disturbed'

# surface reference and disturbance site info
site_data_surface <- data.frame(
  site_index = 1:length(my_row_indexes),
  habitat_type = 'surface',
  my_xy_coordinates,
  m_recovery = my_surface_m,
  Ef = 0,
  JL_recovery = my_surface_JL)

site_data_surface <- site_data_surface %>%
  mutate(
    JL_disturbance = case_when(
      disturbance_class == 'none' ~ JL_recovery,
      disturbance_class == 'disturbed' ~ JL_recovery * (1 - my_surface_mortality_percent)),
    m_disturbance = case_when(
      disturbance_class == 'none' ~ m_recovery,
      disturbance_class == 'disturbed' ~ my_surface_m_immigration_disturbance))

# subsurface reference and disturbance site info
site_data_subsurface <- data.frame(
  site_index = 1:length(my_row_indexes),
  habitat_type = 'subsurface',
  my_xy_coordinates,
  m_recovery = my_subsurface_m,
  Ef = -.5,
  JL_recovery = my_subsurface_JL)

site_data_subsurface <- site_data_subsurface %>%
  mutate(
    JL_disturbance = case_when(
      disturbance_class == 'none' ~ JL_recovery,
      disturbance_class == 'disturbed' ~ JL_recovery * (1 - my_subsurface_mortality_percent)),
    m_disturbance = case_when(
      disturbance_class == 'none' ~ m_recovery,
      disturbance_class == 'disturbed' ~ my_subsurface_m_immigration_disturbance))

# combine into one data.frame
site_data <- bind_rows(site_data_surface, site_data_subsurface) %>%
  mutate(site.ID = paste(habitat_type, site_index, sep = '_'))


#############
# send function landscape list and interval duration list, along with other metaSim parameters that will be fixed across all simulations

my_recovery_landscape <- MCSim::make.landscape(
  site.info = site_data,
  site.coords = site_data[,c('x','y')],
  JL = site_data$JL_recovery,
  m = site_data$m_recovery,
  list.of.stuff = list(d_traits = d_traits))

my_disturbance_landscape <- MCSim::make.landscape(
  site.info = site_data,
  site.coords = site_data[,c('x','y')],
  JL = site_data$JL_disturbance,
  m = site_data$m_disturbance,
  list.of.stuff = list(d_traits = d_traits))

# make a list of landscapes to cycle disturbance and recovery 
my_landscape_list <- c(list(my_recovery_landscape = my_recovery_landscape), 
                       rep(list(my_disturbance_landscape = my_disturbance_landscape, 
                                my_recovery_landscape = my_recovery_landscape), n_cycles))

# a vector for time intervals for each landscape type
my_time_interval_durations <- c(recovery_duration, rep(c(disturbance_duration, recovery_duration), n_cycles))

# -- call the wrapper function
my_sim_result <- MCSim::metasim.disturbance(
  ###### below are the parameters that are new to this wrapper function
  scenario_name = scenario_short_name,
  landscape_list = my_landscape_list,
  time_interval_durations = my_time_interval_durations,
  J.t0_initial = my_JL_t0,
  ####### below is stuff that would be similar to a regular MCSim::metasim() function call
  # traits
  trait.Ef = d_traits$my_niche_positions,
  trait.Ef.sd = d_traits$my_niche_breadths,
  trait.dispersal = d_traits$my_dispersal_traits,
  # other metacommunity params fixed across all sims
  W.r = my_W.r,
  nu = my_nu,
  speciation.limit = my_speciation.limit,
  # saving output
  save.sim = FALSE,
  output.dir.path = 'SIM_RESULTS')

#########
# -- save my_sim_result

my_save_path <- paste0('SIM_RESULTS/SIM_RESULT',
                       my_sim_result$scenario.ID,
                       '.RDA')
save(my_sim_result, file = my_save_path)

