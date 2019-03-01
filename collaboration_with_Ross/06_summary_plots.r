rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

# use devtools package to install MCSim from github
# devtools::install_github('sokole/MCSim@dev')
library(MCSim)

library(tidyverse)
###############################################################################
# sim output dir
sim_result_dir_name <- 'WORKING_SIMS'

###############################################################################
# read in summary data table
read_file_name <- paste0(sim_result_dir_name,'/RESULTS_SUMMARY.csv')
d_results_summary <- read_csv(read_file_name)

###############################################################################
# adjusting gropuing variables 

d_results_summary <- d_results_summary %>%
  mutate(
    `Metacommunity type` = case_when(
      niche_breadth > 1.5 ~ 'No env. filter',
      niche_breadth <= 1.5 ~ 'Strong env. filter'),
    habitat_type = factor(habitat_type, ordered = TRUE, levels = c('surface', 'subsurface')),
    dist_freq = round(recovery_dur/timestep, 2))

########################################
# plotting richness

####
graphics.off()
windows(6, 5, pointsize = 10)

my_metric_type <- 'richness'

d_plotting <- d_results_summary %>% 
  filter(scale != 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() 

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(as.factor(dist_extent), metric_value, color = `Metacommunity type`)) +
  geom_boxplot() +
  facet_grid(scale ~ .) +
  ylab('Richness') + 
  xlab('Disturbance extent') +
  ggtitle('Richness at local (alpha) and regional (gamma) scales') +
  theme_bw()


####
graphics.off()
windows(6, 5, pointsize = 10)

my_metric_type <- 'richness'

d_plotting <- d_results_summary %>% 
  filter(scale == 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() 


d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(as.factor(dist_extent), metric_value, color = `Metacommunity type`)) +
  geom_boxplot() +
  facet_grid(habitat_type ~ .) +
  ylab('Richness') + 
  xlab('Disturbance extent') +
  ggtitle('Local (alpha) richness in surface and subsurface habitats') +
  theme_bw()


####
graphics.off()
windows(6, 5, pointsize = 10)

my_metric_type <- 'richness'

d_plotting <- d_results_summary %>% 
  filter(scale != 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() 

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(as.factor(dist_freq), metric_value, color = `Metacommunity type`)) +
  geom_boxplot() +
  facet_grid(scale ~ .) +
  ylab('Richness') + 
  xlab('Disturbance frequency (per time step)') +
  ggtitle('Richness at local (alpha) and regional (gamma) scales') +
  theme_bw()


####
graphics.off()
windows(6, 5, pointsize = 10)

my_metric_type <- 'richness'

d_plotting <- d_results_summary %>% 
  filter(scale == 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() 


d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(as.factor(dist_freq), metric_value, color = `Metacommunity type`)) +
  geom_boxplot() +
  facet_grid(habitat_type ~ .) +
  ylab('Richness') + 
  xlab('Disturbance frequency (per time step)') +
  ggtitle('Local (alpha) richness in surface and subsurface habitats') +
  theme_bw()


########################################
# plotting counts of each trait type

####
graphics.off()
windows(12, 5, pointsize = 10)

my_metric_type <- 'counts_by_trait'

d_plotting <- d_results_summary %>% 
  filter(scale != 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() %>%
  filter(trait_code_desc != 'Other')

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(trait_code_desc, metric_value, color = trait_code_desc)) +
  geom_boxplot() +
  facet_grid(scale ~ `Metacommunity type` + dist_extent, scales = 'free_y') +
  ylab('Count') + 
  xlab('Trait group type') +
  ggtitle('Abundance of each trait group at local (alpha) and regional (gamma) scales') +
  theme_bw() +
  guides(color = FALSE)

####
graphics.off()
windows(12, 5, pointsize = 10)

my_metric_type <- 'counts_by_trait'

d_plotting <- d_results_summary %>% 
  filter(scale == 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() %>%
  filter(trait_code_desc != 'Other')

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(trait_code_desc, metric_value, color = trait_code_desc)) +
  geom_boxplot() +
  facet_grid(habitat_type ~ `Metacommunity type` + dist_extent, scales = 'free_y') +
  ylab('Count') + 
  xlab('Trait group type') +
  ggtitle('Local abundance of each trait group in surface and subsurface habitats') +
  theme_bw() +
  guides(color = FALSE)


####
graphics.off()
windows(12, 5, pointsize = 10)

my_metric_type <- 'counts_by_trait'

d_plotting <- d_results_summary %>% 
  filter(scale != 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() %>%
  filter(trait_code_desc != 'Other')

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(trait_code_desc, metric_value, color = trait_code_desc)) +
  geom_boxplot() +
  facet_grid(scale ~ `Metacommunity type` + dist_freq, scales = 'free_y') +
  ylab('Count') + 
  xlab('Trait group type') +
  ggtitle('Abundance of each trait group at local (alpha) and regional (gamma) scales') +
  theme_bw() +
  guides(color = FALSE)

####
graphics.off()
windows(12, 5, pointsize = 10)

my_metric_type <- 'counts_by_trait'

d_plotting <- d_results_summary %>% 
  filter(scale == 'alpha_by_habitat', 
         metric_type == my_metric_type) %>% 
  as.data.frame() %>%
  filter(trait_code_desc != 'Other')

d_plotting %>% head()  

d_plotting %>% 
  ggplot(aes(trait_code_desc, metric_value, color = trait_code_desc)) +
  geom_boxplot() +
  facet_grid(habitat_type ~ `Metacommunity type` + dist_freq, scales = 'free_y') +
  ylab('Count') + 
  xlab('Trait group type') +
  ggtitle('Local abundance of each trait group in surface and subsurface habitats') +
  theme_bw() +
  guides(color = FALSE)

