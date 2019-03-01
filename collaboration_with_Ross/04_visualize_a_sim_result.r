rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

# use devtools package to install MCSim from github
# devtools::install_github('sokole/MCSim@dev')
library(MCSim)

library(tidyverse)

sim_result_dir_name <- 'WORKING_SIMS'

my_file_list <- list.files(sim_result_dir_name)
sim_result_list <- my_file_list[grep('\\.RDA', my_file_list)]

which_is_newest_sim <- (str_split(sim_result_list, '_') %>% data.frame() %>% t() )[,8] %>% 
  gsub('\\.RDA','',.) %>% which.max()

# loads an object called 'my_sim_result'
my_sim_result_obj_name <- load(file = paste0(sim_result_dir_name,'/',sim_result_list[which_is_newest_sim]))

# pick specific sims
# my_sim_result_obj_name <- load(file = paste0('SIM_RESULTS/','SIM_RESULT_sigma-0.25_distext-0.75_distmor-0.5_recovdur-50_20190226_174155.RDA'))
# my_sim_result_obj_name <- load(file = paste0('SIM_RESULTS/','SIM_RESULT_sigma-20_distext-0.75_distmor-0.5_recovdur-50_20190226_173903.RDA'))


#########

# -- plot dispersal kernel for sim
plot_standardized_disp_kernel(unlist(my_sim_result$W.r.list[[1]]), landscape = my_sim_result$landscape.list[[1]])

# plot map of sites for sim
my_sim_result$landscape.list[[1]]$site.info %>%
  filter(habitat_type == 'surface') %>% 
  ggplot(aes(x, y,
             size = JL_disturbance,
             color = site_type,
             shape = disturbance_class)) +
  geom_point()

##################################################
##################################################
##################################################
##################################################

# # --- ordinations and plotting ------------------
# # convert long format species count data to wide
# spp_list <- my_sim_result$J.long$spp %>% unique() %>% as.character()
# data_wide <- my_sim_result$J.long %>% 
#   dplyr::select(-scenario.ID) %>% 
#   group_by(timestep, site, sim.result.name) %>% 
#   tidyr::spread(spp, count)
# head(data_wide)
# 
# # hellinger transform abundance data
# data_wide_hell <- data_wide[, spp_list] %>%
#   vegan::decostand('hellinger')
# 
# # reduce dimensionality using pco -- takes 1-2 mins on my laptop with 133 timesteps
# mod.pco <- data_wide_hell %>%
#   dist() %>%
#   labdsv::pco()
# 
# # return the pco scores for each site x timestep
# data_wide$pco1 <- mod.pco$points[,1]
# data_wide$pco2 <- mod.pco$points[,2]
# 
# data_wide <- data_wide %>% mutate(
#   landscape_list_label = case_when(
#     grepl('my_recovery_landscape', sim.result.name) ~ 'my_recovery_landscape',
#     grepl('my_disturbance_landscape', sim.result.name) ~ 'my_disturbance_landscape',
#     TRUE ~ NA_character_
#   )
# )
# 
# landscape_site_info_flat <- data.frame()
# for(i_landscape in 1:length(names(my_landscape_list))){
#   landscape_site_info_flat <- bind_rows(
#     landscape_site_info_flat,
#     data.frame(
#       landscape_list_label = names(my_landscape_list)[i_landscape],
#       site = 1:nrow(my_landscape_list[[i_landscape]]$list.of.stuff$river_site_info),
#       my_landscape_list[[i_landscape]]$list.of.stuff$river_site_info))
# }
# 
# data_wide <- data_wide %>% 
#   left_join(landscape_site_info_flat)
# 
# 
# my_save_path <- paste0('SIM_RESULTS/JL_WIDE_with_PCO_',
#                        scenario_description,
#                        '.csv')
# write_csv(my_sim_result$J.long, my_save_path)

#################################################
#################################################
#################################################
#################################################
# map traits onto J_long data frame
d_traits <- my_sim_result$landscape.list[[1]]$list.of.stuff$d_traits
d_traits$spp <- paste0('spp',1:nrow(d_traits))
d_traits <- d_traits %>%
  select(spp, taxa, Trait_code, my_niche_positions, my_niche_breadths, my_dispersal_traits) %>%
  mutate(trait_code_desc = case_when(
    Trait_code == 1 ~ 'Resilient',
    Trait_code == 2 ~ 'Resistant',
    Trait_code == 3 ~ 'Both'))

d_plot_long <- my_sim_result$J.long %>%
  left_join(
    data.frame(site = 1:nrow(my_sim_result$landscape.list[[1]]$site.info),
               my_sim_result$landscape.list[[1]]$site.info %>% select(-c(JL, Ef.specificity, IL, m)) )) %>%
  left_join(d_traits, by = 'spp')
  
# # plot abundances by site
# d_plot_long %>% ggplot(aes(timestep, count,
#                               color = spp)) +
#   geom_line() +
#   facet_grid(as.factor(site_index) ~ habitat_type)

# sum trait counts for each trait type by timestep / site.ID
d_trait_counts_long <- d_plot_long %>% group_by(timestep, site, site_index, habitat_type, site_type, disturbance_class, Trait_code, trait_code_desc) %>%
  summarize(count_trait_type = sum(count))

d_richness <- d_plot_long %>% group_by(timestep, site, site_index, habitat_type, site_type, disturbance_class) %>%
  filter(count > 0) %>%
  summarize(richness = spp %>% unique() %>% length())

graphics.off()
save_plot_name <- paste0('FIG EXAMPLE trait counts by time for SIM',my_sim_result$scenario.ID,'.pdf')
pdf(save_plot_name, 12, 6)
d_trait_counts_long %>%
  ggplot(aes(timestep, count_trait_type, color = trait_code_desc, line = as.factor(site))) +
  geom_line() + 
  # geom_point() + 
  # stat_smooth() + 
  # geom_smooth() + 
  # facet_grid(habitat_type ~ site_type) + 
  facet_grid(habitat_type ~ site_type + disturbance_class) +
  ggtitle(my_sim_result$scenario.ID)
dev.off()

graphics.off()
save_plot_name <- paste0('FIG EXAMPLE richness by time for SIM',my_sim_result$scenario.ID,'.pdf')
pdf(save_plot_name, 12, 6)
d_richness %>%
  ggplot(aes(timestep, richness, line = as.factor(site))) +
  geom_line() + 
  # geom_point() + 
  # stat_smooth() + 
  # geom_smooth() + 
  # facet_grid(habitat_type ~ site_type) + 
  facet_grid(habitat_type ~ site_type + disturbance_class) +
  ggtitle(my_sim_result$scenario.ID)
dev.off()

