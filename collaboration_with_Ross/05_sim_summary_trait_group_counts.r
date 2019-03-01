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

################################################################################
# look in working dir for sim output files
my_file_list <- list.files(sim_result_dir_name)
sim_result_list <- my_file_list[grep('\\.RDA', my_file_list)]


# i_sim_result <- 1

d_results_summary <- data.frame()

# loop
for(i_sim_result in 1:length(sim_result_list)){
# for(i_sim_result in 1:10){
    
  try({
    
    # remove info from last sim result 
    my_sim_result <- NULL
    d_results_summary_i <- data.frame()
    
    # load this sim result
    my_sim_result_obj_name <- load(file = paste0(sim_result_dir_name,'/',sim_result_list[i_sim_result]))
    # obj name is "my_sim_result"
    
    #################################################
    # map traits onto J_long data frame
    d_traits <- my_sim_result$dat.gamma.t0.list %>% last() %>% as.data.frame()
    d_traits <- d_traits %>%
      rename(spp = dat.gamma.t0.taxa.list) %>%
      mutate(
        Trait_code = case_when(
          dat.gamma.t0.trait.Ef > 0 & dat.gamma.t0.trait.dispersal == max(dat.gamma.t0.trait.dispersal) ~ 1,
          dat.gamma.t0.trait.Ef < 0 & dat.gamma.t0.trait.dispersal == min(dat.gamma.t0.trait.dispersal) ~ 2,
          TRUE ~ 3),
        trait_code_desc = case_when(
          Trait_code == 1 ~ 'Resilient',
          Trait_code == 2 ~ 'Resistant',
          Trait_code == 3 ~ 'Other'))
    
    d_long <- my_sim_result$J.long %>%
      left_join(
        data.frame(site = 1:nrow(my_sim_result$landscape.list[[1]]$site.info),
                   my_sim_result$landscape.list[[1]]$site.info %>% select(-c(JL, Ef.specificity, IL, m)) )) %>%
      left_join(d_traits, by = 'spp')
    
    ##############################################
    # sum trait counts for each trait type by timestep / site.ID
    d_trait_counts_alpha_by_site_habitat <- data.frame()
    d_trait_counts_alpha_by_site <- data.frame()
    d_trait_counts_gamma <- data.frame()
    d_richness_alpha_by_site_habitat <- data.frame()
    d_richness_alpha_by_site <- data.frame()
    d_richness_gamma <- data.frame()
    
    d_trait_counts_alpha_by_site_habitat <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep, site, site_index, habitat_type, site_type, disturbance_class, Trait_code, trait_code_desc) %>%
      summarize(metric_value = sum(count)) %>%
      mutate(
        metric_type = 'counts_by_trait',
        scale = 'alpha_by_habitat',
        temporal_aggregation = 'final_timestep')
    
    d_trait_counts_alpha_by_site <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep, site_index, site_type, disturbance_class, Trait_code, trait_code_desc) %>%
      summarize(metric_value = sum(count)) %>%
      mutate(
        metric_type = 'counts_by_trait',
        scale = 'alpha',
        temporal_aggregation = 'final_timestep')
    
    d_trait_counts_gamma <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep, Trait_code, trait_code_desc) %>%
      summarize(metric_value = sum(count)) %>%
      mutate(
        metric_type = 'counts_by_trait',
        scale = 'gamma',
        temporal_aggregation = 'final_timestep')
    
    ########
    d_richness_alpha_by_site_habitat <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep, site, site_index, habitat_type, site_type, disturbance_class) %>%
      summarize(metric_value = spp %>% unique() %>% length()) %>%
      mutate(
        metric_type = 'richness',
        scale = 'alpha_by_habitat',
        temporal_aggregation = 'final_timestep')
    
    d_richness_alpha_by_site <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep, site_index, site_type, disturbance_class) %>%
      summarize(metric_value = spp %>% unique() %>% length()) %>%
      mutate(
        metric_type = 'richness',
        scale = 'alpha',
        temporal_aggregation = 'final_timestep')
    
    d_richness_gamma <- d_long %>% 
      dplyr::filter(timestep == max(timestep)) %>%
      dplyr::filter(count > 0) %>%
      group_by(timestep) %>%
      summarize(metric_value = spp %>% unique() %>% length()) %>%
      mutate(
        metric_type = 'richness',
        scale = 'gamma',
        temporal_aggregation = 'final_timestep') 
    
    ##
    d_results_summary_i <- d_trait_counts_alpha_by_site_habitat %>% 
      bind_rows(d_trait_counts_alpha_by_site) %>%
      bind_rows(d_trait_counts_gamma) %>%
      bind_rows(d_richness_alpha_by_site_habitat) %>%
      bind_rows(d_richness_alpha_by_site) %>%
      bind_rows(d_richness_gamma) %>%
      mutate(scenario_id = my_sim_result$scenario.ID)
    
    d_results_summary <- bind_rows(d_results_summary, d_results_summary_i)
  
  })
  
  print(paste0('Completed ',i_sim_result,' out of ',length(sim_result_list)))
}

d_results_summary <- d_results_summary %>% 
  ungroup() %>%
  tidyr::separate(scenario_id, 
                  into = c('blank','niche_breadth','dist_extent','dist_mortality','recovery_dur','rep','sim_date','sim_time'),
                  sep = '_',
                  remove = FALSE) %>%
  select(-blank) %>%
  rowwise() %>%
  mutate(
    niche_breadth = gsub('sigma\\-','',niche_breadth),
    dist_extent = gsub('distext\\-','',dist_extent),
    dist_mortality = gsub('distmor\\-','',dist_mortality),
    recovery_dur = gsub('recovdur\\-','',recovery_dur),
    rep = gsub('rep\\-','',rep))

write_file_name <- paste0(sim_result_dir_name,'/RESULTS_SUMMARY.csv')
write_csv(d_results_summary, write_file_name)
