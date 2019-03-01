rm(list=ls())
gc()

options(stringsAsFactors = FALSE)

library(tidyverse)
library(vegan)
library(ade4)

# read in data raw data from Ross
d_env_in <- read_csv('SEMS_envmatrix_20190108.csv')
d_comm_in <- read_csv('SEMS_sitematrix_20181203.csv')

# create dir for output
if(!file.exists('analysis_files')) dir.create('analysis_files')

# filter to use only "before" dates
d_comm <- d_comm_in %>% filter(before_after == 'before')
d_env <- d_env_in %>% filter(before_after == 'before')

# get spp list
spp_list <- d_comm %>% 
  dplyr::select(-dataset, -site, -date_year_month, -intermittent_perennial, -before_after) %>%
  names()

# env vars to explore
env_var_list <- c('TKN','NO3','NH3','TP','Fecal','Chloride','Turbidity',
                  'DO','Cond','pH','TempW','TempA',
                  'Wdth','Dpth','Flow','TSS', 'TDS')

# join data in one table so we know the row orders are all correct
d_all <- left_join(d_env, d_comm)
write_csv(d_all, 'analysis_files/d_all_rawdata.csv')

# check names
env_var_list %in% names(d_env)

# transform (if necessary) and scale env data
d_env_scaled <- d_all[,env_var_list]
for(i_col in env_var_list){
  y_raw <- d_env_scaled[,i_col] %>% unlist()
  y_log1p <- d_env_scaled[,i_col] %>% log1p() %>% unlist()
  
  if(shapiro.test(y_raw)$p.val > 0.05){
    d_env_scaled[,i_col] <- y_raw %>% base::scale()
  }else if(shapiro.test(y_raw)$p.val < shapiro.test(y_log1p)$p.val){
    d_env_scaled[,i_col] <- y_log1p %>% base::scale()
  }
  
  print(i_col)
}

# write out transformed data
write_csv(d_env_scaled, 'analysis_files/d_env_scaled.csv')

# hellinger transform comm data
d_comm_hell <- d_all[,spp_list] %>% decostand(method = 'hellinger')

d_comm_raw <- d_all[,spp_list]

#########################33
##########################
############################
########################
####################################
####################################
####################################
####################################
# If NOT using ALL ENV VARS
# pre-screen for environmental vars with a pval < user defined cutoff
# if no variable has a p value better than the cutoff, then we use the one 
# with the highest R2 value.
####################################
E.pvals <- apply(X = d_env_scaled,
                 MARGIN = 2,
                 FUN = function(X){
                   anova(
                     vegan::capscale(
                       vegan::vegdist(d_comm_hell, 'euclidian') ~ X,
                       na.action = "na.omit",
                       add = TRUE))[1,"Pr(>F)"]
                 }
)

# calculate adj R2 values for each env var.
E.R2 <- apply(X = d_env_scaled,
              MARGIN = 2,
              FUN = function(X){
                vegan::RsquareAdj(vegan::capscale(
                  vegan::vegdist(d_comm_hell, method = 'euclidean') ~ X,
                  na.action="na.omit",
                  add=TRUE))$adj.r.squared
              }
)

env_sig <- (E.R2 %>% abs() %>% sort(decreasing = TRUE) %>% names())[1:4] 


d_env_scaled.sig <- d_env_scaled[,env_sig]

# write out
write_csv(d_env_scaled.sig, 'analysis_files/d_env_scaled_sig.csv')
write_csv(d_comm_hell, 'analysis_files/d_comm_hell.csv')
write_csv(d_comm_raw, 'analysis_files/d_comm_raw.csv')

##########################################################
rm(list=ls())
gc()

d_all <- read_csv('analysis_files/d_all_rawdata.csv') 
d.comm.ra <- read_csv('analysis_files/d_comm_hell.csv')
d.env <- read_csv('analysis_files/d_env_scaled_sig.csv')

d.siteinfo <- d_all %>% 
  dplyr::select(site, date_year_month, intermittent_perennial, Lat, Long)

# -- calculate niches for species
dudi.pca.env <- dudi.pca(d.env, scale = TRUE, scan = FALSE, nf=1)
niche.species <- niche(dudi.pca.env, Y = d.comm.ra, scann = FALSE)
d.niche <- data.frame(
  niche.pos=niche.species$li,
  as.data.frame(niche.param(niche.species))
) %>% tibble::rownames_to_column('taxa')

# -- calculate niche positions for each of the sites
d.site.niche  <-  data.frame(
  d.siteinfo,
  dudi.pca.env$li
)

# -------------------------------------------------------------
# -- plot the coenoclines, species' niche positions along the environmental gradient
# -- 
# -- Note that the "rug" marks along the x-axis denote the sites' locations 
# -- on the environmental gradient
# ------------------------
env.axis <- d.site.niche$Axis1

species.niche <- d.niche
niche.factor <- .5 #scale niche breadths

# -- function for plotting bell curves
fn_norm_curve <- function(sigma=1, mu = 0,...) {
  curve(
    (1/sigma * sqrt(2 * pi)) * exp((-1  *(x - mu)^2) / (2 * sigma^2)), ...) #formula for bell curve
}

# -- Initialize plot of coenoclines
plot(1,1,
     xlim = c(min(env.axis), max(env.axis)),
     ylim = c(0, 10),
     type = 'n',
     xlab = 'Environmental gradient (PCA axis 1 scores)',
     ylab = 'Prob. dens.',
     main = 'Mite niche positions and niche widths')

mypal <- rainbow(nrow(species.niche))

# -- loop to plot each species' habitat preference
for (i.spp in 1:ncol(d.comm.ra)){
  fn_norm_curve(
    mu = species.niche[i.spp, 'Axis1'],
    sigma = niche.factor*sqrt(species.niche[i.spp, 'Tol']),
    add = TRUE,
    col = mypal[i.spp])
}

# -- plot sites along the x-axis
rug(env.axis)

# -- legend for mite species codes
legend(x = 'topleft',
       legend = row.names(species.niche),
       lty = 1,
       col = mypal,
       cex = .75,
       ncol = 4)

# write output
write_csv(d.site.niche, 'analysis_files/d_site_niche.csv')
write_csv(d.niche, 'analysis_files/d_niche.csv')
