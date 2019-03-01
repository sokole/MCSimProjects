rm(list=ls())
gc()

options(stringsAsFactors = FALSE)

##############
n_sites <- 20
n_outlier_sites <- 20

##############
library(spatstat)
library(tidyverse)

set.seed(12)
###############################

map_runifpoint <- spatstat::runifpoint(n_outlier_sites) #uniform random points
plot(map_runifpoint)

map_rpoint <- spatstat::rpoint(n_sites) #random points
plot(map_rpoint)

# each cluster consist of 10 points in a disc of radius 0.2
nclust <- function(x0, y0, radius, n) {
  return(runifdisc(n, radius, centre=c(x0, y0)))
}

# rPoissonCluster(kappa = 2, expand = 0.05, rcluster = nclust, radius=0.05, n=2) %>% plot()

# make data frames
d_rnd <- data.frame(
  site_type = 'outlier',
  x = map_runifpoint$x, 
  y = map_runifpoint$y)


map_clust <- nclust(.7, .7, .1, 20)
d_clust <- data.frame(
  site_type = 'cluster',
  x = map_clust$x, 
  y = map_clust$y)

d_all_points <- bind_rows(d_rnd, d_clust)

graphics.off()
pdf('FIG site map.pdf', width = 6, height = 6)
plot(x = d_all_points$x, y = d_all_points$y, 
     col = as.numeric(as.factor(d_all_points$site_type)),
     pch = as.numeric(as.factor(d_all_points$site_type)))
dev.off()

write_csv(d_all_points, 'analysis_files/map_xy_cluster_with_outliers.csv')
write_csv(d_clust, 'analysis_files/map_xy_cluster_no_outliers.csv')
