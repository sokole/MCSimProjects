## This directory includes script to create simulations to assess the tradeoffs between resistant and resilient taxa in a metacommunity under different disturbance levels, where each site has a "surface" and "subsurface" habitat. Resistant taxa can use the subsurface habitat, resilient taxa are better dispersers among surface habitats. 

## NOTE -- the sims will produce data files that are too big for github, thus I just have the code here with a set.seed so you can produce the same simulation results on your local machine. I think it would take a few hours to produce all the data on a normal laptop or desktop machine. 

## Notes from conversation with Ross -- goals fo sims
(1) alter the map to have an even number of outlier and clustered sites (e.g., a cluster of 20, and then 20 outliers scattered around the map margins)

(2) run the simulation with and without the subsurface refugia

(3) cross the above with a few different disturbance regimes -- we might want to talk about this. Levels to consider 
	-- number of sites disturbed (e.g., 5, 50, and 95% of sites disturbed) crossed with 
	-- morality rates (something like 5, 50, and 95%) crossed with 
	-- disturbance frequencies (once, every 30 time steps, every 5 time steps, etc.). 

(4) output to look at would be relative abundance of each tait type at different site and habitat types at the final time step of each simulation. 