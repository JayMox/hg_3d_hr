#script pulling data for 3d HR work, w/ Beatrice
#JHMoxley Feb 2022

#script-ionary
#dive_fxns - library of custom functions for data labelling & transformation
#dive_data_annotation - roughed-in raw data transformations for a working dataset
#dive_plots - plot codes for all/most figs in my dissertation
#Raw source is dive2_df.Rdata, mirroring the source data of figure plotting in dive_plots.R

library(tidyverse)
library(here)
source(here('r', 'dive_fxns.R')) 

load(here('data', 'dive2_df.RData'))
#trim up
dat <- dive2 %>% janitor::clean_names() %>% 
  select(tag, ref, jday = jday_r, tagday, 
         ds_date, de_date, de_date_date, de_date_time,
         surf_dur, dive_dur, max_dep, percent_area,
         utm_x, utm_y, #these are dive endpts; all locs are endpts unless denoted w/ start
         lon, lat, lon_start = start_lon, lat_start = start_lat, 
         #nb: max dep is a separate measurement than dive profiling (i.e., d1:9)
         matches('d\\d{1}\\b'), matches('t\\d{1}\\b'),
         pseg_idx #this is an idx for merging dive data & movement model
  ) %>% 
  mutate(pseg_fac = as.factor(paste(tag, as.factor(pseg_idx), sep = "."))) #create an easy key for merge w/ moveDat

#add dist2shore & bathy sampler using custom fxns in dive_fxns
#NB.. both functions ingest the df, modify it to add a field, and return the ENTIRE df
#dat <- shore_dist_sampler(dat, here('data/GIS/PhysicalOceanography', 'shoreline_UTM.shp'))
#dat <- bathy_sampler(dat, here('data/GIS/PhysicalOceanography', 'bathy_utm.tif'))
#dat <- dive_phases2(dat, threshold = .95, min_dep = 5)
#dat <- dive_phases(dat, threshold = .95, min_dep = 5)

#THESE SAMPLERS MAY WORK W/ SOME TWEAKING.  IGNORING FN.
#particularly worthwhile to inspect the dive_phase labeller

load(here('data', 'traj4.RData'))
mov <- traj4 %>% janitor::clean_names() %>% 
  select(id, date, season, phase_movt = phase, 
         foray_segs, dur_test,
         foray_idx, foray_idx3, foray_idx4, 
         pseg_idx, pseg_fac, dist_test, d2s_m,
         contains('outbound'), contains('inbound'), offshore, total,
         dd_movtX = x_dd, dd_movtY = y_dd)

#merge via pseg_fac to deliver foray idx & trip phase data
dvdf <- dat %>% 
  merge(mov %>% select(pseg_fac, season, phase_movt, foray_idx3, d2s_m, dd_movtX, dd_movtY),
        by = "pseg_fac", all.x = T)
# save(dvdf, file = here('data', 'dive_df_feb22.RData'))
# dvdf %>% write_csv(here('data', 'dive_df_feb22.csv'))



#foray_idx3 definitions (incl. duration/distance tests) need to be extracted from tripid2
#my undertsanding is: idx3 = d2shore > 2000m; 
#offshore segments = 6 segs/3 hrs[summer]; 12/6 hrs [winter]

#method for extract foraging trip behavior
#load(here('data', 'dive4_df.RData'))
# fdive <- filter(dive4, !is.na(foray_idx3))    #only focused on dives on a foraging trip
# fdive <- ddply(arrange(fdive, tag, DE_DATE), .(tag), mutate, d.idx = seq_along(tag))   #add idx

##methods in tripID2 report
#From previous TripID script:
#This script identifies and indexes trips away from the coast 
#for further analysis at the trip scale using a custom function called tripID.  
#Trips are identified by consecutive points beyond a distance threshold (default = 2000m).  
#These consecutive points must be greater than some minimum duration (expected default for 
#summer, 6 segments = 3 hrs; expected winter default, 48 segments = 24 hrs).  
#Then, another function (foray_endpts) is used to identify arrival and departure points at 
#the coast.  Arrival and departure points are identified by scanning a pre-determined window
#(default, 6 segments or 3 hrs) for minimum distances to the shoreline, so long as they do 
#not intersect with a polygon mask encompassing land & inshore/harbor waters.  
