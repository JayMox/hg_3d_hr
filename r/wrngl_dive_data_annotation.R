#Script for adding fields to clean dive data

rm(list=ls(all=TRUE)) 
Sys.setenv(TZ='GMT')

library(dplyr)
library(lubridate)

#set working space, load data, and source fxns
home_dir <- "/Volumes/CARCHAR/Seals_GOM/analysis_diving"; clip <- 28;
#home_dir <- "/Volumes/Yoda/Users/mox/analysis_diving"; clip <- 23;
load(paste(home_dir, "dive2_df.RData", sep = "/"))
source(paste(home_dir, "dive_fxns.R", sep = "/"))

#Dive2 is a data frame of the entire dive dataset, UTM & DD coords (from end of the dive),
#and the predicted movement segment

#subset useful fields
data <- dplyr::select(dive2, tag, utm_x, utm_y, lon, lat, jday.y, tagday, DE_DATE, SURF_DUR, DIVE_DUR, MAX_DEP, 
       D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, PERCENT_AREA, pseg_idx)

#build a factor composite of tag & move segment index
data$pseg_fac <- factor(paste(data$tag, data$pseg_idx, sep = "."))

#add dist2shore & bathy sampler using custom fxns in dive_fxns
#NB.. both functions ingest the df, modify it to add a field, and return the ENTIRE df
data2 <- shore_dist_sampler(data, paste(substr(home_dir, 0, clip), 
                                       "GIS/PhysicalOceanography/shoreline_UTM.shp", sep = "/"))
data3 <- bathy_sampler(data2, paste(substr(home_dir, 0, clip), "GIS/PhysicalOceanography/bathy_utm.tif", sep = "/")
save(data3, file = paste(home_dir, "data3.RData", sep = "/"))

#use custom dive fxn to define bottom phase parameters, fxn returns entire data frame
#include a min depth threshold
dive <- dive_phases(data3, threshold = .95, min_dep = 5)
#UPDATE USING DIVE_PHASES2 fxn that uses what DJ and I agreed upon for defs of bottom phase/ascent/descent
#DIVE_PHASE2 Needs to be completed

#save data
save(dive, file = paste(home_dir, "diveDF_annotated.RData", sep = "/"))

##NB: TRNRD factors can only be set for summer dives, until the winter foray idx is functional
#add trnrd segments (this should also add foray idx?)
load(paste(substr(home_dir, 0, clip), "analysis_movements/hg_traj.df3.RData", sep = "/"))

#subset month datato summer months
mos <- c(6, 7, 8, 9, 10);
sumr_dive <- filter(dive, month(DE_DATE) %in% mos)
sumr_traj <- filter(hg_traj.df, month(date) %in% mos)

#add forya & trnrd indexes
sumr_dive$foray_idx <- sumr_traj$foray_idx[match(sumr_dive$pseg_fac, sumr_traj$pseg_fac)]
sumr_dive <- merge(sumr_dive, dplyr::select(sumr_traj, id, pseg_idx, trnrd), by.x = c("tag", "pseg_idx"), by.y = c("id", "pseg_idx"), all.x = TRUE)

#save partial dataset to file
save(sumr_dive, file = paste(home_dir, "sumr_dive_annotated.RData", sep = "/"))

#dataset to test new dive_phase parameterizations
####I DON"T THINK THIS WORKS YET!! & NEEDS TO BE TROUBLESHOT
reg_dive <- filter(dive, bot_segs != 0 & !is.na(dsc_segs))
oneseg_dive <- filter(dive, bot_segs == 0 & !is.na(dsc_segs))
vee_dive <- filter(dive, !is.na(bot_segs) & is.na(dsc_segs))

idx_reg <- runif(5, 1, nrow(reg_dive))
idx_one <- runif(5, 1, nrow(oneseg_dive))
idx_vee <- runif(5, 1, nrow(vee_dive))

test <- rbind(reg_dive[idx_reg,], oneseg_dive[idx_one,], vee_dive[idx_vee,])
#look at areas w/ depths greater than dthresh
apply(select(test, D1:D9), 2, function(x) match(x >= test$dthresh, TRUE))