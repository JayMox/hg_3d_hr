#Quick script to print plots of dive trajectories for mtg with andreas. 
#this should be turned into a custom plot function
#rm(list=ls(all=TRUE)) 
Sys.setenv(TZ='UTC')

library(zoo)
library(rgdal)
library(lubridate)
library(dplyr)
library(plyr)
library(reshape2)
library(rpart)
library(ggplot2)

#set working directory and read in data
home_dir <- "/Volumes/HELLBENDY/Seals_GOM/analysis_diving"; clip = 28
#home_dir <- "/Volumes/Yoda/Users/mox/analysis_diving"; clip = 23;
setwd(home_dir);

#load in custom fxns
source(paste(home_dir, "dive_fxns.R", sep = "/"))

load("dive2_df.RData")
fdive <- filter(dive4, !is.na(foray_idx3))    #only focused on dives on a foraging trip
fdive <- ddply(arrange(fdive, tag, DE_DATE), .(tag), mutate, d.idx = seq_along(tag))   #add idx


####plot manipulations.. to be generalized for all phases
#11/5 BUILT AS BASIC FOR LOOP JUST TO GET DONE.. SHOULD BE GENERALIZED
#tmp <- filter(fdive, foray_idx3 == 4 & tag == 654 & phase == "offshore")
for(i in 2:length(unique(fdive$tag))){
  tmp3 <- filter(fdive, tag == unique(fdive$tag)[i])
  
  for(j in 1:length(unique(tmp3$season))){
    tmp2 <- filter(tmp3, season == unique(tmp3$season)[j])
    forays <- unique(tmp2$foray_idx3)
    
    for(k in 1:length(forays)){
      tmp <- filter(tmp2, foray_idx3 == forays[k])
    
      mtmp <- arrange(melt(tmp, id.vars = c('tag', 'foray_idx3', 'season', 'phase', 'month', 'SURF_DUR', 'DIVE_DUR', 'd.idx'), 
                           measure.vars = c('D1', "D2", "D3", "D4", "D5", 'D6', "D7", 'D8', 'D9', 'SURF_DUR'), variable.name = "d_strata", value.name = "depth"), d.idx)
      mtmp <- mutate(mtmp, t.idx = seq_along(d.idx), time = ifelse(d_strata == "SURF_DUR", depth, DIVE_DUR * .10), etime = cumsum(time))    #add new time index, time per strata, and elapsed time
      mtmp <- mutate(mtmp, depth = ifelse(d_strata == "SURF_DUR", 0, -depth), diving = ifelse(d_strata == "SURF_DUR", "surface", "depth"))    #adjust depth of surface durations from time at surface to 0m depth
      
      #ggplot
      p <- ggplot(mtmp, aes(x = etime, y = depth, colour = phase)) + geom_line() + labs(title = paste(unique(mtmp$tag), unique(mtmp$season), unique(mtmp$foray_idx3)))
      ggsave(paste(unique(mtmp$tag), unique(mtmp$season), unique(mtmp$foray_idx3), ".pdf", sep = "_"), p, width = 45, height = 28)
      }
  }
}


#scratch code
tmp <- filter(fdive, tag == 654)
for(i in 1:length())
mtmp <- arrange(melt(tmp, id.vars = c('tag', 'foray_idx3', 'season', 'phase', 'month', 'SURF_DUR', 'DIVE_DUR', 'd.idx'), 
                     measure.vars = c('D1', "D2", "D3", "D4", "D5", 'D6', "D7", 'D8', 'D9', 'SURF_DUR'), variable.name = "d_strata", value.name = "depth"), d.idx)
mtmp <- mutate(mtmp, t.idx = seq_along(d.idx), time = ifelse(d_strata == "SURF_DUR", depth, DIVE_DUR * .10), etime = cumsum(time))    #add new time index, time per strata, and elapsed time
mtmp <- mutate(mtmp, depth = ifelse(d_strata == "SURF_DUR", 0, -depth), diving = ifelse(d_strata == "SURF_DUR", "surface", "depth"))    #adjust depth of surface durations from time at surface to 0m depth

#ggplot
p <- ggplot(mtmp, aes(x = etime, y = depth, colour = phase)) + geom_line() + labs(title = paste(unique(mtmp$tag), unique(mtmp$season), unique(mtmp$foray_idx3)))
ggsave(paste(unique(mtmp$tag), unique(mtmp$season), unique(mtmp$foray_idx3), ".pdf", sep = "_"), p, width = 12, height = 5)

############
###QUick plot exercises for mtg w/ andreas
tmp <- mutate(fdive, ratio = DIVE_DUR/SURF_DUR)
tmp <- melt(tmp, id.vars = c('tag', 'foray_idx3', 'season', 'phase', 'month'), 
                             measure.vars = c("ratio"))
tmp2 <- filter(tmp, value < 2000)

ggplot(data = tmp2, aes(x=value)) + geom_histogram() + facet_grid(~season, scale = "free")
