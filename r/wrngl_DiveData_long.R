#script for 3D Home range work of GSM tagged gray seals
#collating a long data frame for beatrice's analysis
#rows represent individual estimates of dive depths encountered along a dive.

library(tidyverse)
library(here)
#load raw
load(here('data', 'dive_df_feb22.RData'))

dvdf_long <- dvdf %>% 
  select(season, pseg_fac, jday, tagday, foray_idx3, tag, pseg_idx, 
         surf_dur, dive_dur, max_dep, percent_area, d2s_m, phase_movt,
         matches('d\\d{1}\\b'), 
         contains('date'), contains('lon'), contains('lat')) %>% 
  gather(didx,  depth,
         -c(season, pseg_fac, jday, tagday, foray_idx3, tag, pseg_idx, 
            surf_dur, dive_dur, max_dep, percent_area, d2s_m, phase_movt,
            contains('date'), contains('lon'), contains('lat')))

dvdf %>% save(here('data', 'dive_long_feb22.Rdata'))
#dvdf %>% write_csv(here('data', 'dive_long_feb22.csv'))



#work with a scratch individual
tags <- unique(dive2$ref)

sc <- dive2 %>% filter(ref == tags[runif(1, 1, length(tags))])
#probably need to add a d0 column w/ 0
sc %>% 
  select(jday_r, 
         max_dep, starts_with("d")) %>% 
  select(-d11, -d12, -d13, -d14, -d15, -d16, -d17, -d18, 
    -d19, -d20, -d21, -d22, -d23, -d24, -d25, -d_speed) %>% 
  gather(strata, depth, 
         -jday_r, -max_dep, -de_date, -dive_dur, -depth_str, 
         -ds_date, -de_date_date, -de_date_time) %>% 
  arrange(jday_r) %>% 
  head(50) %>% view
#can manage later how to add max_dep
#don't miss percent area
#add in any other data transformations necessary from dive-annotation scripts
