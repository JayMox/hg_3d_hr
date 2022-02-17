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

return(dvdf_long)
#dvdf %>% save(file = here('data', 'dive_long_feb22.Rdata'))
#dvdf %>% write_csv(here('data', 'dive_long_feb22.csv'))


