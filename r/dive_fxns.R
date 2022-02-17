##############################
##Script for fxns manipulating dive data
##############################


#######
##Returns a dive dataset with segment indices that reference the movement segment they occured during
##takes in a list of evenly-spaced predicted locatiosn (p_pred) & dive records
##back is a conditional to control if dives are associated with the preceeding(FALSE)/postceeding(TRUE) fix
######
dive2gps.segs <- function(dive, p_pred, back, df){
  #create ts (zoo) objects
  require(zoo)
  require(lubridate)
  
  #convert ONLY time field & ts field (seg_idx, here)
  z_pred <- lapply(p_pred, function(x) zoo(x, order.by = x$tagday))
  z_dive <- dlply(dive, .(tag), function(x) zoo(x, order.by = x$tagday))
  
  #z_pred <- lapply(p_pred, function(x) zoo(x[,c(1, 38)], order.by = x$tagday))
  #z_dive <- dlply(dive[,c(83, 87)], .(tag), function(x) zoo(x, order.by = x$tagday))
  
  dive_segs <- lapply(z_dive, function(x) {
    preds <- z_pred[[which(names(z_pred) == unique(x$tag))]]    #extract the proper movement track
    z_link <- merge(x, preds[,c(1, 38)])                        #merge time series
    #z_link <- merge(x, preds, all = TRUE)
    tmp_dive <- filter(dive, tag == unique(x$tag)); 
    tmp_dive$ref <- droplevels(tmp_dive$ref); tmp_dive$tag <- droplevels(tmp_dive$tag);           
    #bc ts objs use matrices, data frame conversion comes with a shit ton of facts
    #build this temp. dive object to append the pseg_idx to preserve data frame class structure
    
    #associate each dive record with the segment index of the GPS data
    z_link$pseg_idx <- na.locf(z_link$seg_idx, fromLast = FALSE)
    ##ADD IN CODE TO FILL IN BACKWARDS (i.e., par backward = FALSE)
    
    #extract only dive records & salient data (time step & idx) and append to tmp_diveDF
    z_data <- as.data.frame(z_link[!is.na(z_link$DE_DATE), c(ncol(z_link)-4,ncol(z_link))]);
    colnames(z_data) <- c("jday", "pseg_idx");
    z_data$jday <- as.numeric(as.character(z_data$jday))
    
    #round off tagdays to make the merge work more cleanly
    z_data$jdayR <- round(z_data$jday, 4)
    tmp_dive$jdayR <- round(tmp_dive$jday, 4)
    
    z_final <- merge(tmp_dive, z_data, by = "jdayR", all.x = TRUE)
    
    #z_link <- z_link[!is.na(z_link$DE_DATE), -c(88,89)]     #subset only dive data; remove extra merge field
    #colnames(z_link)[ncol(z_link)-1] <- "tagday"
    #z_link <- as.data.frame(z_link)
    #return(cbind())
  })
  
  #return data in the structure requested by df
  if(df == TRUE) dive_segs <- do.call('rbind', dive_segs);
  return(dive_segs)
}

################################
##fxn to calculate dive metrics for track segments USING the GPSsegment index
##2/15; designed to take a list of individual tag ltraj's
##metrics available: "freq" = dive count;
###############################
dive_per_seg <- function(movt, dive, met){
  #if(class(movt)[1] == "ltraj") movt <- ld(movt)
  
  #set up working obs, bursts to cycle through, list for storing metrics, etc.
  burstID <- burst(movt);   #vector of bursts to cycle through
  activity <- list();       #empty list for stroing activity metrics of all bursts
  
  
}

#################################
##fxn to calculate dive metrics for track segments
##parameters needed: ltraj object, dive data, name of the metric desired 
##metrics available: "freq" = dive count;
################################
###Workflow:
#read in fxn lTraj obj; convert to df & force tz setting

####Notes:
##Need to separate out bursts
##CAN YOU DO THIS W/ DPLYR??  group by burst/id?; select data w/in track segments; summarise dive data
##LOOK INTO USING DPLYR WHEN DONE FOR TAG DEPLOYMENT
#####################################
dive_metric <- function(lt, dive, met){  
  burstID <- burst(lt);     #vector of the names of bursts
  activity <- list();       #list for storing activity metrics for all bursts
  for(j in 1:length(burstID)){
    #read in each burst of ltraj obj, transform to df, & force UTC as tz
    tmp <- ld(lt[burst(lt) == burstID[j]]); tmp$date <- with_tz(tmp$date, "UTC");
    dive_act <- as.vector(rep(NA, length = nrow(tmp)));  #vector to store dive metrics
    
    #loop through data records, extract time windows of track segments, subset dive data & calc metric
    for(i in 1:nrow(tmp)){
      if(i == 1) dive_act[i] <- 0; #tmp$dive[i] <- NA; 
      if(i != 1) {
        ######TIME ZONE ISSUE HERE!!!!!################
        window = interval(tmp$date[i-1], tmp$date[i]); #window to subset dive data across
        dtmp <- dive[which(dive$DE_DATE %within% window),]; #dive data w/in the window
        
        if(met == "freq") dive_act[i] <- nrow(dtmp);
        ##ADD OTHER DIVE METRICS HERE
      }
    }
    #normalize dive_freq
    dive_act1 <- dive_act/max(dive_act)
    #store data for each burst in list
    activity[[j]] <- dive_act1;    
  }
  #get whole data set & append new data
  tmp <- cbind(ld(lt), unlist(activity)); 
  colnames(tmp) <- c(colnames(tmp[-ncol(tmp)]), paste("dive", met, sep = "_"))
  return(dl(tmp))
}

##Function to sample bathymetry (saved as a .tif in GIS/PhysicalOceanography)
#dive is a df of dive data in UTM w/ utm_x & utm_y
#bathy is the file directory to a .tif file of seafloor depth
bathy_sampler <- function(dive, bathy){
  require(raster)
  require(sp)
  # require(plyr)
  # require(dplyr)
  library(tidyverse)
  require(raster)
  require(rgdal)
  
  #convert dive to spdf
  coords <- cbind(dplyr::select(dive, utm_x, utm_y))
  dive_spdf <- SpatialPointsDataFrame(coords, dive, proj4string = CRS("+proj=utm +zone=19 +north +datum=WGS84"))
  
  #read in bathymetry data
  bathy <- readGDAL(bathy)
  bathy_rast = raster(bathy, layer = 1, values = TRUE)
  
  #extract raster data at each loc of dive
  dive2 <- extract(bathy_rast, dive_spdf, method = "simple", sp = TRUE)
  
  #convert spdf back to data frame, rename bathy column
  dive_df <- as(dive2@data, "data.frame")
  dive_df <- plyr::rename(dive_df, replace = c("band1" = "bathy"))
  
  #return data frame
  return(dive_df)
}

###SHORE DISTANCE SAMPLER
#dive is the dive data df
#coast is the file location of the shoreline GIS layer in UTM coords
shore_dist_sampler <- function(dive, coast){
  require(raster)
  require(sp)
  # require(plyr)
  # require(dplyr)
  library(tidyverse)
  require(raster)
  require(rgdal)
  require(maptools)
  require(rgeos)
  #set projection
  utm.19 <- "+proj=utm +zone=19 +north +datum=WGS84"
  
  #read in coastline data
  coastUTM <- readShapeSpatial(coast)
  proj4string(coastUTM) <- CRS(utm.19)
  
  #extract locs & calc distance
  dive.pts <- SpatialPoints(dplyr::select(dive, utm_x, utm_y), proj4string = CRS(utm.19))
  dive$shoredist_m <- sapply(seq(1:nrow(dive)), function(i) gDistance(dive.pts[i], coastUTM))
  
  return(dive)
}

######
#Dive phases.. bottom (w/in .95 of max depth), descend (surface to first dive seg in bottom phase), 
#ascend (last dive seg in bottom phase to surface)
#calculates the # of depth segments & approximate duration of a dive's bottom phase
#bottom phase is defined as time at a depth below a specified threshold
dive_phases <- function(data, threshold, min_depth){
  #require(dplyr)
  library(tidyverse)
  data$dthresh <- round(data$MAX_DEP * threshold, 1)
  
  #caclulate bottom segs for dives deeper than the specified minimum depth
  data$bot_segs <- ifelse(data$MAX_DEP >= min_depth, apply(apply(select(data, D1:D9), 2, function(x) x >= data$dthresh), 1, sum), NA)
  #bottom segments are reduced by 1 b/c D1:D9 are right closed.. 
  #i.e, surface -> D1 is 1st segment (e.g., part of descent)
  #but don't subtract 1 if the dive contains no bottom segments
  data$bot_segs <- ifelse(data$bot_segs != 0, data$bot_segs - 1, data$bot_segs)
  data$bot_secs <- data$DIVE_DUR * (data$bot_segs/10)
  
  #descent/ascent phases
  #use match fxn to id depths w/in bottom phase, and then pull out first/last segment
  #for ascent, # of ascent segments is 10 minus the last depth w/in bottom phase
  data$dsc_segs <- ifelse(data$MAX_DEP >= min_depth, 
                          apply(apply(select(data, D1:D9), 2, function(x) match(x >= data$dthresh, TRUE)),
                                1, function(x) first(which(x == 1))), NA)
  data$asc_segs <- 10 - ifelse(data$MAX_DEP >= min_depth, 
                               apply(apply(select(data, D1:D9), 2, function(x) match(x >= data$dthresh, TRUE)),
                                     1, function(x) last(which(x == 1))), NA)
  
  #v-shaped dives lack a bottom phase (bot_segs = 0) & dsc/asc_segs are listed NA
  #for these, descent/ascent phases will divided by the maximum intermediate depth
  #the assumption is the max depth would occur just before/after max intermediate depth
  #data$dsc_segs <- ifelse(is.na(data$dsc_segs),  )
  #FINISH DEALING HERE!!
  
  #caclulate percentage & apply to dive duration for estimated time
  data$dsc_secs <- (data$dsc_segs/10) * data$DIVE_DUR
  data$asc_secs <- (data$asc_segs/10) * data$DIVE_DUR
  
  return(data)
}
#####


########
#Second dive phase fxn implements what dave & I agreed upon
#intermediate depths are static observations
#thus all depths > dthresh occur during bottom phase
#depths mark the end of the 10% segment they summarize, thus 1 unaccounted 10% segment after D9 (where should this go??)
#Could append this 10% to ascent, so long as first 10% is automatically appended to descent?
dive_phases2 <- function(data, threshold, min_depth){
  #require(dplyr)
  library(tidyverse)
  data$dthresh <- round(data$MAX_DEP * threshold, 1)
  
  #caclulate bottom segs for dives deeper than the specified minimum depth
  data$bot_segs <- ifelse(data$MAX_DEP >= min_depth, apply(apply(select(data, D1:D9), 2, function(x) x >= data$dthresh), 1, sum), NA)
  #bottom segments are the sum of all pts below threshold
  data$bot_secs <- data$DIVE_DUR * (data$bot_segs/10)
  ##DIVIDE BY 9.. 9 SAMPLES OF DEPTH, calculate proportion from that
  
  #descent/ascent phases
  #use match fxn to id depths w/in bottom phase, and then pull out first/last segment
  #for ascent, # of ascent segments is 10 minus the last depth w/in bottom phase
  data$dsc_segs <- ifelse(data$MAX_DEP >= min_depth, 
                          apply(apply(select(data, D1:D9), 2, function(x) match(x >= data$dthresh, TRUE)),
                                1, function(x) first(which(x == 1))), NA)
  data$asc_segs <- 10 - ifelse(data$MAX_DEP >= min_depth, 
                               apply(apply(select(data, D1:D9), 2, function(x) match(x >= data$dthresh, TRUE)),
                                     1, function(x) last(which(x == 1))), NA)
  
  #v-shaped dives lack a bottom phase (bot_segs = 0) & dsc/asc_segs are listed NA
  #for these, descent/ascent phases will divided by the maximum intermediate depth
  #the assumption is the max depth would occur just before/after max intermediate depth
  #data$dsc_segs <- ifelse(is.na(data$dsc_segs),  )
  #FINISH DEALING HERE!!
  
  #caclulate percentage & apply to dive duration for estimated time
  data$dsc_secs <- (data$dsc_segs/10) * data$DIVE_DUR
  data$asc_secs <- (data$asc_segs/10) * data$DIVE_DUR
  
  return(data)
}

#custom fxn to estimate dive phases in low-res dive data (e.g., N-number of evenly spaced depth samples) & return an updated df
#requires a threshold to define %age of max depth considered "bottom phase", e.g., .95
#requires min_depth as a minimum max depth to calculate dive phase metrics
dive_phases3 <- function(data, thresh, min_depth){
  #require(dplyr)
  libary(tidyverse)
  #append dthresh for accounting
  data$dthresh <- round(data$MAX_DEP * thresh, 1);
  
  #identify depth samples w/in bottom phase depths
  tmp <- apply(select(data, D1:D9), 2, function(x) findInterval(x/data$MAX_DEP, c(0, thresh)))
  
  ##BOTTOM PHASE
  #identify bottom time for dives deeper than min_depth
  data$bot_segs <- sapply(1:nrow(data), function(i) ifelse(data$MAX_DEP[i] >= min_depth, sum(tmp[i,] == 2), NA))
  
  ##ASCENT/DESCENT PHASES
  #descent/ascent phases are leading/trailing runs of depth samples until the first/last sample w/in the bottom phase
  #N.B.: 1's are the interval 0 - thresh; 2's are the interval accounting for bottom phase
  #2nd ifelse statement tests if the depth samples begin/end in the bottom phase)
  data$dsc_segs <- sapply(1:nrow(data), function(i) ifelse(data$MAX_DEP[i] >= min_depth, ifelse(first(rle(tmp[i,])$values == 1), first(rle(tmp[i,])$lengths), 0), NA))
  data$asc_segs <- sapply(1:nrow(data), function(i) ifelse(data$MAX_DEP[i] >= min_depth, ifelse(last(rle(tmp[i,])$values == 1), last(rle(tmp[i,])$lengths), 0), NA))
  
  ##VEE SHAPE DIVES ADJUSTMENT
  #b/c no bottom phase, asc/dsc phase on v-shaped dives needs to be handled separately
  #descent/ascent phases are separated by the depth sample closest to max_dep
  #depth sample closest to MAX_DEP is split evenly bw asc/dsc phase
  data$dsc_segs <- ifelse(data$bot_segs == 0, sapply(1:nrow(data), function(i) which.min(abs(as.numeric(select(data[i,], D1:D9)) - data[i,]$MAX_DEP)) - .5), data$dsc_segs)
  data$asc_segs <- ifelse(data$bot_segs == 0, sapply(1:nrow(data), function(i) 9.5 - which.min(abs(as.numeric(select(data[i,], D1:D9)) - data[i,]$MAX_DEP))), data$asc_segs)
  
  #non segs are segments not w/in the bottom phase, but not in the leading/trailing legs
  data$non_segs <- sapply(1:nrow(data), function(i) ifelse(data$MAX_DEP[i] >= min_depth, 9 - sum(data$bot_segs[i], data$dsc_segs[i], data$asc_segs[i]), NA))
  
  #calculate durations
  data$bot_secs <- data$DIVE_DUR * (data$bot_segs/9)
  data$dsc_secs <- data$DIVE_DUR * (data$dsc_segs/9)
  data$asc_secs <- data$DIVE_DUR * (data$asc_segs/9)
  
  return(data)
}