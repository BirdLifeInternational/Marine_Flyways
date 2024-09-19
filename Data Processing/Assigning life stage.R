library(dplyr)
library(sf)
library(tidyr)
library(geosphere)
library(amt)
library(ggplot2)
library(adehabitatLT)
library(rnaturalearth)
library(migrateR)
library(data.table)
library(move)
library(adehabitatHR)
library(eks)

# Open basemap
countries <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries")
# ne_load will give you an sp object (this is the old spatial format in R). 
borders <- st_as_sf(countries)# Convert the sp into an sf object (a newer spatial object class). Use sf package with function st_as_sf 

ggplot()+
  geom_sf(data = borders) +
  geom_point(data = subset, aes(x = longitude, y = latitude, colour = species)) +
  theme_bw()

#### USING SIMPLIFIED VERSION OF AMELINAEU METHODS #####

# find mid point of the distance travelled and then kernel each half. 
# kernels should be around breeding and non-breeding sites
# final point to leave core kernel is migration. points before are breeding/non-breeding

# run with Atlantic Ocean species only
sm.subset <- read.csv("./Atlantic Ocean/Cleaned Atl data_n = 542_equinox removed_interpolated.csv")

sm.subset <- sm.subset %>% 
  mutate(POSIX = ymd_hms(POSIX)) # change character to POSIXct
#  displacement already calculated and in column disp in master dataframe


# divide each dataset in two (new column - 1 or 2) if closer to colony or closer to non-breeding
# then plot this to check works
individuals <- data.frame(unique(sm.subset$track_id)) 
subset <- NULL

for (j in 1:nrow(individuals)) { # outer loop to select one individual at a time
  
  ID <- print(individuals[j,])
  subsetA <- sm.subset %>% filter(track_id == ID) # change data frame here for the different number of years
  
  for (i in 1:nrow(subsetA)) {
    
    limit <-  (max(subsetA$disp) / 2)
    limit2 <- (max(subsetA$disp) - min(subsetA$disp)) /2
    
    subsetA[i, "prox2col"] <- (ifelse(subsetA$disp[i] < limit, 1, 2))
    
    
    
    }
  subsetA$prox2col <- as.factor(subsetA$prox2col)
  subset <- rbind(subset, subsetA)
}

#plot one bird to check that dividing track in two has worked
ggplot()+
  geom_sf(data = borders)+
  geom_point(data = subset[which(subset$track_id == "1810_5H41344_109083"),], aes(x = longitude, y = latitude, colour = prox2col))+
  theme_bw()

#### Core Non-breeding areas ####
# Define the core non-breeding and breeding areas

#select one individual at a time
individuals <- data.frame(unique(subset$track_id))
# individualsA <- data.frame(unique(sm.subset2$track_id))
# individualsB <- anti_join(individuals,individualsA)
# MIGassigned <- NULL
# MIGassigned1 <- NULL
for (j in 1:nrow(individuals)) { # outer loop to select one individual at a time
  
  ID <- print(individuals[j,])
  subsetA <- subset %>% filter(track_id == ID) # 
  
  # for (i in 1:nrow(subsetA)) {
    
    # two dataframes = one for locations nearer to colony and one for locations nearer to non-breeding site
   nonbreed <- subsetA %>% 
  filter(prox2col == "2") 
  breed <- subsetA %>% 
  filter(prox2col == "1") 
  
  
  # create a core area around this polygon and identify after which point they never return to the core area
  nonbreedsf <- nonbreed %>%  
    sf::st_as_sf( coords=c("longitude","latitude"), crs = st_crs(4326))
  
  breedsf <- breed %>% 
    sf::st_as_sf(coords=c("longitude", "latitude"), crs = st_crs(4326))
    
  
   # calculate the kde of all contours (1 - 99) of the sf object for non  breeding and breeding sites
  kdeNB <- eks::st_kde(nonbreedsf)
  kdeNB$sf %>% 
    st_set_crs(st_crs(borders)) %>% 
    filter(contlabel == 90 | contlabel == 75 | contlabel == 50 | contlabel == 30 | contlabel == 25) %>% 
    ggplot()+  
    geom_sf(aes(fill=(contlabel)))+
    colorspace::scale_fill_discrete_sequential(palette="Heat2") +
    geom_sf(data=nonbreedsf, fill=NA, alpha = 0.3) +
    geom_sf(data = borders) +
    coord_sf(xlim = c((min(nonbreed$longitude - 10)),(max(nonbreed$longitude + 10))), ylim = c((min(nonbreed$latitude - 7.5)),(max(nonbreed$latitude + 7.5))))+
    # coord_sf(xlim = c(-65, 20), ylim = c(-65, 60))+
    ggtitle(paste0("Bird ",nonbreed$track_id[1],"\n",nonbreed$common_name[1]))+
    theme_bw()
  
  # save plots so they can be checked later
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/AtlOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",nonbreed$track_id[1],"_kernel contours around nonbreeding area.jpg"), dpi = 400, height = 8, width = 8)
  
   
    if(nrow(breedsf)>=8) {
       #Skips kernelling if less than 5 rows in breeding half of the plot
    
  kdeB <- eks::st_kde(breedsf) # 25% contour too large when not many breeding locations - use 20%
  kdeB$sf %>% 
    st_set_crs(st_crs(borders)) %>% 
    filter(contlabel == 90 | contlabel == 75 | contlabel == 50 | contlabel == 25 | contlabel == 10) %>% 
    ggplot()+  
    geom_sf(aes(fill=(contlabel)))+
    colorspace::scale_fill_discrete_sequential(palette="Heat2") +
    geom_sf(data=breedsf, fill=NA, alpha = 0.3) +
    geom_sf(data = borders) +
    coord_sf(xlim = c((min(breed$longitude - 10)),(max(breed$longitude + 10))), ylim = c((min(breed$latitude - 7.5)),(max(breed$latitude + 7.5))))+
    # coord_sf(xlim = c(-65, 20), ylim = c(-65, 60))+
    ggtitle(paste0("Bird ",nonbreed$track_id[1],"\n",nonbreed$common_name[1]))+
    theme_bw()
  
  # save plots so that they can be checked if needed
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/AtlOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",breed$track_id[1],"_kernel contours around breeding area.jpg"), dpi = 400, height = 8, width = 8)
  
    }else{
    
     kdeB <- NA # if there are not enough locations to kernel around the breeding site, no need to create a kdeB polygon 
  }
  # find first and final points in the breeding and non-breeding contours
  
  # NONBREEDING #
  # use the 30% contour and find first point in full tracking dataset after which no locations enter the polygon
  poly50NB <- kdeNB$sf %>% 
    st_set_crs(st_crs(borders)) %>% 
    filter(contlabel == 30)
  
  sf::sf_use_s2(FALSE)
  # Filter points that are inside the polygon
  pointsinpolyNB <- st_filter(nonbreedsf, poly50NB)
  # Sort the filtered points by timestamp
  pointsinpolyNB <- pointsinpolyNB[order(pointsinpolyNB$POSIX), ]
  
  # Extract the first point in the time series (when it entered the poly for the first time)
  first_pointNB <- pointsinpolyNB[1,]
  # Extract the last point in the time series (when it left the poly for the last time)
  final_pointNB <- pointsinpolyNB[nrow(pointsinpolyNB), ]
  
  
  if(nrow(breedsf)>=8) {
  # BREEDING #
  # use the 10% contour and find first point in full tracking dataset after which no locations enter the polygon
 
   poly20B <- kdeB$sf %>% 
    st_set_crs(st_crs(borders)) %>% 
    filter(contlabel == 10)
  
  # Filter points that are inside the polygon
  pointsinpolyB <- st_filter(breedsf, poly20B)
  # Sort the filtered points by timestamp
  pointsinpolyB <- pointsinpolyB[order(pointsinpolyB$POSIX), ]
  
  # Extract the first point in the time series (when it entered the poly for the first time)
  first_pointB <- pointsinpolyB[1,] 
  # note that in some cases there maybe no breeding locations, but may have some after the bird returns post-migration
  # in these cases need to ensure that the first_point is less than the non-breeding seasons. if not change to origin
  origintime <- anytime::anytime(0, asUTC = TRUE)
  first_pointB$POSIX <- if_else(first_pointB$POSIX > final_pointNB$POSIX, origintime, first_pointB$POSIX)
  
  # add filter that only looking for points less than 3 months apart- so as not to get any return to breeding season after migration at the other end of the year
  # Extract the last point in the time series (when it left the poly for the last time)
  final_pointB <- pointsinpolyB %>% 
    filter(POSIX < (pointsinpolyB$POSIX[1] + 7776000))  # remove any locations more than 90 days from first
  final_pointB <-  final_pointB[nrow(final_pointB), ]
  # note that in some cases there maybe no breeding locations, but may have some after the bird returns post-migration
  # in these cases need to ensure that the first_point is less than the non-breeding seasons. if not change to origin

  final_pointB$POSIX <- if_else(final_pointB$POSIX > final_pointNB$POSIX, origintime, final_pointB$POSIX)
  
  
  }else{
    origintime <- anytime::anytime(0, asUTC = TRUE)
    first_pointB <- origintime
     final_pointB <- max(breedsf$POSIX) + 31536000 # add one year (units = seconds)
    
  }
  
  # create a reenter breeding kernel at the end of return migration time point
  if(max(pointsinpolyB$POSIX) > max(pointsinpolyNB$POSIX)){ 
    T1 <- final_pointNB$POSIX
    
    reenterB <- pointsinpolyB %>%  filter(POSIX > T1)
    reenterB <- min(reenterB$POSIX)
    
  }else{
    
reenterB <- final_pointNB$POSIX + lubridate::years(10)  # add date 10 years into the future
  
}
  
  if(nrow(breedsf)>8) {
    subsetA <- subsetA %>% 
      mutate(mig.stage2 = if_else(POSIX <= final_pointB$POSIX, "0premig", 
                                  if_else(POSIX > final_pointB$POSIX & POSIX < first_pointNB$POSIX, "1outward",
                                          if_else(POSIX >= first_pointNB$POSIX & POSIX <= final_pointNB$POSIX, "2winter", 
                                                  if_else(POSIX >final_pointNB$POSIX & POSIX < reenterB, "3return", "4postmig")))))
   
    }else{
      subsetA <- subsetA %>% 
        mutate(mig.stage2 = if_else(POSIX >= first_pointNB$POSIX & POSIX <= final_pointNB$POSIX, "2winter", "3return"))
  }
  
  # add extra stage for cases where there are lots of locations after they return back at the colony
  
  # for (i in 1:nrow(subsetA)) {
  #   
  #   if(((subsetA[i, "POSIX"])) >= ((final_pointNB$POSIX)+5184000)){ # if the location time is more than 60 days after the final location in the non-breeding kernel, reclassify to postmigration
  #   subsetA[i,]$mig.stage2 <- "4postmig"
  # }
  # 
  # }
  # plot and compare the simplied assigning stage methods with Anne-Sophies visual assignments
  yrng <- range(sm.subset$latitude) # range of y axis
  xrng <- range(sm.subset$longitude) # range of x axis
  
  A <- ggplot()+
    geom_sf(data = borders)+
    geom_point(data = subsetA, aes(x = longitude, y = latitude, colour = mig.stage2))+
    # scale_color_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"))+
    coord_sf(xlim = c((min(subsetA$longitude - 7)),(max(subsetA$longitude + 7))), ylim = c((min(subsetA$latitude - 5)),(max(subsetA$latitude + 5))))+
    xlab("longitude")+
    ylab("latitude")+
    ggtitle(paste0("Bird",subsetA$track_id[1], "\n", subsetA$common_name[1]))+
    annotate("text", x = (xrng[1] + 10), y = (yrng[2]+5), label = "simplified auto-assign method", size = 2.5)+
    theme_bw()
  
    
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/AtlOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",subsetA$track_id[1],"simplified method to .jpg"), dpi = 500, height = 8, width = 14)
  
  
  # }
  MIGassigned <- rbind(MIGassigned, subsetA)
  
  }


write.csv(MIGassigned, "./Figures/Autoassigning methods/simplified methods/AtlOc sp. CLEANED_autoassign 30kde NB, 10kde B/a.Migration assigned_AtlOc datasets auto assign.csv", row.names = FALSE)


