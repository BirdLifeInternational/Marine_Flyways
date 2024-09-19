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

##### SCRIPT CONTENTS #####
# 1. opening and sorting tracking data
# 2. calculate net squared displacement (two methods)
# 3. Lavielle segmentation
# 4. migrateR to calculate start date of migration
# 5. core non breeding areas

#### 1. open data and sort ####
tracks <- read.csv("STDB_data from Anne-Sophie_migration and winter periods already separated.csv")

# open fully filtered datasets
MIG <- read.csv("data filtered by migration based on displacement plots_MIGRATION.v8_reclassified including additional data Jul23.csv")
NONMIG <- read.csv("data filtered by migration based on displacement plots_NONMIGRATION.v8_reclassified including additional data Jul23.csv")

MIG <- MIG %>% 
  mutate(POSIX = as.POSIXct(POSIX,  tz = "UTC"))

# # In AS dataset change the date_time column into a posix and remove the letters
# tracks$date_time <- stringr::str_replace_all(tracks$date_time, c("T" = " ", "Z" = "")) # remove letters
# tracks <- tracks %>% 
#   mutate(POSIX = as.POSIXct(date_time,  tz = "UTC")) # add  additional column with a POSIXct class date and time
# 
# # add lat lon of deployment for selected colonies
# tracks <- tracks %>% 
#   mutate(deploy_lat = "NA", deploy_lon = "NA" ) 
# 
# tracks$deploy_lat <- as.numeric(if_else(tracks$colony == "Kidney Island FK","-51.624164" , 
#                              if_else(tracks$colony == "Selvagem","30.097680" , "NA")))
# tracks$deploy_lon <- as.numeric(if_else(tracks$colony == "Kidney Island FK","-57.751911 ", 
#                              if_else(tracks$colony == "Selvagem","-15.952657" , "NA")))
# 
# 
# # Use a smaller dataset whilst working on script
# # filter any NAs in colony location. This will only leave a few species
# subset <- tracks %>% 
#   drop_na(deploy_lat)

# plot locations
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


# select one to test
sm.subset <- MIG %>% filter(AS == "Yes")
ggplot()+
  geom_sf(data = borders) +
  geom_point(data = sm.subset, aes(x = longitude, y = latitude, colour = common_name)) +
  theme_bw()

# alternative dataframe: 
# run with Indian Ocean species only
sm.subset <- read.csv("./Indian Ocean/Cleaned Ind data_n = 156_equinox removed_GLS only no interpolation.csv")
# sm.subset <- MIG %>% 
#   filter(track_id %in% tracklist$track_id) %>% 
#   mutate(common_name = as.factor(common_name))

# run with Atlantic Ocean species only
sm.subset <- read.csv("./Atlantic Ocean/Cleaned Atl data_n = 542_equinox removed_interpolated.csv")
# run with Pacific Ocean species only
sm.subset <- read.csv("./Pacific Ocean/Cleaned Pac data_n = 530_equinox removed_interpolated.csv")
# run with Southern Ocean species only
sm.subset <- read.csv("./Southern Ocean/Cleaned Southern data_n= 88_equinox removed.csv")

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
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",nonbreed$track_id[1],"_kernel contours around nonbreeding area.jpg"), dpi = 400, height = 8, width = 8)
  
   
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
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",breed$track_id[1],"_kernel contours around breeding area.jpg"), dpi = 400, height = 8, width = 8)
  
    }else{
    
     kdeB <- NA # if there are not enough locations to kernel around the breeding site, no need to create a kdeB polygon 
  }
  # find first and final points in the breeding and non-breeding contours
  
  # NONBREEDING #
  # use the 50% contour and find first point in full tracking dataset after which no locations enter the polygon
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
  # use the 20% contour and find first point in full tracking dataset after which no locations enter the polygon
 
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
  
  # ONLY RUN ON DATASETS WHERE AS assigned lifestage
  # plot and compare the simplied assigning stage methods with Anne-Sophies visual assignments
  # B <- ggplot()+
  #   geom_sf(data = borders)+
  #   geom_point(data = subsetA, aes(x = longitude, y = latitude, colour = stage))+
  #   scale_color_manual(values = c("#7CAE00", "#00BFC4", "#C77CFF"))+
  #   coord_sf(xlim = c((min(subsetA$longitude - 7)),(max(subsetA$longitude + 7))), ylim = c((min(subsetA$latitude - 5)),(max(subsetA$latitude + 5))))+
  #   xlab("longitude")+
  #   ylab("latitude")+
  #   ggtitle(paste0("Bird",subsetA$track_id[1], "\n", subsetA$common_name[1]))+
  #   annotate("text", x = (xrng[1] + 10), y = (yrng[2]+5), label = "A-S visual assign method", size = 2.5)+
  #   theme_bw()
  # 
  # 
  # cowplot::plot_grid(A, B, ncol = 2)
  
  ggsave(plot = last_plot(), paste0("./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/bird",subsetA$track_id[1],"simplified method to .jpg"), dpi = 500, height = 8, width = 14)
  
  
  # }
  MIGassigned <- rbind(MIGassigned, subsetA)
  
  }


# Remove any individuals after checking and fix two classifications
MIGassigned1 <- MIGassigned %>% 
  filter(track_id != "1321_bs20561_79171" & track_id != "1321_bs25422_79195" & track_id != "1319_bs7625_79024" &
           track_id != "1319_bs21136_78894"  & track_id != "1321_bs20054_79165" & track_id != "462_5142629_1779" &
           track_id != "495_1332082_3508" & track_id != "1319_bs4666_78955" &track_id != "1319_bs6791_79020" &
           track_id != "1322_bs5115_79269" & track_id != "1322_bs11675_79238" & track_id != "1322_bs25236_79264" &
           track_id != "1405_518393_83121" & track_id != "1405_5183013_83121" & track_id != "95148" &track_id != "95151" & track_id != "95178")

MIGassigned1$mig.stage2[which(MIGassigned1$track_id == "95166" & MIGassigned1$mig.stage2 == "0premig")] <- "1outward"
MIGassigned1$mig.stage2[which(MIGassigned1$track_id == "495_1332079_3507" & MIGassigned1$mig.stage2 == "0premig")] <- "1outward"


write.csv(MIGassigned1, "./Figures/Autoassigning methods/simplified methods/SouthOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_SOc datasets auto assign.csv", row.names = FALSE)


# Remove any individuals after checking and fix two classifications
MIGassigned1 <- MIGassigned 
MIGassigned1$mig.stage2[which(MIGassigned1$track_id == "1810_5H41062_109055" & MIGassigned1$mig.stage2 == "0premig")] <- "1outward"
MIGassigned1$mig.stage2[which(MIGassigned1$track_id == "1810_5H41303_109076" & MIGassigned1$mig.stage2 == "4postmig")] <- "3return"
MIGassigned1$mig.stage2[which(MIGassigned1$track_id == "1810_5H41848_109124" & MIGassigned1$mig.stage2 == "0premig")] <- "1outward"

write.csv(MIGassigned1, "./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/a.Migration assigned_IndOc datasets auto assign.csv", row.names = FALSE)


# open the seperate migassigned csvs and merge into one

MIGassigned1<- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_AtlOc datasets auto assign_individuals 1 to 200.csv")
MIGassigned2<- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_AtlOc datasets auto assign_individuals 201 to 400.csv")
MIGassigned3<- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_AtlOc datasets auto assign_individuals 401 to 516.csv")
MIGassigned4<- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_AtlOc datasets auto assign_individuals 517 to 542.csv")

MIGassignedA <- rbind(MIGassigned1, MIGassigned2)
MIGassignedB <- rbind(MIGassigned3, MIGassigned4)
MIGassigned <- rbind(MIGassignedA, MIGassignedB)

#remove duplicates
MIGassigned <- MIGassigned %>% 
  group_by(track_id) %>% 
  dplyr::distinct()

write.csv(MIGassigned, "./Figures/Autoassigning methods/simplified methods/PacOc sp. autoassign 30kde NB, 10kde B/a. Migration assigned_ALL PacOc datasets auto assign_individuals 1 to 542.csv", row.names = FALSE)


ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y, color = mig.stage2))+
  # scale_color_manual(values = c("red", "black"))+
  scale_color_manual(values = c("red", "black"))+
  geom_path(data = total.path.df.8280, aes(x = x, y = y, color = (mig.stage2), group = 1), lwd = 1.2, alpha = 0.4)+
  geom_sf(data = poly90, fill = "darkgreen", alpha = 0.5)+
  annotate("text", x = -30, y = -40, label = "Bird 60868\nCalonectris borealis", size = 3)+
  xlim(-60, -10) +
  xlab("longitude")+
  ylim(-45, 40) +
  ylab("latitude")+
  ggtitle("migratory dates, based on 90% kernel \naround nonbreeding area") + # change depending on the p value in mvmdt function
  # ggtitle("migratory dates, corresponding to 10% and \n90% of the movement completion")+
  theme_bw()


#### BELOW METHODS NO LONGER NEEDED ####

##### USING MORE COMPLEX METHODS FROM PAPER AMELINAEU #####
#### 2. Net Squared Displacement ####
# NOTE: ideally want net squared colony displacement (i.e. the displacement distance from colony rather than the first location)
# calculating displacement distance
subset$disp <- NA

for (i in 1:nrow(subset)) {
  
  # firstly change the GLS and colony locations into spatial dataframes (sf)
  p <- st_sfc(st_point(c(subset[i,"longitude"],subset[i,"latitude"])), crs = 4326)
  q <- st_sfc(st_point(c(subset[i,"deploy_lon"], subset[i,"deploy_lat"])), crs = 4326)
  
  # option 1 to calculate distance between two points: 
  # test[i,"disp"] = st_distance(p,q) %>% 
  #   set_units("km") %>% 
  #   drop_units()
  
  # option 2 to calculate distance between 2 points
  subset[i,"disp"] = geosphere::distGeo(c(subset[i,"longitude"],subset[i,"latitude"]),c(subset[i,"deploy_lon"], subset[i,"deploy_lat"]))/1000
  
}

# plot displacement
ggplot(subset)+
  geom_line(aes(POSIX, disp, colour = species), lwd = 1.3) +
  xlab("Date") +
  ylab("Displacement from colony (km)") +
  theme_bw()


# two package options to calculate NSD: 
# 1: adehabitatLT::summary.ltraj() - gives a summary including column R2n = net squared displacement
# 2: amt:nsd - requires data to be in 'track' format and calculates from first location, not a set colony location


# work on an even smaller subset for now
# sm.subset <- subset %>% filter(bird_id == "8280" | bird_id == "34137"| bird_id ==  "23758")  # try with three birds
# sm.subset <- subset %>% filter(bird_id == "88646093" | bird_id == "88646075" | bird_id == "88646100")
sm.subset <- subset %>% filter(bird_id == "43822" | bird_id == "60868"| bird_id ==  "74133")
# plot the three birds and check that they complete a full migration cycle
ggplot(sm.subset)+
  geom_line(aes(POSIX, disp, colour = bird_id), lwd = 1.3) +
  xlab("Date") +
  ylab("Displacement from colony (km)") +
  theme_bw()

# # 1, adehabitatLT - uses sp so is it better to avoid as sp will no longer be supported next year?
# # create an ltraj object - the R2n column = net squared displacement
# subset.ltraj <- adehabitatLT::as.ltraj(xy = sm.subset[,c("longitude", "latitude")], 
#                                        date = sm.subset$POSIX, id = sm.subset$bird_id)
# (subset.ltraj[[1]]) # an ltraj is a list of each individual - check that it contains all the info for the first individual
# plot(subset.ltraj) # will plot the locations of all individuals 
# 
# # double check that this looks right by plotting the locations on a map
# ggplot()+ 
#   geom_sf(data = borders) +
#   geom_point(data = sm.subset, aes(longitude, latitude, colour = bird_id)) + 
#   theme_bw()
# 
# # change the ltraj into a dataframe, then can use the NSD from the R2n column
# total.path.df <- data.frame(subset.ltraj[[1]], id = attr(subset.ltraj[[1]], "id")) # first df of one individual
# 
# # ld funcation also converts ltraj to a dataframe
# # testing <- ld(subset.ltraj)
# 
# # then use a 'for' loop to fill the larger dataframe with the rest of the trajectories for n individuals.
# for(i in 2:length(subset.ltraj)) {
#   total.path.df <- rbind(total.path.df, 
#                          data.frame(subset.ltraj[[i]], id = attr(subset.ltraj[[i]], "id")))
# }
# 
# ggplot(total.path.df)+
#   geom_line(aes(date, R2n, colour = id), lwd = 1.3) + 
#   xlab("Date") +
#   ylab("NSD from colony (km)") +
#   theme_bw()
# 
# ggplot() + # check again the data are still correct (i.e. mapping as would expect)
#   geom_sf(data = borders)+
#   geom_point(data = total.path.df, aes(x,y, colour = id))
# 
#  # second NSD method: amt package function nsd calculates the net squared displacement from the first location (not a set colony location)
# 
# # create a 'track' object first to use amt::nsd
# # test2 <- make_track(test, longitude, latitude, POSIX, id = bird_id, crs = 4326) # add ID to restart calculation for each bird
# test2 <- make_track(sm.subset, longitude, latitude, POSIX, id = bird_id, crs = 4326)
# 
# disp <-data.frame(amt::add_nsd(test2)) # the output is the sum of (locx1 - locX2)^2 and  (locY1 - locY2)^2
# # it hasn't sqrt although online reading suggests that the NSD is the sqrt?
# 
# # net squared displacement - the square of the Euclidean distance from origin point
# 
# ggplot(disp)+
#   geom_line(aes(t_, nsd_, colour = id), lwd = 1.3) + 
#   xlab("Date") +
#   ylab("NSD from colony (km)") +
#   theme_bw()


# compare the two NSD methods and the method not using a function to calculate

# total.path.df <- total.path.df %>% 
#   rename(nsd = R2n)
# disp <- disp %>% 
#   rename(date = t_, x = x_, y = y_)
# sm.subset <- sm.subset %>% 
#   rename(x = longitude, y = latitude, nsd_ = disp)
# 
# compareNSDs <- full_join(total.path.df, disp,  keep = TRUE) %>% 
#   mutate(diffNSD = nsd_ - nsd)


# ERROR TO SOLVE: for two birds the NSDs are different - why?? One from colony, one from first recorded location
# method one is from the colony, so going forward use this method

total.path.df <- sm.subset %>% 
  rename(x = longitude, y = latitude, nsd = disp, id = bird_id, date = date_time) %>% 
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#### 3. Laveielle segmentation  ####

# Lmin = 5 and Kmax = 15 were the values from Amelineau et al 2021 MEPS
# have tried reducing here because was pulling out too many segments on the example species
# we really just want to identify three correctly - outbound migration, wintering and return migration
# any key stopover sites can be identified with other methods such as kernels.

# try setting the Kmax to 9 as this is 3 times the expected number of segments (see methods in Amelineau)

lon <- adehabitatLT::lavielle(total.path.df$x[which(total.path.df$id =="60868")], 
                              Lmin = 5, Kmax = 9, type = "mean")  # for ONE bird
chooseseg(lon)

lat <- adehabitatLT::lavielle(total.path.df$y[which(total.path.df$id =="60868")], 
                              Lmin = 5, Kmax = 9, type = "mean") # Lmin and Kmax values same as in Amelineau et al 2021 MEPS 6 pelagicc species...
chooseseg(lat)

NSD <- adehabitatLT::lavielle(total.path.df$nsd[which(total.path.df$id =="60868")], 
                              Lmin = 5, Kmax = 9, type = "mean") # Lmin and Kmax values same as in Amelineau et al 2021 MEPS 6 pelagicc species...
chooseseg(NSD)

# when visualising using the chooseseg() function there is a clear break in the decrease of the contrast at K = 3

kk <- findpath(NSD, 2) # NOTE that the limits are the midpoint of a migratory movement
k1 <- findpath(lon, 2)
k2 <- findpath(lat, 2)

# QUESTION: can there be different numbers of segments for each variable, and how will this affect automation?

# index (x axis relates to the number of rows of data i.e. there are only 200 locations for bird 8280)
# series is the variable (i.e. lat / lon/ NSCD)

# need to work out the segmentation for each individual - they cannot be segmented together
# TO DO: will need to put into a loop

k2
seg <- 1
total.path.df.8280 <- total.path.df %>% filter(id == "60868")
firstseg <- total.path.df.8280$nsd[kk[[seg]][1]:kk[[seg]][2]]

# alternatively can use the ltraj object (however, this will not calculate the lon, lat and NSCD seperately)
bird.8280 <-subset.ltraj[3] # change number in square bracket to select one bird at a time
plot(bird.8280)
# Show the changes in the distance between successive relocations with the time
plotltr(bird.8280, "R2n") # can change what is plotted: R2n = nsd, dist = length of each move, dx = increase of the move in x direction, dy = increase of the move in y direction, dt = time interval, x = x coord, y = y coord)

lon <- lavielle(bird.8280[[1]][['x']], Lmin = 5, Kmax = 9) # use the list selection (double brackets) to select the appropriate column (i.e. x is column 1 in list, y is column 2, NSD is column 8)
chooseseg(lon)
k1 <- findpath(lon, 3)

lat <- lavielle(bird.8280[[1]][['y']], Lmin = 5, Kmax = 9)
chooseseg(lat)
k2 <- findpath(lat, 3)

NSD <- lavielle(bird.8280[[1]][['R2n']], Lmin = 5, Kmax = 9)
chooseseg(NSD)
kk <- findpath(NSD, 3)

# if want to visualise the plot with seperate segments need to divide the list as the output below shows. 
# the three sections do need to be segmented separately (as per Amelineau), but could try and format as seen below
# fullseg <- lavielle(bird.8280, Lmin = 5, Kmax = 9)
# chooseseg(fullseg)
# kk <- findpath(fullseg, 3)
# plot(kk) # what it is doing - when all together dividing the lists up into 3 (segments) and plotting subsequently


# plot the bird in example - check that the lats and longs make sense
total.path.df %>% 
  filter(id == "60868") %>% 
ggplot()+ 
  geom_point( aes(x, y ), colour = "blue") + 
  xlab("longitude")+
  ylab("latitude")+
  xlim(-60, -10) + # adjust limits as needed for each bird
  ylim(40, -45)+
  geom_sf(data = borders, alpha = 0.3) +
  theme_bw()

total.path.df %>% 
  filter(id == "60868") %>% 
ggplot()+
  geom_line(aes(date, nsd, colour = id), lwd = 1.3) + 
  xlab("Date") +
  ylab("NSD from colony (km)") +
  theme_bw()


# pull out the day of each segment
# make into a table with the start and end day as a separate column and each row as a lavielle segment
k2[1] # change the number for the start and end of each segment (i.e. segment 1, segment 2... segment Kmax - 1)
k2[1:3] # all segments (last number is Kmax - 1)

 data.table(latsegstart = c(k2[[1]][[1]], k2[[2]][[1]]), #, k2[[3]][[1]]
           lonsegstart = c(k1[[1]][[1]], k1[[2]][[1]]), # , k1[[3]][[1]]
           NSDsegstart = c(kk[[1]][[1]], kk[[2]][[1]])) #, kk[[3]][[1]]


# list all the dates when segments occur, and then average the three if occur less than 7 days apart

seg.date <-  c(total.path.df.8280$date[kk[[1]][[1]]], 
               total.path.df.8280$date[k1[[1]]][[1]],
               total.path.df.8280$date[k2[[1]][[1]]],
               total.path.df.8280$date[kk[[2]][[1]]],
               total.path.df.8280$date[k1[[2]][[1]]],
               total.path.df.8280$date[k2[[2]][[1]]] #,
               # total.path.df.8280$date[kk[[3]]][[1]],
               # total.path.df.8280$date[k1[[3]]][[1]],
               # total.path.df.8280$date[k2[[3]][[1]]]
               )


# to avoid detecting duplicate events for lat, lon and NSCD calculate the mean event for days that are < 7 days apart  
# (bird 8280) the third segments are within seven days of each other, so calculate the mean

difftime(total.path.df.8280$date[k1[[2]]][[1]],total.path.df.8280$date[k2[[2]][[1]]]) # check the time difference
mean.calc <- c(total.path.df.8280$date[kk[[2]]][[1]], #change on an individual bird basis
               total.path.df.8280$date[k2[[2]]][[1]])
qq <- mean.POSIXct(mean.calc)

mean.calc <- c(total.path.df.8280$date[kk[[2]]][[1]], #change on an individual bird basis
               total.path.df.8280$date[k1[[2]]][[1]],
               total.path.df.8280$date[k2[[2]][[1]]])
q <- mean.POSIXct(mean.calc)

plotsegdate <- ( c(seg.date[3], q)) # adjust depending on which values you need to include (removing duplicates) seg.date[3:6],q

plotsegdate <- plotsegdate[order(plotsegdate)] # put in chronological order

plotA <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, x)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  ylab("longitude")+
  theme_bw()

plotB <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, y)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  ylab("latitude")+
  theme_bw()

plotC <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, nsd)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  ylab("net squared displacement") +
  theme_bw()

cowplot::plot_grid(plotA,plotB,plotC, nrow = 3)

# in paper also added mean value for each segment (but possibly uneccessary to run for every individual, just as a way to explain in paper?
# calculations below

total.path.df.8280 <- total.path.df.8280 %>% # first label the segments in a new column
  mutate(segment = 
           (if_else(date >= as.POSIXct(plotsegdate[1], tz = "UTC") & date < as.POSIXct(plotsegdate[2], tz= "UTC"), 
                    "1", "2"
                    # if_else(date >= as.POSIXct(plotsegdate[2], tz= "UTC") & date < as.POSIXct(plotsegdate[3], tz = "UTC"),
                    #         "2", 
                            # if_else(date >= as.POSIXct(plotsegdate[3], tz = "UTC") & date < as.POSIXct(plotsegdate[4], tz = "UTC"), 
                            #         "3", "4"
                                    # if_else(date >= as.POSIXct(plotsegdate[4], tz = "UTC") & date < as.POSIXct(plotsegdate[5], tz = "UTC"),
                                    #         "4","5"
                                            )))#)#)#)

# plot the segments to visualise where they divided. (remember they are midpoint of a migratory movement)
ggplot()+
  geom_sf(data = borders)+
  geom_point(data= total.path.df.8280, aes(x = x, y = y, colour = segment))+
  # xlim(-40, 20) +
  xlim(-60, -10) + # adjust limits as needed for each bird
  ylim(-45, 40)+
  xlab("longitude")+
  # ylim(-50, 30) +
  ylab("latitude")+
  theme_bw()
ggsave(plot = last_plot(), "Lavielle segmentation_example bird 60868_map of segments.jpg", dpi = 400, width = 8, height = 8)

# calculate the mean for each segment and the lat/lon/NSD
av.x <- aggregate(data = total.path.df.8280, x ~ segment,FUN= "mean") 
av.y <- aggregate(data = total.path.df.8280, y ~ segment,FUN= "mean")
av.nsd <- aggregate(data = total.path.df.8280, nsd ~ segment,FUN= "mean")

q<- inner_join(av.x, av.y)
av.x.y.nsd <- inner_join(q, av.nsd)

plotAA <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, x)) +
  geom_vline(xintercept = plotsegdate, # add red lines for each segment
             colour = "red",
             lwd = 1.2) +
  geom_vline(xintercept = max(total.path.df.8280$date),
             colour = "red", 
             lwd = 1.2)+
  geom_segment(data = av.x.y.nsd, (aes(x = plotsegdate[1], y = x[1], xend = plotsegdate[2], yend = x[1])), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[2], y = x[2], xend = plotsegdate[3], yend = x[2]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[3], y = x[3], xend = plotsegdate[4], yend = x[3]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[4], y = x[4], xend = plotsegdate[5], yend = x[4]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[5], y = x[5], xend = max(total.path.df.8280$date), yend = x[5]), linetype = 2)+
  ylab("longitude")+
  theme_bw()

plotAB <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, y)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  geom_segment(data = av.x.y.nsd, (aes(x = plotsegdate[1], y = y[1], xend = plotsegdate[2], yend = y[1])), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[2], y = y[2], xend = plotsegdate[3], yend = y[2]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[3], y = y[3], xend = plotsegdate[4], yend = y[3]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[4], y = y[4], xend = plotsegdate[5], yend = y[4]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[5], y = y[5], xend = max(total.path.df.8280$date), yend = y[5]), linetype = 2)+
  geom_vline(xintercept = max(total.path.df.8280$date),
             colour = "red", 
             lwd = 1.2)+
  ylab("latitude")+
  theme_bw()

plotAC <- ggplot() +
  geom_line(data = total.path.df.8280, aes(date, nsd)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  geom_segment(data = av.x.y.nsd, (aes(x = plotsegdate[1], y = nsd[1], xend = plotsegdate[2], yend = nsd[1])), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[2], y = nsd[2], xend = plotsegdate[3], yend = nsd[2]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[3], y = nsd[3], xend = plotsegdate[4], yend = nsd[3]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[4], y = nsd[4], xend = plotsegdate[5], yend = nsd[4]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[5], y = nsd[5], xend = max(total.path.df.8280$date), yend = nsd[5]), linetype = 2)+
  geom_vline(xintercept = max(total.path.df.8280$date),
             colour = "red", 
             lwd = 1.2)+
  ylab("net squared dsiplacement")+
  theme_bw()

cowplot::plot_grid(plotAA, plotAB, plotAC, nrow = 3)

ggsave(plot = last_plot(), "Lavielle segmentation_example bird 60868.jpg", height = 10, width = 10, dpi = 300)
# used centroid of locations within each segment to plot simplified trajectories between the stationary sections

#### 4.  migrateR (section 2.3.2 of https://www.int-res.com/articles/meps_oa/m676p127.pdf) ####

# step 1 
# cut at the midpoint of each Lavielle segment so that the new segments contain the second half of one Lavielle segment and the first half of the next
# find mid-time point of middle segments (in example bird 8280 there are 5 segments, need the midpoint of segment 2:4)

NS1 <- median(total.path.df.8280$date[which(total.path.df.8280$segment == 2)])
NS2 <- median(total.path.df.8280$date[which(total.path.df.8280$segment == 3)])
NS3 <- median(total.path.df.8280$date[which(total.path.df.8280$segment == 4)])
allNS <- c( NS1, NS2, NS3)

# plot and check if as expected
ggplot() +
  geom_line(data = total.path.df.8280, aes(date, nsd)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  geom_segment(data = av.x.y.nsd, (aes(x = plotsegdate[1], y = nsd[1], xend = plotsegdate[2], yend = nsd[1])), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[2], y = nsd[2], xend = plotsegdate[3], yend = nsd[2]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[3], y = nsd[3], xend = plotsegdate[4], yend = nsd[3]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[4], y = nsd[4], xend = plotsegdate[5], yend = nsd[4]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[5], y = nsd[5], xend = max(total.path.df.8280$date), yend = nsd[5]), linetype = 2)+
  geom_vline(xintercept = max(total.path.df.8280$date),
             colour = "red", 
             lwd = 1.2)+
  geom_vline(xintercept = allNS, colour = "darkgreen", linetype = 2, lwd = 1.5)+
  ylab("net squared dsiplacement")+
  theme_bw()


total.path.df.8280 <- total.path.df.8280 %>% 
  mutate(seg4migrateR = if_else(date >= min(date) & date < allNS[1] ,"1", "2"))
                                if_else(date >= allNS[1] & date < allNS[2], "2", 
                                        if_else(date >= allNS[2] & date < allNS[3], "3", "3"))))

# plot and check if as expected
ggplot() +
  geom_line(data = total.path.df.8280, aes(date, nsd)) +
  geom_vline(xintercept = plotsegdate,
             colour = "red",
             lwd = 1.2) +
  geom_segment(data = av.x.y.nsd, (aes(x = plotsegdate[1], y = nsd[1], xend = plotsegdate[2], yend = nsd[1])), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[2], y = nsd[2], xend = plotsegdate[3], yend = nsd[2]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[3], y = nsd[3], xend = plotsegdate[4], yend = nsd[3]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[4], y = nsd[4], xend = plotsegdate[5], yend = nsd[4]), linetype = 2)+
  geom_segment(data = av.x.y.nsd, aes(x = plotsegdate[5], y = nsd[5], xend = max(total.path.df.8280$date), yend = nsd[5]), linetype = 2)+
  geom_vline(xintercept = max(total.path.df.8280$date),
             colour = "red", 
             lwd = 1.2)+
  geom_vline(xintercept = allNS, colour = "darkgreen", linetype = 2, lwd = 1.5)+
  annotate("rect", xmin = min(total.path.df.8280$date), xmax = max(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR==1)]),
           ymin = min(total.path.df.8280$nsd), ymax = max(total.path.df.8280$nsd),
           alpha = .25, fill = "darkgreen")+
  annotate("rect", xmin = min(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR == 2)]), xmax = max(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR==2)]),
           ymin = min(total.path.df.8280$nsd), ymax = max(total.path.df.8280$nsd),
           alpha = .25, fill = "darkblue")+
  annotate("rect", xmin = min(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR == 3)]), xmax = max(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR==3)]),
           ymin = min(total.path.df.8280$nsd), ymax = max(total.path.df.8280$nsd),
           alpha = .25, fill = "darkorange")+
  annotate("rect", xmin = min(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR == 4)]), xmax = max(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR==4)]),
           ymin = min(total.path.df.8280$nsd), ymax = max(total.path.df.8280$nsd),
           alpha = .25, fill = "darkred")+
  ylab("net squared dsiplacement")+
  theme_bw()

ggsave(plot = last_plot(), "migrateR_step 1_example bird 88646100_changing segments to midpoint of Lavielle_colour boxes.jpg", dpi = 400, width = 12, height = 5)

# NOTE DO NOT NEED TO RECALCULATE THE NSD FROM CENTRE OF FIRST LAVIELLE SEGMENT FOR LONG DISTANCE MIGRANTS
# second find the centroid of the first LAVIELLE segment (the original segments, not the recalculated ones in step 1) and use this location to recalculate the NSD from
cent.lav1<- total.path.df.8280 %>% filter(segment == 1)
ggplot()+
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y), colour = "blue")+
  geom_point(data = cent.lav1, aes(x = x, y = y), colour = "red")

# make a minimum convex polygon around the locations in Lavielle segment 1
cent.lav1.sf <- cent.lav1  %>%
  st_as_sf( coords = c( "x", "y" ), crs = 4326 ) # first make the locations into an sf
cent.lav1.hull <-   st_convex_hull(st_union(cent.lav1.sf)) # create a polygon around the points (need to union points into multipoint sf)

ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y), colour = "blue")+
  geom_point(data = cent.lav1, aes(x = x, y = y), colour = "red")+
  geom_sf(data = cent.lav1.hull, fill = "red", alpha = 0.3)

cent.lav1 <- st_centroid(cent.lav1.hull)

ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y), colour = "blue")+
  geom_sf(data = cent.lav1.sf, colour = "red")+
  geom_sf(data = cent.lav1.hull, fill = "red", alpha = 0.3)+
  geom_sf(data = cent.lav1, fill = "green", size = 3)+
  xlim(-65, 0) +
  xlab("longitude")+
  ylim(-45, 40) +
  ylab("latitude")+
  theme_bw()

ggsave(plot = last_plot(), "migrateR_step 2_example bird 60868_centroid of first Lavielle segment.jpg", dpi = 400, height = 10, width = 10)

# NOTE: centroid was calculated on unprojected data - could this cause problems due to inaccuracy? and how can I project if running on multiple individuals?

# recalculate the NSD based on the centroid of the first Lavielle segment (just calculated)
cent.lav1
total.path.df.8280$recalc.nsd <- NA
for (i in 1:nrow(total.path.df.8280)) {
  
  # calculate distance between 2 points
  total.path.df.8280[i,"recalc.nsd"] = geosphere::distGeo(c(total.path.df.8280[i,"x"],total.path.df.8280[i,"y"]),c(-22.49323, 5.123744))/1000
  
}

# plot to check happy with NSD recalculation
ggplot() +
  geom_line(data = total.path.df.8280, aes(date, recalc.nsd))+
  theme_bw()

# third fit the disperser, nomadic and resident models and compare AIC (FOR EACH SEGMENT SEPARATELY)
# divide each segment into a separate data frame
migR.seg1 <- total.path.df.8280 %>% filter(seg4migrateR == 1 )
migR.seg2 <- total.path.df.8280 %>% filter(seg4migrateR == 2)
migR.seg3 <- total.path.df.8280 %>% filter(seg4migrateR == 3)
migR.seg4 <- total.path.df.8280 %>% filter(seg4migrateR == 4)

ggplot() +
  geom_line(data = migR.seg2, # plot each segment in turn
            aes(date, nsd))+ # if recalculated the NSD then y = recalc.nsd
  theme_bw()

ggsave(plot = last_plot(), "migrateR_step 2_example bird 60868_recalculated NSD from centroid_seg2.jpg", dpi = 300, width = 4, height = 9)

# create ltraj objects for each segment
migR.seg1.ltraj <- as.ltraj(migR.seg1, id = migR.seg1$id, date = migR.seg1$date)
seg.1.8280 <- mvmtClass(migR.seg1.ltraj)

# alternatively make one ltaj, but use the segment as ID and run at the same time
migR.seg.ltraj <- as.ltraj(xy = total.path.df.8280[,c("x", "y")],
                           id = total.path.df.8280$seg4migrateR, date = total.path.df.8280$date)
migR.seg.ltraj # check no segments have less than 15 locations, these were excluded in Amelineau

b8280 <- mvmtClass(migR.seg.ltraj) # errors for some models are possible. These will be omitted. Only want to fit disperser, nomadic and resident.
!fullmvmt(b8280) # shows which bursts (segment in this case, rather than different individual) are incomplete (true)
fullmvmt(b8280, out = "numer") ## shows how many models were fitted (out of 5: migrant, mixmigrant, disperser, nomad, resident)
fullmvmt(b8280, out = "name") # shows the names of models that were fitted to each burst (each segment in this case, rather than different individuals)


migR.seg.ltraj # reminder of number of locations per segment/burst
# plot one segment/burst at a time
migrateR::plot.mvmt(b8280[[1]], xlim = c(0, 60)) # xlimits depend on the number of locations in the segment, as there tends to be 2 location sper day, divide the number of locations by two then add one to view
title(sub= "bird60868, segment1")
migrateR::plot.mvmt(b8280[[2]], xlim = c(0, 50))
title(sub= "bird60868, segment2")
migrateR::plot.mvmt(b8280[[3]], xlim = c(0, 75))
title(sub= "bird88646100, segment3")
migrateR::plot.mvmt(b8280[[4]], xlim = c(0, 12))
title(sub= "bird88646100, segment4")

migrateR::plot.mvmts(b8280) # plot all bursts in turn, each plot will override the last. add new = T to open a new window for each plot

summary(migR.seg.ltraj)
b8280[[1]] # view output of all models each segment in turn
 
top.b8280 <- topmvmt(b8280, omit = c("migrant", "mixmig")) # exclude the migrant and mixmigrant models (as in Amelineau)
# find start/ end dates of migration movements
mig.dates <- mvmt2dt(b8280, mod = "disperser", p = 0.10) # p = 0.10


# Amenilaeu excluded segments if the start/end dates were outside of the migrateR segment range
# compare to the range of segment dates for each in turn
min(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR == "2")]) # must be earlier than the start migration date
max(total.path.df.8280$date[which(total.path.df.8280$seg4migrateR == "2")]) # must be after the end migration date

# label the migration sections
total.path.df.8280 <- total.path.df.8280 %>% 
  mutate(mig.stage = if_else(date >= mig.dates[[1]][1,2] & date <= mig.dates[[1]][2,2], "migration", "NONmig"))
                             # if_else(date >=mig.dates[[2]][1,2] & date <= mig.dates[[2]][2,2], "migration", 
                                     # if_else(date >=mig.dates[[3]][1,2] & date <= mig.dates[[3]][2,2], "migration",
                                             # if_else(date>= mig.dates[[4]][1,2] & date <= mig.dates[[4]][2,2], "migration",
                                                     # "NONmig")))#)#)

# plot the full dataset, with the migration sections in a different colour

ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y, color = mig.stage))+
  scale_color_manual(values = c("red", "black"))+
  xlim(-60, 0) +
  xlab("longitude")+
  ylim(-70, 60) +
  ylab("latitude")+
  theme_bw()

# plot with lines (to replicate Amelineau figure)
total.path.df.8280.sf <- st_as_sf(total.path.df.8280, coords = c("x", "y"), 
                                  crs = 4326) %>% 
  group_by(mig.stage) %>% 
  st_cast("LINESTRING")

ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y, color = mig.stage))+
  # scale_color_manual(values = c("red", "black"))+
  geom_path(data = total.path.df.8280, aes(x = x, y = y, color = (mig.stage), group = 1), lwd = 1.2, alpha = 0.4)+
  scale_color_manual(values = c("red", "black"))+
  xlim(-70, 0) +
  xlab("longitude")+
  ylim(-70, 60) +
  ylab("latitude")+
  ggtitle("migratory dates, corresponding to full \nmovement completion") + # change depending on the p value in mvmdt function
  # ggtitle("migratory dates, corresponding to 10% and \n90% of the movement completion")+
  theme_bw()

ggsave(plot = last_plot(), "migrateR_example bird 60868_migration dates correspond to 10 and 90 percentile of movements.jpg", dpi = 500, height = 10, width = 10)

# plot Anne-Sophie's assigning of migration stage
AS.bird8280 <- tracks %>% 
  filter(bird_id == "88646100")

ggplot()+
  geom_sf(data = borders)+
  geom_point(data = AS.bird8280, aes(x = longitude, y = latitude, color = stage))+
  geom_path(data = AS.bird8280, aes(x = longitude, y = latitude, color = stage, group = 1), lwd = 1.2, alpha = 0.4)+
  scale_color_manual(values = c("red", "black", "orange"))+
  xlim(-65, 0) +
  xlab("longitude")+
  ylim(-70, 60) +
  ylab("latitude")+
  ggtitle("AS assigned migration stage")+
  theme_bw()

ggsave(plot = last_plot(), "AS assigned migration stage_example bird 60868.jpg", dpi = 500, height = 10, width = 10)

#### Core Non-breeding areas ####
#Try using the methods to define the core non-breeding areas
mig.dates # check arrival to nonbreeding areas

nonbreed8280 <- total.path.df.8280 %>% 
  filter(mig.stage == "NONmig") %>% 
  # filter(date > "2006-12-24 20:32:51")
  filter(y < -10)

ggplot()+
  geom_sf(data = borders)+
  geom_point(data = nonbreed8280, aes(x = x, y = y))+
  xlim(-65, 0) +
  xlab("longitude")+
  ylim(-70, 60) +
  ylab("latitude")+
  ggtitle("Non-migration locations")+
  theme_bw()

# create a core area around this polygon and identify after which point they never return to the core area
nonbreed8280sf <- sf::st_as_sf(nonbreed8280, coords=c("x","y"), crs = st_crs(4326))

# calculate the kde of all contours (1 - 99) of the sf object
s1 <- st_kde(nonbreed8280sf)


s1$sf %>% 
  st_set_crs(st_crs(borders)) %>% 
  filter(contlabel == 90 | contlabel == 75 | contlabel == 50 | contlabel == 25) %>% 
ggplot()+  
  geom_sf(aes(fill=(contlabel)))+
  colorspace::scale_fill_discrete_sequential(palette="Heat2") +
  geom_sf(data=nonbreed8280sf, fill=NA, alpha = 0.3) +
  geom_sf(data = borders) +
  coord_sf(xlim = c(-60, -20), ylim = c(-45, 40))+
  annotate("text", x = -30, y = -40, label = "Bird 60868\nCalonectris borealis", size = 3)+
  theme_bw()

ggsave(plot = last_plot(), "./Figures/Autoassigning methods/kernel contours around nonbreeding area_example bird 60868.jpg", dpi = 400, height = 8, width = 8)

# use the 90% contour and find first point in full tracking dataset after which no locations enter the polygon
poly90 <- s1$sf %>% 
  st_set_crs(st_crs(borders)) %>% 
  filter(contlabel == 90)

total.path.df.8280.sf <- # need to create object again as converted it to a linestring above - make back into points
  st_as_sf(total.path.df.8280, coords = c("x", "y"), crs = 4326)

# Filter points that are inside the polygon
pointsinpoly <- st_filter(total.path.df.8280.sf, poly90)
# Sort the filtered points by timestamp
pointsinpoly <- pointsinpoly[order(pointsinpoly$date), ]

# Extract the last point in the time series
final_point <- pointsinpoly[nrow(pointsinpoly), ]

total.path.df.8280 <- total.path.df.8280 %>% 
  mutate(mig.stage2 = if_else(date > mig.dates[[1]][[2]][2] & date < final_point$date, "NONmig", "migration"))

ggplot()+ # plot to check it worked
  geom_sf(data = borders)+
  geom_point(data = total.path.df.8280, aes(x = x, y = y, color = mig.stage2))+
  # scale_color_manual(values = c("red", "black"))+
  scale_color_manual(values = c("red", "black"))+
  geom_path(data = total.path.df.8280, aes(x = x, y = y, color = (mig.stage2), group = 1), lwd = 1.2, alpha = 0.4)+
  geom_sf(data = poly90, fill = "darkgreen", alpha = 0.5)+
  annotate("text", x = -30, y = -40, label = "Bird 60868\nCalonectris borealis", size = 3)+
  xlim(-60, -10) +
  xlab("longitude")+
  ylim(-45, 40) +
  ylab("latitude")+
  ggtitle("migratory dates, based on 90% kernel \naround nonbreeding area") + # change depending on the p value in mvmdt function
  # ggtitle("migratory dates, corresponding to 10% and \n90% of the movement completion")+
  theme_bw()

ggsave(plot = last_plot(), "./Figures/Autoassigning methods/90perc kernel around nonbreeding area_example bird 60868.jpg", dpi = 400, height = 10, width = 8)



# behavioural change point analysis package: BCPA

