library(dplyr)
library(ggplot2)
library(sf)
library(geosphere)
library(rnaturalearth)
library(tidyr)
library(purrr)
library(readr)
library(RColorBrewer)
library(lubridate)

#### SCRIPT CONTENTS ####
# 1. load basemaps
# 2. OPENING AND SORTING ALL DATA 
#    load data downloaded from STDB following data requests
# 3. FILTERING DATA
#    3a. plot tracking locations and colonies - one plot per species
#    3b. calculate and plot the displacement from the colony of all remaining individuals
#    3c. filtering steps based displacement - to select indviduals that migrate only
#    3d. visual assessment of displacement plots for select species. Change reclassified non-migratory individuals
# 5. SUMMARISE DATA
# 4b. displacement plots of all data - printed as jpgs to folder


#### 1. OPEN BASEMAPS ####
# Open the layers. Once downloaded they will be found in working directory (or wherever you chose to save them) and can be loaded with following line
basemap <- ne_load(scale = 10, type = 'land', category = "physical", destdir = "./Basemaps/ne_10m_land",  returnclass = c( "sf"))
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))

# plot basemap to check it looks ok
ggplot()+
  geom_sf(data = basemap) +
  geom_sf(data= borders)

#### 2. COMBINE ALL DATA  ####
# OPEN DOWNLOADED DATA
setwd("./Flyways data_STDB fulfilled data requests") # the working directory needs to be in the folder containing all csv files downloaded from the STDB
file_names <- list.files(pattern = "*.csv") # list of all  STDB databases downloaded
STDB.DL <- do.call(rbind,lapply(file_names,read.csv)) # open all csvs 

setwd('..') # change directory back to higher up folder now that the STDB data frames are open

# If including data not downloaded from STDB, standardise the format and combine with the STDB.DL dataframe here

#### 3. FILTER DATA and add any other missing variables ####

# remove any duplicated locations
STDB.DLtest <- STDB.DL %>% 
  distinct(track_id, latitude, longitude, date_gmt,time_gmt, .keep_all= TRUE)

STDB.DL <- STDB.DLtest 
STDB.DL <- STDB.DL[!is.na(STDB.DL$longitude), ] # remove any rows with missing coords

# remove juveniles and immatures
STDB.DL <- STDB.DL %>% filter((age != "immature" & age != "juvenile") %>% replace_na(TRUE)) # replace_na(TRUE) stops rows with NA in age column being deleted

# remove birds with < 40 locations
STDB.DL.filt <- STDB.DL %>% group_by(bird_id, track_id) %>% filter(n() >= 40)

length(unique(STDB.DL$track_id)) # number of individuals before filtering by minimum number of locations
length(unique(STDB.DL.filt$track_id)) # number of individuals after filtering by minimum number of locations
length(unique(STDB.DL.filt$common_name)) # number of species

STDB.DL <- STDB.DL.filt

# check which datasets have missing colony (birds captured at sea)
unique(STDB.DL$dataset_id[which(is.na(STDB.DL$lat_colony))])
# dataset 464 from STDB has no lat and lon colony locations - they were caught at sea https://www.publish.csiro.au/mu/pdf/MU9949 
STDB.DL <- STDB.DL %>% filter((dataset_id != 464)%>% replace_na(TRUE))# remove dataset 464

#### 3a. plot tracking locations and colonies - one plot per species ####

# new dataframe with all unique individuals listed
species <- data.frame(unique(STDB.DL$common_name))  

for (j in 1:nrow(species)) { # outer loop to select one individual at a time
  ID <- print(species[j,])
  subset <- STDB.DL %>% filter(common_name == ID)
  
  ggplot(subset) +
    geom_sf(data = borders) +
    geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
    geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony), fill = colony_name), alpha = 0.5, size = 5,  shape = 23)+
    geom_sf(data = borders, fill = NA, colour = "black")+# annotate("text", x = -150, y = 85, label = (paste(ID)))+
    theme_bw()
  
  ggsave(plot = last_plot(), path = "Figures/Maps of species with colony locations", filename=paste("Map all ",ID," locations before displacement filtering and cleaning, including colony.jpg",sep=""), dpi = 400, height = 10, width = 15)
  
}

#### 3b. calculate and plot the displacement from the colony of all remaining individuals ####

# plotting the displacement of each individual and save to a folder 
# create a new dataframe with all unique individuals listed
individuals <- data.frame(unique(STDB.DL$track_id)) # 2040 individuals - should produce same number of displacement plots in the folder using loop below 

for (j in 1:nrow(individuals)) { # outer loop to select one individual at a time
  ID <- print(individuals[j,])
  subset <- STDB.DL %>% filter(track_id == ID)
  
  subset$disp <- NA
  
  for (i in 1:nrow(subset)) { # inner loop to calculate the displacement and then plot
    
    # calculate distance between 2 points
    subset[i,"disp"] = geosphere::distGeo(c(subset[i,"longitude"],subset[i,"latitude"]),c(subset[i,"lon_colony"], subset[i,"lat_colony"]))/1000
    
    # change the date/time columns into POSIXct
    subset$POSIX <- as.POSIXct(paste(subset$date_gmt, subset$time_gmt), format="%Y-%m-%d %H:%M", tz = "GMT")
    
    # plot displacement
    sp <- c(subset$common_name[1], subset$scientific_name[1])
    ggplot(subset)+
      geom_line(aes(POSIX, disp), lwd = 1.3) +
      xlab("Date") +
      ylab("Displacement from colony (km)") +
      labs(title = "Bird =", subtitle = ID, caption = sp[2])+
      theme_bw()#+
    # theme(plot.caption = element_text(vjust = 155)) 
    
  }
   ggsave(plot = last_plot(), path = "Figures/Displacement plots", filename=paste("Displacement plot_Bird",ID,".jpg",sep=""), dpi = 200)
 }

##### 3c. filtering steps based displacement - to select indviduals that migrate only ####
# using displacement to filter datasets for individuals that do migrate. number of time crosses the mid-distance

STDB.DL$disp <- NA # calculate colony displacement
STDB.DL <- as.data.frame(STDB.DL)
for (i in 1:nrow(STDB.DL)) {

  STDB.DL[i,"disp"] = geosphere::distGeo(c(STDB.DL[i,"longitude"],STDB.DL[i,"latitude"]),c(STDB.DL[i,"lon_colony"], STDB.DL[i,"lat_colony"]))/1000
  print(i)
}

# determine if individuals are migratory based on how many times they cross half the maximum displacement (this will vary with the number of years of tracking data available so calculate seperately for indivduals with <1, 1 to 2, 2 to 3 or > 3 years of data
# create separate dataframes for individuals with < 12 months of tracking data, between 12 and 24 months, between 24 and 36 months and > 36 months of data

df0.12 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
           # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))<=365, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE")  

df12.24 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
  # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))>365 & (max(POSIX)- min(POSIX)) <= 730, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE") 

df24.36 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
  # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))>730 & (max(POSIX)- min(POSIX)) <= 1095, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE")
df24.36 <- as.data.frame(df24.36)

df36.48 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
  # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))>1095 & (max(POSIX)- min(POSIX)) <= 1460, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE")

# plot each dataframe in turn to check that the individuals have been classified correctly (i.e. the duration of their tracking)
df0.12 %>% 
  group_by(track_id) %>%
  ggplot()+
  geom_line(aes(POSIX, track_id), lwd = 1)+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")

# next find the median displacement for each individual and if bird crosses more than twice in a year remove

individuals <- data.frame(unique(df0.12$track_id)) 
 MIG <- NULL # do not run these two lines after the first dataset as it will override the previous data
 NONMIG <- NULL

# for each dataframe (0-12 months, 12 - 24M, 24- 36M an 36-48 M) repeat the loop, change dataframe in first loop
# also change the number of crossings (2 per year) at end of outer loop

 for (j in 1:nrow(individuals)) { # outer loop to select one individual at a time

   ID <- print(individuals[j,])
  subset <- df0.12 %>% filter(track_id == ID) # change data frame here for the different number of years

for (i in 1:nrow(subset)) {
    limit <-  (max(subset$disp) / 2)
  limit2 <- (max(subset$disp) -min(subset$disp)) /2 
    
  sp <- c(subset$common_name[1], subset$scientific_name[1])
  disp.plot <- ggplot(subset)+
    geom_line(aes(POSIX, disp), lwd = 1.3) +
    geom_point(aes(POSIX, disp), colour = "red")+
    geom_hline(yintercept = limit)+
    geom_hline(yintercept = limit2, colour = "blue")+
    xlab("Date") +
    ylab("Displacement from colony (km)") +
    labs(title = "Bird =", subtitle = ID, caption = paste(sp[1],"/",sp[2]))+
    theme_bw()
 # 
 
subset <- subset[order(subset$POSIX),] # order subset by date

# Calculate the number of times the displacement crosses the mid distance 
 num_crossings <- 0
 for (ii in 2:length(subset$disp)) {
     if ((subset$disp[ii-1] < limit && subset$disp[ii] > limit) ||
       (subset$disp[ii-1] > limit && subset$disp[ii] < limit)) {
     num_crossings <- num_crossings + 1
   } 
   # print(ii)
   # print(num_crossings)
 } # close loop ii (number of mid point displacemetn distance crossing)

} # close loop i (finding mid distance, displacement plot, ordering data by date)

  print(j)  
 
if (num_crossings > 0.1 & num_crossings <=2) { # change from 2 crossings per year to 4 for 12 - 24 months, 6 for 24 - 36 months etc.

  ggsave(plot = disp.plot, path = "Figures/Displacement plots/migration" , filename = paste("Displacement plot_Bird",ID,"MIGRATION.jpg",sep=""))
  
  MIG <- rbind(MIG, subset)

    
}else {
  
  ggsave(plot = disp.plot, path = "Figures/Displacement plots/non-migration" , filename = paste("Displacement plot_Bird",ID,"NON-MIGRATION.jpg",sep=""))
  
  NONMIG <- rbind(NONMIG, subset)
}
 

# d <- min(subset$POSIX)
# d2 <- d %m+% months(24) # add 24 months to a posixt date 
  
} # closing outer loop j (selecting one individual at a time from full dataset)

# save data for individuals classified as migratory and non-migratory into two separate csv files
write.csv(MIG, "data filtered by migration based on displacement plots_MIGRATION.csv", row.names =  FALSE)
write.csv(NONMIG, "data filtered by migration based on displacement plots_NONMIGRATION.csv", row.names =  FALSE)


# check the number of individuals matches the numbers in the two folders of displacement plots
length(unique(NONMIG$track_id)) 
length(unique(MIG$track_id))

# check there are no species with no individuals classified as migratory or non-migratory 
spNONMIG <- data.frame(species = c(unique(NONMIG$common_name)))
spMIG <- data.frame(species = c(unique(MIG$common_name))) 
missingsp <- anti_join(spNONMIG, spMIG)
# 3 species all individuals classed as non-migratory: MacGillivray's Prion, Broad-billed Prion, White-headed Petrel
missingsp <- anti_join(spMIG, spNONMIG)
# 2 species all individuals classed as migratory: Westland Petrel, Mottled Petrel

# check the numbers of individuals for each species within the NONMIG and MIG datasets to see if lots have been removed from one species
  
 countMIG <- MIG %>% 
    group_by(common_name, scientific_name, colony_name) %>% 
    summarise(individual.count.mig = n_distinct(track_id))
 
 countNONMIG <- NONMIG %>% 
   group_by(common_name, scientific_name, colony_name) %>% 
   summarise(individual.count.nonmig = n_distinct(track_id))

 ind.counts <- full_join(countMIG, countNONMIG)
   
 write.csv(ind.counts, "Figures/Displacement plots/1. number of individuals per species_sorted into mig and nonmig based on displacement plots.csv", row.names =  FALSE)

#### 3d. visual assessment of displacement plots for select species. Change reclassified non-migratory individuals ####
# visually check subset of displacement plots (see methods) and create a csv listing any individuals that need to be reclassified from non-migratory to migratory. 

ind2Δ <- read.csv("./Figures/Displacement plots/2. Visual check on individuals from select species classed as non-migratory.csv")

NMtoM <- NONMIG %>% 
  filter(track_id %in% ind2Δ$track_id)
missingsp <- anti_join( ind2Δ, NMtoM) # check for missing individuals caused by typos)

unique(ind2Δ$track_id)
unique(NMtoM$track_id)

MIG2 <- rbind(MIG, NMtoM) # add to MIG df
NONMIG2 <- NONMIG %>% 
  filter(!track_id %in% c(unique(ind2Δ$track_id))) # remove from the NONMIG df

length(unique(NONMIG2$track_id)) # number of non-migrating individuals
length(unique(MIG2$track_id)) # number of migrating individuals 

length(unique(NONMIG2$common_name))
length(unique(MIG2$common_name))

write.csv(MIG2, "data filtered by migration based on displacement plots_MIGRATION.v2_reclassified2.csv", row.names =  FALSE)
write.csv(NONMIG2, "data filtered by migration based on displacement plots_NONMIGRATION.v2_reclassified2.csv", row.names =  FALSE)


#### 3k. write csv of all individuals that need cleaning ####

tosave <- fullMIG %>% 
  dplyr::select(c("scientific_name", "common_name", "track_id", "device")) %>% 
  mutate(cleaned = "N") %>% 
  distinct(track_id, .keep_all = TRUE)

write.csv(tosave, "./Cleaning data_from Marie/Datasets to cleanv2.csv", row.names = F)

#### 3k. histograms per species to find outliers in distance travelled ####


#put this into a loop to generate a plot per species
MIG %>% 
  filter(common_name == "South Polar Skua" )%>% 
  group_by(track_id) %>%
  summarise(max = max(disp, na.rm=TRUE)) %>% 
  ggplot()+
  geom_histogram(aes(max), colour = "white", binwidth = 500)+
  xlab("Maximum displacement (km)")+
  ggtitle("South Polar Skua")+
  theme_bw()

MIG %>% 
  filter(common_name == "Magenta Petrel") %>% 
  group_by(track_id) %>%
  summarise(max = max(disp), median = median(disp), Q1 = quantile(disp, .25), Q3 = quantile(disp, .75)) %>% 
  mutate(perc.diff = ((Q3 - Q1)/Q3)*100)


