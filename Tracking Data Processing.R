library(dplyr)
library(ggplot2)
library(sf)
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
#    3a. plot tracking and colony locations of each species
#    3b. calculate displacement from the colony for every individual
# 4. PLOT DATA
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

# new dataframe with all unique individuals listed
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
   ggsave(plot = last_plot(), path = "Figures/Displacement plots/extra species", filename=paste("Displacement plot_Bird",ID,".jpg",sep=""), dpi = 200)
 }


##### 3c. filtering steps based displacement - to select indviduals that migrate only ####
# using displacement to filter datasets for individuals that do migrate. number of time crosses the mid-distance

STDB.DL$disp <- NA
STDB.DL <- as.data.frame(STDB.DL)
for (i in 1:nrow(STDB.DL)) {

  STDB.DL[i,"disp"] = geosphere::distGeo(c(STDB.DL[i,"longitude"],STDB.DL[i,"latitude"]),c(STDB.DL[i,"lon_colony"], STDB.DL[i,"lat_colony"]))/1000
  print(i)
}


# create separate dataframes for individuals with < 12 months of tracking data, between 12 and 24 months, between 24 and 36 months and > 36 months of data

df0.12 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
           # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))<=365, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE") # %>% # add this extra filter if adding one dataset after already filtered them all
 # filter(dataset_id == "1541") 
  

df12.24 <- STDB.DL %>% 
  mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC")) %>% 
  # as.POSIXct(paste(date_gmt, time_gmt), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  group_by(track_id) %>% 
  mutate(duration = ifelse((max(POSIX)- min(POSIX))>365 & (max(POSIX)- min(POSIX)) <= 730, "TRUE", "FALSE")) %>% 
  filter(duration =="TRUE") #%>%
  # filter(dataset_id == "1541")

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
# MIG <- NULL # do not run these two lines after the first dataset as it will override the previoius data
# NONMIG <- NULL
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
 
if (num_crossings > 0.1 & num_crossings <=2) {

  ggsave(plot = disp.plot, path = "Figures/Displacement plots/extra species/migration" , filename = paste("Displacement plot_Bird",ID,"MIGRATION.jpg",sep=""))
  
  MIG <- rbind(MIG, subset)

    
}else {
  
  ggsave(plot = disp.plot, path = "Figures/Displacement plots/extra species/non-migration" , filename = paste("Displacement plot_Bird",ID,"NON-MIGRATION.jpg",sep=""))
  
  NONMIG <- rbind(NONMIG, subset)
}
 

# d <- min(subset$POSIX)
# d2 <- d %m+% months(24) # add 24 months to a posixt date 
  
} # closing outer loop j (selecting one individual at a time from full dataset)

#when only running the above for one or a few new datasets, do bind the dataframe with the larger, sorted mig and non mig dfs. (open these below and call then MIG2 and NONMIG2 to not override the dataframes just sorted)
# MIG <- rbind(MIG, MIG2)
# NONMIG <-rbind(NONMIG, NONMIG2)

### 3j. open dataframes with migrating and non-migrating individuals filtered based on displacement plots ####
write.csv(MIG, "data filtered by migration based on displacement plots_MIGRATION.v4.csv", row.names =  FALSE)
write.csv(NONMIG, "data filtered by migration based on displacement plots_NONMIGRATION.v4.csv", row.names =  FALSE)

MIG <- read.csv("data filtered by migration based on displacement plots_MIGRATION.v4.csv")
NONMIG <- read.csv("data filtered by migration based on displacement plots_NONMIGRATION.v4.csv")

# check the number of individuals matches the numbers in the two folders of displacement plots
length(unique(NONMIG$track_id)) 
length(unique(MIG$track_id))

# check no species are missing
spNONMIG <- data.frame(species = c(unique(NONMIG$common_name)))
spMIG <- data.frame(species = c(unique(MIG$common_name))) 
missingsp <- anti_join(spNONMIG, spMIG)
# 3 species all individuals classed as non-migratory: MacGillivray's Prion, Broad-billed Prion, White-headed Petrel
missingsp <- anti_join(spMIG, spNONMIG)
# 2 species all individuals classed as migratory: Westland Petrel, Mottled Petrel

# check the MacGillivray's Prion, Broad-billed Prion, White-headed Petrel plots
lostsp <- STDB.DL %>% filter(common_name == "MacGillivray's Prion" | common_name == "Broad-billed Prion" | common_name == "White-headed Petrel") %>% 
    mutate(POSIX = parse_date_time(paste(date_gmt, time_gmt), c("ymd HMS", "ymd HM"), tz = "UTC"))

length(unique(lostsp$track_id)) # 23 individuals
lostsp %>% group_by(common_name) %>% count(track_id) %>% print(n = 23) # 8 MacGillivray's prion, 13 broad-billed prion, 2 white-headed petrel

# re generate all displacement plots for the missing species to assess
individuals <- data.frame(unique(lostsp$track_id))

  ID <- print(individuals[23,])
  subset <- lostsp %>% filter(track_id == ID) # change data frame here for the different number of years
  
  limit <-  (max(subset$disp) / 2)
  sp <- c(subset$common_name[1], subset$scientific_name[1])
  
  ggplot(subset)+
      geom_line(aes(POSIX, disp), lwd = 1.3) +
      geom_point(aes(POSIX, disp), colour = "red")+
      geom_hline(yintercept = limit)+
      # geom_hline(yintercept = limit2, colour = "blue")+
      xlab("Date") +
      ylab("Displacement from colony (km)") +
      labs(title = "Bird =", subtitle = ID, caption = sp[2])+
      theme_bw()
   
  ggsave(plot = last_plot(), path = "Figures/Displacement plots/lostspecies" , filename = paste("Displacement plot_Bird",ID,"NON-MIGRATION_no individuals from this sp. classified as migratory.jpg",sep=""))
  
# check the numbers of individuals for each species within the NONMIG and MIG datasets to see if lots have been removed from one species
  
 countMIG <- MIG %>% 
    group_by(common_name, scientific_name, colony_name) %>% 
    summarise(individual.count.mig = n_distinct(track_id))
 
 countNONMIG <- NONMIG %>% 
   group_by(common_name, scientific_name, colony_name) %>% 
   summarise(individual.count.nonmig = n_distinct(track_id))

 ind.counts <- full_join(countMIG, countNONMIG)
   
 write.csv(ind.counts, "Figures/Displacement plots/1. number of individuals per species_sorted into mig and nonmig based on displacement plots_eight new species.v5 with colonies.19062023.csv", row.names =  FALSE)
# some Pterodoma ultima/ Murphy's petrel have no locations crossing the midpoint and are classified as migrating. 
# plot these to visualise why

murphy <- MIG %>%  
  filter(common_name == "Murphy's Petrel")
  
# plot to find which individuals actaually migrate
murphy %>% 
  filter(
      # no migration locations
    # track_id ==  "NonDB_26_NA" | 
    # migration locations
    track_id == "NonDB_18_NA" | track_id == "NonDB_19_NA"| track_id == "NonDB_22_NA" |
    track_id ==  "NonDB_29_NA" | track_id ==  "NonDB_30_NA" | track_id == "NonDB_31_NA"  |
    track_id ==  "NonDB_23_NA" | track_id == "NonDB_24_NA"  | track_id ==  "NonDB_25_NA"
    ) %>% 
  group_by(track_id) %>% 
  arrange(POSIX) %>% # need to order by date if using lines rather than points
ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude))+
  geom_path(aes(x = longitude, y = latitude, colour = track_id), alpha = 0.5, lwd = 2)+
  # geom_path(aes(x = longitude, y = latitude, colour = interaction(track_id,stage,sep="-",lex.order=TRUE)), alpha = 0.5, lwd = 2) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony), fill = colony_name), alpha = 0.5, size = 5,  shape = 23)+
  xlim(c(-180, -50))+
  geom_sf(data = borders, fill = NA, colour = "black")+# annotate("text", x = -150, y = 85, label = (paste(ID)))+
  theme_bw()

#### 3k. visual assessment of displacement plots for select species. Change reclassified non-migratory individuals ####

ind2Δ <- read.csv("./Figures/Displacement plots/2. Visual check on individuals from select species classed as non-migratory.csv")
ind2Δ <- read.csv("./Figures/Displacement plots/2. Visual check on individuals from select species classed as non-migratory_eight new species.csv")

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

write.csv(MIG2, "data filtered by migration based on displacement plots_MIGRATION.v7_reclassified eight species.csv", row.names =  FALSE)
write.csv(NONMIG2, "data filtered by migration based on displacement plots_NONMIGRATION.v7_reclassified eight species.csv", row.names =  FALSE)


# merge new eight species with 40 species dataset
MIG.0 <- read.csv("data filtered by migration based on displacement plots_MIGRATION.v6_reclassified.csv")
NONMIG.0 <- read.csv("data filtered by migration based on displacement plots_NONMIGRATION.v6_reclassified.csv")

# add extra column to eight new species dataframes
MIG2 <- MIG2 %>% 
  mutate(stage = NA, AS = "No") %>% 
  relocate(stage, .after = equinox) %>% 
  relocate(AS, .before = disp)

NONMIG2 <- NONMIG2 %>% 
  mutate(stage = NA, AS = "No") %>% 
  relocate(stage, .after = equinox) %>% 
  relocate(AS, .before = disp)

fullMIG <- rbind(MIG.0, MIG2)
fullNONMIG <- rbind(NONMIG.0, NONMIG2)


length(unique(fullNONMIG$track_id)) # number of non-migrating individuals
length(unique(fullMIG$track_id)) # number of migrating individuals 

length(unique(fullNONMIG$common_name))
length(unique(fullMIG$common_name))

write.csv(fullMIG, "data filtered by migration based on displacement plots_MIGRATION.v8_reclassified including additional data Jul23.csv", row.names = FALSE)
write.csv(fullNONMIG, "data filtered by migration based on displacement plots_NONMIGRATION.v8_reclassified including additional data Jul23.csv", row.names = FALSE)

##### Fully FILTERED DATA - OPEN THESE CSVs ####
MIG <- read.csv("data filtered by migration based on displacement plots_MIGRATION.v5_reclassified.csv")
NONMIG <- read.csv("data filtered by migration based on displacement plots_NONMIGRATION.v5_reclassified.csv")

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

#### 4. PLOT DOWNLOADED DATA ####

ggplot()+
  geom_sf(data = basemap)+
  geom_sf(data = borders)+
  geom_point(data = STDB.DL, aes(x = longitude, y = latitude, colour = as.factor(common_name)))+
  theme_bw()

ggsave(plot = last_plot(), "Map of all locations_all fulfilled data requests_06.03.23.jpg", dpi = 500, height = 10, width = 15)

#### 4a. Plot with different oceans at centre (code from 'Sorting Anne-Sophie datasett script) ####

ALLlocs <- st_as_sf(fullMIG,  coords = c("longitude", "latitude"), crs = 4326)
ALL_moll <- st_transform(ALLlocs, crs = "+proj=moll")# Changing projections to Mollweide projection
ALL_robin <- st_transform(ALLlocs, fcrs = "+proj=robin")# Changing projections to Robinson projection


world_moll = st_transform(basemap, crs = "+proj=moll")
borders_moll <- st_transform(borders, crs = "+proj=moll")

world_robin = st_transform(basemap, crs = "+proj=robin")
borders_robin <- st_transform(borders, crs = "+proj=robin")

# create a colour palette with as many colours as there are species 
colourCount = length(unique(ALL_moll$common_name))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))

# Plot all locations (with Greenwich meridian at centre - Atlantic as centre)
Atl <- ggplot()+
  # geom_sf(data = world_moll, fill = "#009E73", alpha = 0.5) +
  geom_sf(data = borders_robin, fill = "#009E73", alpha = 0.3, lwd = 0.2) +
  geom_sf(data = ALL_robin, aes(colour = common_name), size = 0.6, alpha = 0.3) +
  scale_colour_manual(values = getPalette(colourCount))+
  geom_text(aes(label = 'All locations', x = -Inf, y = Inf),
            hjust = -0.1, vjust = 1.2) +
  ylab("Latitude")+
  xlab("Longitude")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.3))) + # override legend so that it isn't transparent
  theme_bw()

# Now plot with a different map centre
world <- st_transform(borders, crs = 4326) #start with unprojected data
# define a long & slim polygon that overlaps the meridian line & set its CRS to match world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

sf_use_s2(FALSE) # this line avoids an error message if there are duplicate polygon edges

# modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>%  
  st_difference(polygon)

# project the data
world_robin <- st_transform(world2, 
                           crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
# alternative to mollweide is robinson. replace moll with robin after +proj=

# plot all locations with 180 as centre - Pacific orientation
Pac <- ggplot() +
  geom_sf(data = world_robin, fill = "#009E73", alpha = 0.3, lwd = 0.2)+
  geom_sf(data = ALL_robin, aes(colour = common_name), size = 0.6, alpha = 0.3) +
  scale_colour_manual(values = getPalette(colourCount))+
  geom_text(aes(label = 'All locations', x = -Inf, y = Inf),
            hjust = -0.1, vjust = 1.2) +
  ylab("Latitude")+
  xlab("Longitude")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.3, colour = "white"))) + # override legend so that it isn't transparent, also for this plot colour white so it's invisible but same size as previous plot
  theme_bw()+
  theme(legend.text = element_text(colour = "white"), 
        legend.title = element_blank())


cowplot::plot_grid(Atl, Pac, nrow = 2)

ggsave(plot = last_plot(), "All data from individuals classified as migrating_Anne-Sophie and fulfilled requests_19.07.2023, 2 parts - centre in Atlantic and Pacific_Robinson proj.jpg", dpi = 500, width = 15, height = 15)

# plot locations on polar projections
borders_ortho <- st_transform(borders, crs = "+proj=ortho +lat_0=90 +lon_0=0")
ALL_ortho <- st_transform(ALLlocs, crs = "+proj=ortho +lat_0=90 +lon_0=0")# Changing projections to ortho north polar projection


ARCT <- ggplot() +
  geom_sf(data = borders_ortho, fill = "#009E73", alpha = 0.3) +
  geom_sf(data = ALL_ortho, aes(colour = common_name), size = 0.6, alpha = 0.3) +
  scale_colour_manual(values = getPalette(colourCount))+
  geom_text(aes(label = 'All locations', x = -Inf, y = Inf),
            hjust = -0.1, vjust = 1.2) +
  ylab("Latitude")+
  xlab("Longitude")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.3, colour = "white"))) + # override legend so that it isn't transparent, also for this plot colour white so it's invisible but same size as previous plot
  theme_bw()+
  theme(legend.text = element_text(colour = "white", 
                                   size=rel(0.5)), 
        legend.title = element_blank(), 
        legend.key.width = unit(0.3, 'cm'))

# south polar projection
borders_ortho <- st_transform(borders, crs = "+proj=ortho +lat_0=-90 +lon_0=0")
ALL_ortho <- st_transform(ALLlocs, crs = "+proj=ortho +lat_0=-90 +lon_0=0")# Changing projections to ortho north polar projection


ANTAR <- ggplot() +
  geom_sf(data = borders_ortho, fill = "#009E73", alpha = 0.3) +
  geom_sf(data = ALL_ortho, aes(colour = common_name), size = 0.6, alpha = 0.3) +
  scale_colour_manual(values = getPalette(colourCount))+
  geom_text(aes(label = 'All locations', x = -Inf, y = Inf),
            hjust = -0.1, vjust = 1.2) +
  ylab("Latitude")+
  xlab("Longitude")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.3))) + # override legend so that it isn't transparent, also for this plot colour white so it's invisible but same size as previous plot
  theme_bw()+
  theme(legend.text = element_text(size=rel(0.5)), 
        legend.key.width = unit(0.3, 'cm')) 
        


cowplot::plot_grid(ARCT, ANTAR, nrow = 2)

ggsave(plot = last_plot(), "All data from individuals classified as migrating_Anne-Sophie and fulfilled requests_19.07.2023, 2 parts - polar projections_Ortho proj.jpg", dpi = 300, width = 12, height = 8)

#### 4b. plot one species at a time and colour code by dataset ID ####

PlotU <- ggplot()+
  geom_sf(data = basemap)+
  geom_sf(data = borders) +
  geom_point(data = STDB.DL[which(STDB.DL$common_name == "Cory's Shearwater"),], # change for species
             aes(x = longitude, y = latitude, colour = as.factor(bird_id)), alpha = 0.2) +
  labs(color='Individial ID')  +
  # xlim(c(30, 120))+
  # ylim(c(-60, 25))+
  # scale_colour_discrete(guide = "none")+
  # scale_color_brewer(palette = "Dark2")+
  ggtitle("Cory's Shearwater") +
  theme_bw()

ggsave(plot = PlotU, "./Figures/Requested data_Cory's shearwater.jpg", dpi = 500)

# diff projection
ggplot()+
  geom_sf(data = world_moll, fill = "#009E73", alpha = 0.3) +
  # geom_sf(data = borders_moll, fill = "#009E73", alpha = 0.3, lwd = 0.2) +
  geom_sf(data = ALL_moll[which(ALL_moll$common_name == "South Polar Skua"),], aes(colour = bird_id), size = 0.6, alpha = 0.3) +
  geom_text(aes(label = 'South Polar Skua', x = -Inf, y = Inf),
            hjust = -0.1, vjust = 1.2) +
  ylab("Latitude")+
  xlab("Longitude")+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1.5))) + # override legend so that it isn't transparent
  theme_bw()+
  theme(legend.text = element_text(size=rel(0.5))) 


ggsave(plot = last_plot(), "./Figures/AnneSo data_south polar skua.jpg", dpi = 500)

#### 5. DATA SUMMMARY ####
colnames(STDB.DL)

p <- STDB.DL %>% 
  group_by(scientific_name) %>%
  count(track_id) %>%
  mutate(bird_id = as.factor(track_id)) # or change bird_id to originial_track_id

table.summary <- aggregate(data = p, bird_id ~ scientific_name
          , FUN = length, na.action = na.pass) %>% 
  rename(sample.size_ind.count = bird_id)
  
q <- STDB.DL %>% 
  group_by(scientific_name) %>% 
  count(dataset_id) %>% 
  mutate(dataset_id = as.factor(dataset_id))
  # summarise(n = n()) %>% # sample size for each species

q2 <- aggregate(data = q, dataset_id ~ scientific_name, 
                FUN = length, na.action = na.pass) %>% 
  rename(num.of.STDBprojects = dataset_id)

table.summary <- inner_join(table.summary, q2)

p <- STDB.DL %>% 
  group_by(scientific_name, sex) %>% 
  count(original_track_id) %>% # original_track_id
  mutate(bird_id = as.factor(original_track_id))
p2 <- aggregate(data = p, bird_id ~ scientific_name + sex, 
                FUN = length, na.action = na.pass) 
p3 <- pivot_wider(data = p2, names_from = sex, id_cols = scientific_name, values_from = bird_id ) %>% 
  rename(sex.female = female, sex.male = male, sex.unknown = unknown) %>% 
  mutate(sex.female = as.numeric(sex.female), sex.male = as.numeric(sex.male), sex.unknown = as.numeric(sex.unknown)) %>% 
  mutate_at(c('sex.female','sex.male', 'sex.unknown'), ~replace_na(.,0))


table.summary <- merge.data.frame(table.summary,p3,by="scientific_name",all.x=TRUE)

# total numbers of males, females and unknowns and check that this matches the sample size
for (i in 1:nrow(table.summary)) {
  table.summary$sex.sum[i] <- table.summary$sex.female[i] + table.summary$sex.male[i] + table.summary$sex.unknown[i]
}


q <- STDB.DL %>% 
  group_by(scientific_name, age) %>% 
  count(original_track_id) %>% #original_track_id
  mutate(bird_id = as.factor(original_track_id))
q2 <- aggregate(data = q, bird_id ~ scientific_name + age, 
                FUN = length, na.action = na.pass)
q3 <- pivot_wider(data = q2, names_from = age, id_cols = scientific_name, values_from = bird_id) %>%
  rename(age.adult = adult, age.immature = immature, age.juvenile = juvenile, age.unknown = unknown) %>% 
  mutate(age.adult = as.numeric(age.adult), age.immature = as.numeric(age.immature), age.juvenile = as.numeric(age.juvenile), age.unknown = as.numeric(age.unknown)) %>% 
  mutate_at(c('age.adult', 'age.immature', 'age.juvenile', 'age.unknown'), ~replace_na(.,0))


table.summary <- merge.data.frame(table.summary,q3,by="scientific_name",all.x=TRUE)

# total numbers of adults, immatures, juveniles and unknowns and check that this matches the sample size
for (i in 1:nrow(table.summary)) {
  table.summary$age.sum[i] <- table.summary$age.adult[i] + table.summary$age.immature[i] + table.summary$age.juvenile[i]+ table.summary$age.unknown[i]
}

table.summary[37,] = c("Total", colSums(table.summary[,2:12])) # change the row number depending on the number of rows in table.summary. Should be nrow+1 to make new total row


# total numbers of individuals grouped by device type
AA <- tracks %>% # make a dataframe only keeping one row of each individual (tracks might be MIG or whatever else has all the individuals)
       group_by(track_id, device) %>% 
       filter(row_number() == 1)
 AA %>% 
       group_by( device) %>%
       summarise(number = n()) # summarise based on device type (bettter than aggregate)

#### 5a.  What proportion of individuals have a full migration cycle? ####



# Have Anne-Sophie or Marie cleaned these data?

# add a column fpr if dataset was requested through the plastics work (so Marie may have cleaned and labelled)

STDB.DL$Req.for.plastics<- ifelse(STDB.DL$dataset_id %in% c(464, 517, 518, 555, 561, 627, 635, 639, 
                                                            663, 683, 705, 706, 888, 708, 710, 712, 
                                                            739, 858, 883, 884, 885, 886, 889, 891, 
                                                            715, 892, 971, 973, 977, 978, 982, 1110, 
                                                            1295, 1318, 1319, 1320, 1321, 1322, 1413, 
                                                            1450, 1485, 1486, 1487, 1579, 1580, 1581, 
                                                            1603, 1692, 1704), "Yes", "No")

q <- STDB.DL %>% 
  group_by(dataset_id, Req.for.plastics, scientific_name, AS) %>% 
  count(bird_id)# %>% 
  mutate(dataset_id = as.factor(bird_id))
q2 <- aggregate(data = q, bird_id ~ scientific_name + Req.for.plastics +AS, 
                FUN = length, na.action = na.pass)

plastics <- pivot_wider(data = q2, names_from = c(Req.for.plastics, AS), 
                        id_cols = scientific_name, values_from = bird_id) %>% 
  rename(Req.for.plastics.NO.AS.NO = No_No, Req.for.plastics.YES.AS.NO = Yes_No, Req.for.plastics.NO.AS.YES = No_Yes) %>% 
  mutate(Req.for.plastics.NO.AS.NO = as.numeric(Req.for.plastics.NO.AS.NO), 
         Req.for.plastics.YES.AS.NO = as.numeric(Req.for.plastics.YES.AS.NO), 
         Req.for.plastics.NO.AS.YES = as.numeric(Req.for.plastics.NO.AS.YES)) %>% 
  mutate_at(c('Req.for.plastics.NO.AS.NO', 'Req.for.plastics.YES.AS.NO', 'Req.for.plastics.NO.AS.YES'), 
            ~replace_na(.,0)) 

plastics$proportionMar <- NA
plastics$proportionAS <- NA
for (i in 1:nrow(plastics)) {
  
   sum = sum(plastics$Req.for.plastics.NO.AS.NO[i] + plastics$Req.for.plastics.YES.AS.NO[i] + plastics$Req.for.plastics.NO.AS.YES[i])
   
  p <- (plastics$Req.for.plastics.YES.AS.NO[i] / sum)*100
   
   plastics[i,5] <- p
   
   q = (plastics$Req.for.plastics.NO.AS.YES[i] / sum)*100
   
   plastics[i,6] <- q
  
}

plastics$total <- (plastics$proportionMar + plastics$proportionAS)

write.csv(plastics, "Proportion of bird datasets requested for plastics or cleaned by AS.csv", row.names = FALSE)


# ACAP species 

ACAP <- STDB.DL %>% filter(common_name == "Wandering Albatross"  | common_name =="Grey-headed Albatross" | 
                             common_name == "Black-browed Albatross" | common_name == "Spectacled Petrel" | 
                             common_name == "Westland Petrel" | common_name == "White-chinned Petrel")

aggregate(data = ACAP, track_id ~ common_name, function(track_id)length(unique(track_id)))

# common_name         number of individuals
# 1 Black-browed Albatross      127
# 2  Grey-headed Albatross       25
# 3      Spectacled Petrel        8
# 4    Wandering Albatross      443
# 5        Westland Petrel        8
# 6   White-chinned Petrel       50

ACAP %>% group_by(common_name) %>% summarise(count = n_distinct(track_id)) # does the same as above

# how many studies were listed on STDB?
ACAP %>% group_by(dataset_id, common_name) %>% summarise(count= n_distinct(track_id))

# how many locations recorded by each technology
STDB.DL %>%  group_by(device) %>% summarise(count = n_distinct(latitude))

ggplot()+
  geom_sf(data = borders)+
  geom_point( data = ACAP, aes(x = longitude, y = latitude) ) +
  facet_wrap(~common_name)
