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
# 2a. combine STDB requests with AS data and remove duplicates
# 2b. add missing details (colony lat, lon) to Anne-So datasets
# 2c. add arctic tern data requested by email from R.Fijn 

# 3. FILTERING DATA
# filter data (remove individuals with too few locations)
# 3e. write combined and filtered data into a new csv

# 4. PLOT DATA

# 5. SUMMARISE DATA
# 4b. displacement plots of all data - printed as jpgs to folder



#### 1. OPEN BASEMAPS ####
# Open the layers. Once downloaded they will be found in working directory (or wherever you chose to save them) and can be loaded with following line
basemap <- ne_load(scale = 10, type = 'land', category = "physical", destdir = "./Basemaps/ne_10m_land",  returnclass = c( "sf"))
# placelabels <- ne_load(scale = 10, type = 'populated_places', destdir = "./Basemaps/ne_10m_populated_places",  returnclass = c( "sf"))
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))

# plot basemap to check it looks ok
ggplot()+
  geom_sf(data = basemap) +
  geom_sf(data= borders)

#### 2. COMBINE ALL DATA  ####
# OPEN DOWNLOADED DATA
setwd("./Flyways data_STDB fulfilled data requests") # for the next two lines to work, the working directory needs to be in the folder

# skip next to steps if wish to add one dataset (rather than run everything again)
file_names <- list.files(pattern = "*.csv") # list of all  STDB databases downloaded
STDB.DL <- do.call(rbind,lapply(file_names,read.csv)) # open all csvs 

# NOTE have redownloaded as the old website download format did not match the new website download format

# ALTERNATIVE TO RUNNING EVERYTHING WHEN WISHING TO ADD ONE ADDITIONAL DATASET 
TBprion <- read.csv("Dataset_15725_1541_Thin-billed_prion_GLS_New_Isl_imported.csv")

#final data addition
file_names <- list.files (pattern = "*.csv")
file_names2 <- file_names[c(1,47,63,64,65, 74:77, 79:87)]
STDB.DL <- do.call(rbind,lapply(file_names2,read.csv))

setwd('..') # change directory back to higher up folder now that the STDB data frames are open

#### 2a. Combine with Anne-Sophie's sorted data ####

tracks <- read.csv("STDB_data from Anne-Sophie_migration and winter periods already separated.csv")
colnames(tracks)
colnames(STDB.DL)

# change the date_time column in AS dataset into a posix and remove the letters
tracks$date_time <- stringr::str_replace_all(tracks$date_time, c("T" = " ", "Z" = "")) # remove letters
# tracks <- tracks %>% 
#   mutate(POSIX = as.POSIXct(date_time,  tz = "UTC")) # add  additional column with a POSIXct class date and time


# make the two dataset columns match to combine
tracks2 <- tracks %>% 
  rename(scientific_name = species, colony_name = colony) %>% 
  # add columns that are missing and in STDB.DL
  mutate(dataset_id = NA,  site_name = NA, lat_colony = NA, 
         lon_colony = NA, device = NA, track_id = NA, original_track_id = NA,
         age = NA, sex = NA, breed_stage = NA, breed_status = NA, argos_quality = NA, 
         equinox = NA, AS = "Yes") %>% 
  # add common names
  mutate(common_name = if_else(scientific_name == "Ardenna grisea", "Sooty Shearwater", 
                               if_else(scientific_name == "Catharacta maccormicki", "South Polar Skua", 
                                       if_else(scientific_name == "Stercorarius longicaudus", "Long-tailed Skua", 
                                               if_else(scientific_name == "Pterodroma baraui", "Barau's Petrel", 
                                                       if_else(scientific_name == "Phaethon rubricauda", "Red-tailed Tropicbird", 
                                                               if_else(scientific_name == "Oceanodroma leucorhoa", "Leach's Storm Petrel", 
                                                                       if_else(scientific_name == "Pterodroma ultima", "Murphy's Petrel", 
                                                                               if_else(scientific_name == "Thalassarche melanophrys", "Black-browed Albatross", 
                                                                                       if_else(scientific_name == "Thalassarche chrysostoma", "Grey-headed albatross", 
                                                                                               if_else(scientific_name == "Diomedea exulans" , "Wandering Albatross", 
                                                                                                       if_else(scientific_name == "Calonectris borealis", "Cory's Shearwater", 
                                                                                                               if_else(scientific_name == "Puffinus gravis", "Great Shearwater", 
                                                                                                                       if_else(scientific_name == "Puffinus puffinus", "Manx Shearwater", 
                                                                                                                               if_else(scientific_name == "Procellaria aequinoctialis", "White-chinned Petrel", "NA"))))))))))))))) %>% 
  # change combined column for date and time into two separate columns
  separate(date_time, into = c('date_gmt', 'time_gmt'), sep = " ") %>% 
  #reorder columns to match STDB.DL
  select(dataset_id, scientific_name, common_name, site_name, colony_name, lat_colony, lon_colony,
         device, bird_id, track_id, original_track_id, age, sex, breed_stage, breed_status, date_gmt,
         time_gmt, latitude, longitude, argos_quality, equinox, stage, AS)


STDB.DL <- STDB.DL %>% 
  mutate(stage = NA, AS = "No")

STDB.DL <- rbind(STDB.DL, tracks2)


#### 2b. add details to AS dataset when known ####
unique(STDB.DL$common_name[which(STDB.DL$AS == "Yes")])# check the details of data from AS 

# Species 1: Sooty Shearwater
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater")
head(subset_data, n = 6)

# missing lat_colony, lon_colony, age and sex
unique(STDB.DL$bird_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater")])
ggplot()+
  # geom_sf(data = basemap) +
  geom_sf(data= borders)+
  geom_point(data = subset_data, aes(x = longitude, y = latitude, colour = bird_id), alpha = 0.5) +
  geom_point(aes(x=-51.62,y=-57.76), fill ="red", size = 3, shape = 23)  # Kidney Island
  
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater" &
                           STDB.DL$colony_name =="Kidney Island FK")] <- -57.76
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater" &
                           STDB.DL$colony_name =="Kidney Island FK")] <- -51.62


STDB.DL %>%  # plot and check that assigning colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# add age and sex based on publication (https://www.int-res.com/articles/meps2012/449/m449p277.pdf)
unique(STDB.DL$bird_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater")])
# only adults in study
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Sooty Shearwater" )] <- "adult"

# F =  6073, 6097, 6075, 6095, 6076, 6090, 6077, 6085, 3622, 6091
# M = 6092, 6079, 6093, 6100, 3623, 6083, 6088
# U = 6096
STDB.DL.test <- STDB.DL %>% 
  mutate(sex = if_else(bird_id =="88646073" | bird_id == "88646097" | bird_id == "88646075" | bird_id == "88646095" | bird_id == "88646076" | bird_id == "88646090" | bird_id == "88646077" | bird_id == "88646085" | bird_id == "84533622" | bird_id =="88646091", "female", 
                                                                            if_else(bird_id == "88646092" | bird_id == "88646079" | bird_id == "88646093" | bird_id =="88646100" | bird_id == "84533623" | bird_id == "88646083" | bird_id == "88646088", "male", 
                                                                                    if_else(bird_id == "88646096", "unknown", sex))))
# double check that sex information have been added correctly
# number of locations from sooty shearwater females
nrow(STDB.DL %>% filter(bird_id =="88646073" | bird_id == "88646097" | bird_id == "88646075" | bird_id == "88646095" | bird_id == "88646076" | bird_id == "88646090" | bird_id == "88646077" | bird_id == "88646085" | bird_id == "84533622" | bird_id =="88646091"))  
# number of  locations from Sooty shearwater males
nrow(STDB.DL %>% filter(bird_id == "88646092" | bird_id == "88646079" | bird_id == "88646093" | bird_id =="88646100" | bird_id == "84533623" | bird_id == "88646083" | bird_id == "88646088"))
# number of locations from unknown sooty shearwaters
nrow(STDB.DL %>% filter(bird_id == "88646096"))

# plot histograms of original dataset sex counts and updated with sooty shearwater classified. 
ggplot(data.frame(STDB.DL), aes(x=sex)) +
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

ggplot(data.frame(STDB.DL.test), aes(x=sex)) +
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

# do the numbers add up?
#females: previous # locations = 132437, updated # = 135398 CORRECT 132437 + 2961 = 135398
#males: previous # locations = 157473, updated = 159534 CORRECT 157473 + 2061 =  159534
#unknown: previous # locations = 217185, updated = 196222 ?? 217185 + 245 = 217430
#NA: previous # locations= 101068, updated = 117009 ?? 101068 + 0 = no change

# assign sooty shearwater sex from AS dataset to full dataset
STDB.DL$sex <- if_else(STDB.DL$bird_id =="88646073" | STDB.DL$bird_id == "88646097" | STDB.DL$bird_id == "88646075" | STDB.DL$bird_id == "88646095" | STDB.DL$bird_id == "88646076" | STDB.DL$bird_id == "88646090" | STDB.DL$bird_id == "88646077" | STDB.DL$bird_id == "88646085" | STDB.DL$bird_id == "84533622" | STDB.DL$bird_id =="88646091", "female", 
                                                                            if_else(STDB.DL$bird_id == "88646092" | STDB.DL$bird_id == "88646079" | STDB.DL$bird_id == "88646093" | STDB.DL$bird_id =="88646100" | STDB.DL$bird_id == "84533623" | STDB.DL$bird_id == "88646083" | STDB.DL$bird_id == "88646088", "male", 
                                                                                    if_else(STDB.DL$bird_id == "88646096", "unknown", STDB.DL$sex)))
# Species 2: South Polar Skua
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua")
head(subset_data, n = 6)
# missing lat_colony, lon_colony, age and sex


STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua" &
                           STDB.DL$colony_name =="King George Island (Fildes Peninsula and Ardley Island)")] <- -62.19
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua" &
                           STDB.DL$colony_name =="King George Island (Fildes Peninsula and Ardley Island)")] <- -58.95
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua" &
                           STDB.DL$colony_name =="Terre Ad<e9>lie")] <- 140.02
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua" &
                           STDB.DL$colony_name =="Terre Ad<e9>lie")] <- -66.67

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "South Polar Skua") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# all adults (from publication https://www.int-res.com/articles/meps_oa/m435p263.pdf)
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "South Polar Skua" )] <- "adult"
# birds were sexed but no info in paper based on IDs


# Species 3: Barau's petrel
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Barau's Petrel")
head(subset_data, n = 6)
# missing lat_colony, lon_colony, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Barau's Petrel" &
                           STDB.DL$colony_name =="Reunion_Island_Grand_Benare")] <- -21.12
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Barau's Petrel" &
                           STDB.DL$colony_name =="Reunion_Island_Grand_Benare")] <- 55.42

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Barau's Petrel") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

unique(STDB.DL$bird_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Barau's Petrel")])

# in the publication (https://www.int-res.com/articles/meps2010/423/m423p291.pdf)
# bird Ids do not correspond to the numbers here, so cannot add sex information
# all tagged as adults 
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Barau's Petrel" )] <- "adult"

# Species 4: red-tailed tropicbird
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird")
head(subset_data, n = 6)
# missing lat_colony, lon_colony, colony name, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird") ] <- -23.6
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird")] <- 43.61
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird")] <- "Nosy Ve, Madagascar"

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# tracked as adults
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Red-tailed Tropicbird" )] <- "adult"
# no sex infor in the publication (https://www.sciencedirect.com/science/article/pii/S0006320711004332#b0415)


# Species 5: Leach's storm petrel
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Leach's Storm Petrel")
head(subset_data, n = 6)
# missing lat_colony, lon_colony, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Leach's Storm Petrel")] <- 48.117
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Leach's Storm Petrel")] <- -52.8

STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Leach's Storm Petrel")] <- "Baccalieu Island"

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Leach's Storm Petrel") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)


# Species 6: Murphy's Petrel
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Murphy's Petrel")
head(subset_data, n = 6)
# missing lat_colony, lon_colony, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Murphy's Petrel")] <- -24.37
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Murphy's Petrel")] <- -128.33

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Murphy's Petrel") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# all tagged as adults. no sex info in paper (https://www.int-res.com/articles/meps_oa/m579p139.pdf)
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Murphy's Petrel" )] <- "adult"

# Species 7: Black-browed albatross
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross")
head(subset_data, n = 6)
# missing lat and lon colony, colony name, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross")] <- -54.00
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross")] <- -38.05
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross")] <- "Bird Island, South Georgia"

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# all tagged as adults
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Black-browed Albatross" )] <- "adult"

# meta data from Anne-Sophie has sex info
unique(subset_data$bird_id)
# females: 1141192, 1147164, 1254477, 1181691, 1254489, 1301150, 1181222, 1301183
# males: 1301149, 5079777, 1254460, 1320029, 1320030, 1271657, 1320441, 1141384, 1145058, 1301567, 1301180, 1254490, 1254433

STDB.DL$sex <- if_else(STDB.DL$bird_id =="1141192" | STDB.DL$bird_id == "1147164" | STDB.DL$bird_id == "1254477" | STDB.DL$bird_id == "1181691" | STDB.DL$bird_id == "1254489" | STDB.DL$bird_id == "1301150" | STDB.DL$bird_id == "1181222" | STDB.DL$bird_id == "1301183" , "female", 
                       if_else(STDB.DL$bird_id == "1301149" | STDB.DL$bird_id == "5079777" | STDB.DL$bird_id == "1254460" | STDB.DL$bird_id =="1320029" | STDB.DL$bird_id == "1320030" | 
                                 STDB.DL$bird_id == "1271657" | STDB.DL$bird_id == "1320441" | STDB.DL$bird_id == "1141384" | STDB.DL$bird_id == "1145058" | STDB.DL$bird_id == "1301567" | 
                                 STDB.DL$bird_id == "1301180" | STDB.DL$bird_id ==  "1254490" | STDB.DL$bird_id == "1254433", "male", STDB.DL$sex))

# Species 8: Grey-headed albatross
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross")
head(subset_data, n = 6)
# missing lat and lon colony, colony name, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross")] <- -54.00
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross")] <- -38.05
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross")] <- "Bird Island, South Georgia"

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# all tagged as adults
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Grey-headed albatross" )] <- "adult"

# meta data from Anne-Sophie has sex info
unique(subset_data$bird_id)
# female: W153, W152, U080, O582
# male:
# unknown: W159, W156, W150
# no info for 0702b18
STDB.DL$sex <- if_else(STDB.DL$bird_id =="W153" | STDB.DL$bird_id == "W152" | STDB.DL$bird_id == "U080" | STDB.DL$bird_id == "O582" , "female", 
                       if_else(STDB.DL$bird_id == "W159" | STDB.DL$bird_id == "W156" | STDB.DL$bird_id == "W150", "unknown", STDB.DL$sex))

# Species 9: Wandering albatross
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross")
head(subset_data, n = 6)

# missing lat and lon colony, colony name, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross")] <- -54.00
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross")] <- -38.05
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross")] <- "Bird Island, South Georgia"


STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Wandering Albatross") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

unique(STDB.DL$bird_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross")])

# all tagged as breeders = adults
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Wandering Albatross" )] <- "adult"

# sex info in metadata from Anne-Sophie
# female: 5132339, 5142629, 5146288
# male: 5143006, 5147491, 5121065

STDB.DL$sex <- if_else(STDB.DL$bird_id =="5132339" | STDB.DL$bird_id == "5142629" | STDB.DL$bird_id == "5146288" , "female", 
                       if_else(STDB.DL$bird_id == "5143006" | STDB.DL$bird_id == "5147491" | STDB.DL$bird_id == "5121065", "male", STDB.DL$sex))

# Species 10: Cory's shearwater
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Cory's Shearwater")
head(subset_data, n = 6)
# missing lat and lon colony, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Cory's Shearwater")] <- 30.15
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Cory's Shearwater")] <- -15.87

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Cory's Shearwater") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# no meta data from Anne-Sophie or publication to refer to for other missing info

# Species 11: Great Shearwater
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Great Shearwater")
head(subset_data, n = 6)

# missing lat and lon colony, colony name age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Great Shearwater")] <- -40.35
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Great Shearwater")] <- -9.88
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Great Shearwater")] <- "Gough Island"

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Great Shearwater") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)


# no metadata or publication for missing age and sex data

# Species 12: Manx Shearwater
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Manx Shearwater")
head(subset_data, n = 6)
# missing lat and lon colony, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Manx Shearwater")] <- 51.737
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Manx Shearwater")] <- -5.298

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Manx Shearwater") %>% 
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# publication (https://royalsocietypublishing.org/doi/full/10.1098/rspb.2008.1577): tags on breeders
STDB.DL$age[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Manx Shearwater" )] <- "adult"

# metadata contains sex information
unique(STDB.DL$bird_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Manx Shearwater")])
# female: eb52820, ex41736, ex41738, ex83002, ex83006, ex93604, fb20883, fb24549, fb30249, fb30553, fb30567, fb32344, fp52833, fr53294
# male: eb52813, el60769, ex41752, ex93605, ex93923, fb24522, fb30390, fb30683, fb32245, fb32249, fb32297, fb32319
# unknown: eb52824, ex93836, ex93862, ex93919

STDB.DL$sex <- if_else(STDB.DL$bird_id == "eb52820" | STDB.DL$bird_id =="ex41736" | STDB.DL$bird_id == "ex41738" | STDB.DL$bird_id == "ex83002" | STDB.DL$bird_id == "ex83006" | STDB.DL$bird_id == "ex93604" | STDB.DL$bird_id == "fb20883" | STDB.DL$bird_id == "fb24549" | STDB.DL$bird_id == "fb30249" | STDB.DL$bird_id == "fb30553" | STDB.DL$bird_id == "fb30567" | STDB.DL$bird_id == "fb32344" | STDB.DL$bird_id == "fp52833" | STDB.DL$bird_id == "fr53294" , "female", 
                       if_else(STDB.DL$bird_id == "eb52813" | STDB.DL$bird_id == "el60769" | STDB.DL$bird_id == "ex41752" | STDB.DL$bird_id == "ex93605" | STDB.DL$bird_id == "ex93923" | STDB.DL$bird_id == "fb24522" | STDB.DL$bird_id == "fb30390" | STDB.DL$bird_id == "fb30683" | STDB.DL$bird_id == "fb32245" | STDB.DL$bird_id == "fb32249" | STDB.DL$bird_id == "fb32297" | STDB.DL$bird_id == "fb32319",  "male", 
                               if_else(STDB.DL$bird_id == "eb52824" | STDB.DL$bird_id == "ex93836" | STDB.DL$bird_id == "ex93862" | STDB.DL$bird_id == "ex93919", "unknown", STDB.DL$sex)))

#  Species 13: White-chinned petrel
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel")
head(subset_data, n = 6)
# missing lat and lon colony, colony name, age and sex
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel")] <- -49.68
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel")] <- 178.8
STDB.DL$colony_name[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel")] <- "Antipodes Islands"
STDB.DL$dataset_id[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel")] <- 627

# meta data - datasets are from STDB (627, 634 and 635) Have downloaded 627 and 635 - so why were these duplicates not removed?
# Anne-Sophie only sent birds from STDB 627 originally. I have this from STDB download
# merge the two to keep the information from both (ie. dataset_id, sex, age from STDB and stage from AS)

STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "White-chinned Petrel") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

# Species 14. Long tailed Skua
subset_data <- subset(STDB.DL, STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua")
head(subset_data, n = 6)
# missing lat and lon colony, age and sex
unique(subset_data$bird_id)
# some individuals do not have a colony name - find these and assign
P <- STDB.DL %>% filter(common_name == "Long-tailed Skua") %>% 
  filter(is.na(colony_name))
unique(P$bird_id)
# AMM 6107271,  6107299, 6149555, 6149570, 6149573, 6175744, 6175745, 6149585, 6175746, 6175747, 6175753
# SVA 6218052, 6218053, 6218057, 6223840
# ZAC 6238720
STDB.DL$colony_name <- if_else(STDB.DL$bird_id == "6107271" | STDB.DL$bird_id == "6107299" | STDB.DL$bird_id == "6149555"| STDB.DL$bird_id == "6149570" | STDB.DL$bird_id == "6149573" | STDB.DL$bird_id == "6175744" | STDB.DL$bird_id == "6175745" | STDB.DL$bird_id == "6149585" | STDB.DL$bird_id =="6175746" | STDB.DL$bird_id == "6175747" | STDB.DL$bird_id =="6175753" , "AMM", 
                               if_else(STDB.DL$bird_id == "6218052" | STDB.DL$bird_id == "6218053" |STDB.DL$bird_id == "6218057" | STDB.DL$bird_id == "6223840",  "SVA", 
                                       if_else(STDB.DL$bird_id == "6238720" , "ZAC", STDB.DL$colony_name)))



STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="KVP")] <- 72.55417
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="KVP")] <- -23.7784
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="HOC")] <-75.15 
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="HOC")] <- -19.667
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="AMM")] <- 66.0048
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="AMM")] <- 16.02
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="SVA")] <- 78.9
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="SVA")] <- 12.217
STDB.DL$lat_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="ZAC")] <- 74.483
STDB.DL$lon_colony[which(STDB.DL$AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua" &
                           STDB.DL$colony_name =="ZAC")] <- -20.58




STDB.DL %>%  # plot and check that assinging colony lat and lon worked correctly
  filter(AS == "Yes" & STDB.DL$common_name == "Long-tailed Skua") %>% 
  ggplot() +
  geom_sf(data = borders) +
  geom_point(aes(x = longitude, y = latitude, colour = colony_name), alpha = 0.5) +
  geom_point(aes(x = as.numeric(lon_colony), y = as.numeric(lat_colony)), alpha = 0.5, size = 5, fill ="red",  shape = 23)

#### 2c. add Arctic tern data from R.Fijn (email request) ####

setwd("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/Flyways data_STDB fulfilled data requests/Fijn ARTE data") # for the next two lines to work, the working directory needs to be in the folder

file_names2 <- list.files(pattern = "*.csv") # list of all  STDB databases downloaded
ARTE.DL <- do.call(rbind,lapply(file_names2,read.csv)) # open all csvs 

setwd("./../..") # change directory back to higher up folder now that the STDB data frames are open

# match colnames to STDB.DL dataframe
colnames(ARTE.DL)
colnames(STDB.DL)

# Colony Eemshaven		
# Col_long	6.83	
# Col_lat 53.45


ARTE.DL2 <- ARTE.DL %>% 
  rename(bird_id = Bird_ID, date_gmt = Date..GMT., time_gmt = Time..GMT., latitude = Latitude, longitude = Longitude) %>% 
  # add columns that are missing and in STDB.DL
  mutate(dataset_id = NA,  scientific_name = "Sterna paradisaea", common_name = "Arctic Tern", 
         site_name = NA, colony_name = "Eemshaven", lat_colony = 53.45, lon_colony = 6.83, 
         device = "GLS", track_id = paste0('NonDB_', bird_id, '_NA'), original_track_id = NA,
         age = "adult", sex = NA, breed_stage = NA, breed_status = NA, argos_quality = NA, 
         equinox = NA, stage = NA, AS = "No") %>% 
  #reorder columns to match STDB.DL
  select(dataset_id, scientific_name, common_name, site_name, colony_name, lat_colony, lon_colony,
         device, bird_id, track_id, original_track_id, age, sex, breed_stage, breed_status, date_gmt,
         time_gmt, latitude, longitude, argos_quality, equinox, stage, AS)


STDB.DL <- rbind(STDB.DL, ARTE.DL2)
# add mssing colony name for Blue Petrel
STDB.DL <- STDB.DL %>% 
  mutate(colony_name = ifelse(common_name == "Blue Petrel" | common_name == "Slender-billed Prion" | common_name == "Antarctic Prion",
                                         "Kerguelen",
                                         colony_name)) %>% 
  mutate(common_name = ifelse(common_name == "Grey-headed albatross", 
                              "Grey-headed Albatross", 
                              common_name))

#### 2d. Arctic tern data from Autumn-Lynn sorting ####

PacARTE <- read.csv("./Flyways data_STDB fulfilled data requests/Autumn-Lynn data/MCPARTE_Harrison_Mallory_Atlas_Classified.csv")

colnames(PacARTE)
unique(PacARTE$id)

# plot each individual and remove any non-migratory
PacARTE %>% 
  filter(id == "BF334") %>%  
ggplot()+
  # geom_sf(data = ocean)+
  geom_point(aes(x = lon,y = lat))

# fix dates (some in american mdy some in normal dmy)
PacARTE2 <- PacARTE %>% 
  mutate(POSIX = parsedate::parse_date(date)) %>% 
  mutate(dataset_id = NA,  scientific_name = "Sterna paradisaea", common_name = "Arctic Tern", 
         site_name = NA, device = "GLS", bird_id = id, track_id = paste0('NonDB_',id, '_NA'), original_track_id = NA,
         age = "adult", sex = NA, breed_stage = NA, breed_status = annualcycle, 
         date_gmt = as.Date(POSIX), time_gmt = strftime(POSIX, format = "%H:%M:%S", tz = "UTC"), argos_quality = NA, 
         equinox = NA, stage = NA, AS = "No") %>% 
  rename(lat_colony = colony_lat, lon_colony = colony_lon, latitude = lat, longitude = lon) %>% 
  dplyr::select(dataset_id, scientific_name, common_name, site_name, colony_name, lat_colony, lon_colony,
         device, bird_id, track_id, original_track_id, age, sex, breed_stage, breed_status, date_gmt,
         time_gmt, latitude, longitude, argos_quality, equinox, stage, AS, POSIX)

# add displacement
individuals <- data.frame(unique(PacARTE2$track_id))
PacARTE3 <- NULL
for (j in 1:nrow(individuals)) {
 ID <- print(individuals[j,])
  subset <- PacARTE2 %>% filter(track_id == ID)
  
for (i in 1:nrow(subset)) {
  subset[i,"disp"] = geosphere::distGeo(c(subset[i,"longitude"],subset[i,"latitude"]),c(subset[i,"lon_colony"], subset[i,"lat_colony"]))/1000
  
}
PacARTE3 <- rbind(PacARTE3, subset)
}

# some ARTE have lon values < -180, fix these rows
for (i in 1:nrow(PacARTE3)) {
  
  if(PacARTE3[i, "longitude"] < -180){
    
    PacARTE3[i, "longitude"] <- PacARTE3[i, "longitude"] + 360
    
  }else{
    
    PacARTE3[i, "longitude"] <- PacARTE3[i, "longitude"]
  }
  
  
}
PacARTE3$longitude[PacARTE3$longitude < -180] <- 55

PacARTE4 <- PacARTE3 %>% 
  dplyr::select(1:23, disp, POSIX) #reorder columns again

#### 3. FILTER DATA and add any other missing variables ####

# not every dataset has a bird_id, track_id and original_track_id. Add these if missing
# track_id generally follows the pattern 'STDB dataset ID_bird id_original track_id'. 
# original_track_id looks generally like it is the tracking device number

# IF ADDING ONE DATASET THAT WAS APPROVED LATER OPEN HERE. IF FIRST TIME SKIP THE NEXT  2LINES
STDB.DL <- read.csv("Combining Anne-Sophie and STDB download data_adding colony locations to calcuate NSD_birds with less than 40 locations removed_updated dataset_10May2023.csv")
TBprion <- TBprion %>% mutate(stage = NA, AS = "No")
STDB.DL <- rbind(STDB.DL, TBprion)

# Subsetting the track_ids with NA, change the values and then add back to main dataframe
STDB.DL.filt.2 <-  subset(STDB.DL, is.na(track_id)) %>% 
  mutate(track_id = paste0('NonDB_', bird_id, '_NA'))

STDB.DL <- STDB.DL %>% drop_na(track_id)

STDB.DL.filt <- rbind(STDB.DL, STDB.DL.filt.2)

STDB.DL <- STDB.DL.filt

# remove any duplicates
STDB.DLtest <- STDB.DL %>% 
  distinct(track_id, latitude, longitude, date_gmt,time_gmt, .keep_all= TRUE)

STDB.DL <- STDB.DLtest 
STDB.DL <- STDB.DL[!is.na(STDB.DL$longitude), ] # five missing coords

# remove juveniles and immatures
STDB.DL <- STDB.DL %>% filter((age != "immature" & age != "juvenile") %>% replace_na(TRUE)) # replace_na(TRUE) stops rows with NA in age column being deleted

#  change date format (not all rows match some in year-month-day; others in day/month/year)
STDB.DL$date_gmt <- parse_date_time(STDB.DL$date_gmt, orders = c("ymd", "dmy"))

# remove birds with < 20 locations
STDB.DL.filt <- STDB.DL %>% group_by(bird_id, track_id) %>% filter(n() >= 40)
# length(unique(STDB.DL$track_id)) # before filtering by minimum numberr of locations
length(unique(STDB.DL.filt$track_id)) # number of individuals
length(unique(STDB.DL.filt$common_name)) # number of species
length(STDB.DL.filt$track_id) # number of locations

STDB.DL <- STDB.DL.filt

# check which datasets have missing colony (birds captured at sea)
unique(STDB.DL$dataset_id[which(is.na(STDB.DL$lat_colony))])
# dataset 464 from STDB has no lat and lon colony locations - they were caught at sea https://www.publish.csiro.au/mu/pdf/MU9949 
# will have to replace the blanks with the first location recorded by each individual

STDB.DL.464 <- STDB.DL %>% 
  subset(dataset_id == 464)

# remove dataset 464
STDB.DL <- STDB.DL %>% filter((dataset_id != 464)%>% replace_na(TRUE))

# spelling error on species: Thalassarche melanophrys and Thalassarche melanophris

STDB.DL.Tm <- STDB.DL %>% 
  subset(scientific_name == "Thalassarche melanophris" | scientific_name == "Thalassarche melanophrys")

STDB.DL <- STDB.DL %>% 
  mutate(scientific_name = replace(scientific_name, scientific_name == "Thalassarche melanophrys", "Thalassarche melanophris"))

# two scientific names used for brown skua - change to one
STDB.DL <- STDB.DL %>% 
  mutate(scientific_name= replace(scientific_name, scientific_name == "Stercorarius antarcticus", "Catharacta antarctica"))

# colony name missing from black petrel STDB ID 659
STDB.DL <- STDB.DL %>%
  mutate(colony_name = replace(colony_name, is.na(colony_name) & common_name == "Black Petrel", "Little Barrier Island"))

# dataset 2075 - uses 360 to cross the date line. Make any longitude values > +180 into standarad -180 : +180 scale
STDB.DL <- STDB.DL %>% 
  # filter(dataset_id == "2075") %>% 
  mutate(longitude = case_when(
  longitude >= 180 ~ longitude-360, 
  TRUE   ~ longitude 
))
  
# plot for locations and colonies of all species
# new dataframe with all unque individuals listed
species <- data.frame(unique(STDB.DL$common_name)) # 2180 individuals - should produce same number of displacement plots in the folder using loop below 

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

#### 3e. save the combined data with colony names and locations to a new csv to use ####
write.csv(STDB.DL, "Combining Anne-Sophie and STDB download data_adding colony locations to calcuate NSD_birds with less than 40 locations removed_updated dataset_16May2023.csv", row.names = FALSE)

STDB.DL <- read.csv("Combining Anne-Sophie and STDB download data_adding colony locations to calcuate NSD_birds with less than 40 locations removed_updated dataset_16May2023.csv")

write.csv(STDB.DL, "Extra eight species_adding colony locations to calcuate NSD_birds with less than 40 locations removed_updated dataset_18Jul2023.csv", row.names = FALSE)
#### 3 c. calculate the displacement of all remaining individuals and then further filtering steps follow ####
# start by calculating displacement and plotting
# calculating displacement distance

# list all the individuals in another dataframe. 
# use this dataframe to select one individual in the outer most for loop

#test with smaller dataframe
STDB.DLsm <- STDB.DL %>% 
  drop_na(lat_colony) %>% 
  drop_na(lon_colony) # %>% # remove any with missing colony coordinates - dataset 464
# arrange(scientific_name, dataset_id, bird_id)%>% # for the smaller subset ensure in correct order as slicing based on row number in next line
# slice(595000:704239)
# change from tibble to dataframe
STDB.DLsm <-  as.data.frame(STDB.DLsm)

# new dataframe with all unique individuals listed
individuals <- data.frame(unique(STDB.DLsm$track_id)) # 2040 individuals - should produce same number of displacement plots in the folder using loop below 
# when just adding one dataframe use the following: 
individuals<- data.frame(unique(STDB.DLsm$track_id[which(STDB.DLsm$dataset_id == "1541")]))

for (j in 1:nrow(individuals)) { # outer loop to select one individual at a time
  ID <- print(individuals[j,])
  subset <- STDB.DLsm %>% filter(track_id == ID)
  
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


# create separate dataframes for individuals with < 12 months, between 12 and 24 months, between 24 and 36 months and > 36 months of data

STDB.DL$disp <- NA
STDB.DL <- as.data.frame(STDB.DL)
for (i in 1:nrow(STDB.DL)) {

  STDB.DL[i,"disp"] = geosphere::distGeo(c(STDB.DL[i,"longitude"],STDB.DL[i,"latitude"]),c(STDB.DL[i,"lon_colony"], STDB.DL[i,"lat_colony"]))/1000
  print(i)
}

#### 3g. save / open data frame with all locations filtered up to displacement including column with displacements ####
write.csv(STDB.DL, "Combined dataset of STDB downloads, A-S, and ARTE_colony displacement calculated_filtering dataset as far as displacement_updated dataset_16May2023.csv", row.names =  FALSE)
STDB.DL <- read.csv("Combined dataset of STDB downloads, A-S, and ARTE_colony displacement calculated_filtering dataset as far as displacement_updated dataset_16May2023.csv")

write.csv(STDB.DL, "Extra eight species_colony displacement calculated_filtering dataset as far as displacement_updated dataset_18July2023.csv", row.names = FALSE)
##### 3h. filter by displacement - number of time crosses the mid-distance ####
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
