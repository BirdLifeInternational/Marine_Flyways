library(dplyr)
library(dtwclust)
library(dtw)
library(TSclust) # trying a few package options initially
# devtools::install_github("fpsanz/tscR")
# library(tscR)
library(amt)
library(tidyr)
library(ggplot2)
library(gplots)
library(data.table)
library(zoo)
library(rnaturalearth)
library(sf)
library(terra)
library(gridExtra)

#### Dynamic time-warping clustering analysis ####

# open and organise uptodate data set
#Indian
tracks <- read.csv("./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/a.Migration assigned_IndOc datasets auto assign.csv")
#Atlantic
tracks <- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a. Migration assigned_ALL AtlOc datasets auto assign_individuals 1 to 542.csv") 
# Pacific
tracks <- read.csv("./Figures/Autoassigning methods/simplified methods/PacOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_PacOc datasets auto assign.csv") 
# Southern
tracks <- read.csv("./Figures/Autoassigning methods/simplified methods/SouthOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_SOc datasets auto assign.csv") 
 # open full dataset before migration assigned --->'read.csv("./Archived dataframes/data filtered by migration based on displacement plots_MIGRATION.v6_reclassified.csv")
  # open Anne-Sophie's sorted data --->  read.csv("STDB_data from Anne-Sophie_migration and winter periods already separated.csv")
unique(tracks$common_name)

# change date time column to POSIXct
# AS DATASET ONLY: change the date_time column into a posix and remove the letters
# tracks$date_time <- stringr::str_replace_all(tracks$date_time, c("T" = " ", "Z" = "")) # remove letters

tracks <- tracks %>% 
  mutate(POSIX = as.POSIXct(POSIX,  tz = "UTC")) %>% # add  additional column with a POSIXct class date and time
  mutate(track_id = as.factor(track_id))
# open basemap
countries <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))

#### Organising Indian Ocean species #####
#filter and cluster one Indian Ocean species
# TRPL <- tracks %>% 
#   filter(common_name == "Trindade Petrel")
# 
# plot(latitude~longitude, data=TRPL,  asp=1, main="",
#      frame = T, ylab="", xlab=paste(TRPL$scientific_name[1]), 
#      col= track_id, pch = 16, cex = 0.5)
# plot(countries, col='lightgrey', add=T)
# 
# RTTB <- tracks %>% 
#   filter(common_name == "Red-tailed Tropicbird")
# plot(latitude~longitude, data=RTTB,  asp=1, main="",
#      frame = T, ylab="", xlab=paste(RTTB$scientific_name[1]), 
#      col= track_id, pch = 16, cex = 0.5)
# plot(countries, col='lightgrey', add=T)
# 
# BAPL <- tracks %>% 
#   filter(common_name == "Barau's Petrel") %>% 
#   filter(AS == "Yes")
# plot(latitude~longitude, data=BAPL,  asp=1, main="",
#      frame = T, ylab="", xlab=paste(BAPL$scientific_name[1]), 
#      col= track_id, pch = 16, cex = 0.5)
# plot(countries, col='lightgrey', add=T)
# 
# BBAL <- tracks %>% 
#   filter(common_name == "Black-browed Albatross")
#combine two species
# RTTB_BAPL <- rbind(RTTB, BAPL)

####  Organising Indian Ocean Species ####

# open list of track IDs for Indian Ocean species (IF USING A DATAFRAME THAT HASN'T ALREADY BEEN FILTERED BY OCEAN)
tracklist <- read.csv("./Indian Ocean/List of track IDs in Indian Ocean.csv")
INDO <- tracks %>% 
  filter(track_id %in% tracklist$track_id) %>% 
  mutate(common_name = as.factor(common_name))
  # filter(common_name == "Antarctic Prion"  | common_name == "Barau's Petrel" | common_name == "Black-browed Albatross" |
  #          common_name == "Red-tailed Tropicbird" | common_name == "Trindade Petrel" | common_name == "Wedge-tailed Shearwater")

# Indian Ocean equinox plots already checked and any wrong individuals (diff ocean, no migration) removed in autoassign script

# Indian Ocean data divide birds with both outward and return in two - change the track_ids so it appears as though there are two individuals
INDO.outward <- INDO %>% 
  filter(mig.stage2 == "1outward")
INDO.return <- INDO %>% 
  filter(mig.stage2 == "3return")

length(unique(INDO.outward$track_id))
length(unique(INDO.return$track_id))

INDO.outward <- INDO.outward %>% 
  mutate(track_id=  paste0(track_id,".a"))
INDO.return<- INDO.return %>% 
  mutate(track_id = paste0(track_id, ".b"))

INDO1 <- rbind(INDO.outward, INDO.return)
INDO1 <- INDO1 %>% 
  arrange(dataset_id, track_id, POSIX)

#### Organising Atlantic Ocean species ####
# remove individuals where individual plots have been checked and they are in the wrong ocean / there is no migration etc. 
ATLO <- tracks %>% 
  filter(track_id != "439_6759_849" & track_id != "439_6773_852" & track_id != "487_121-44624_2628" &#
           track_id != "493_O071_3048" & track_id != "493_O124_3161" & track_id != "493_O313_3068" & track_id != "493_O564_3191" &
           track_id != "493_O564_3192" & track_id != "493_O645_3100" & track_id != "493_U042_3210" & 
           track_id != "518_5_4179" & track_id != "627_k4380_6424" & track_id != "663_96807a_7516"  &
           track_id != "663_96809a_7520" & track_id != "739_705_ARTE_395_10324" & track_id != "858_Ringeye_4_64272" &
           track_id != "971_3M005921_68243" & track_id != "982_89522129_77695" & track_id != "1295_cf40857_78012" &
           track_id != "1500_HT65423_89366" & track_id != "1500_HT65425_89367" & track_id != "1500_HT65632_89370" &
           track_id != "1500_HT65634_89372" & track_id != "1500_HT65678_89378" & track_id != "1692_6059707_104452" &
           track_id != "1704_0201_wht-blk_104625" & track_id != "1704_F01796_104643" & track_id != "1704_F01796_104644" &
           track_id != "1705_5323910_104658" & track_id != "NonDB_88646093_NA" & track_id != "NonDB_88646097_NA" &
           track_id != "NonDB_EA 108367_NA" & track_id != "NonDB_EA 123801_NA" & track_id != "NonDB_EA 138372_NA" &
           track_id != "NonDB_EA 156170_NA" & track_id != "NonDB_EA 156238_NA" & track_id != "NonDB_fb32297_NA" & 
           track_id != "NonDB_fp52833_NA" & track_id != "NonDB_EA 127268_NA") %>% 
  droplevels()

# reassign the two GPS Arctic Tern tracks (change 2winter and 3return to 1outbound)

ATLO1 <- ATLO 
  ATLO1$mig.stage2[ATLO1$track_id == "1905_BH_16806_115012" & ATLO1$mig.stage2 == '2winter'] <- "1outward"
  ATLO1$mig.stage2[ATLO1$track_id == "1905_BH_16806_115012" & ATLO1$mig.stage2 == '3return'] <- "1outward"
  ATLO1$mig.stage2[ATLO1$track_id == "1905_CD_21829_115013" & ATLO1$mig.stage2 == '2winter'] <- "1outward"
  ATLO1$mig.stage2[ATLO1$track_id == "1905_CD_21829_115013" & ATLO1$mig.stage2 == '3return'] <- "1outward"
  
ATLO <-ATLO1
unique(ATLO$common_name)
    

# for Atlantic data divide birds with both outward and return in two - change the track_ids so it appears as though there are two individuals
ATLO.outward <- ATLO %>% 
  filter(mig.stage2 == "1outward")
ATLO.return <- ATLO %>% 
  filter(mig.stage2 == "3return")

length(unique(ATLO.outward$track_id))
length(unique(ATLO.return$track_id))

ATLO.outward <- ATLO.outward %>% 
  mutate(track_id=  paste0(track_id,".a"))
ATLO.return<- ATLO.return %>% 
  mutate(track_id = paste0(track_id, ".b"))

ATLO1 <- rbind(ATLO.outward, ATLO.return)
ATLO1 <- ATLO1 %>% 
  arrange(dataset_id, track_id, POSIX)

##### Organising Pacific Ocean Birds ####
PAOC <- tracks

# for Pacific data divide birds with both outward and return in two - change the track_ids so it appears as though there are two individuals
PAOC.outward <- PAOC %>% 
  filter(mig.stage2 == "1outward")
PAOC.return <- PAOC %>% 
  filter(mig.stage2 == "3return")

length(unique(PAOC.outward$track_id))
length(unique(PAOC.return$track_id))

PAOC.outward <- PAOC.outward %>% 
  mutate(track_id=  paste0(track_id,".a"))
PAOC.return<- PAOC.return %>% 
  mutate(track_id = paste0(track_id, ".b"))

PAOC1 <- rbind(PAOC.outward, PAOC.return)
PAOC1 <- PAOC1 %>% 
  arrange(dataset_id, track_id, POSIX)

#remove wrong location in Atlantic
PAOC1 <- PAOC1 %>% 
  slice(1:39265, 39271:59335)

# plot the raw locations
plot(latitude~longitude, data=PAOC1,  asp=1, main="",
     frame = T, ylab="", xlab="Pacific Ocean species n = 522", 
     col= as.factor(common_name), pch = 16, cex = 0.5)
plot(countries, col='lightgrey', add=T)

# For Southern Filter Outward and return
SOOC <- tracks %>% 
  filter(mig.stage2 == "1outward" | mig.stage2 == "3return")

# cluster by net square colony displacement
# NOTE: amt calcualtes NSD from the first location (which is not necessarily the colony)  - go back at some point and add 
# the first row to each bird with the colony locations

# INDO.noBBAL <- INDO %>% 
#   filter(common_name != "Black-browed Albatross")
# INDO.noBBAL.ATPR <- INDO %>% 
#   filter(common_name != "Black-browed Albatross") %>% 
#   filter(common_name != "Antarctic Prion")
# INDO.noBBAL.ATPR.TRPL <- INDO %>% 
#   filter(common_name != "Black-browed Albatross") %>% 
#   filter(common_name != "Antarctic Prion") %>% 
#   filter(common_name != "Trindade Petrel")

# calculate distance between points
# then do hierarchical clustering dtw::
# plot dendogram plot(clusterobject)

# DATA FORMATTING
# need a dataframe where each column is an individual to cluster (i.e. column per bird ID) and the rows are the NSD 
# to find out - presumably the rows are in time order


# INDO.noBBAL.ATPR.track <- make_track(INDO.noBBAL.ATPR, longitude, latitude, POSIX, mig.stage2, common_name, id = track_id, crs = 4326)
# INDO.noBBAL.ATPR.FRMT <- data.frame(amt::add_nsd(INDO.noBBAL.ATPR.track))  # the output is the sum of (locx1 - locX2)^2 and  (locY1 - locY2)^2
# 
# TRPL.track <- make_track(TRPL, longitude, latitude, POSIX, stage, id = track_id, crs = 4326)
# TRPL.FRMT <- data.frame(amt::add_nsd(TRPL.track))  # the output is the sum of (locx1 - locX2)^2 and  (locY1 - locY2)^2


INDO.track <- make_track(INDO1, longitude, latitude, POSIX, stage, id = track_id, crs = 4326)
INDO.FRMT <- data.frame(amt::add_nsd(INDO.track))  # the output is the sum of (locx1 - locX2)^2 and  (locY1 - locY2)^2

ATLO.track <- make_track(ATLO1, longitude, latitude, POSIX, mig.stage2, common_name,id = track_id, crs = 4326)
ATLO.FRMT <- data.frame(amt::add_nsd(ATLO.track))

PAOC.track <- make_track(PAOC1, longitude, latitude, POSIX, mig.stage2, common_name,id = track_id, crs = 4326)
PAOC.FRMT <- data.frame(amt::add_nsd(PAOC.track))

# it hasn't sqrt although online reading suggests that the NSD is the sqrt?

# ggplot(TRPL.FRMT)+
ggplot(INDO.FRMT)+
  geom_line(aes(t_, nsd_, colour = id), lwd = 1.3) + 
  xlab("Date") +
  ylab("NSD from colony (km)") +
  theme_bw()+
  theme(legend.position = "none")

#### NOTE: check whether they all have a full cycle before clustering - could this be causing some issues 
# remove any that do not have locations for breeding, outbound and return

# change all stage to migration (only if data have not already been assigned)
# TRPL$stage <- "migration"
# INDO$stage2 <- ifelse(is.na(INDO$stage), "migration", INDO$stage)
# INDO.noBBAL$stage2 <- ifelse(is.na(INDO.noBBAL$stage), "migration", INDO$stage)
# INDO.noBBAL.ATPR$stage2 <- ifelse(is.na(INDO.noBBAL.ATPR$stage), "migration", INDO$stage)
# INDO.noBBAL.ATPR.TRPL$stage2 <- ifelse(is.na(INDO.noBBAL.ATPR.TRPL$stage), "migration", INDO$stage)
aggregate(data = INDO1, longitude~ stage+ track_id, FUN = length)

#### organising the dataframe format for first method of clustering ####
# INDO.noBBAL.ATPR.TRPL.FRMT <- INDO.noBBAL.ATPR.TRPL.FRMT %>% 
#   group_by(id) %>% 
#   arrange(t_, .by_group = TRUE)# %>% 
#   # dplyr::select(c(id, nsd_))
#  
# 
# # transpose so that one column per bird 
# INDO.noBBAL.ATPR.TRPL.clust <- INDO.noBBAL.ATPR.TRPL.FRMT[,c(5,2)] # column with track_id and nsd_
#   
# INDO.noBBAL.ATPR.TRPL.clust2 <- dcast(setDT(INDO.noBBAL.ATPR.TRPL.clust), 
#                   id ~ rowid(id), 
#                   value.var = "y_")
# INDO.noBBAL.ATPR.TRPL.clust3 <- as.data.frame(t(INDO.noBBAL.ATPR.TRPL.clust2))
#   names(INDO.noBBAL.ATPR.TRPL.clust3) <- INDO.noBBAL.ATPR.TRPL.clust3[1,]
#   INDO.noBBAL.ATPR.TRPL.clust3 <- INDO.noBBAL.ATPR.TRPL.clust3[-1,]
#   
# 
# # make an empty data frame to populate with the values
# # TRPL.clust <- data.frame((colnames(TRPL.FRMT$id)))
# # columns <- c(unique(TRPL.FRMT$id))
# # TRPL.clust <-  data.frame(matrix(nrow = 0, ncol = length(columns)))
# 
# 
# ### try plotting a few individuals to visualise any route differences
# 
# # ggplot() +
# #   geom_sf(data = countries) +
# #   geom_point(data = bird_1810_587220_108867, aes(x = x_, y = y_), colour = "blue")+
# #   geom_point(data = bird_1810_587238_108870, aes(x = x_, y = y_), colour = "orange") +
# #   geom_point(data = bird_1810_587242_108871, aes(x = x_, y = y_), colour = "purple") +
# #   geom_point(data = bird_1810_587246_108872, aes(x = x_, y = y_), colour = "green")
# #   geom_point(data = bird_eb52813, aes(x = x_, y = y_), colour = "blue") + 
#   # geom_point(data = bird_eb52820, aes(x = x_, y = y_), colour = "orange") + # missing lots of migration locations - consider removing
#   # geom_point(data = bird_eb52824, aes(x = x_, y = y_), colour = "purple") + # similar to eb52813 - should cluster together
#   # geom_point(data = bird_el60769, aes(x = x_, y = y_), colour = "green") + # missing data  during migration - may cause issues
#   # geom_point(data = bird_ex41736, aes(x = x_, y = y_), colour = "red")
#   
#   
# 
# # dtw_dist <- function(x){dist(x, method="DTW")}
# INDO.noBBAL.ATPR.TRPL.clust4 <- INDO.noBBAL.ATPR.TRPL.clust3 %>% 
#   mutate_at(c(1:38), as.numeric) # change range depending on the number of individuals
# 
# # cannot have NAs in the dissimilarity matrix (below), replace NA with last displacement value
# 
# ##### probably not a sensible way to deal with NAs
# INDO.noBBAL.ATPR.TRPL.clust5 <- na.locf(INDO.noBBAL.ATPR.TRPL.clust4) # replace all NAs in a column with the last NSD value using function from zoo package
# 
# # create a heatmap of subset to see how the clustering is working
# jpeg("./cluster analysis/example heatmap_ALL Indian Ocean_226 individuals.jpg", width = 900, height = 900)# to save
# 
# # TRPL.clust5[90:110] %>% # use sq brackets to subset
# INDO.clust5 %>%   
# as.matrix() %>%
#   gplots::heatmap.2 (
#     # dendrogram control
#     distfun = dtw_dist,
#     hclustfun = hclust,
#     dendrogram = "column",
#     Rowv = FALSE,
#     labRow = FALSE
#   )
# # ISSUE: x axis names too long - how to fit whole label in?
# dev.off()  
# 
# 
# #subset
# TRPL.clust6 <- TRPL.clust5[,90:110]
# RTTB.clust6 <- RTTB.clust5
# RTTB_BAPL.clust6 <- RTTB_BAPL.clust5
# INDO.clust6 <- INDO.clust5
# BAPL.clust6 <- BAPL.clust5
# INDO.noBBAL.clust6 <- INDO.noBBAL.clust5
# INDO.noBBAL.ATPR.clust6 <- INDO.noBBAL.ATPR.clust5
# INDO.noBBAL.ATPR.TRPL.clust6 <- INDO.noBBAL.ATPR.TRPL.clust5
# 
# #. cluster analysis
# dist_ts <- TSclust::diss(SERIES = t(INDO.noBBAL.ATPR.TRPL.clust6), METHOD = "DTWARP") # ERROR if there are NAs
# dist_lon <- TSclust::diss(SERIES = t(INDO.noBBAL.ATPR.TRPL.clust6), METHOD = "DTWARP") # ERROR if there are NAs
# 
# 
# 
# #Brainstorming methods in MarSci meeting 
# # latvec<- dist_ts[1:703]
# # lonvec <- dist_lon[1:703]
# # distboth <- dist_ts
# # 
# # vecsum <- latvec + lonvec
# # distboth[1:703] <- vecsum
# # for each matrix will need to do the same projection (one per ocean basin)
# # custom lambert albers equal area with a custom centroid / or find the UTM zone in middle
# 
# hc <- stats::hclust(dist_ts, method = "centroid")
# # hc <- stats::hclust(dist(t(TRPL.clust6)), method = "centroid") # need to transpose the data method can also be average - only use the columns with nsd, not the lat, lon
# 
# # k means clustering
# # km <- NbClust(distboth, distance = "euclidean", # euclidean distance
# #         min.nc = 2, max.nc = 8, # searching for optimal k between 
# #         # k=2 and k=51
# #         method = "kmeans", # using the k-means method
# #         index = "all") 
# 
# hclus <- stats::cutree(hc, k = 6) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
#   as.data.frame(.) %>%
#   dplyr::rename(.,cluster_group = .) %>%
#   tibble::rownames_to_column("type_col")
# 
# hcdata <- ggdendro::dendro_data(hc)
# names_order <- hcdata$labels$label
# 
# p1 <- hcdata %>%
#   ggdendro::ggdendrogram(., rotate=TRUE, leaf_labels=TRUE)
# 
# p2 <- INDO.noBBAL.ATPR.TRPL.clust6 %>%
#   dplyr::mutate(index = 1:336) %>% # change to the number of rows
#   tidyr::gather(key = type_col,value = value, -index) %>%
#   dplyr::full_join(., hclus, by = "type_col") %>% 
#   mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>% 
#   ggplot(aes(x = index, y = value, colour = cluster_group)) +
#   scale_colour_distiller(palette ="Set1")+
#   geom_line() +
#   facet_wrap(~type_col, ncol = 1, strip.position="left") + 
#   guides(color=FALSE) +
#   theme_bw() + 
#   theme(strip.background = element_blank(), strip.text = element_blank())
# 
# gp1<-ggplotGrob(p1)
# gp2<-ggplotGrob(p2) 
# 
# grid.arrange(gp2, gp1, ncol=2, widths=c(2,2))
# # save image using export - previous imges width = 800, height = 1000
# 
# # in hsclust there is a list of track_ids and which cluster they are in
# # use this to plot maps showing which bird clustered with what. 
# 
# # add cluster number to the dataset containing all other information (lat, lon etc)
# 
# INDO.noBBAL.ATPR.TRPL.map <- INDO.noBBAL.ATPR.TRPL %>% 
#   filter(track_id %in% hclus$type_col)
# names(hclus)[names(hclus) == "type_col"] <- "track_id"
# 
# # # map of the subset species locations
# # ggplot(TRPL.map)+
# #   geom_sf(data = countries)+
# #   geom_point(aes(y = latitude, x = longitude, colour = as.factor(track_id)), alpha = 0.2)+
# #   coord_sf(xlim = c(30, 100), ylim = c(30, -50))+
# #   theme_bw()+
# #   labs(colour = "Bird ID")
# # ggsave(plot = last_plot(), "./cluster analysis/Example Trindade petrel locations_21 individuals (90 -110).jpg", dpi = 300, height = 8, width = 12)
# 
# INDO.noBBAL.ATPR.TRPL.map2 <- left_join(INDO.noBBAL.ATPR.TRPL.map, hclus, by = "track_id")
# 
# mytable <- hclus %>% 
#   dplyr::arrange(cluster_group) %>% 
#   select(cluster_group, track_id)
# #separate clusters by facet  
# mp1 <- ggplot(INDO.noBBAL.ATPR.TRPL.map2)+
#   geom_sf(data = countries) +
#   geom_point(aes(x = longitude, y = latitude, group = cluster_group, 
#                                    colour = as.factor(cluster_group), alpha = 0.5))+
#   scale_colour_brewer(palette ="Set1")+
#   coord_sf(xlim = c(30, 100), ylim = c(30, -50))+
#   theme_bw()+
#   theme(legend.position = "none")+
#   facet_wrap(~cluster_group)
# 
# #separate clusters by colour
# INDO.noBBAL.ATPR.TRPL.map3 <-   INDO.noBBAL.ATPR.TRPL.map2 %>% 
#   group_by(track_id) %>% 
#   arrange(POSIX)  
# INDO.noBBAL.ATPR.TRPL.map3 <- st_as_sf(x = INDO.noBBAL.ATPR.TRPL.map2, coords = c("longitude", "latitude"), crs = 4326) 
# 
# INDO.noBBAL.ATPR.TRPLlines <- st_sf(
#   aggregate(
#     INDO.noBBAL.ATPR.TRPL.map3$geometry,
#     list(INDO.noBBAL.ATPR.TRPL.map3$cluster_group),
#     function(g){
#       st_cast(st_combine(g),"LINESTRING")
#     }
#   ))
# mp1 <- INDO.noBBAL.ATPR.TRPLlines %>% 
#   mutate(cluster.num = as.factor(Group.1)) %>% 
#   ggplot()+
#   geom_sf(data = countries)+
#   geom_sf( aes(colour = cluster.num, alpha  = 0.4), lwd = 1.2) +
#   scale_colour_brewer(palette ="Set1")+
#   coord_sf(xlim = c(30, 120), ylim = c(30, -60))+
#   theme_bw()
#   
# 
# mp2<- tableGrob(mytable, rows = NULL, theme=ttheme_minimal(base_size = 8, padding = unit(c(4, 1), "mm")))
# mpfinal <- grid.arrange(mp1, mp2, ncol = 2, widths=c(2,1))
# 
# 
# ggsave(plot = last_plot(), "./cluster analysis/example maps of 4 clusters_Indian Ocecan species minus black-browed albatross, Antarctic prion and Trindade petrel 38 individuals_lines_centroid clustering method.jpg", dpi = 300, height = 8, width = 12)
# 
# # try plotting the clusters individually to check the routes more clearly
# RColorBrewer::brewer.pal(n=6,"Set1") # check hex codes to use matching colours
# RTTB_BAPL.map2 %>% 
#   filter(cluster_group == 5) %>%
#   mutate(track_id = as.factor(track_id)) %>% 
#   ggplot()+
#   geom_sf(data = countries)+
#   geom_point(aes(x = longitude, y = latitude), color = "#FF7F00", alpha = 0.5)+
#   coord_sf(xlim = c(30, 100), ylim = c(30,-50)) +
#   theme_bw()+
#   theme(legend.position = "none")+
#   facet_wrap(~track_id)
#   
# ggsave(plot = last_plot(), "./cluster analysis/example maps of individuals in cluster 5of5_red-tailed tropicbird_9 individuals_AND_Barau's petrel_15 individuals_complete clustering method.jpg", dpi = 300, height = 8, width = 14)
# 
# 
# 
# # check the correlations between the different methods
# # Hierarchical Agglomerative Clustering
# # not selecting a different number of clusters here - only once per dataset used aboe
# h1=hclust(dist_ts, method = "mcquitty")
# h2=hclust(dist_ts,method='complete')
# h3=hclust(dist_ts,method='ward.D')
# h4=hclust(dist_ts,method='single')
# h5=hclust(dist_ts,method = "median")
# h6=hclust(dist_ts,method = "centroid")
# 
# # Cophenetic Distances, for each linkage
# c1=cophenetic(h1)
# c2=cophenetic(h2)
# c3=cophenetic(h3)
# c4=cophenetic(h4)
# c5=cophenetic(h5)
# c6=cophenetic(h6)
# 
# # Correlations  clusters
# cor(dist_ts,c1) # 0.746321 (TRPL); 0.5944575 (RTTB)
# cor(dist_ts,c2) # 0.7390315 (TRPL); 0.5856362 (RTTB)
# cor(dist_ts,c3) # 0.7366089 (TRPL); 0.5857964 (RTTB)
# cor(dist_ts,c4) # 0.654279 (TRPL); 0.6299423 (RTTB)
# cor(dist_ts,c5) # 0.5234569 (TRPL);  0.6553969 (RTTB)
# cor(dist_ts,c6) # 0.7449435 (TRPL); 0.6619033 (RTTB)

#### dataformatting as Autumn-Lynn's methods #### NO CLUSTERING IN SOUTHERN OCEAN - SKIP THESE STEPS

INDO.FRMT <- INDO.FRMT %>% 
  group_by(id) %>% 
  rename(ID = id) %>% 
  # filter(mig.stage2 == "3return") %>% # change depending on whether want to cluster ==1outward, == 3return or both !=2winter
  arrange(t_, .by_group = TRUE) %>% 
  droplevels() %>% 
  ungroup()

l = split(INDO.FRMT[,1:2], INDO.FRMT$ID)
summary(l)


# TRPL.FRMT <- TRPL.FRMT %>%
#   group_by(id) %>%
#   rename(ID = id) %>%
#   arrange(t_, .by_group = TRUE) %>%
#   droplevels() %>%
#   ungroup()
# 
# l2 = split(TRPL.FRMT[,1:2], TRPL.FRMT$ID)
# summary(l2)

# RTTB.FRMT <- RTTB.FRMT %>% 
#   group_by(id) %>% 
#   rename(ID = id) %>% 
#   arrange(t_, .by_group = TRUE) %>% 
#   droplevels() %>% 
#   ungroup()
# 
# 
# l3 = split(RTTB.FRMT[,1:2], RTTB.FRMT$ID)
# summary(l3)

hcAuto <- tsclust(l, type = "hierarchical", 
                  distance = "dtw", trace = TRUE, 
                  control = hierarchical_control(method = "complete", symmetric = FALSE))

clusters = data.frame(hcAuto@cluster)
clusters$ID = row.names(clusters)
subset.clust <- merge(INDO.FRMT, clusters, by = "ID")


hc3 <- tsclust(l, type = "hierarchical", 
                  distance = "dtw", trace = TRUE, k = 3,
                  control = hierarchical_control(method = "complete", symmetric = FALSE))

clusters = data.frame(hc3@cluster)
clusters$ID = row.names(clusters)
subset.clust <- merge(INDO.FRMT, clusters, by = "ID")

hc4 <- tsclust(l, type = "hierarchical", 
               distance = "dtw", trace = TRUE, k = 4,
               control = hierarchical_control(method = "complete", symmetric = FALSE))

clusters = data.frame(hc4@cluster)
clusters$ID = row.names(clusters)
subset.clust <- merge(PAOC.FRMT, clusters, by = "ID")

hc5 <- tsclust(l, type = "hierarchical", 
               distance = "dtw", trace = TRUE, k = 5,
               control = hierarchical_control(method = "complete", symmetric = FALSE))

clusters = data.frame(hc5@cluster)
clusters$ID = row.names(clusters)
subset.clust <- merge(PAOC.FRMT, clusters, by = "ID")

# 
INDO.map <- INDO1 %>%
  filter(track_id %in% subset.clust$ID)
names(subset.clust)[names(subset.clust) == "ID"] <- "track_id"

# TRPL.map <- TRPL %>%
#   filter(track_id %in% subset.clust$ID)
# names(subset.clust)[names(subset.clust) == "ID"] <- "track_id"



# add cluster group number to main dataframe
# INDO.noBBAL.ATPR.map2 <- left_join(INDO.noBBAL.ATPR.map, subset.clust, by = "track_id")
dfa<- subset.clust %>% 
  dplyr::distinct(track_id, .keep_all = TRUE) %>% 
  dplyr::select(track_id, hc3.cluster)
INDO.map2 <- merge(INDO.map, dfa, by = "track_id", all.x = TRUE)


# TRPL.map2 <- left_join(TRPL.map, subset.clust, by = "track_id")

mytable <- subset.clust %>% 
  dplyr::arrange(hc3.cluster) %>% #cluster name varies depending on dataname above (e.g. hcAuto.cluster, hc2.cluster))
  dplyr::select(hc3.cluster, track_id) %>% 
  dplyr::distinct()
# add species name to table
mytable1 <- INDO1 %>% 
  dplyr::select(track_id, common_name) %>% 
  distinct() 

mytable <- left_join(mytable, mytable1, by = "track_id")

#separate clusters by facet  
mp1 <- ggplot(INDO.map2)+
  geom_sf(data = countries) +
  geom_point(aes(x = longitude, y = latitude, group = hc3.cluster, 
                 colour = as.factor(hc3.cluster), alpha = 0.5))+
  scale_colour_brewer(palette ="Set1")+
  # coord_sf(xlim = c(30, 120), ylim = c(30,-50))+
  theme_bw()+
  theme(legend.position = "none")+
  facet_wrap(~hc3.cluster)

# count how many individuals per species
INDO.FRMT %>% group_by(common_name) %>% summarise(Unique_elements = n_distinct(ID) )


ggsave(plot = mp1, "./cluster analysis/A-L method to format/INDIAN OCEAN_3 clusters_outbound and return seperate_points and facet.jpg", dpi = 500, height = 10, width = 10 )

#separate clusters by colour
INDO.map3 <-   INDO.map2 %>% 
  group_by(track_id) %>% 
  arrange(POSIX)  
INDO.map3 <- st_as_sf(x = INDO.map2, coords = c("longitude", "latitude"), crs = 4326) 
sf_use_s2(TRUE)

INDO.lines <- st_sf(
  aggregate(
    INDO.map3$geometry,
    list(INDO.map3$hc3.cluster), #change depending on number of clusters label
    function(g){
      st_cast(st_combine(g),"LINESTRING") %>% 
        st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
    }
  ))
mp1 <- INDO.lines %>% 
  mutate(cluster.num = as.factor(Group.1)) %>% 
  ggplot()+
  geom_sf(data = countries)+
  geom_sf( aes(colour = cluster.num), lwd = 1.2, alpha  = 0.4) +
  scale_colour_brewer(palette ="Set1")+
  # coord_sf(xlim = c(-80, 120), ylim = c(90, -90))+
  theme_bw()

# use the below lines to add a table with the individual ID and cluster assigned - may need multiple columns and adjust according to sample size
mp2<- tableGrob(mytable[1:200, 1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(2, 1), "mm")))
mp3<- tableGrob(mytable[201:400, 1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(2, 1), "mm")))
mp4<- tableGrob(mytable[401:600, 1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(2, 1), "mm")))
mp5<- tableGrob(mytable[601:800, 1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(2, 1), "mm")))
mp6<- tableGrob(mytable[801:947, 1:3], rows = NULL, theme=ttheme_minimal(base_size = 3, padding = unit(c(2, 1), "mm")))

# for Pac
mpfinal <- grid.arrange(mp1, mp2, mp3, mp4, mp5,mp6, ncol = 6, widths=c(4,0.75,0.75, 0.75, 0.75, 0.75))
# for Ind
mpfinal <- grid.arrange(mp1, mp2, mp3, ncol = 3, widths=c(4,0.75, 0.75))

ggsave(plot = mpfinal, "./cluster analysis/A-L method to format/INDIAN_2 clusters_seperate outbound and return_lines.jpg", dpi = 600, height = 20, width = 15)



# save a csv with all data + the cluster group
write.csv(INDO.map2, "./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_WITH 3 CLUSTER GROUPS.csv", row.names = FALSE)
clust1<- INDO.map2 %>% 
  filter(hc3.cluster == 1)
clust2<- INDO.map2 %>% 
  filter(hc3.cluster == 2)
clust3<- INDO.map2 %>% 
  filter(hc3.cluster == 3)
clust4<- PAOC.map2 %>% 
  filter(hc5.cluster == 4)
clust5<- PAOC.map2 %>% 
  filter(hc5.cluster == 5)

write.csv(clust1, "./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_1 OF 3 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)
write.csv(clust2, "./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_2 OF 3 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)
write.csv(clust3, "./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_3 OF 3 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)
write.csv(clust4, "./cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_4 OF 4 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)
write.csv(clust5, "./cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_5 OF 5 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)


#### Moving into ARC ####
# project the data to Lambert Azimuthal equal areas

ocean <- read_sf(dsn = "./Basemaps/GOaS_v1_20211214/goas_v01.shp")
EEZ <- read_sf(dsn = "./Basemaps/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")

# find centroid of Indian Ocean
IndOcean <- ocean %>% 
  filter(name == "Indian Ocean")
centre<- st_centroid(IndOcean)

ggplot(IndOcean)+
  geom_sf()+
  geom_sf(data = centre, colour = "red", size = 1.2)
centre[1] # coordinates of centre

# find centre of Atlantic Ocean polygon for the projection
AtlOcean <- ocean %>% 
  filter(name == "North Atlantic Ocean" | name == "South Atlantic Ocean") 
AtlOcean <- st_combine(AtlOcean) # there are north and south polygons so need to combine
sf_use_s2(FALSE)
centre<- st_centroid(AtlOcean) # location to use in custom projection

# find the centre of the Pacific Ocean polygon for the projection
PacOcean <- ocean %>% 
  filter(name == "North Pacific Ocean" | name == "South Pacific Ocean") 
PacOcean <- st_combine(PacOcean)
sf_use_s2(TRUE)
centre<- st_centroid(st_make_valid(PacOcean)) # location to use in custom projection

## changing land projection into custom projection
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))

# borders_laea = st_transform(borders, 
#                             crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") # Atlantic
# borders_laea = st_transform(borders, 
#                             crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346") # Pacific 
# borders_laea = st_transform(borders, 
#                             crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90") # Southern
borders_laea = st_transform(borders, 
                            crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754") # Indian
                            
# borders_laea = borders %>%
#   st_break_antimeridian(lon_0 = -80) %>%
#   st_transform(borders,
#                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346") # Pacific

# transform basemaps using centre of Indian Ocean polygon
IndOc_laea = st_transform(IndOcean, 
                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

AtlOc_laea = st_transform(AtlOcean, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

PacOc_laea = st_transform(PacOcean, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

SOc_laea = st_transform(SOcean, 
                        crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

ocean_laea = st_transform(ocean, 
                              crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

ocean_laea_union <- st_combine(ocean_laea)
# EEZ_laea = st_transform(EEZ, 
#                         crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
# transform line data for Indian Ocean species

IndOc2clust <- read.csv("./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_WITH 2 CLUSTER GROUPS_hierachical complete.csv")
IndOc3clust <- read.csv("./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_WITH 3 CLUSTER GROUPS_hierarchical complete.csv")
# PacOc2clust <- read.csv("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_WITH 2 CLUSTER GROUPS.csv")
# PacOc3clust <- read.csv("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_WITH 3 CLUSTER GROUPS.csv")
# PacOc4clust <- read.csv("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_WITH 4 CLUSTER GROUPS.csv")
# PacOc5clust <- read.csv("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_WITH 5 CLUSTER GROUPS.csv")

length(unique(IndOc3clust$track_id))

IndOc3clust <- IndOc3clust %>% 
  mutate(POSIX = as.POSIXct(POSIX,  tz = "UTC"))

IndOc2clustgeo <- st_as_sf(IndOc2clust, coords = c("longitude", "latitude"), 
                           crs = 4326)
IndOc3clustgeo <- st_as_sf(IndOc3clust, coords = c("longitude", "latitude"), 
                           crs = 4326)

# SOcgeo <- st_as_sf(SOOC, coords = c("longitude", "latitude"), 
#                            crs = 4326)
# points to lines and plot to check transformation
INDO3_lines <- IndOc3clustgeo %>%
  # group_by(track_id, mig.stage2) %>% 
  group_by(hc3.cluster,track_id, mig.stage2) %>%
  arrange(POSIX) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%  
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  ungroup()

INDO3_lines <- st_as_sf(INDO3_lines, crs = 4326)

PacOc5_laea_lines <- st_transform(PacOc5_lines, 
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

SOOC_laea_lines <- st_transform(SOOC_lines, 
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90" )

INDO2_laea_lines <- st_transform(INDO2_lines, 
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
INDO3_laea_lines <- st_transform(INDO3_lines, 
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

sf::sf_use_s2(use_s2 = TRUE )
PacOc5_laea_lines <- PacOc5_laea_lines %>%
  mutate(cluster.num = as.factor(hc5.cluster))

INDO2_laea_lines <- INDO2_laea_lines %>%
  mutate(cluster.num = as.factor(hcAuto.cluster))
INDO3_laea_lines <- INDO3_laea_lines %>%
  mutate(cluster.num = as.factor(hc3.cluster))

check <- data.frame(st_is_valid(INDO3_laea_lines))
INDO2_laea_linesa <- (INDO2_laea_lines[-231,]) # remove one row that isn't working
INDO3_laea_linesa <- (INDO3_laea_lines[-208,]) # remove one row that isn't working

  ggplot()+
  geom_sf(data = borders_laea , fill = "#a6cee3", alpha = 0.4)+
  # geom_sf(data = IndOc_laea, fill = "#a6cee3")+
  # geom_sf(data = centre, colour = "red", size = 1.2)+
    # geom_sf(data = SOOC_laea_lines, lwd = 1.2, alpha = 0.8)+
  # geom_sf(data = (PacOc3_laea_lines), aes(colour = cluster.num), lwd = 1.2, alpha  = 0.8) +
    geom_sf(data = INDO3_laea_linesa, aes(colour = cluster.num), lwd = 1.2, alpha = 0.8)+
  # scale_colour_manual(name = "cluster.num", values =c("#b2df8a", "#33a02c", "red"))+
  # coord_sf(xlim = c(-5000000, 6000000), ylim = c(-4000000, 6000000))+
    coord_sf(xlim = c(-5000000, 6000000), ylim = c(-5000000, 6000000)) + # zoom for Indian projection
  theme_bw()


ggsave(plot = last_plot(), "./cluster analysis/A-L method to format/Map of three Indian cluster groups_n = 152_custom Lambert azimuthal equal area projection centered at 78.61895 -30.4754.jpg", dpi = 500, height= 8, width = 8)


# save the projected line data into a shapefile
st_write(PacOc5_laea_lines, "./cluster analysis/csv files_cluster assigned_PacOc/Lines_withMigStageInfo_n=522_5Clusters_ProjectedCustomLambertCenteredAt-154.94,-6.064.shp")
# st_write(SOOC_laea_lines, "./cluster analysis/csv files_SOOC/Lines_withMigStageInfo_n=72_no clustering_ProjectedCustomLambertCenteredAt0,-90.shp")
st_write(INDO2_laea_linesa, "./cluster analysis/csv files_cluster assigned_IndOc/Lines_withMigStageInfo_n=156_2Clusters_byO&R_ProjectedCustomLambertCenteredAt78.62,-30.48.shp")
st_write(INDO3_laea_linesa, "./cluster analysis/csv files_cluster assigned_IndOc/Lines_withMigStageInfo_n=156_3Clusters_byO&R_ProjectedCustomLambertCenteredAt78.62,-30.48.shp")


#### Move to ArcGIS for line density estimations and then open output rasters in R #####


#### Everything below is now in it's own csv: 'Plottingline densities from ARC' - go to that script ##### 
# open rasters
raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/SouthOc_rast1of1_smallergrid.tif") # already projected
# raster_proj <- terra::project(raster_read, "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

max_raster <- raster_read/max(values(raster_read), na.rm=T)
cellSize(raster_read) # how big are cells m2

rast_01.1 <- max_raster
rast_02.1 <- max_raster
rast_025.1 <- max_raster
rast_03.1 <- max_raster
rast_04.1 <- max_raster
rast_05.1 <- max_raster
rast_10.1 <- max_raster
rast_20.1 <- max_raster
rast_25.1 <- max_raster
rast_30.1 <- max_raster
rast_40.1 <- max_raster
rast_50.1 <- max_raster
rast_75.1 <- max_raster
rast_90.1 <- max_raster
rast_95.1 <- max_raster
rast_100.1 <- max_raster

raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/PacOc_rast2of4.tif") # already projected
max_raster <- raster_read/max(values(raster_read), na.rm=T)
cellSize(raster_read) # how big are cells m2

rast_01.2 <- max_raster
rast_02.2 <- max_raster
rast_025.2 <- max_raster
rast_03.2 <- max_raster
rast_04.2 <- max_raster
rast_05.2 <- max_raster
rast_10.2 <- max_raster
rast_20.2 <- max_raster
rast_25.2 <- max_raster
rast_30.2 <- max_raster
rast_40.2 <- max_raster
rast_50.2 <- max_raster
rast_75.2 <- max_raster
rast_90.2 <- max_raster
rast_95.2 <- max_raster
rast_100.2 <- max_raster

raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/PacOc_rast3of3.tif") # already projected
max_raster <- raster_read/max(values(raster_read), na.rm=T)
cellSize(raster_read) # how big are cells m2

rast_01.3 <- max_raster
rast_02.3 <- max_raster
rast_025.3 <- max_raster
rast_03.3 <- max_raster
rast_04.3 <- max_raster
rast_05.3 <- max_raster
rast_10.3 <- max_raster
rast_20.3 <- max_raster
rast_25.3 <- max_raster
rast_30.3 <- max_raster
rast_40.3 <- max_raster
rast_50.3 <- max_raster
rast_75.3 <- max_raster
rast_90.3 <- max_raster
rast_95.3 <- max_raster
rast_100.3 <- max_raster

raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/Rast1of2.tif") # already projected
  <- raster_read/max(values(raster_read), na.rm=T)
cellSize(raster_read) # how big are cells m2

rast_01.4 <- max_raster
rast_02.4 <- max_raster
rast_025.4 <- max_raster
rast_03.4 <- max_raster
rast_04.4 <- max_raster
rast_05.4 <- max_raster
rast_10.4 <- max_raster
rast_20.4 <- max_raster
rast_25.4 <- max_raster
rast_30.4 <- max_raster
rast_40.4 <- max_raster
rast_50.4 <- max_raster
rast_75.4 <- max_raster
rast_90.4 <- max_raster
rast_95.4 <- max_raster
rast_100.4 <- max_raster

rast_025.1[rast_025.1 < 0.025] <- NA
rast_025.1[rast_025.1 >= 0.025] <- 1
rast_025.2[rast_025.2 < 0.025] <- NA
rast_025.2[rast_025.2 >= 0.025] <- 1
rast_025.3[rast_025.3 < 0.025] <- NA
rast_025.3[rast_025.3 >= 0.025] <- 1
rast_025.4[rast_025.4 < 0.025] <- NA
rast_025.4[rast_025.4 >= 0.025] <- 1

rast_05.1[rast_05.1 < 0.05] <- NA
rast_05.1[rast_05.1 >= 0.05] <- 1
rast_05.2[rast_05.2 < 0.05] <- NA
rast_05.2[rast_05.2 >= 0.05] <- 1
rast_05.3[rast_05.3 < 0.05] <- NA
rast_05.3[rast_05.3 >= 0.05] <- 1
rast_05.4[rast_05.4 < 0.05] <- NA
rast_05.4[rast_05.4 >= 0.05] <- 1

rast_10.1[rast_10.1 < 0.10] <- NA
rast_10.1[rast_10.1 >= 0.10] <- 1
rast_10.2[rast_10.2 < 0.10] <- NA
rast_10.2[rast_10.2 >= 0.10] <- 1
rast_10.3[rast_10.3 < 0.10] <- NA
rast_10.3[rast_10.3 >= 0.10] <- 1
rast_10.4[rast_10.4 < 0.10] <- NA
rast_10.4[rast_10.4 >= 0.10] <- 1


rast_20.1[rast_20.1 < 0.20] <- NA
rast_20.1[rast_20.1 >= 0.20] <- 1
rast_20.2[rast_20.2 < 0.20] <- NA
rast_20.2[rast_20.2 >= 0.20] <- 1
rast_20.3[rast_20.3 < 0.20] <- NA
rast_20.3[rast_20.3 >= 0.20] <- 1
rast_20.4[rast_20.4 < 0.20] <- NA
rast_20.4[rast_20.4 >= 0.20] <- 1

rast_25.1[rast_25.1 < 0.25] <- NA
rast_25.1[rast_25.1 >= 0.25] <- 1
rast_25.2[rast_25.2 < 0.25] <- NA
rast_25.2[rast_25.2 >= 0.25] <- 1
rast_25.3[rast_25.3 < 0.25] <- NA
rast_25.3[rast_25.3 >= 0.25] <- 1
rast_25.4[rast_25.4 < 0.25] <- NA
rast_25.4[rast_25.4 >= 0.25] <- 1

rast_30.1[rast_30.1 < 0.30] <- NA
rast_30.1[rast_30.1 >= 0.30] <- 1
rast_30.2[rast_30.2 < 0.30] <- NA
rast_30.2[rast_30.2 >= 0.30] <- 1
rast_30.3[rast_30.3 < 0.30] <- NA
rast_30.3[rast_30.3 >= 0.30] <- 1
rast_30.4[rast_30.4 < 0.30] <- NA
rast_30.4[rast_30.4 >= 0.30] <- 1


rast_40.1[rast_40.1 < 0.40] <- NA
rast_40.1[rast_40.1 >= 0.40] <- 1
rast_40.2[rast_40.2 < 0.40] <- NA
rast_40.2[rast_40.2 >= 0.40] <- 1
rast_40.3[rast_40.3 < 0.40] <- NA
rast_40.3[rast_40.3 >= 0.40] <- 1
rast_40.4[rast_40.4 < 0.40] <- NA
rast_40.4[rast_40.4 >= 0.40] <- 1


rast_50.1[rast_50.1 < 0.50] <- NA
rast_50.1[rast_50.1 >= 0.50] <- 1
rast_50.2[rast_50.2 < 0.50] <- NA
rast_50.2[rast_50.2 >= 0.50] <- 1
rast_50.3[rast_50.3 < 0.50] <- NA
rast_50.3[rast_50.3 >= 0.50] <- 1
rast_50.4[rast_50.4 < 0.50] <- NA
rast_50.4[rast_50.4 >= 0.50] <- 1

rast_75.1[rast_75.1 < 0.75] <- NA
rast_75.1[rast_75.1 >= 0.75] <- 1
rast_75.2[rast_75.2 < 0.75] <- NA
rast_75.2[rast_75.2 >= 0.75] <- 1
rast_75.3[rast_75.3 < 0.75] <- NA
rast_75.3[rast_75.3 >= 0.75] <- 1
rast_75.4[rast_75.4 < 0.75] <- NA
rast_75.4[rast_75.4 >= 0.75] <- 1


rast_90.1[rast_90.1 < 0.90] <- NA
rast_90.1[rast_90.1 >= 0.90] <- 1
rast_90.2[rast_90.2 < 0.90] <- NA
rast_90.2[rast_90.2 >= 0.90] <- 1
rast_90.3[rast_90.3 < 0.90] <- NA
rast_90.3[rast_90.3 >= 0.90] <- 1
rast_90.4[rast_90.4 < 0.90] <- NA
rast_90.4[rast_90.4 >= 0.90] <- 1

rast_95.1[rast_95.1 < 0.95] <- NA
rast_95.1[rast_95.1 >= 0.95] <- 1
rast_95.2[rast_95.2 < 0.95] <- NA
rast_95.2[rast_95.2 >= 0.95] <- 1
rast_95.3[rast_95.3 < 0.95] <- NA
rast_95.3[rast_95.3 >= 0.95] <- 1
rast_95.4[rast_95.4 < 0.95] <- NA
rast_95.4[rast_95.4 >= 0.95] <- 1


rast_100.1[rast_100.1 < 1] <- NA
rast_100.1[rast_100.1 >= 1] <- 1
rast_100.2[rast_100.2 < 1] <- NA
rast_100.2[rast_100.2 >= 1] <- 1
rast_100.3[rast_100.3 < 1] <- NA
rast_100.3[rast_100.3 >= 1] <- 1
rast_100.4[rast_100.4 < 1] <- NA
rast_100.4[rast_100.4 >= 1] <- 1

# colur hexcodes
RColorBrewer::brewer.pal(n = 9, name = "RdYlGn")

plot(st_geometry(SOc_laea))
plot(rast_025.1, col = "#D73027", add = TRUE) # change depending on which raster want to check
# plot(rast_02.1, col = "#F46D43", add = TRUE) # indian and Atl: 5, 10, 20, 25, 30, 40
# plot(rast_03.1, col = "#FDAE61", add = TRUE) # pacific 1, 2, 3, 4, 5, 10
# plot(rast_04.1, col = "#FEE08B", add = TRUE)
plot(rast_05.1, col = "#FFFFBF", add = TRUE)
plot(rast_10.1, col = "#D9EF8B", add = TRUE)
plot(rast_50.1, col = "#A6D96A", add = TRUE)
plot(rast_75.1, col = "#66BD63", add = TRUE)
plot(rast_90.1, col = "#1A9850", add = TRUE)
# plot(rast_95, col = "red", add = TRUE)
# plot(rast_100, col = "red", add = TRUE)

plot(st_geometry(PacOc_laea))
plot(rast_01.2, col = "#D73027", add = TRUE)
plot(rast_02.2, col = "#F46D43", add = TRUE)
plot(rast_03.2, col = "#FDAE61", add = TRUE)
plot(rast_04.2, col = "#FEE08B", add = TRUE)
plot(rast_05.2, col = "#FFFFBF", add = TRUE)
plot(rast_10.2, col = "#D9EF8B", add = TRUE)
plot(rast_50.2, col = "#A6D96A", add = TRUE)
plot(rast_75.2, col = "#66BD63", add = TRUE)
plot(rast_90.2, col = "#1A9850", add = TRUE)
# plot(rast_95, col = "red, add = TRUE)
# plot(rast_100, col = "red", add = TRUE)

plot(st_geometry(PacOc_laea))
plot(rast_01.3, col = "#D73027", add = TRUE)
plot(rast_02.3, col = "#F46D43", add = TRUE)
plot(rast_03.3, col = "#FDAE61", add = TRUE)
plot(rast_04.3, col = "#FEE08B", add = TRUE)
plot(rast_05.3, col = "#FFFFBF", add = TRUE)
plot(rast_10.3, col = "#D9EF8B", add = TRUE)
plot(rast_50.3, col = "#A6D96A", add = TRUE)
plot(rast_75.3, col = "#66BD63", add = TRUE)
plot(rast_90.3, col = "#1A9850", add = TRUE)
# plot(rast_95, col = "red, add = TRUE)
# plot(rast_100, col = "red", add = TRUE)

plot(st_geometry(PacOc_laea))
plot(rast_01.4, col = "#D73027", add = TRUE)
plot(rast_02.4, col = "#F46D43", add = TRUE)
plot(rast_03.4, col = "#FDAE61", add = TRUE)
plot(rast_04.4, col = "#FEE08B", add = TRUE)
plot(rast_05.4, col = "#FFFFBF", add = TRUE)
plot(rast_10.4, col = "#D9EF8B", add = TRUE)
plot(rast_50.4, col = "#A6D96A", add = TRUE)
plot(rast_75.4, col = "#66BD63", add = TRUE)
plot(rast_90.4, col = "#1A9850", add = TRUE)

# save as polygons
pol_05.1 <- terra::as.polygons(rast_05.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.1)[1]) 
pol_05.1_latlon <- sf::st_transform(pol_05.1, crs = 4326)

pol_05.2 <- terra::as.polygons(rast_05.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.2)[1]) 
pol_05.2_latlon <- sf::st_transform(pol_05.2, crs = 4326)

pol_05.3 <- terra::as.polygons(rast_05.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.3)[1]) 
pol_05.3_latlon <- sf::st_transform(pol_05.3, crs = 4326)

pol_05.4 <- terra::as.polygons(rast_05.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.4)[1]) 
pol_05.4_latlon <- sf::st_transform(pol_05.4, crs = 4326)

pol_10.1 <- terra::as.polygons(rast_10.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.1)[1]) 
pol_10.1_latlon <- sf::st_transform(pol_10.1, crs = 4326)

pol_10.2 <- terra::as.polygons(rast_10.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.2)[1]) 
pol_10.2_latlon <- sf::st_transform(pol_10.2, crs = 4326)

pol_10.3 <- terra::as.polygons(rast_10.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.3)[1]) 
pol_10.3_latlon <- sf::st_transform(pol_10.3, crs = 4326)

pol_10.4 <- terra::as.polygons(rast_10.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.4)[1]) 
pol_10.4_latlon <- sf::st_transform(pol_10.4, crs = 4326)

pol_20.1 <- terra::as.polygons(rast_20.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_20.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_20.1)[1]) 
pol_20.1_latlon <- sf::st_transform(pol_20.1, crs = 4326)

pol_20.2 <- terra::as.polygons(rast_20.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_20.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_20.2)[1]) 
pol_20.2_latlon <- sf::st_transform(pol_20.2, crs = 4326)

pol_20.3 <- terra::as.polygons(rast_20.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_20.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_20.3)[1]) 
pol_20.3_latlon <- sf::st_transform(pol_20.3, crs = 4326)

pol_20.4 <- terra::as.polygons(rast_20.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_20.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_20.4)[1]) 
pol_20.4_latlon <- sf::st_transform(pol_20.4, crs = 4326)

pol_25.1 <- terra::as.polygons(rast_25.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.1)[1]) 
pol_25.1_latlon <- sf::st_transform(pol_25.1, crs = 4326)

pol_25.2 <- terra::as.polygons(rast_25.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.2)[1]) 
pol_25.2_latlon <- sf::st_transform(pol_25.2, crs = 4326)

pol_25.3 <- terra::as.polygons(rast_25.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.3)[1]) 
pol_25.3_latlon <- sf::st_transform(pol_25.3, crs = 4326)

pol_25.4 <- terra::as.polygons(rast_25.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.4)[1]) 
pol_25.4_latlon <- sf::st_transform(pol_25.4, crs = 4326)

pol_30.1 <- terra::as.polygons(rast_30.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_30.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_30.1)[1]) 
pol_30.1_latlon <- sf::st_transform(pol_30.1, crs = 4326)

pol_30.2 <- terra::as.polygons(rast_30.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_30.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_30.2)[1]) 
pol_30.2_latlon <- sf::st_transform(pol_30.2, crs = 4326)

pol_30.3 <- terra::as.polygons(rast_30.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_30.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_30.3)[1]) 
pol_30.3_latlon <- sf::st_transform(pol_30.3, crs = 4326)

pol_30.4 <- terra::as.polygons(rast_30.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_30.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_30.4)[1]) 
pol_30.4_latlon <- sf::st_transform(pol_30.4, crs = 4326)

pol_40.1 <- terra::as.polygons(rast_40.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_40.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_40.1)[1]) 
pol_40.1_latlon <- sf::st_transform(pol_40.1, crs = 4326)

pol_40.2 <- terra::as.polygons(rast_40.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_40.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_40.2)[1]) 
pol_40.2_latlon <- sf::st_transform(pol_40.2, crs = 4326)

pol_40.3 <- terra::as.polygons(rast_40.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_40.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_40.3)[1]) 
pol_40.3_latlon <- sf::st_transform(pol_40.3, crs = 4326)

pol_40.4 <- terra::as.polygons(rast_40.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_40.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_40.4)[1]) 
pol_40.4_latlon <- sf::st_transform(pol_40.4, crs = 4326)

pol_50.1 <- terra::as.polygons(rast_50.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.1)[1]) 
pol_50.1_latlon <- sf::st_transform(pol_50.1, crs = 4326)

pol_50.2 <- terra::as.polygons(rast_50.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.2)[1]) 
pol_50.2_latlon <- sf::st_transform(pol_50.2, crs = 4326)

pol_50.3 <- terra::as.polygons(rast_50.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.3)[1]) 
pol_50.3_latlon <- sf::st_transform(pol_50.3, crs = 4326)

pol_50.4 <- terra::as.polygons(rast_50.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.4)[1]) 
pol_50.4_latlon <- sf::st_transform(pol_50.4, crs = 4326)

pol_75.1 <- terra::as.polygons(rast_75.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_75.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_75.1)[1]) 
pol_75.1_latlon <- sf::st_transform(pol_75.1, crs = 4326)

pol_75.2 <- terra::as.polygons(rast_75.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_75.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_75.2)[1]) 
pol_75.2_latlon <- sf::st_transform(pol_75.2, crs = 4326)

pol_75.3 <- terra::as.polygons(rast_75.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_75.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_75.3)[1]) 
pol_75.3_latlon <- sf::st_transform(pol_75.3, crs = 4326)

pol_75.4 <- terra::as.polygons(rast_75.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_75.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_75.4)[1]) 
pol_75.4_latlon <- sf::st_transform(pol_75.4, crs = 4326)

pol_90.1 <- terra::as.polygons(rast_90.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_90.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_90.1)[1]) 
pol_90.1_latlon <- sf::st_transform(pol_90.1, crs = 4326)

pol_90.2 <- terra::as.polygons(rast_90.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_90.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_90.2)[1]) 
pol_90.2_latlon <- sf::st_transform(pol_90.2, crs = 4326)

pol_90.3 <- terra::as.polygons(rast_90.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_90.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_90.3)[1]) 
pol_90.3_latlon <- sf::st_transform(pol_90.3, crs = 4326)

pol_90.4 <- terra::as.polygons(rast_90.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_90.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_90.4)[1]) 
pol_90.4_latlon <- sf::st_transform(pol_90.4, crs = 4326)

pol_95.1 <- terra::as.polygons(rast_95.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_95.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_95.1)[1]) 
pol_95.1_latlon <- sf::st_transform(pol_95.1, crs = 4326)

pol_95.2 <- terra::as.polygons(rast_95.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_95.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_95.2)[1]) 
pol_95.2_latlon <- sf::st_transform(pol_95.2, crs = 4326)

pol_95.3 <- terra::as.polygons(rast_95.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_95.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_95.3)[1]) 
pol_95.3_latlon <- sf::st_transform(pol_95.3, crs = 4326)

pol_95.4 <- terra::as.polygons(rast_95.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_95.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_95.4)[1]) 
pol_95.4_latlon <- sf::st_transform(pol_95.4, crs = 4326)

pol_100.1 <- terra::as.polygons(rast_100.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_100.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_100.1)[1]) 
pol_100.1_latlon <- sf::st_transform(pol_100.1, crs = 4326)

pol_100.2 <- terra::as.polygons(rast_100.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_100.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_100.2)[1]) 
pol_100.2_latlon <- sf::st_transform(pol_100.2, crs = 4326)

pol_100.3 <- terra::as.polygons(rast_100.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_100.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_100.3)[1]) 
pol_100.3_latlon <- sf::st_transform(pol_100.3, crs = 4326)

pol_100.4 <- terra::as.polygons(rast_100.4) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_100.4)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_100.4)[1]) 
pol_100.4_latlon <- sf::st_transform(pol_100.4, crs = 4326)

# colur hexcodes
RColorBrewer::brewer.pal(n = 9, name = "Blues")

# png("./cluster analysis/ArcGIS files_line density/R_map of cluster 2 of 2_polygons 10 - 95_lambert custom proj.png", res = 500, units = "cm", height = 15, width = 15)
clust1map<- ggplot()+
  geom_sf(data = borders_laea, fill = "#a6cee3", alpha = 0.2)+
  # geom_sf(data = pol_01.1, fill = "#F7FCF5")+
  # geom_sf(data = pol_02.1, fill = "#E5F5E0")+
  # geom_sf(data = pol_025.1, fill = "#C7E9C0")+
  # geom_sf(data = pol_03.1, fill = "#A1D99B")+
  # geom_sf(data = pol_04.1, fill = "#74C476")+
  geom_sf(data = pol_05.1, fill = "#41AB5D")+
  geom_sf(data = pol_10.1, fill = "#238B45")+
  # geom_sf(data = pol_75.1, fill = "#006D2C")+
  # geom_sf(data = pol_90.1, fill = "#00441B")+
  # geom_sf(data = pol_01.2, fill = "#FFF5EB", alpha = 0.9)+
  # geom_sf(data = pol_02.2, fill = "#FEE6CE", alpha = 0.9)+
  # geom_sf(data = pol_025.2, fill = "#FDD0A2", alpha = 0.9)+
  # geom_sf(data = pol_03.2, fill = "#FDAE6B", alpha = 0.9)+
  # geom_sf(data = pol_04.2, fill = "#FD8D3C", alpha = 0.9)+
  # geom_sf(data = pol_05.2, fill = "#F16913", alpha = 0.9)+
  # geom_sf(data = pol_10.2, fill = "#D94801", alpha = 0.9)+
  # geom_sf(data = pol_75.2, fill = "#A63603", alpha = 0.9)+
  # geom_sf(data = pol_90.2, fill = "#7F2704", alpha = 0.9)+
  # # geom_sf(data = pol_95, fill = "red")+
  # geom_sf(data = pol_01.3, fill = "#FCFBFD", alpha = 0.9)+
  # geom_sf(data = pol_02.3, fill = "#EFEDF5", alpha = 0.9)+
  # geom_sf(data = pol_025.3, fill = "#DADAEB", alpha = 0.9)+
  # geom_sf(data = pol_03.3, fill = "#BCBDDC", alpha = 0.9)+
  # geom_sf(data = pol_04.3, fill = "#9E9AC8", alpha = 0.9)+
  # geom_sf(data = pol_05.3, fill = "#807DBA", alpha = 0.9)+
  # geom_sf(data = pol_10.3, fill = "#6A51A3", alpha = 0.9)+
  # geom_sf(data = pol_75.3, fill = "#54278F", alpha = 0.9)+
  # geom_sf(data = pol_90.3, fill = "#3F007D", alpha = 0.9)+
  # #
  # geom_sf(data = pol_01.4, fill = "#F7FBFF", alpha = 0.9)+
  # geom_sf(data = pol_02.4, fill = "#DEEBF7", alpha = 0.9)+
  # geom_sf(data = pol_025.4, fill = "#C6DBEF", alpha = 0.9)+
  # geom_sf(data = pol_03.4, fill = "#9ECAE1", alpha = 0.9)+
  # geom_sf(data = pol_04.4, fill = "#6BAED6", alpha = 0.9)+
  # geom_sf(data = pol_05.4, fill = "#4292C6", alpha = 0.9)+
  # geom_sf(data = pol_10.4, fill = "#2171B5", alpha = 0.9)+
  # geom_sf(data = pol_75.4, fill = "#08519C", alpha = 0.9)+
  # geom_sf(data = pol_90.4, fill = "#08306B", alpha = 0.9)+
# coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6500000))+ # Atlantic Ocean projection limits  
  coord_sf(ylim = c(-6500000, 6500000)) + # southern ocean projection
  theme_bw()

ggsave(plot = clust1map, "./cluster analysis/ArcGIS files_line density/R_map_Southern_no clusters_poly 5, 10_not smoothed_custom lambert.jpg", dpi = 500, height = 15, width = 15)

# create an apporpriate viewport.  Modify the dimensions and coordinates as needed
vp.BottomRight <- grid::viewport(height=unit(1, "npc"), width=unit(1, "npc"), 
                           just=c("centre","centre"), 
                           y=0.5, x=0.5)

# make legend
legend <- plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("bottomright", legend =c('polygon 10', 'polygon 20', 'polygon 25', 'polygon 30', 'polygon 40', "polygon 50", "polygon 75", "polygon 90", "polygon 95"), pch=15, pt.cex=3, cex=0.7, bty='n',
       col = c('#FEE6CE', '#FDD0A2', '#FDAE6B', '#FD8D3C', '#F16913', "#D94801", "#A63603", "#7F2704","red"))
# mtext("cluster 1 of 2_not smoothed", at=0.2, cex=0.7)
legend

print(clust1map, vp = vp.BottomRight)

dev.off()

# save polygons unprojected
st_write(pol_10.1_latlon, "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/No clusters_poly10_unsmoothed_unprojected.shp", driver = "ESRI Shapefile")

pol05.clust0 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/No clusters_poly05_unsmoothed_unprojected.shp")

# colur hexcodes
RColorBrewer::brewer.pal(n = 9, name = "Purples")

A<-ggplot()+
  geom_sf(data = ocean, fill = "#a6cee3", alpha = 0.4)+
  geom_sf(data = pol05.clust0, fill = "#E5F5E0", colour = "red", alpha = 0.5) +
  geom_sf(data = pol10.clust0, fill = "#A1D99B", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol20.clust4, fill = "#41AB5D", colour= "red", alpha = 0.5)+
  # coord_sf(xlim = c(40, 100), ylim = c(20, -50)) #Indian Ocean limits
  # coord_sf(xlim = c(-60, 30), ylim = c(65, -70)) # Atlantic Ocean limits

smoothed10.clust2 <- smoothr::smooth(pol10.clust2, method = "ksmooth")

B<-ggplot()+
  geom_sf(data = ocean, fill = "#a6cee3", alpha = 0.4)+
  geom_sf(data = smoothed10.clust2, fill = "#FEE6CE", colour = "blue")+
  geom_sf(data = pol10.clust2, fill = NA, colour = "red")+
  coord_sf(xlim = c(40, 100), ylim = c(20, -50))

cowplot::plot_grid(A, B)

ggsave(plot = last_plot(), filename= "./cluster analysis/ArcGIS files_line density/Smoothed polygons/test.poly10.clust2.plot2.jpg", dpi = 700, height = 10, width = 15)

# on projected data

pol05.clust0.proj <- st_transform(pol05.clust0, 
             # crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")
             # crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346"
             crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

# borders_laea_union <- st_union(st_make_valid(borders_laea)) # lose country borders to make the map less busy

ggplot()+
  geom_sf(data = borders_laea, fill = "#a6cee3", alpha = 0.4)+#
  # geom_sf(data = ocean_laea, fill = "#a6cee3", alpha = 0.4)+
  # geom_sf(data = pol01.clust1.proj, fill = "#EFEDF5", colour = "red", alpha = 0.5 )+
  geom_sf(data = pol05.clust0.proj, fill= "#BCBDDC", colour = "red", alpha = 0.5)+
  geom_sf(data = pol_10.1, fill = "#807DBA", colour = "red", alpha = 0.5)+
  geom_sf(data = pol_20.1, fill = "#E5F5E0", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol025.clust2.proj, fill = "#A1D99B", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol05.clust2.proj, fill = "#41AB5D", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol30.clust1.proj, fill = "#41AB5D", colour= "red", alpha = 0.5)+
  # geom_sf(data = pol01.clust3.proj, fill = "#FEE6CE", colour = "red", alpha = 0.5 )+
  # geom_sf(data = pol025.clust3.proj, fill= "#FDAE6B", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol05.clust3.proj, fill = "#F16913", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol01.clust4.proj, fill = "#DEEBF7", colour = "red", alpha = 0.5 )+
  # geom_sf(data = pol025.clust4.proj, fill= "#9ECAE1", colour = "red", alpha = 0.5)+
  # geom_sf(data = pol10.clust4.proj, fill = "#4292C6", colour = "red", alpha = 0.5)+
  # coord_sf(xlim = c(-5000000, 6000000), ylim = c(-4000000, 6000000)) # Indian Ocean projection
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6500000)) # Atlantic Ocean projection
  # coord_sf(xlim = c(-8500000, 9000000), ylim = c(-8000000, 8000000)) # Pacific projection
  coord_sf(ylim = c(-6500000, 6500000))  # southern ocean projection
  
smoothed05.clust0.proj <- smoothr::smooth(pol05.clust0.proj, method = "ksmooth", smoothness = 10)
smoothed10.clust0.proj <- smoothr::smooth(pol_10.1, method = "ksmooth", smoothness = 10)
smoothed20.clust0.proj <- smoothr::smooth(pol_20.1, method = "ksmooth", smoothness = 10)
# smoothed30.clust1.proj <- smoothr::smooth(pol30.clust1.proj, method = "ksmooth", smoothness = 2)

ggplot()+
  geom_sf(data = borders_laea, fill = "#a6cee3", alpha = 0.4)+
  # geom_sf(data = ocean_laea, fill = "#a6cee3", alpha = 0.4)+
  # geom_sf(data = smoothed05.clust1.proj, fill = "#EFEDF5", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust0.proj, fill = "#BCBDDC", alpha = 0.5)+
  geom_sf(data = smoothed10.clust0.proj, fill = "#807DBA", alpha = 0.5)+
  # geom_sf(data = smoothed20.clust0.proj, fill = "#41AB5D", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust2.proj, fill = "#E5F5E0", alpha = 0.5)+
  # geom_sf(data = smoothed025.clust2.proj, fill = "#A1D99B", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust2.proj, fill = "#41AB5D", alpha = 0.5)+
  # geom_sf(data = smoothed30.clust2.proj, fill = "#F16913", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust3.proj, fill = "#FEE6CE", alpha = 0.5)+
  # geom_sf(data = smoothed025.clust3.proj, fill = "#FDAE6B", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust3.proj, fill = "#F16913", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust4.proj, fill = "#DEEBF7", alpha = 0.5 )+
  # geom_sf(data = smoothed05.clust4.proj, fill= "#9ECAE1", alpha = 0.5)+
  # geom_sf(data = smoothed10.clust4.proj, fill = "#4292C6", alpha = 0.5)+
  # geom_sf(data = pol10.clust1.proj, fill = NA, colour = "red")+
  # coord_sf(xlim = c(-5000000, 6000000), ylim = c(-4000000, 6000000)) # Indian Ocean projectin limits
  # coord_sf(xlim = c(-4000000, 8000000), ylim = c(-7000000, 6500000)) # Atlantic Ocean proj limits
  # coord_sf(xlim = c(-8500000, 9000000), ylim = c(-8000000, 8000000)) # Pacific Ocean projectin limits
coord_sf(ylim = c(-6500000, 6500000))  # southern ocean projection

ggsave(plot = last_plot(), filename= "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/poly10only.no clusters.vheavilysmoothed(10)_projected.jpg", dpi = 700, height = 10, width = 15)

# open unsmoothed shapefiles
Ind10.1 <- st_read("./cluster analysis/ArcGIS files_line density/Polygons from rasters/Cluster1of2_poly10_unsmoothed_unprojected.shp")
Ind10.2 <- st_read("./cluster analysis/ArcGIS files_line density/Polygons from rasters/Cluster2of2_poly10_unsmoothed_unprojected.shp")

Ind10.1.proj <- st_transform(Ind10.1, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
Ind10.2.proj <- st_transform(Ind10.2, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
borders_laea_Ind <- st_transform((borders), crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

ggplot()+
  geom_sf(data = borders_laea_Ind)+
  geom_sf(data = Ind10.1.proj, fill = "red")+
  geom_sf(data = Ind10.2.proj, fill = "blue")

Atl05.1 <- st_read("./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/Cluster1of3_poly05_unsmoothed_unprojected.shp")
Atl05.2 <- st_read("./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/Cluster2of3_poly05_unsmoothed_unprojected.shp")
Atl05.3 <- st_read("./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/Cluster3of3_poly05_unsmoothed_unprojected.shp")

Atl05.1.proj <- st_transform(Atl05.1, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726" )
Atl05.2.proj <- st_transform(Atl05.2, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726" )
Atl05.3.proj <- st_transform(Atl05.3, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726" )

borders_laea_Atl <- st_transform(borders, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

Atl05.1.2.proj <- st_union(Atl05.1.proj, Atl05.2.proj)  
Atl05.1.2.3.proj <- st_union(Atl05.1.2.proj, Atl05.3.proj)

ggplot()+
  geom_sf(data = borders_laea_Atl)+
  geom_sf(data = Atl05.1.2.3.proj, fill = "red")
  geom_sf(data = Atl05.1.proj, fill = "red")+
  geom_sf(data = Atl05.2.proj, fill = "blue")+
  geom_sf(data = Atl05.3.proj, fill = "green")
  

Pac025.1.2 <- st_read("./Pacific Ocean/shp files/PacOc flyway_1 and 2 unioned_poly.025_smoothed_projected.shp")  
Pac10.4 <- st_read("./Pacific Ocean/shp files/PacOc flyway_4of4_poly.1_smoothed_projected.shp")

Pac025.1.2.proj <- st_transform(Pac025.1.2, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
Pac05.4.proj <- st_transform(Pac05.4, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

borders_laea_Pac <- st_transform(borders, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

ggplot()+
  geom_sf(data = borders_laea_Pac)+
  geom_sf(data = Pac025.1.2.proj, fill = "red")+
  geom_sf(data = Pac05.4.proj, fill = "green")
# remove holes before smoothing
# each raster sq in Indian Ocean is ~920-970km2 so for one raster grid cell use 990
# each raster sq in Atlantic Ocean is ~3270-4035 km2 so for one raster grid use 4050
# each raster sq in Pacific Ocean is ~ 900 - 950km2 for one raster grid use 950
r_poly_dropped <- smoothr::drop_crumbs(pol025.clust2.proj, units::set_units(0.1, km^2)) # each raster sq in Indian Ocean is ~920-970km2 so for one raster grid cell use 990
r_poly_filled <- smoothr::fill_holes(r_poly_dropped, units::set_units(950, km^2))
smoothed025.clust2.proj.dropsmall <- smoothr::smooth(r_poly_filled, method = "ksmooth", smoothness = 1)

Indsmooth1 <- smoothr::smooth(Ind10.1.proj, method = "ksmooth", smoothness = 10)
Indsmooth2 <- smoothr::smooth(Ind10.2.proj, method = "ksmooth", smoothness = 10)

Atlsmooth <- smoothr::smooth(Atl05.1.2.3.proj, method = "ksmooth", smoothness = 7.5)


#INDOC POLYGONS
# poly10, clust 1 = drop_crumbs 49500, fill_holes 4950
# poly20, clust 1 = no extra small polygons or holes to drop
# poly10, clust 2 = drop crumbs 64350, fill_holes 59400
# poly20, clust 2 = drop crumbs 59400, fill_holes 59400

#ATLOC POLYGONS (3 clusters)
# poly05, clust 1 = drop_crumbs 607500 (4050*150) to remove Atlantic blob near Central America, 1296000 (4050*320) to remove all blobs, no holes
# poly10, clust 1 = drop_crumbs 810000 (4050*200) to remove all blobs, no holes
# pol20, clust 1 = drop_crumbs 405000 (4050*100) to remove all blobs, no holes
# poly05, clust 2 = drop_crumbs 607500 (4050*150) to remove blobs, 405000 (4050*100) to remove holes
# poly10, clust 2 = drop_crumbs 405000 (4050*100) to remove blobs, no holes
# poly20, clust 2 = no blobs or holes
# poly05, clust 3 =  no blobs or holes
# poly10, clust 3 = no blobs, fill_holes 20250 (4050*5)
# poly20, clust 3 = no blobs or holes

# ATLOC POLYGONS (4 clusters)
# poly05, clust1 = drop_crumbs 1296000, fill_holes 4050
# poly10, clust1 = drop_crumbs 810000, fill_holes 4050
# poly20, clust1 = drop_crumbs 405000
# poly05, clust2 = drop_crumbs 607500, fill_holes 405000
# poly10, clust2 = drop_crumbs 405000, no holes
# poly20, clust2 = no blobs dropped
# poly05, clust3 = no holes or blobs
# poly10, clust3 = fill_holes 20250
# poly20, clust3 = no blobs or holes
# poly05, clust4 = no blobs or holes
# poly10, clust4 = drop_crumbs 405000
# poly20, clust4 = drop_crumbs 405000

# PACOC POLYGONS (4 clusters)
# poly025, clust 1 = no holes or crumbs (when smoother 10)
# poly05, clust 1 = no holes or crumbs (when smoother 10)
# poly025, clust 2 = 
# poly05, clust 2 = no holes or crumbs (when smoother 10)
# poly025, clust 3 = no holes or crumbs (when smoother 10)
# poly05, clust 3 = 
# poly05, clust 4 = 
# poly10. clust 4 = 

 # ocean_comb <- st_combine(ocean_laea)
# borders_laea <- st_transform(borders_laea, 
#                              crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

# union Pacific cluster 1 and 2
sm.025.025.clust1.2 <- st_union(st_make_valid(smoothed025.clust1.proj), st_make_valid(smoothed025.clust2.proj))
sm.05.05.clust1.2 <- st_union(st_make_valid(smoothed05.clust1.proj), st_make_valid(smoothed05.clust2.proj))

ggplot()+
  # geom_sf(data = ocean_laea)+ # , fill = "#a6cee3", alpha = 0.4
  geom_sf(data = borders_laea_Pac, fill = "NA")+ #, fill = "#a6cee3"
  geom_sf(data= Pac05.4.proj, fill = "#EFEDF5")+
  geom_sf(data = Pac025.1.2.proj, fill = "#A1D99B", alpha = 0.6)+
  # geom_sf(data = sm.025.025.clust1.2, fill = "#EFEDF5")+
  # geom_sf(data = sm.05.05.clust1.2, fill = "#BCBDDC")+
  # geom_sf(data = smoothed025.clust1.proj, fill = "#EFEDF5")+
  # geom_sf(data = smoothed05.clust1.proj, fill = "#BCBDDC")+ #, alpha = 0.5
  # geom_sf(data = smoothed20.clust1.proj.dropsmall, fill ="#807DBA")+ #, alpha = 0.5
  # # geom_sf(data = smoothed30.clust1.proj, fill = "#41AB5D", alpha = 0.5)+
  # geom_sf(data = smoothed025.clust2.proj, fill = "#E5F5E0", alpha = 0.5)+ #, alpha = 0.5
  # geom_sf(data = smoothed05.clust2.proj, fill = "#A1D99B", alpha = 0.5)+ #, alpha = 0.5
  # geom_sf(data = smoothed20.clust2.proj, fill = "#41AB5D")+ #, alpha = 0.5
  # geom_sf(data = smoothed025.clust3.proj, fill = "#FEE6CE", alpha = 0.3)+ #, alpha = 0.5
  # geom_sf(data = smoothed05.clust3.proj, fill = "#FDAE6B", alpha = 0.2)+ #, alpha = 0.5
  # geom_sf(data = smoothed20.clust3.proj, fill = "#F16913")+ #, alpha = 0.5
  # geom_sf(data = smoothed05.clust4.proj, fill = "#DEEBF7")+ #, alpha = 0.5
  # geom_sf(data = smoothed05.clust4.proj, fill = "#9ECAE1", alpha = 0.5)+ #, alpha = 0.5
  # geom_sf(data = smoothed10.clust4.proj, fill = "#4292C6", alpha = 0.1)+ #, alpha = 0.5
  # geom_sf(data = pol10.clust1.proj, fill = NA, colour = "red")+
  # coord_sf(xlim = c(-5000000, 6000000), ylim = c(-4000000, 6000000)) +# Indian Ocean limits
  # coord_sf(xlim = c(-4000000, 8000000), ylim = c(-7000000, 6500000))+ # Atlantic Ocean proj limits
  coord_sf(xlim = c(-8500000, 9000000), ylim = c(-8000000, 8000000))+ # Pacific Ocean projection limits
  theme_bw()
  # when saving an eps file cannot use alpha on the colours

ggsave(plot = last_plot(), filename= "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/Pacific flyway_poly.05 and 0.25_extra heavy smooth(10).jpg", dpi = 700, height = 10, width = 15)

# save projected polygons - smoothed and no holes/crumbs
# st_write(smoothed20.clust2.proj.dropsmall, "./Indian Ocean/shp files/IndOc flyway_2of2_poly20_smoothed_projected.shp", driver = "ESRI Shapefile")
# st_write(sm.05.05.clust1.2, "./Pacific Ocean/shp files/PacOc flyway_1 and 2 unioned_poly.05_smoothed_projected.shp", driver = "ESRI Shapefile")
st_write(smoothed05.clust0.proj, "./Southern Ocean/shp files/SoOc flyway_poly.05_smoothed.shp", driver = "ESRI Shapefile")
st_write(Atlsmooth, "./Atlantic Ocean/shp files/AtlOc flyway unioned_poly.05_extra heavy smooth(7.5).shp", driver = "ESRI Shapefile")

smoothed10.clust4.proj <- st_read("./Pacific Ocean/shp files/PacOc flyway_4of4_poly.1_smoothed_projected.shp")
# add focal species example to plot
baraus1<- tracks %>% filter(track_id == "715_bp-run-9095b1_8089")
baraus2<- tracks %>% filter(track_id == "715_bp-run-81051_8091")
baraus3<- tracks %>% filter(track_id == "715_bp-run-81071_8099")
baraus4<- tracks %>% filter(track_id == "715_bp-run-81211_8095")
baraus5<- tracks %>% filter(track_id == "715_bp-run-80931_8086")
baraus6<- tracks %>% filter(track_id == "715_bp-run-81191_8097")
baraus7<- tracks %>% filter(track_id == "715_bp-run-81231_8093")

unique(tracks$track_id[which(tracks$common_name == "Arctic Tern")])
arte1 <- tracks %>% filter(track_id == "739_705_ARTE_370_10319")
arte2 <- tracks %>% filter(track_id == "739_705_ARTE_371_10320")
arte3 <- tracks %>% filter(track_id == "739_705_ARTE_373_10321")
arte4 <- tracks %>% filter(track_id == "739_705_ARTE_376_10322")
arte5 <- tracks %>% filter(track_id == "739_705_ARTE_395_10324")
arte6 <- tracks %>% filter(track_id == "739_705_ARTE_410_10327") #2?
arte7 <- tracks %>% filter(track_id == "739_705_ARTE_390_10323") #1
arte8 <- tracks %>% filter(track_id == "739_705_ARTE_406_10325") #2?
arte9 <- tracks %>% filter(track_id == "739_705_ARTE_408_10326")

unique(tracks$track_id[which(tracks$common_name == "South Polar Skua")])
spsk1 <- tracks %>% filter(track_id == "NonDB_EA 156052_NA") #2
spsk2 <- tracks %>% filter(track_id == "NonDB_EA 110517_NA") #1
spsk3 <- tracks %>%  filter(track_id == "NonDB_EA 156054_NA") #1
spsk4 <- tracks %>%  filter(track_id == "NonDB_EA 123551_NA") #2
spsk5 <- tracks %>%  filter(track_id == "NonDB_EA 156218_NA") #2
spsk6 <- tracks %>%  filter(track_id == "NonDB_EA 156248_NA")
spsk7 <- tracks %>%  filter(track_id == "NonDB_EA 157771_NA")

unique(tracks$track_id[which(tracks$common_name == "Sooty Shearwater")])
sss1 <- tracks %>% filter(track_id == "518_18810_4141")
sss2 <- tracks %>% filter(track_id == "518_34166_4151")
sss3 <- tracks %>% filter(track_id == "518_3_4174")

unique(tracks$track_id[which(tracks$common_name == "Buller's Shearwater")])
bush1 <- tracks %>% filter(track_id == "1910_H-27837_115164")
bush2 <- tracks %>% filter(track_id == "1910_H-37817_115166")
bush3 <- tracks %>% filter(track_id == "1910_H27827_115163")

unique(tracks$track_id[which(tracks$common_name == "Antipodean Albatross")])
AA1 <- tracks %>% filter(track_id == "2072_223767_223767_22")
AA2 <- tracks %>% filter(track_id == "2072_223772_223772_22")
# "2072_206931_206931_21" only outbound
# "2072_206965_206965_21" dips slightly outside on return

unique(tracks$track_id[which(tracks$common_name == "Cook's Petrel")])
cook1 <- tracks %>% filter(track_id == "639_6_10223")
cook2 <- tracks %>% filter(track_id == "888_7_15794")
# "888_7_15794" goes outside flyway at points


unique(tracks$track_id[which(tracks$common_name == "Grey-headed Albatross")])
GHAL1 <- tracks %>% filter(track_id == "495_1332079_3507")
GHAL2 <- tracks %>% filter(track_id == "1450_RA98_84058")
GHAL3 <- tracks %>% filter(track_id == "1450_RC67_84068")
GHAL4 <- tracks %>% filter(track_id == "1450_RA22_84052")

# project and convert to lines

baraus7sf<-st_as_sf(x=baraus7, coords = c("longitude", "latitude"), crs = 4326)
baraus7proj<- st_transform(baraus7sf, 
                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

spsk2sf<-st_as_sf(x=spsk2, coords = c("longitude", "latitude"), crs = 4326)
spsk2proj<- st_transform(spsk2sf, 
                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

arte7sf<-st_as_sf(x=arte6, coords = c("longitude", "latitude"), crs = 4326)
arte7proj<- st_transform(arte7sf, 
                           crs =  "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

sss3sf<-st_as_sf(x=sss3, coords = c("longitude", "latitude"), crs = 4326)
sss3proj<- st_transform(sss3sf, 
                        crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

AA2sf<-st_as_sf(x=AA1, coords = c("longitude", "latitude"), crs = 4326)
AA2proj<- st_transform(AA2sf, 
                        crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

bush2sf<-st_as_sf(x=bush2, coords = c("longitude", "latitude"), crs = 4326)
bush2proj<- st_transform(bush2sf, 
                         crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

#connect with lines
spsk2_lines <- spsk2proj %>% 
  # group_by(hcAuto.cluster,track_id, mig.stage2) %>%
  arrange(POSIX) %>% 
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

# union the polygons
# smoothed5_clust1_2 <- st_union(smoothed05.clust1.proj, smoothed05.clust2.proj)
# smoothed5_clust1_2_3 <- st_union(smoothed5_clust1_2, smoothed05.clust3.proj)
# smoothed10_clust1_2 <- st_union(smoothed10.clust1.proj, smoothed10.clust2.proj)
# smoothed10_clust1_2_3 <- st_union(smoothed10_clust1_2, smoothed10.clust3.proj)



ggplot()+
  geom_sf(data = borders_laea_Atl, fill = "NA")+#, alpha = 0.4
  # geom_sf(data= Indsmooth1, fill = "#EFEDF5", alpha = 0.6)+
  # geom_sf(data = Indsmooth2, fill = "#A1D99B", alpha = 0.6)+
  # geom_sf(data = Atlsmooth, fill = "#EFEDF5")+
  # geom_sf(data= Pac05.4.proj, fill = "#EFEDF5")+
  # geom_sf(data = Pac025.1.2.proj, fill = "#A1D99B", alpha = 0.6)+
  # geom_sf(data = smoothed5_clust1_2_3, fill = "#FEE6CE")+
  # geom_sf(data = smoothed10_clust1_2_3, fill = "#FDAE6B")+
  # geom_sf(data = sm.025.025.clust1.2, fill = "#BCBDDC")+
  # geom_sf(data = smoothed05.clust1.proj, fill = "#EFEDF5")+
  # geom_sf(data = smoothed10.clust0.proj, fill = "#BCBDDC")+
  # geom_sf(data = smoothed30.clust1.proj, fill = "#41AB5D", alpha = 0.5)+
  # geom_sf(data = smoothed05.clust2.proj, fill = "#E5F5E0", alpha = 0.5)+ #, alpha = 0.5
  # geom_sf(data = smoothed10.clust2.proj, fill = "#A1D99B", alpha = 0.5)+ #, alpha = 0.5
  # geom_sf(data = smoothed05.clust4.proj, fill = "#FEE6CE", alpha = 0.6)+ #, alpha = 0.5
  # geom_sf(data = smoothed10.clust4.proj, fill = "#FDAE6B", alpha = 0.2)+ #, alpha = 0.5
  # geom_sf(data = smoothed30.clust2.proj, fill = "#F16913", alpha = 0.5)+
  geom_sf(data = spsk2_lines, lwd = 0.2, colour = "red")+
  # geom_sf(data = spsk2proj, aes(colour = mig.stage2))+
  # annotate("text", x = 5000000, y = 6000000, label= "NonDB_EA 123551_NA")+ #atlantic 
  # annotate("text", x = 6500000, y = 7000000, label = "Buller's shearwater\n1910_H-37817_115166",
  #          size = 8/.pt)+
  annotate("text", x = 4000000, y = 3000000,label = "South Polar Skua\nNonDB_EA 110517_NA", size = 8/.pt)+
  xlab("")+
  ylab("")+
  # geom_sf(data = pol10.clust1.proj, fill = NA, colour = "red")+
  # coord_sf(xlim = c(-5000000, 6000000), ylim = c(-4000000, 6000000))+ # Indian proj limits
  coord_sf(xlim = c(-4000000, 8000000), ylim = c(-7000000, 6500000))+ # Atl proj limits
    # coord_sf(xlim = c(-8500000, 9000000), ylim = c(-8000000, 8000000))+ # Pacific proj limits
  # coord_sf(ylim = c(-6500000, 6500000)) + # southern ocean projection
  theme_bw()

ggsave(plot = last_plot(), filename = "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/No flyways__example south polar skua thin lines.png", dpi = 700, height = 10, width = 10)
# ggsave(plot = last_plot(), filename = "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/Noclusters_heavily smoothed_example4 grey-headed albatross thin lines.svg", dpi = 700, height = 10, width = 10)



#### Map of Atlantic and Southern Flyways on an Atlantic projection ####

borders_laea_Atl = st_transform(borders,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") # Atlantic

Atlfly <- st_read("./Atlantic Ocean/shp files/wgs84/AtlFlyway_wgs84_reprojArc_smallpolysdropped.shp")
SOfly <- st_read("./Southern Ocean/shp files/wgs84/SthOFlyway_wgs84_reprojArc.shp")

Atlfly.proj <- st_transform(Atlfly,
                            crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") # Atlantic

SOfly.proj <- st_transform(SOfly,
                            crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") # Atlantic

ggplot()+
  geom_sf(data = Atlfly.proj, fill = "#EFEDF5")+
  geom_sf(data = SOfly.proj, fill = "#A1D99B", alpha = 0.4)+
  geom_sf(data = borders_laea_Atl, fill = "gray60")+
  coord_sf(xlim = c(-4000000, 6500000), ylim = c(-7000000, 6500000))+ # Atl proj limits - oringial y max = 6500000, extended for Arte colony: 7200000
  theme_bw()

ggsave(plot = last_plot(), "../Infographics and animation/Atlantic and Southern Ocean Flyways_Atl projection.svg", dpi = 700, height = 10, width = 10)

#### AVERAGE ROUTE OF TRAVEL IN INDIAN OCEAN ####

clustered<- read.csv("./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean birds_RTTB,BAPL,TRPL,WDGS_n = 152_OUTBOUND_WITH 2 CLUSTER GROUPS_hierarchical complete.csv")

length(unique(clustered$track_id))

clust1.av <- clustered %>% 
  filter(hcAuto.cluster == "1") # cluster 1 moves west to east so need to compare the latitudes

latclust1<- aggregate(data = clust1.av, latitude~  mig.stage2, FUN = mean)
clust1.av %>%
  group_by(mig.stage2) %>%
dplyr::summarise(mean= mean(latitude), sd = sd(latitude), 
                 median = median(latitude), n = n())


clust2.av <- clustered %>% 
  filter(hcAuto.cluster == "2")

lonclust2<- aggregate(data = clust2.av, longitude ~mig.stage2, FUN = mean)
clust2.av %>%
  group_by(mig.stage2) %>%
  dplyr::summarise(mean= mean(longitude), sd = sd(longitude), 
                   median = median(longitude), n = n())

#### AVERAGE ROUTE OF TRAVEL IN WEST PACIFIC OCEAN ####
clustered <- read.csv("./cluster analysis/csv files_cluster assigned_PacOc/Pacific Ocean_28 sp_n = 522_outbound and return_WITH 4 CLUSTER GROUPS.csv")

clust4 <- clustered %>% filter(hc4.cluster == 4)

length(unique(clust4$track_id)) # 85 outbound and return journeys

# WPF1 <- clust4%>% 
#   filter(track_id == "561_06sangan1554_5084.b") %>% 
#   mutate(POSIX = as.POSIXct(POSIX)) %>% 
#   arrange(POSIX)
# 
#   ggplot() + 
#     geom_sf(data = countries) +
#   geom_point(data = WPF1, aes(longitude, latitude)) + 
#   geom_path()



# need to use a circular 
clust4$latfalse <- 0
colnames(clust4)
xy.out <- cbind(clust4$longitude[which(clust4$mig.stage2 == "1outward")], clust4$latfalse[which(clust4$mig.stage2 == "1outward")])
xy.ret <- cbind(clust4$longitude[which(clust4$mig.stage2 == "3return")], clust4$latfalse[which(clust4$mig.stage2 == "3return")])
# make a column of same latitude as only want to calculate average longitude
geosphere::geomean(xy.out) # 153.48 south to north
geosphere::geomean(xy.ret) # 155.45 north to south



lonclust1<- aggregate(data = clust4, lonplus~  mig.stage2, FUN = mean)

clust4 %>%
  group_by(mig.stage2) %>%
  dplyr::summarise(mean= mean(lonplus), sd = sd(lonplus), 
                   median = median(lonplus), n = n())



#### CREATE A GLOBAL FLYWAYS ROBINSON MAP  - reproject and move to ARC ####

borders_rob <- st_transform(borders, crs = "+proj=robin")
# hexcodes of colours
RColorBrewer::brewer.pal(n = 8, "Dark2")

ggplot()+
  geom_sf(data = borders_rob)

# open Indian flyways
Ind1 <- st_read("./Indian Ocean/shp files/IndOc flyway 1_poly.1_extra heavy smooth(10).shp")
Ind2 <- st_read("./Indian Ocean/shp files/IndOc flyway 2_poly.1_extra heavy smooth(10).shp")

# back to lat-lon
Ind1unprj <- st_transform(Ind1, crs = 4326)
Ind2unprj <- st_transform(Ind2, crs = 4326)

Ind1rob <- st_transform(Ind1unprj,  crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
Ind2rob <- st_transform(Ind2,  crs = "+proj=robin")

st_write(Ind2rob, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/IndOc_flyway2_robproj.shp")
Ind2rob <- st_read("./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/IndOc_flyway2_robproj.shp")

ggplot()+
  geom_sf(data = borders_rob)+
  geom_sf(data = Ind1rob, fill = "#1B9E77", alpha = 0.4)+
  geom_sf(data = Ind2rob, fill = "#D95F02", alpha = 0.4)+
  theme_bw()

# Atlantic flyway
# Atlrob <- st_transform(Atlsmooth, crs = "+proj=robin")
st_write(Atlrob, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/AtlOc_flyway_robproj.shp")
Atlrob <- st_read("./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/AtlOc_flyway_robproj.shp")
AtlOc_laea <- st_read("./Atlantic Ocean/shp files/AtlOc flyway unioned_poly.05_extra heavy smooth(7.5).shp")

Atl_poly_dropped <- smoothr::drop_crumbs(AtlOc_laea, units::set_units(10000000, km^2)) 
plot(Atl_poly_dropped)

st_write(Atl_poly_dropped, "./Atlantic Ocean/shp files/AtlOc flyway unioned_poly.05_extra heavy smooth(7.5)_smallpolysdropped.shp")


Atlunprj <- st_transform(AtlOc_laea, crs = 4326)

ggplot()+
  geom_sf(data = borders_rob)+
  geom_sf(data = Ind1rob, fill = "#1B9E77", alpha = 0.4)+
  geom_sf(data = Ind2rob, fill = "#D95F02", alpha = 0.4)+
  geom_sf(data = Atlrob, fill = "#7570B3", alpha = 0.4)+
  theme_bw()

# southern 
SOFLY <- st_read("./Southern Ocean/shp files/SoOc flyway_poly.10_smoothed.shp")

SOunprj <- st_transform(SOFLY, crs = 4326)
SOunproj_wrap <- st_wrap_dateline(SOunprj, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=179')) %>% 
  st_make_valid()

SOrob <- st_transform(SOunprj, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
st_write(SOrob, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/SouthOc_flyway_robproj.shp")
# SOrob <- st_read("./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/SouthOc_flyway_robproj.shp")

ggplot()+
  geom_sf(data = borders_rob)+
  # geom_sf(data = Ind1rob, fill = "#1B9E77", alpha = 0.4)+
  # geom_sf(data = Ind2rob, fill = "#D95F02", alpha = 0.4)+
  # geom_sf(data = Atlrob, fill = "#7570B3", alpha = 0.4)+
  geom_sf(data = SOrob, fill = "#E7298A", alpha = 0.4)+
  theme_bw()

# Pacific Flyways

Pac1 <- st_read("./Pacific Ocean/shp files/PacOc flyway_1 and 2 unioned_poly.025_smoothed_projected.shp")
Pac2 <- st_read("./Pacific Ocean/shp files/PacOc flyway_4of4_poly.05_smoothed_projected.shp")

Pac1unprj <- st_transform(Pac1, crs = 4326)
Pac2unprj <- st_transform(Pac2, crs = 4326)

st_write(Pac1unprj, "./Pacific Ocean/shp files/wgs84/Pacific flyway 1_unprojected wgs.shp")
st_write(Pac2unprj, "./Pacific Ocean/shp files/wgs84/Pacific flyway 2_unprojected wgs.shp")


Pac1unproj_wrap <- st_wrap_dateline(Pac1unprj, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=179'))
Pac2unproj_wrap <- st_wrap_dateline(Pac2unprj, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=179'))


ggplot()+
  geom_sf(data = borders)+
  geom_sf(data = Pac1unproj_wrap, fill = "#D95F02")+
  geom_sf(data = Pac2unproj_wrap, fill = "#7570B3")

Pac1rob <- st_transform(Pac1unprj, crs = "+proj=robin") # at this stage the crs is somehow lost
Pac2rob <- st_transform(Pac2unprj, crs = "+proj=robin")

Pac1rob_wrap <- st_wrap_dateline(Pac1rob, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=-180'))
Pac2rob_wrap <- st_wrap_dateline(Pac2rob, options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=-180'))

borders2 <- st_shift_longitude(borders) %>% 
  st_wrap_dateline(options= c('WRAPDATELINE=YES', 'DATELINEOFFSET=179'))

#plot all wgs84
ggplot()+
  geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt)+
  geom_sf(data = borders)+
  geom_sf(data = Pac1unproj_wrap, fill = "#D95F02", alpha = 0.4)+
  geom_sf(data = Pac2unproj_wrap, fill = "#7570B3", alpha = 0.4)+
  geom_sf(data = Ind1unprj, fill = "#E7298A", alpha = 0.4)+
  geom_sf(data = Ind2unprj, fill = "#66A61E", alpha = 0.4)+
  geom_sf(data = Atlunprj, fill = "#E6AB02", alpha = 0.4)+
  theme_bw()

# TRIED REPROJECTING EVERYTHING IN ARC TO WGS84 - open these and plot

Atlunprj2 <- st_read("./Atlantic Ocean/shp files/wgs84/AtlFlyway_wgs84_reprojArc_smallpolysdropped.shp")
Pac1unprj2 <- st_read("./Pacific Ocean/shp files/wgs84/PacificFlyway1_wgs84_reprojArc.shp")
Pac2unprj2 <- st_read("./Pacific Ocean/shp files/wgs84/PacificFlyway2_wgs84_reprojArc.shp")
SOunprj2 <- st_read("./Southern Ocean/shp files/wgs84/SthOFlyway_wgs84_reprojArc.shp")
Ind1unprj2 <- st_read("./Indian Ocean/shp files/wgs84/IndFlyway1_wgs84_reprojArc.shp")
Ind2unprj2 <- st_read("./Indian Ocean/shp files/wgs84/IndFlyway2_wgs84_reprojArc.shp")

ggplot()+
  geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt)+
  geom_sf(data = Pac1unprj2, fill = "#D95F02", alpha = 0.4)+
  geom_sf(data = Pac2unprj2, fill = "#7570B3", alpha = 0.4)+
  geom_sf(data = Ind1unprj2, fill = "#E7298A", alpha = 0.4)+
  geom_sf(data = Ind2unprj2, fill = "#66A61E", alpha = 0.4)+
  geom_sf(data = Atlunprj2, fill = "#E6AB02", alpha = 0.4)+
  geom_sf(data = SOunprj2, fill ="#1B9E77", alpha = 0.4 )+
  geom_sf(data = borders)+
  theme_bw()

ggsave(plot = last_plot(), "./Global Flyways/6Flyways_wgs84.svg", dpi = 700, height = 10, width = 15)


# try projecting these to Robinson
Atlrob2 <- st_transform(Atlunprj2, crs = "+proj=robin")
Pac1rob2 <- st_transform(Pac1unprj2, crs = "+proj=robin")
Pac2rob2 <- st_transform(Pac2unprj2, crs = "+proj=robin")
SOrob2 <- st_transform(SOunprj2, crs = "+proj=robin")
Ind1rob2 <- st_transform(Ind1unprj2, crs = "+proj=robin")
Ind2rob2 <- st_transform(Ind2unprj2, crs = "+proj=robin")


ggplot()+
  # geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt)+
  geom_sf(data = Pac1rob2, fill = "#D95F02", alpha = 0.4)+
  geom_sf(data = Pac2rob2, fill = "#7570B3", alpha = 0.4)+
  geom_sf(data = Ind1rob2, fill = "#E7298A", alpha = 0.4)+
  geom_sf(data = Ind2rob2, fill = "#66A61E", alpha = 0.4)+
  geom_sf(data = Atlrob2, fill = "#E6AB02", alpha = 0.4)+
  geom_sf(data = SOrob2, fill ="#1B9E77", alpha = 0.4 )+
  geom_sf(data = borders_rob)+
  theme_bw()

ggsave(plot = last_plot(), "./Global Flyways/6Flyways_robin.png", dpi = 700, height = 10, width = 15)


#   geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt)+# try winkel tipel

borders_win <- lwgeom::st_transform_proj(borders_rob, crs = "+proj=wintri +datum=WGS84 +no_defs +over")

ggplot()+
  geom_sf(data = borders_win, size = 0.5/.pt)+
  coord_sf(datum = NULL)+
  theme_bw()

Pac1_win <- lwgeom::st_transform_proj(Pac1unprj,  crs = "+proj=wintri +datum=WGS84 +no_defs +over +lon_wrap=180") %>% 
  st_make_valid()
Pac2_win <- lwgeom::st_transform_proj(Pac2unprj,  crs = "+proj=wintri +datum=WGS84 +no_defs +over +lon_wrap=180")



ggplot()+
  geom_sf(data = borders_win, size = 0.5/.pt)+
  geom_sf(data= Pac1_win, fill = "#D95F02", alpha = 0.4)+
  coord_sf(datum = NULL)+
  theme_bw()

## create background ocean polygon to ensure the edge of globe is smooth
# vectors of latitudes and longitudes that go once around the globe in 1-degree steps
lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# proj <- "+proj=robin"
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# turn into correctly projected sf collection
outline <-
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ) %>%
  st_sf() %>% lwgeom::st_transform_proj(crs = proj) # project

# 
# ggplot()+
#   geom_sf(data = outline, fill = "#56B4E950", size = 0.5/.pt)+
#   geom_sf(data = borders_rob)+
#   geom_sf(data = Pac1rob, fill = "#D95F02", alpha = 0.4)+
#   geom_sf(data = Pac2rob, fill = "#7570B3", alpha = 0.4)+
#   coord_sf(datum = NA)+
#   theme_bw()


st_write(Pac1rob, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Global Robinson proj/Pacific_flyway1_robproj.shp")
Pac

# colour hexcodes
RColorBrewer::brewer.pal(8, "Dark2")

ggplot()+
  geom_sf(data = borders_rob)+
  geom_sf(data = Ind1rob, fill = "#1B9E77", alpha = 0.4)+
  geom_sf(data = Ind2rob, fill = "#D95F02", alpha = 0.4)+
  geom_sf(data = Atlrob, fill = "#7570B3", alpha = 0.4)+
  # geom_sf(data = SOrob, fill = "#E7298A", alpha = 0.4)+
  # geom_sf(data = Pac1rob, fill = "#66A61E", alpha = 0.4)+
  # geom_sf(data = Pac2rob, fill = "#E6AB02", alpha = 0.4)+
  theme_bw()

test <- st_read("./Indian Ocean/shp files/IndOc flyway 1_poly.1_extra heavy smooth(10).shp")

#####
columns[12]
# "ex93836"

ggplot() +
  geom_sf(data = countries)+
  geom_point(data = TRPL.FRMT, aes(x = x_, y = y_ ))+
  geom_point(data = TRPL.FRMT[TRPL.FRMT$id == "1810_5H09142_108918",], aes(x = x_, y = y_ ), colour = "red")
# now how to get from the variable numbers to the bird ID?

gp1<-ggplotGrob(p1)

p2 <- MXSW.clust %>%
  dplyr::mutate(index = 1:8907) %>%
  tidyr::gather(key = type_col,value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>% 
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, ncol = 1, strip.position="left") + 
  guides(color=FALSE) +
  theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_blank())


# learning the method - following example here:  https://damien-datasci-blog.netlify.app/post/time-series-clustering-with-dynamic-time-warp/
# generate random data used in example
# classic run
noise <- runif(420) # random noise
x <- seq(1,420) # 42km with a measure every 100m
pace_min <- 5 # min/km (corresponds to fast run)

ts_sim_classic_run <- (sin(x/10)+x/100+noise+pace_min) %>%
  as.ts(.)

ts.plot(ts_sim_classic_run, xlab = "Distance [x100m]", ylab = "Differential pace [min/km]", main = "Example of classic run", ylim=c(0,25))

# wall run
# noise <- runif(210) # random noise
# x <- seq(1,210) # 21km with a measure every 100m
# pace_min <- 5 # min/km (corresponds to fast run)
# pace_wall <- 20 # min/km (corresponds to very slow run)
# ts_sim_part1 <- sin(x/5)+x/50+noise+pace_min
# ts_sim_part2 <- sin(x/5)+noise+pace_wall
# 
# ts_sim_wall_run <- c(ts_sim_part1,ts_sim_part2) %>%
#   as.ts(.)
# 
# ts.plot(ts_sim_wall_run, xlab = "Distance [x100m]", ylab = "Differential pace [min/km]", main = "Example of wall run", ylim=c(0,25))
# 
# pace_min <- 5 # min/km (corresponds to fast run)
# pace_wall <- 20 # min/km (corresponds to very slow run)
# 
# # classic run
# ts_sim_classic_run <- abs(arima.sim(n = 420, mean = 0.001, model = list(order = c(1,0,0), ar = 0.9))) + pace_min
# 
# ts.plot(ts_sim_classic_run, xlab = "Distance [x100m]", ylab = "Differential pace [min/km]", main = "Example of classic run", ylim=c(0,25))
# 
# # wall run
# ts_sim_part1 <- abs(arima.sim(n = 210, model = list(order = c(1,0,0), ar = 0.9))) + pace_min
# ts_sim_part2 <- ts(arima.sim(n = 210, model = list(order = c(1,0,0), ar = 0.9)) + pace_wall, start = 211,end =420)
# 
# ts_sim_wall_run <- ts.union(ts_sim_part1,ts_sim_part2)
# ts_sim_wall_run<- pmin(ts_sim_wall_run[,1], ts_sim_wall_run[,2], na.rm = TRUE)
# 
# ts.plot(ts_sim_wall_run, xlab = "Distance [x100m]", ylab = "Differential pace [min/km]", main = "Example of wall run", ylim=c(0,25))
# 
# ts_sim_boot_classic <- ts_sim_classic_run %>%
#   tseries::tsbootstrap(., nb=5, b=200, type = "block") %>%
#   as.data.frame(.) %>%
#   dplyr::rename_all(funs(c(paste0("classic_",.))))
# 
# ts_sim_boot_wall <- ts_sim_wall_run %>%
#   tseries::tsbootstrap(., nb=5, b=350, type = "block") %>%
#   as.data.frame(.) %>%
#   dplyr::rename_all(funs(c(paste0("wall_",.))))
# 
# ts_sim_df <- cbind(ts_sim_boot_classic,ts_sim_boot_wall)

# cluster analysis
dist_ts <- TSclust::diss(SERIES = t(ts_sim_df), METHOD = "DTWARP") # note the dataframe must be transposed
hc <- stats::hclust(dist_ts, method="complete") # meathod can be also "average" or diana (for DIvisive ANAlysis Clustering)
# k for cluster which is 2 in our case (classic vs. wall)
hclus <- stats::cutree(hc, k = 2) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
  as.data.frame(.) %>%
  dplyr::rename(.,cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata <- ggdendro::dendro_data(hc)
names_order <- hcdata$labels$label
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking hcdata$labels$label <- ""

p1 <- hcdata %>%
  ggdendro::ggdendrogram(., rotate=TRUE, leaf_labels=FALSE)

p2 <- ts_sim_df %>%
  dplyr::mutate(index = 1:420) %>%
  tidyr::gather(key = type_col,value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>% 
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, ncol = 1, strip.position="left") + 
  guides(color=FALSE) +
  theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_blank())

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 

grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))
