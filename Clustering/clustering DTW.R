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

# open and organise uptodate data set (one ocean at a time)
#Indian
INDO <- read.csv("./Figures/Autoassigning methods/simplified methods/IndOc sp. CLEANED_autoassign 30kde NB, 10kde B/a.Migration assigned_IndOc datasets auto assign.csv")
#Atlantic
ATLO <- read.csv("./Figures/Autoassigning methods/simplified methods/AtlOc sp. autoassign 30kde NB, 10kde B/a. Migration assigned_ALL AtlOc datasets auto assign_individuals 1 to 542.csv") 
# Pacific
PAOC <- read.csv("./Figures/Autoassigning methods/simplified methods/PacOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_PacOc datasets auto assign.csv") 
# Southern
tracks <- read.csv("./Figures/Autoassigning methods/simplified methods/SouthOc sp. autoassign 30kde NB, 10kde B/a.Migration assigned_SOc datasets auto assign.csv") 
 # open full dataset before migration assigned --->'read.csv("./Archived dataframes/data filtered by migration based on displacement plots_MIGRATION.v6_reclassified.csv")
  # open Anne-Sophie's sorted data --->  read.csv("STDB_data from Anne-Sophie_migration and winter periods already separated.csv")
unique(tracks$common_name)

# change date time column to POSIXct
tracks <- tracks %>% # replace with correct dataframe name
  mutate(POSIX = as.POSIXct(POSIX,  tz = "UTC")) %>% # add  additional column with a POSIXct class date and time
  mutate(track_id = as.factor(track_id))

# open basemap
countries <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))

####  Organising Indian Ocean Species ####
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


# plot the raw locations
plot(latitude~longitude, data=PAOC1,  asp=1, main="",
     frame = T, ylab="", xlab="Pacific Ocean species n = 522", 
     col= as.factor(common_name), pch = 16, cex = 0.5)
plot(countries, col='lightgrey', add=T)

# For Southern Filter Outward and return
SOOC <- tracks %>% 
  filter(mig.stage2 == "1outward" | mig.stage2 == "3return")

# cluster by net square colony displacement
# NOTE: amt calcualtes NSD from the first location (which is not necessarily the colony)  


# calculate distance between points
# then do hierarchical clustering dtw::
# plot dendogram plot(clusterobject)

# DATA FORMATTING
# need a dataframe where each column is an individual to cluster (i.e. column per bird ID) and the rows are the NSD 
# to find out - presumably the rows are in time order


INDO.track <- make_track(INDO1, longitude, latitude, POSIX, stage, id = track_id, crs = 4326)
INDO.FRMT <- data.frame(amt::add_nsd(INDO.track))  # the output is the sum of (locx1 - locX2)^2 and  (locY1 - locY2)^2

ATLO.track <- make_track(ATLO1, longitude, latitude, POSIX, mig.stage2, common_name,id = track_id, crs = 4326)
ATLO.FRMT <- data.frame(amt::add_nsd(ATLO.track))

PAOC.track <- make_track(PAOC1, longitude, latitude, POSIX, mig.stage2, common_name,id = track_id, crs = 4326)
PAOC.FRMT <- data.frame(amt::add_nsd(PAOC.track))

ggplot(INDO.FRMT)+
  geom_line(aes(t_, nsd_, colour = id), lwd = 1.3) + 
  xlab("Date") +
  ylab("NSD from colony (km)") +
  theme_bw()+
  theme(legend.position = "none")

#### dataformatting #### NO CLUSTERING IN SOUTHERN OCEAN - SKIP THESE STEPS

INDO.FRMT <- INDO.FRMT %>% 
  group_by(id) %>% 
  rename(ID = id) %>% 
  # filter(mig.stage2 == "3return") %>% # change depending on whether want to cluster ==1outward, == 3return or both !=2winter
  arrange(t_, .by_group = TRUE) %>% 
  droplevels() %>% 
  ungroup()

l = split(INDO.FRMT[,1:2], INDO.FRMT$ID)
summary(l)

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


# add cluster group number to main dataframe

dfa<- subset.clust %>% 
  dplyr::distinct(track_id, .keep_all = TRUE) %>% 
  dplyr::select(track_id, hc3.cluster) # change depending on the numbers of clusters
INDO.map2 <- merge(INDO.map, dfa, by = "track_id", all.x = TRUE)

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
  filter(hc3.cluster == 1) # change depending on the number of clusters
clust2<- INDO.map2 %>% 
  filter(hc3.cluster == 2)
clust3<- INDO.map2 %>% 
  filter(hc3.cluster == 3)
clust4<- PAOC.map2 %>% 
  filter(hc5.cluster == 4)
clust5<- PAOC.map2 %>% 
  filter(hc5.cluster == 5)

write.csv(clust1, "./cluster analysis/csv files_cluster assigned_IndOc/Indian Ocean_Cleaned_4 sp_n = 156_outbound and return_1 OF 3 CLUSTER GROUPS_hierarchical complete.csv", row.names = FALSE)

#### Moving into ARC for line density ####
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



