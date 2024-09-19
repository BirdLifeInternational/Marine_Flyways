###############################################
##### Plotting the line density rasters #######
###############################################

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(terra)


#### Open basemaps ####
# ocean polygon
ocean <- read_sf(dsn = "./Basemaps/GOaS_v1_20211214/goas_v01.shp")

# create sf objects for each ocean basin
IndOcean <- ocean %>% 
  filter(name == "Indian Ocean")

AtlOcean <- ocean %>% 
  filter(name == "North Atlantic Ocean" | name == "South Atlantic Ocean") 
AtlOcean <- st_combine(AtlOcean) # there are north and south polygons so need to combine


PacOcean <- ocean %>% 
  filter(name == "North Pacific Ocean" | name == "South Pacific Ocean") 
PacOcean <- st_combine(PacOcean)

SOcean <- ocean %>% 
  filter(name == "Southern Ocean")

#plot to check the ocean polygons are correct
ggplot(PacOcean)+
  geom_sf()

# open land polygon as well
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))
land <- ne_load(scale = 10, category = 'physical', type = 'land', destdir = "./Basemaps/ne_10m_land", returnclass = c("sf"))

# transform basemaps using centre of each Ocean polygon
IndOc_laea = st_transform(IndOcean, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")

AtlOc_laea = st_transform(AtlOcean, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

PacOc_laea = st_transform(PacOcean, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")

SOc_laea = st_transform(SOcean, 
                        crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")



#### Open rasters from ARC and sort ####

#### ATLANTIC ####

# open 3 rasters in turn (each of the clusters)
raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/AtlOc_rast1of3_search400.tif") # already projected
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

raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/AtlOc_rast2of3_search400.tif") # already projected
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

raster_read <- terra::rast("./cluster analysis/ArcGIS files_line density/AtlOc_rast3of3_search400.tif") # already projected
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


rast_01.1[rast_01.1 < 0.01] <- NA
rast_01.1[rast_01.1 >= 0.01] <- 1
rast_01.2[rast_01.2 < 0.01] <- NA
rast_01.2[rast_01.2 >= 0.01] <- 1
rast_01.3[rast_01.3 < 0.01] <- NA
rast_01.3[rast_01.3 >= 0.01] <- 1

rast_02.1[rast_02.1 < 0.02] <- NA
rast_02.1[rast_02.1 >= 0.02] <- 1
rast_02.2[rast_02.2 < 0.02] <- NA
rast_02.2[rast_02.2 >= 0.02] <- 1
rast_02.3[rast_02.3 < 0.02] <- NA
rast_02.3[rast_02.3 >= 0.02] <- 1

rast_025.1[rast_025.1 < 0.025] <- NA
rast_025.1[rast_025.1 >= 0.025] <- 1
rast_025.2[rast_025.2 < 0.025] <- NA
rast_025.2[rast_025.2 >= 0.025] <- 1
rast_025.3[rast_025.3 < 0.025] <- NA
rast_025.3[rast_025.3 >= 0.025] <- 1

rast_05.1[rast_05.1 < 0.05] <- NA
rast_05.1[rast_05.1 >= 0.05] <- 1
rast_05.2[rast_05.2 < 0.05] <- NA
rast_05.2[rast_05.2 >= 0.05] <- 1
rast_05.3[rast_05.3 < 0.05] <- NA
rast_05.3[rast_05.3 >= 0.05] <- 1

rast_10.1[rast_10.1 < 0.10] <- NA
rast_10.1[rast_10.1 >= 0.10] <- 1
rast_10.2[rast_10.2 < 0.10] <- NA
rast_10.2[rast_10.2 >= 0.10] <- 1
rast_10.3[rast_10.3 < 0.10] <- NA
rast_10.3[rast_10.3 >= 0.10] <- 1


rast_20.1[rast_20.1 < 0.20] <- NA
rast_20.1[rast_20.1 >= 0.20] <- 1
rast_20.2[rast_20.2 < 0.20] <- NA
rast_20.2[rast_20.2 >= 0.20] <- 1
rast_20.3[rast_20.3 < 0.20] <- NA
rast_20.3[rast_20.3 >= 0.20] <- 1

rast_25.1[rast_25.1 < 0.25] <- NA
rast_25.1[rast_25.1 >= 0.25] <- 1
rast_25.2[rast_25.2 < 0.25] <- NA
rast_25.2[rast_25.2 >= 0.25] <- 1
rast_25.3[rast_25.3 < 0.25] <- NA
rast_25.3[rast_25.3 >= 0.25] <- 1

rast_30.1[rast_30.1 < 0.30] <- NA
rast_30.1[rast_30.1 >= 0.30] <- 1
rast_30.2[rast_30.2 < 0.30] <- NA
rast_30.2[rast_30.2 >= 0.30] <- 1
rast_30.3[rast_30.3 < 0.30] <- NA
rast_30.3[rast_30.3 >= 0.30] <- 1


rast_40.1[rast_40.1 < 0.40] <- NA
rast_40.1[rast_40.1 >= 0.40] <- 1
rast_40.2[rast_40.2 < 0.40] <- NA
rast_40.2[rast_40.2 >= 0.40] <- 1
rast_40.3[rast_40.3 < 0.40] <- NA
rast_40.3[rast_40.3 >= 0.40] <- 1


rast_50.1[rast_50.1 < 0.50] <- NA
rast_50.1[rast_50.1 >= 0.50] <- 1
rast_50.2[rast_50.2 < 0.50] <- NA
rast_50.2[rast_50.2 >= 0.50] <- 1
rast_50.3[rast_50.3 < 0.50] <- NA
rast_50.3[rast_50.3 >= 0.50] <- 1

rast_75.1[rast_75.1 < 0.75] <- NA
rast_75.1[rast_75.1 >= 0.75] <- 1
rast_75.2[rast_75.2 < 0.75] <- NA
rast_75.2[rast_75.2 >= 0.75] <- 1
rast_75.3[rast_75.3 < 0.75] <- NA
rast_75.3[rast_75.3 >= 0.75] <- 1


rast_90.1[rast_90.1 < 0.90] <- NA
rast_90.1[rast_90.1 >= 0.90] <- 1
rast_90.2[rast_90.2 < 0.90] <- NA
rast_90.2[rast_90.2 >= 0.90] <- 1
rast_90.3[rast_90.3 < 0.90] <- NA
rast_90.3[rast_90.3 >= 0.90] <- 1

rast_95.1[rast_95.1 < 0.95] <- NA
rast_95.1[rast_95.1 >= 0.95] <- 1
rast_95.2[rast_95.2 < 0.95] <- NA
rast_95.2[rast_95.2 >= 0.95] <- 1
rast_95.3[rast_95.3 < 0.95] <- NA
rast_95.3[rast_95.3 >= 0.95] <- 1


rast_100.1[rast_100.1 < 1] <- NA
rast_100.1[rast_100.1 >= 1] <- 1
rast_100.2[rast_100.2 < 1] <- NA
rast_100.2[rast_100.2 >= 1] <- 1
rast_100.3[rast_100.3 < 1] <- NA
rast_100.3[rast_100.3 >= 1] <- 1


# colur hexcodes
RColorBrewer::brewer.pal(n = 9, name = "RdYlGn")

plot(st_geometry(AtlOc_laea))
plot(rast_01.1, col = "#D73027", add = TRUE)
# plot(rast_02.1, col = "#F46D43", add = TRUE) # indian and Atl: 5, 10, 20, 25, 30, 40
plot(rast_025.1, col = "#F46D43", add = TRUE) # change depending on which raster want to check
# plot(rast_03.1, col = "#FDAE61", add = TRUE) # pacific 1, 2, 3, 4, 5, 10
# plot(rast_04.1, col = "#FEE08B", add = TRUE)
plot(rast_05.1, col = "#FFFFBF", add = TRUE)
plot(rast_10.1, col = "#D9EF8B", add = TRUE)
plot(rast_25.1, col = "#A6D96A", add = TRUE)
plot(rast_50.1, col = "#66BD63", add = TRUE)
# plot(rast_75.1, col = "#66BD63", add = TRUE)
# plot(rast_90.1, col = "#1A9850", add = TRUE)
# plot(rast_95, col = "red", add = TRUE)
# plot(rast_100, col = "red", add = TRUE)

plot(st_geometry(AtlOc_laea))
plot(rast_01.2, col = "#D73027", add = TRUE)
# plot(rast_02.2, col = "#F46D43", add = TRUE) # indian and Atl: 5, 10, 20, 25, 30, 40
plot(rast_025.2, col = "#F46D43", add = TRUE) # change depending on which raster want to check
# plot(rast_03.2, col = "#FDAE61", add = TRUE) # pacific 1, 2, 3, 4, 5, 10
# plot(rast_04.2, col = "#FEE08B", add = TRUE)
plot(rast_05.2, col = "#FFFFBF", add = TRUE)
plot(rast_10.2, col = "#D9EF8B", add = TRUE)
plot(rast_25.2, col = "#A6D96A", add = TRUE)
plot(rast_50.2, col = "#66BD63", add = TRUE)
# plot(rast_75.2, col = "#66BD63", add = TRUE)
# plot(rast_90.2, col = "#1A9850", add = TRUE)
# plot(rast_95.2, col = "red", add = TRUE)
# plot(rast_100.2, col = "red", add = TRUE)

plot(st_geometry(AtlOc_laea))
plot(rast_01.3, col = "#D73027", add = TRUE)
# plot(rast_02.3, col = "#F46D43", add = TRUE) # indian and Atl: 5, 10, 20, 25, 30, 40
plot(rast_025.3, col = "#F46D43", add = TRUE) # change depending on which raster want to check
# plot(rast_03.3, col = "#FDAE61", add = TRUE) # pacific 1, 2, 3, 4, 5, 10
# plot(rast_04.3, col = "#FEE08B", add = TRUE)
plot(rast_05.3, col = "#FFFFBF", add = TRUE)
plot(rast_10.3, col = "#D9EF8B", add = TRUE)
plot(rast_25.3, col = "#A6D96A", add = TRUE)
plot(rast_50.3, col = "#66BD63", add = TRUE)
# plot(rast_75.3, col = "#66BD63", add = TRUE)
# plot(rast_90.3, col = "#1A9850", add = TRUE)
# plot(rast_95.3, col = "red", add = TRUE)
# plot(rast_100.3, col = "red", add = TRUE)


# save as polygons
# cluster 1
polAtl400_025.1 <- terra::as.polygons(rast_025.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_025.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_025.1)[1]) 
polAtl400_025.1_latlon <- sf::st_transform(polAtl400_025.1, crs = 4326)

polAtl400_05.1 <- terra::as.polygons(rast_05.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.1)[1]) 
polAtl400_05.1_latlon <- sf::st_transform(polAtl400_05.1, crs = 4326)

polAtl400_10.1 <- terra::as.polygons(rast_10.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.1)[1]) 
polAtl400_10.1_latlon <- sf::st_transform(polAtl400_10.1, crs = 4326)

polAtl400_25.1 <- terra::as.polygons(rast_25.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.1)[1]) 
polAtl400_25.1_latlon <- sf::st_transform(polAtl400_25.1, crs = 4326)

polAtl400_50.1 <- terra::as.polygons(rast_50.1) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.1)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.1)[1]) 
polAtl400_50.1_latlon <- sf::st_transform(polAtl400_50.1, crs = 4326)

# cluster 2
polAtl400_025.2 <- terra::as.polygons(rast_025.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_025.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_025.2)[1]) 
polAtl400_025.2_latlon <- sf::st_transform(polAtl400_025.2, crs = 4326)

polAtl400_05.2 <- terra::as.polygons(rast_05.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.2)[1]) 
polAtl400_05.2_latlon <- sf::st_transform(polAtl400_05.2, crs = 4326)

polAtl400_10.2 <- terra::as.polygons(rast_10.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.2)[1]) 
polAtl400_10.2_latlon <- sf::st_transform(polAtl400_10.2, crs = 4326)

polAtl400_25.2 <- terra::as.polygons(rast_25.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.2)[1]) 
polAtl400_25.2_latlon <- sf::st_transform(polAtl400_25.2, crs = 4326)

polAtl400_50.2 <- terra::as.polygons(rast_50.2) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.2)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.2)[1]) 
polAtl400_50.2_latlon <- sf::st_transform(polAtl400_50.2, crs = 4326)

# cluster 3
polAtl400_025.3 <- terra::as.polygons(rast_025.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_025.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_025.3)[1]) 
polAtl400_025.3_latlon <- sf::st_transform(polAtl400_025.3, crs = 4326)

polAtl400_05.3 <- terra::as.polygons(rast_05.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_05.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_05.3)[1]) 
polAtl400_05.3_latlon <- sf::st_transform(polAtl400_05.3, crs = 4326)

polAtl400_10.3 <- terra::as.polygons(rast_10.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_10.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_10.3)[1]) 
polAtl400_10.3_latlon <- sf::st_transform(polAtl400_10.3, crs = 4326)

polAtl400_25.3 <- terra::as.polygons(rast_25.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_25.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_25.3)[1]) 
polAtl400_25.3_latlon <- sf::st_transform(polAtl400_25.3, crs = 4326)

polAtl400_50.3 <- terra::as.polygons(rast_50.3) %>%
  sf::st_as_sf() %>%
  sf::st_buffer(dist = res(rast_50.3)[1]/4) %>%
  sf::st_simplify(dTolerance = res(rast_50.3)[1]) 
polAtl400_50.3_latlon <- sf::st_transform(polAtl400_50.3, crs = 4326)


# plot and check outputs

# colour hexcodes
RColorBrewer::brewer.pal(n = 5, name = "Blues")
RColorBrewer::brewer.pal(n = 5, name = "Oranges")
RColorBrewer::brewer.pal(n = 5, name = "Greens")
# basemap projection
borders_laea = st_transform(borders, 
                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")
# borders_laea_union <- st_union(st_make_valid(borders_laea)) # remove country borders
ocean_Atllaea = st_transform(ocean,
                             crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")

clust1map<- ggplot()+
  geom_sf(data = ocean_Atllaea, fill = "lightskyblue1", alpha = 0.5)+
  # geom_sf(data = polAtl500_025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polAtl400_05.1, fill = "#B0D8C7", alpha = 0.9)+
  geom_sf(data = polAtl400_10.1, fill = "#5BB191", alpha = 0.9)+
  geom_sf(data = polAtl400_25.1, fill = "#1B9E77", alpha = 0.9)+
  geom_sf(data = polAtl400_50.1, fill = "#117457", alpha = 0.9)+
  ##
  geom_sf(data = borders, fill = "NA", colour = "gray60")+
# coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6000000))+ # Atlantic Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 16),
        axis.text.y = element_text(color = "grey20", size = 16))+
  ggtitle("A") 

ggsave(plot = clust1map, "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 1_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", dpi = 500, height = 15, width = 15)

clust2map<- ggplot()+
  geom_sf(data = ocean_Atllaea, fill = "lightskyblue1", alpha = 0.5)+
  geom_sf(data = polAtl400_05.2, fill = "#F5C1A8", alpha = 0.9)+
  geom_sf(data = polAtl400_10.2, fill = "#E4824E", alpha = 0.9)+
  geom_sf(data = polAtl400_25.2, fill = "#D95F02", alpha = 0.9)+
  geom_sf(data = polAtl400_50.2, fill = "#A14401", alpha = 0.9)+
  ##
geom_sf(data = borders, fill = "NA", colour = "gray60")+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6000000))+ # Atlantic Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 16),
        axis.text.y = element_text(color = "grey20", size = 16))+
  ggtitle("B") 

ggsave(plot = clust2map, "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 2_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", dpi = 500, height = 15, width = 15)

clust3map<- ggplot()+
  geom_sf(data = ocean_Atllaea, fill = "lightskyblue1", alpha = 0.5)+
  geom_sf(data = polAtl400_05.3, fill = "#C5C5E5", alpha = 0.9)+
  geom_sf(data = polAtl400_10.3, fill = "#8E8CCA", alpha = 0.9)+
  geom_sf(data = polAtl400_25.3, fill = "#7570BB", alpha = 0.9)+
  geom_sf(data = polAtl400_50.3, fill = "#55518A", alpha = 0.9)+
  ##
  geom_sf(data = borders, fill = "NA", colour = "gray60")+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6000000))+ # Atlantic Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 16),
        axis.text.y = element_text(color = "grey20", size = 16))+
  ggtitle("C") 

ggsave(plot = clust3map, "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 3_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", dpi = 500, height = 15, width = 15)


cowplot::plot_grid(clust1map, clust2map, clust3map, ncol = 3)

ggsave(plot = last_plot(), "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 1, 2 and 3_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", dpi = 500, height = 15, width = 15)

clustallmap <-  ggplot()+
  geom_sf(data = ocean_Atllaea, fill = "lightskyblue1", alpha = 0.5)+
  geom_sf(data = polAtl400_05.1, fill = "#B0D8C7", alpha = 0.9)+
  geom_sf(data = polAtl400_10.1, fill = "#5BB191", alpha = 0.9)+
  geom_sf(data = polAtl400_25.1, fill = "#1B9E77", alpha = 0.9)+
  geom_sf(data = polAtl400_50.1, fill = "#117457", alpha = 0.9)+
  geom_sf(data = polAtl400_05.2, fill = "#F5C1A8", alpha = 0.7)+
  geom_sf(data = polAtl400_10.2, fill = "#E4824E", alpha = 0.7)+
  geom_sf(data = polAtl400_25.2, fill = "#D95F02", alpha = 0.7)+
  geom_sf(data = polAtl400_50.2, fill = "#A14401", alpha = 0.7)+
  geom_sf(data = polAtl400_05.3, fill = "#C5C5E5", alpha = 0.4)+
  geom_sf(data = polAtl400_10.3, fill = "#8E8CCA", alpha = 0.4)+
  geom_sf(data = polAtl400_25.3, fill = "#7570BB", alpha = 0.4)+
  geom_sf(data = polAtl400_50.3, fill = "#55518A", alpha = 0.4)+
  ##
  geom_sf(data = borders, fill = "NA", colour = "gray60")+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6000000))+ # Atlantic Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color = "grey20", size = 16),
        axis.text.y = element_text(color = "grey20", size = 16))+
  ggtitle("D") 
  
ggsave(plot = clustallmap, "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 1, 2 and 3_one panel_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", height = 15, width = 15)

cowplot::plot_grid(clust1map, clust2map, clust3map, clustallmap, nrow = 2, ncol = 2)

ggsave(plot = last_plot(), "./cluster analysis/ArcGIS files_line density/Figures/R_map_UPDATES_Atlantic_cluster 1, 2 and 3 + merged_search400_polys 5, 10, 25, 50_not smoothed_custom lambert.jpg", height = 15, width = 15)

# save polygons unprojected
st_write(polAtl500_50.3_latlon, "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search500_poly50_unsmoothed_unprojected.shp", driver = "ESRI Shapefile")

##### open the polygons  #####
# if not running the whole script open the saved unprojected polygons here (for each ocean and cluster)
polAtl400_025.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster1_search400_poly02.5_unsmoothed_unprojected.shp")
polAtl400_025.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster2_search400_poly02.5_unsmoothed_unprojected.shp")
polAtl400_025.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search400_poly02.5_unsmoothed_unprojected.shp")

polAtl400_05.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster1_search400_poly05_unsmoothed_unprojected.shp")
polAtl400_05.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster2_search400_poly05_unsmoothed_unprojected.shp")
polAtl400_05.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search400_poly05_unsmoothed_unprojected.shp")

polAtl400_10.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster1_search400_poly10_unsmoothed_unprojected.shp")
polAtl400_10.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster2_search400_poly10_unsmoothed_unprojected.shp")
polAtl400_10.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search400_poly10_unsmoothed_unprojected.shp")

polAtl400_25.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster1_search400_poly25_unsmoothed_unprojected.shp")
polAtl400_25.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster2_search400_poly25_unsmoothed_unprojected.shp")
polAtl400_25.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search400_poly25_unsmoothed_unprojected.shp")

polAtl400_50.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster1_search400_poly50_unsmoothed_unprojected.shp")
polAtl400_50.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster2_search400_poly50_unsmoothed_unprojected.shp")
polAtl400_50.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Atlantic/UPDATEDJan24/Cluster3_search400_poly50_unsmoothed_unprojected.shp")


ggplot()+
  geom_sf(data = polAtl400_025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polAtl400_05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = polAtl400_10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = polAtl400_25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = polAtl400_50.1, fill = "#08519C", alpha = 0.9)+
  geom_sf(data = polAtl400_025.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = polAtl400_05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = polAtl400_10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = polAtl400_25.2, fill = "#E6550D", alpha = 0.7)+
  geom_sf(data = polAtl400_50.2, fill = "#A63603", alpha = 0.7)+
  geom_sf(data = polAtl400_025.3, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = polAtl400_05.3, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = polAtl400_10.3, fill = "#74C476", alpha = 0.4)+
  geom_sf(data = polAtl400_25.3, fill = "#31A354", alpha = 0.4)+
  geom_sf(data = polAtl400_50.3, fill = "#006D2C", alpha = 0.4)+
  ##
  geom_sf(data = borders, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

