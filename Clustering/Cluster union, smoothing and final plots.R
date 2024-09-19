library(dplyr)
library(smoothr)
library(ggplot2)
library(rnaturalearth)
library(sf)

#### open basemaps ####

# open land polygon 
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))
land <- ne_load(scale = 10, category = 'physical', type = 'land', destdir = "./Basemaps/ne_10m_land", returnclass = c("sf"))

####  project basemap centred at each ocean basin ####

# SOUTHERN: lambert equal areas

SOc_laea = st_transform(land,
                        crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

# plot to check
ggplot(data = SOc_laea) +
  geom_sf(fill = "red")+
  # add colonies
  geom_sf(data = colonySO, fill = "blue", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # geom_sf(data = polygon, fill = "blue") # shows centre line
  coord_sf(xlim = c(-6000000, 5500000), ylim = c(-6000000, 5500000))

####

# INDIAN: molleweide centred at Indian Ocean
target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=78.61895") # Indian

# crossing dateline with projected polygons help: https://stackoverflow.com/questions/68278789/how-to-rotate-world-map-using-mollweide-projection-with-sf-rnaturalearth-ggplot

worldrn <- land %>%
  st_make_valid()

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world

offset <- 180 - 78.61895 # Indian centred

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldrn %>% st_difference(polygon)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

# Transform
IndOc_moll <- world2 %>% st_transform(crs = target_crs)

ggplot(data = IndOc_moll) +
  geom_sf(fill = "red")+
  # add colonies
  geom_sf(data = colonyMoll, fill = "blue", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # geom_sf(data = polygon, fill = "blue") # shows centre line
  coord_sf(xlim = c(-5000000, 4500000), ylim = c(-5500000, 3000000))


####

# PACIFIC: molleweide projection

target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=179") # Pacific -154.9429

worldrn <- land %>%
  st_make_valid()

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world

offset <-  180 - 179 # Pacific centred

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldrn %>% st_difference(polygon)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

# Transform
PacOc_moll <- world2 %>% st_transform(crs = target_crs)

ggplot(data = PacOc_moll) +
  geom_sf(fill = "red")+
  # add colonies
  geom_sf(data = colonyMoll, fill = "blue", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # geom_sf(data = polygon, fill = "blue")+ # shows centre line
  coord_sf(xlim = c(-6000000, 10500000), ylim = c(-7000000, 7200000))

####

# ATLANTIC: molleweide
target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=-29.62406") # Atlantic

AtlOc_moll <- land %>% st_transform(crs = "+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=0")



ggplot(data = AtlOc_moll) +
  geom_sf(fill = "red")+
  # add colonies
  geom_sf(data = colonyMoll, fill = "blue", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-9000000, 4000000), ylim = c(-6000000, 7500000))

# 
# ggplot()+
#   geom_sf(data = AtlOc_laea, fill = "red")+
#   coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 6000000))+ # Atlantic Ocean projection limits
#   theme_bw()
#   
# ggplot(PacOc_laea)+
#   geom_sf()
# 
# ggplot(SOc_laea)+
#   geom_sf()


##### open unsmoothed, WGS84 shapefiles for each cluster for each ocean #####

##### Indian ####

polInd400_025.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster1_search400_poly02.5_unsmoothed_unprojected.shp")
polInd400_025.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster2_search400_poly02.5_unsmoothed_unprojected.shp")

polInd400_05.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster1_search400_poly05_unsmoothed_unprojected.shp")
polInd400_05.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster2_search400_poly05_unsmoothed_unprojected.shp")

polInd400_10.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster1_search400_poly10_unsmoothed_unprojected.shp")
polInd400_10.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster2_search400_poly10_unsmoothed_unprojected.shp")

polInd400_25.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster1_search400_poly25_unsmoothed_unprojected.shp")
polInd400_25.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster2_search400_poly25_unsmoothed_unprojected.shp")

polInd400_50.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster1_search400_poly50_unsmoothed_unprojected.shp")
polInd400_50.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Indian/UPDATEDJan24/Cluster2_search400_poly50_unsmoothed_unprojected.shp")

# plot unprojected to view
ggplot()+
  geom_sf(data = polInd400_025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polInd400_05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = polInd400_10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = polInd400_25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = polInd400_50.1, fill = "#08519C", alpha = 0.9)+
  geom_sf(data = polInd400_025.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = polInd400_05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = polInd400_10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = polInd400_25.2, fill = "#E6550D", alpha = 0.7)+
  geom_sf(data = polInd400_50.2, fill = "#A63603", alpha = 0.7)+
  ##
  geom_sf(data = borders, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

# project to Lambert equal areas for smoothing

# IOlaea_poly025.1 <- st_transform(polInd400_025.1,
#                            crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
# IOlaea_poly025.2 <- st_transform(polInd400_025.2,
#                                  crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
# IOlaea_poly05.1 <- st_transform(polInd400_05.1,
#                                  crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
# IOlaea_poly05.2 <- st_transform(polInd400_05.2,
#                                  crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly10.1 <- st_transform(polInd400_10.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly10.2 <- st_transform(polInd400_10.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly25.1 <- st_transform(polInd400_25.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly25.2 <- st_transform(polInd400_25.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly50.1 <- st_transform(polInd400_50.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
IOlaea_poly50.2 <- st_transform(polInd400_50.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")


# plot projected to view
ggplot()+
  # geom_sf(data = IOlaea_poly025.1, fill = "#EFF3FF", alpha = 0.9)+
  # geom_sf(data = IOlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = IOlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = IOlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = IOlaea_poly50.1, fill = "#08519C", alpha = 0.9)+
  # geom_sf(data = IOlaea_poly025.2, fill = "#FEEDDE", alpha = 0.7)+
  # geom_sf(data = IOlaea_poly05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = IOlaea_poly10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = IOlaea_poly25.2, fill = "#E6550D", alpha = 0.7)+
  geom_sf(data = IOlaea_poly50.2, fill = "#A63603", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA")+
  # coord_sf(xlim = c(-7500000, 8000000), ylim = c(-6500000, 7000000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  # coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  theme_bw()


#### Atlantic - repeat for each cluster and raster (5, 10, 25) ####

# first plot to check if any holes that need filling/ small crumbs to drop
ggplot()+
  geom_sf(data = AOlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = AOlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = AOlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = AOlaea_poly05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = AOlaea_poly10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = AOlaea_poly25.2, fill = "#E6550D", alpha = 0.7)+
  geom_sf(data = AOlaea_poly05.3, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = AOlaea_poly10.3, fill = "#74C476", alpha = 0.4)+
  geom_sf(data = AOlaea_poly25.3, fill = "#31A354", alpha = 0.4)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

#ATLOC POLYGONS (3 clusters)
# poly05, clust 1 = no blobs or holes
# poly10, clust 1 = drop_crumbs 200,000km^2 to drop blob below Africa, no holes
# pol25, clust 1 = drop_crumbs 150,000 km^2 to remove blobs in Med, no holes
# poly05, clust 2 = big blob at NACES - leave, no holes
# poly10, clust 2 = no blobs, removed with 150,000 km^2 threshold no holes
# poly25, clust 2 = 2 blobs,  or holes
# poly05, clust 3 =  no blobs or holes
# poly10, clust 3 = no blobs, no holes
# poly20, clust 3 = no blobs or holes

# below is code to drop crumbs and smooth - for each cluster and polygon size at a time
area_thresh <- units::set_units(1000000, km^2)
r_poly_dropped <- smoothr::drop_crumbs(AOlaea_poly10.1, threshold = area_thresh, drop_empty = TRUE) 
#only run next line if holes to fill
r_poly_filled <- smoothr::fill_holes(r_poly_dropped, units::set_units(950, km^2))

ggplot()+
  geom_sf(data = r_poly_dropped, fill = "#6BAED6", alpha = 0.9)+
  # geom_sf(data = r_poly_filled, fill = "red", alpha = 0.9)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()


AOlaea_poly05.1 <- AOlaea_poly05.1
AOlaea_poly10.1.drop <- r_poly_dropped 
AOlaea_poly25.1.drop <- r_poly_dropped 
AOlaea_poly05.2 <- AOlaea_poly05.2
AOlaea_poly10.2 <- AOlaea_poly10.2
AOlaea_poly25.2.drop <- r_poly_dropped
AOlaea_poly05.3 <- AOlaea_poly05.3
AOlaea_poly10.3 <- AOlaea_poly10.3
AOlaea_poly25.3 <- AOlaea_poly25.3

# smooth 
smoothAOlaea_poly05.1 <- smoothr::smooth(AOlaea_poly05.1, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly10.1 <- smoothr::smooth(AOlaea_poly10.1.drop, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly25.1 <- smoothr::smooth(AOlaea_poly25.1.drop, method = "ksmooth", smoothness = 7.5)

smoothAOlaea_poly05.2 <- smoothr::smooth(AOlaea_poly05.2, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly10.2 <- smoothr::smooth(AOlaea_poly10.2, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly25.2 <- smoothr::smooth(AOlaea_poly25.2.drop, method = "ksmooth", smoothness = 7.5)

smoothAOlaea_poly05.3 <- smoothr::smooth(AOlaea_poly05.3, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly10.3 <- smoothr::smooth(AOlaea_poly10.3, method = "ksmooth", smoothness = 7.5)
smoothAOlaea_poly25.3 <- smoothr::smooth(AOlaea_poly25.3, method = "ksmooth", smoothness = 7.5)


# plot smoothed and unsmoothed to compare
ggplot()+
  geom_sf(data = AOlaea_poly25.3, fill = "red", alpha = 0.9)+
  geom_sf(data = smoothAOlaea_poly25.3, fill= "orange", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean laea projection limits
  theme_bw()


# save unprojected smoothed shapefile for each polygon
latlon <- sf::st_transform(smoothAOlaea_poly05.3, crs = 4326)
plot(latlon)
st_write(latlon, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly05_smoothed7.5_unprojected.shp", driver = "ESRI Shapefile")

latlon <- sf::st_transform(smoothAOlaea_poly10.3, crs = 4326)
plot(latlon)
st_write(latlon, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly10_smoothed7.5_unprojected.shp", driver = "ESRI Shapefile")

latlon <- sf::st_transform(smoothAOlaea_poly25.3, crs = 4326)
plot(latlon)
st_write(latlon, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly25_smoothed7.5_unprojected.shp", driver = "ESRI Shapefile")

# open smoothed polygons (if not running full script) - have saved 5 and 7.5 smoothed - check pathway
smoothAOlaea_poly05.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster1_search400_poly05_smoothed7.5_unprojected.shp")
smoothAOlaea_poly10.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster1_search400_poly10_smoothed7.5_unprojected.shp")
smoothAOlaea_poly25.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster1_search400_poly25_smoothed7.5_unprojected.shp")

smoothAOlaea_poly05.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster2_search400_poly05_smoothed7.5_unprojected.shp")
smoothAOlaea_poly10.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster2_search400_poly10_smoothed7.5_unprojected.shp")
smoothAOlaea_poly25.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster2_search400_poly25_smoothed7.5_unprojected.shp")

smoothAOlaea_poly05.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly05_smoothed7.5_unprojected.shp")
smoothAOlaea_poly10.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly10_smoothed7.5_unprojected.shp")
smoothAOlaea_poly25.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Cluster3_search400_poly25_smoothed7.5_unprojected.shp")

# union Atlantic clusters 1, 2 and 3
AO.05.clust1.2 <- st_union(st_make_valid(smoothAOlaea_poly05.1), st_make_valid(smoothAOlaea_poly05.2))
AO.05.clust1.2.3 <- st_union(st_make_valid(AO.05.clust1.2), st_make_valid(smoothAOlaea_poly05.3))
AO.10.clust1.2 <- st_union(st_make_valid(smoothAOlaea_poly10.1), st_make_valid(smoothAOlaea_poly10.2))
AO.10.clust1.2.3 <- st_union(st_make_valid(AO.10.clust1.2), st_make_valid(smoothAOlaea_poly10.3))
AO.25.clust1.2 <- st_union(st_make_valid(smoothAOlaea_poly25.1), st_make_valid(smoothAOlaea_poly25.2))
AO.25.clust1.2.3 <- st_union(st_make_valid(AO.25.clust1.2), st_make_valid(smoothAOlaea_poly25.3))

# plot unioned smoothed polygons
ggplot()+
  geom_sf(data = AO.05.clust1.2.3, fill = "red", alpha = 0.9)+
  geom_sf(data = AO.10.clust1.2.3, fill= "orange", alpha = 0.7)+
  geom_sf(data = AO.25.clust1.2.3, fill= "yellow", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

# save flyway polygon 
st_write(AO.10.clust1.2.3, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Unioned Clusters_search400_poly10_smoothed7.5_unprojected.shp", driver = "ESRI Shapefile")
AtlFlyway <- st_read("./cluster analysis/ArcGIS files_line density/Smoothed polygons/Atlantic/UPDATEDJan24/Unioned Clusters_search400_poly10_smoothed7.5_unprojected.shp")


# check areas of unioned 5, 10 and 25% contours
units::set_units(st_area(AO.05.clust1.2.3), "km^2") # 79,574,610 [km^2]
units::set_units(st_area(AO.10.clust1.2.3), "km^2") # 54,892,833 [km^2]
units::set_units(st_area(AO.25.clust1.2.3), "km^2") # 9,603,274 [km^2]


#### Indian - repeat for each cluster and raster (5, 10, 25) ####

# first plot to check if any holes that need filling/ small crumbs to drop
ggplot()+
  geom_sf(data = IOlaea_poly10.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = IOlaea_poly25.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = IOlaea_poly50.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = IOlaea_poly10.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = IOlaea_poly25.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = IOlaea_poly50.2, fill = "#E6550D", alpha = 0.7)+
  ##
  geom_sf(data = IndOc_moll, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  theme_bw()

#INDOC POLYGONS (3 clusters)
# poly10, clust 1 = drop_crumbs 40,000 km^2, fill holes 40,000 km^2
# poly25, clust 1 = no crumbs, no holes
# pol50, clust 1 = no crumbs, no holes
# poly10, clust 2 = no crumbs, no holes
# poly25, clust 2 = no crumbs,  no holes
# poly50, clust 2 = no crumbs, no holes

# below is code to drop crumbs and smooth - for each cluster and polygon size at a time
area_thresh <- units::set_units(40000, km^2)
r_poly_dropped <- smoothr::drop_crumbs(IOlaea_poly10.1, threshold = area_thresh, drop_empty = TRUE) 
#only run next line if holes to fill
r_poly_filled <- smoothr::fill_holes(r_poly_dropped, units::set_units(40000, km^2))

ggplot()+
  # geom_sf(data = r_poly_dropped, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = r_poly_filled, fill = "#6BAED6", alpha = 0.9)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  theme_bw()


IOlaea_poly10.1.drop <- r_poly_filled
IOlaea_poly25.1 <- IOlaea_poly25.1
IOlaea_poly50.1 <- IOlaea_poly50.1
IOlaea_poly10.2 <- IOlaea_poly10.2
IOlaea_poly10.2 <- IOlaea_poly10.2
IOlaea_poly50.2 <- IOlaea_poly50.2

# smooth (5 and 7.5)
smoothIOlaea_poly10.1 <- smoothr::smooth(IOlaea_poly10.1.drop, method = "ksmooth", smoothness = 5)
smoothIOlaea_poly25.1 <- smoothr::smooth(IOlaea_poly25.1, method = "ksmooth", smoothness = 5)
smoothIOlaea_poly50.1 <- smoothr::smooth(IOlaea_poly50.1, method = "ksmooth", smoothness = 5)

smoothIOlaea_poly10.2 <- smoothr::smooth(IOlaea_poly10.2, method = "ksmooth", smoothness = 5)
smoothIOlaea_poly25.2 <- smoothr::smooth(IOlaea_poly25.2, method = "ksmooth", smoothness = 5)
smoothIOlaea_poly50.2 <- smoothr::smooth(IOlaea_poly50.2, method = "ksmooth", smoothness = 5)


# plot smoothed and unsmoothed to compare
ggplot()+
  geom_sf(data = IOlaea_poly10.1.drop, fill = "red", alpha = 0.9)+
  geom_sf(data = smoothIOlaea_poly10.1, fill= "orange", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  theme_bw()

# save projected smoothed shapefile for each polygon
# latlon <- sf::st_transform(smoothSOlaea_poly05.1, crs = 4326)
plot(smoothIOlaea_poly10.1)
st_write(smoothIOlaea_poly10.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly10_smoothed5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly10.1, crs = 4326)
plot(smoothIOlaea_poly25.1)
st_write(smoothIOlaea_poly25.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly25_smoothed5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly10.1, crs = 4326)
plot(smoothIOlaea_poly50.1)
st_write(smoothIOlaea_poly50.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly50_smoothed5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly05.1, crs = 4326)
plot(smoothIOlaea_poly10.2)
st_write(smoothIOlaea_poly10.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly10_smoothed5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly10.1, crs = 4326)
plot(smoothIOlaea_poly25.2)
st_write(smoothIOlaea_poly25.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly25_smoothed5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly10.1, crs = 4326)
plot(smoothIOlaea_poly50.2)
st_write(smoothIOlaea_poly50.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly50_smoothed5_projected.shp", driver = "ESRI Shapefile")

# open smoothed polygons (if not running full script) - have saved 5 and 7.5 smoothed - check pathway
smoothIOlaea_poly10.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly10_smoothed5_projected.shp")
smoothIOlaea_poly25.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly25_smoothed5_projected.shp")
smoothIOlaea_poly50.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 1_search400_poly50_smoothed5_projected.shp")

smoothIOlaea_poly10.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly10_smoothed5_projected.shp")
smoothIOlaea_poly25.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly25_smoothed5_projected.shp")
smoothIOlaea_poly50.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Indian/UPDATEDJan24/Cluster 2_search400_poly50_smoothed5_projected.shp")

# no unioning needed as each cluster is its own flyway

# check areas of cluster 1 and 2: 10, 25 and 50% contours
# cluster 1 / East Indian Ocean Flyway
units::set_units(st_area(smoothIOlaea_poly10.1), "km^2") # 15,136,312 [km^2]
units::set_units(st_area(smoothIOlaea_poly25.1), "km^2") # 7,220,536 [km^2]
units::set_units(st_area(smoothIOlaea_poly50.1), "km^2") # 2,619,920 [km^2]

# cluster 2 / North Indian Ocean Flyway
units::set_units(st_area(smoothIOlaea_poly10.2), "km^2") # 11,576,617 [km^2]
units::set_units(st_area(smoothIOlaea_poly25.2), "km^2") # 8,159,199 [km^2]
units::set_units(st_area(smoothIOlaea_poly50.2), "km^2") # 2,898,898 [km^2]


# plot with molleweide projection and colonies
# select colonies that were used in Atlantic analysis only
colonyMollInd<- colonyMoll %>% 
  filter(colony_name == "Aride" | colony_name ==  "Cousin" | colony_name == "D Arros"| 
         colony_name == "Nosy Ve, Madagascar"| colony_name == "Reunion" | colony_name == "Round Island" )


plotInd1<- ggplot(data = IndOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothIOlaea_poly10.1, fill= "#b0d8c7")+
  geom_sf(data = smoothIOlaea_poly25.1, fill = "#5bb191")+
  geom_sf(data = smoothIOlaea_poly50.1, fill = "#117457")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = smoothIOlaea_poly25.1, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = IndOc_moll, fill = "grey")+
  # add all colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # add Indian Ocean colonies from analysis only
  geom_sf(data = colonyMollInd, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-5000000, 3500000))+ # Indian Ocean projection
  theme_bw()+
  theme(axis.text=element_text(size=14), plot.title=element_text(size=20))+
  ggtitle("A")

# cluster 2
plotInd2 <- ggplot(data = IndOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothIOlaea_poly10.2, fill= "#f5c1a8")+
  geom_sf(data = smoothIOlaea_poly25.2, fill = "#e4824e")+
  geom_sf(data = smoothIOlaea_poly50.2, fill = "#d95f02")+
  geom_sf(data = smoothIOlaea_poly25.2, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = IndOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # add Indian Ocean colonies from analysis only
  geom_sf(data = colonyMollInd, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-5000000, 5000000), ylim = c(-5000000, 3500000))+ # Indian Ocean projection
  theme_bw()+
  theme(axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=14), plot.title=element_text(size=20))+
  ggtitle("B")


# create manual legend
legend = data.frame(x1=c(1,3,1,5,4,1), x2=c(2,4,3,6,6,2), y1=c(1,1,4,1,3, 1), y2=c(2,2,5,3,5, 2), 
                    t=c('East Indian Ocean Flyway: 10%','East Indian Ocean Flyway: 25%','East Indian Ocean Flyway: 50%','North Indian Ocean Flyway: 10%','North Indian Ocean Flyway: 25%','North Indian Ocean Flyway: 50%'))
plot1 <- ggplot()+
  # create flyway line legend
  geom_line(data=legend, mapping = aes(x = x1, y = y1,  colour = "Flyway"),lwd = 1.5 )+
  scale_colour_manual( name = "",labels = c("Indian Ocean Flyways"), values = c("red"))+
  # create contour filled legend
  geom_rect(data=legend, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black") +
  scale_fill_manual(breaks = c('East Indian Ocean Flyway: 10%','East Indian Ocean Flyway: 25%','East Indian Ocean Flyway: 50%','North Indian Ocean Flyway: 10%','North Indian Ocean Flyway: 25%','North Indian Ocean Flyway: 50%'), 
                    name = "Contour", values = c("#b0d8c7", "#5bb191", "#117457", "#f5c1a8", "#e4824e", "#d95f02"))+
  theme_bw(base_size = 16)+ # increase legend font size
  guides(colour = guide_legend(order = 2), # change legend display order
         shape = guide_legend(order = 1))

legend1 <- cowplot::get_legend(plot1)

cowplot::plot_grid(plotInd1, plotInd2, legend1, nrow = 1, rel_widths = c(1.5,1.5,1))

ggsave(plot = last_plot(), "./Figures/Manuscript figures/Indian cluster groups_dpi600.smooth5.svg", dpi = 600, height = 6, width = 13)

# save the flyways shapefiles to separate location
st_write(smoothIOlaea_poly25.1,"./Global Flyways/Individual flyway shp files/East Indian Ocean Flyway_search400_poly25_smoothed5_projected.shp", driver = "ESRI Shapefile" )
st_write(smoothIOlaea_poly25.2,"./Global Flyways/Individual flyway shp files/North Indian Ocean Flyway_search400_poly25_smoothed5_projected.shp", driver = "ESRI Shapefile" )

