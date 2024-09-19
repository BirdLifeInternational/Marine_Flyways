library(dplyr)
library(smoothr)
library(ggplot2)
library(rnaturalearth)
library(sf)

#### open basemaps ####

# # ocean polygon
# ocean <- read_sf(dsn = "./Basemaps/GOaS_v1_20211214/goas_v01.shp")
# 
# # create sf objects for each ocean basin
# IndOcean <- ocean %>% 
#   filter(name == "Indian Ocean")
# 
# AtlOcean <- ocean %>% 
#   filter(name == "North Atlantic Ocean" | name == "South Atlantic Ocean") 
# AtlOcean <- st_combine(AtlOcean) # there are north and south polygons so need to combine
# 
# 
# PacOcean <- ocean %>% 
#   filter(name == "North Pacific Ocean" | name == "South Pacific Ocean") 
# PacOcean <- st_combine(PacOcean)
# 
# SOcean <- ocean %>% 
#   filter(name == "Southern Ocean")
# 
# #plot to check the ocean polygons are correct
# ggplot(SOcean)+
#   geom_sf()

# transform basemaps using centre of each Ocean polygon
# IndOc_laea = st_transform(IndOcean, 
#                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=78.61895 +lat_0=-30.4754")
# 
# AtlOc_laea = st_transform(AtlOcean, 
#                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")
# 
# PacOc_laea = st_transform(PacOcean, 
#                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
# 
# SOc_laea = st_transform(SOcean, 
#                         crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")


# open land polygon 
borders <- ne_load(scale = 10, type = 'countries', destdir = "./Basemaps/ne_10m_countries",  returnclass = c( "sf"))
land <- ne_load(scale = 10, category = 'physical', type = 'land', destdir = "./Basemaps/ne_10m_land", returnclass = c("sf"))

# open colony locations
MIG <- read.csv("data filtered by migration based on displacement plots_MIGRATION.v8_reclassified including additional data Jul23and A-L arte.csv")

# fix colony names
colony_names <- MIG %>% 
  dplyr::select(colony_name, lat_colony, lon_colony) %>% 
  # remove duplicated colonies
  dplyr::distinct(colony_name, .keep_all = TRUE) %>% 
  # remove any duplicated names with diff spellings
  dplyr::filter((colony_name != "Bird Island (SGSSI (IGSISS))" & colony_name != "Henderson" & colony_name != "Kerguelen" & colony_name != "Reunion_Island_Grand_Benare" & colony_name != "Antipodes Islands"))# remove duplicates

# make the colonies into a spatial object
colonyGeom <- st_as_sf(colony_names, coords = c("lon_colony", "lat_colony"), crs = 4326)
#save as separate shapefile
st_write(colonyGeom,"./Global Flyways/Individual flyway shp files/colony locations.shp", driver = "ESRI Shapefile" )
# project to molleweide and ortho to match the ocean projections (below)  
colonyMoll <- st_transform(colonyGeom, crs = "+proj=moll")
colonySO <- st_transform(colonyGeom, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

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

#### Atlantic ####

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

# plot unprojected to view
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


# project to Lambert Equal Areas for smoothing 

# AOlaea_poly025.1 <- st_transform(polAtl400_025.1,
#                            crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
# AOlaea_poly025.2 <- st_transform(polAtl400_025.2,
#                                  crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
# AOlaea_poly025.3 <- st_transform(polAtl400_025.3,
#                                  crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly05.1 <- st_transform(polAtl400_05.1,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly05.2 <- st_transform(polAtl400_05.2,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly05.3 <- st_transform(polAtl400_05.3,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly10.1 <- st_transform(polAtl400_10.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly10.2 <- st_transform(polAtl400_10.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly10.3 <- st_transform(polAtl400_10.3,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly25.1 <- st_transform(polAtl400_25.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly25.2 <- st_transform(polAtl400_25.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
AOlaea_poly25.3 <- st_transform(polAtl400_25.3,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726") 
# AOlaea_poly50.1 <- st_transform(polAtl400_50.1,
#                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")
# AOlaea_poly50.2 <- st_transform(polAtl400_50.2,
#                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")
# AOlaea_poly50.3 <- st_transform(polAtl400_50.3,
#                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-29.62406 +lat_0=-0.726")


# plot projected to check
ggplot()+
  # geom_sf(data = AOlaea_poly025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = AOlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = AOlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = AOlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
  # geom_sf(data = AOlaea_poly50.1, fill = "#08519C", alpha = 0.9)+
  # geom_sf(data = AOlaea_poly025.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = AOlaea_poly05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = AOlaea_poly10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = AOlaea_poly25.2, fill = "#E6550D", alpha = 0.7)+
  # geom_sf(data = AOlaea_poly50.2, fill = "#A63603", alpha = 0.7)+
  # geom_sf(data = AOlaea_poly025.3, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = AOlaea_poly05.3, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = AOlaea_poly10.3, fill = "#74C476", alpha = 0.4)+
  geom_sf(data = AOlaea_poly25.3, fill = "#31A354", alpha = 0.4)+
  # geom_sf(data = AOlaea_poly50.3, fill = "#006D2C", alpha = 0.4)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-7500000, 8000000), ylim = c(-6500000, 7000000))+ # Pacific Ocean projection limits
  coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  # coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  # coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  theme_bw()

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

#### Pacific ####

polPac400_01.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly01_unsmoothed_unprojected.shp")
polPac400_01.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly01_unsmoothed_unprojected.shp")
polPac400_01.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly01_unsmoothed_unprojected.shp")
polPac400_01.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly01_unsmoothed_unprojected.shp")

polPac400_025.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly02.5_unsmoothed_unprojected.shp")
polPac400_025.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly02.5_unsmoothed_unprojected.shp")
polPac400_025.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly02.5_unsmoothed_unprojected.shp")
polPac400_025.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly02.5_unsmoothed_unprojected.shp")

polPac400_05.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly05_unsmoothed_unprojected.shp")
polPac400_05.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly05_unsmoothed_unprojected.shp")
polPac400_05.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly05_unsmoothed_unprojected.shp")
polPac400_05.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly05_unsmoothed_unprojected.shp")

polPac400_10.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly10_unsmoothed_unprojected.shp")
polPac400_10.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly10_unsmoothed_unprojected.shp")
polPac400_10.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly10_unsmoothed_unprojected.shp")
polPac400_10.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly10_unsmoothed_unprojected.shp")

polPac400_25.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly25_unsmoothed_unprojected.shp")
polPac400_25.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly25_unsmoothed_unprojected.shp")
polPac400_25.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly25_unsmoothed_unprojected.shp")
polPac400_25.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly25_unsmoothed_unprojected.shp")

polPac400_50.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster1_search400_poly50_unsmoothed_unprojected.shp")
polPac400_50.2 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster2_search400_poly50_unsmoothed_unprojected.shp")
polPac400_50.3 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster3_search400_poly50_unsmoothed_unprojected.shp")
polPac400_50.4 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Pacific/UPDATEDJan24/Cluster4_search400_poly50_unsmoothed_unprojected.shp")

# plot unprojected to view
ggplot()+
  geom_sf(data = polPac400_01.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polPac400_025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polPac400_05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = polPac400_10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = polPac400_25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = polPac400_50.1, fill = "#08519C", alpha = 0.9)+
  geom_sf(data = polPac400_01.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = polPac400_025.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = polPac400_05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = polPac400_10.2, fill = "#FD8D3C", alpha = 0.7)+
  geom_sf(data = polPac400_25.2, fill = "#E6550D", alpha = 0.7)+
  geom_sf(data = polPac400_50.2, fill = "#A63603", alpha = 0.7)+
  geom_sf(data = polPac400_01.3, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = polPac400_025.3, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = polPac400_05.3, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = polPac400_10.3, fill = "#74C476", alpha = 0.4)+
  geom_sf(data = polPac400_25.3, fill = "#31A354", alpha = 0.4)+
  geom_sf(data = polPac400_50.3, fill = "#006D2C", alpha = 0.4)+
  geom_sf(data = polPac400_01.4, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = polPac400_025.4, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = polPac400_05.4, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = polPac400_10.4, fill = "#74C476", alpha = 0.4)+
  geom_sf(data = polPac400_25.4, fill = "#31A354", alpha = 0.4)+
  geom_sf(data = polPac400_50.4, fill = "#006D2C", alpha = 0.4)+
  ##
  ##
  geom_sf(data = borders, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

# project to Lambert equal areas for smoothing

POlaea_poly01.1 <- st_transform(polPac400_01.1,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly01.2 <- st_transform(polPac400_01.2,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly01.3 <- st_transform(polPac400_01.3,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly01.4 <- st_transform(polPac400_01.4,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly025.1 <- st_transform(polPac400_025.1,
                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly025.2 <- st_transform(polPac400_025.2,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly025.3 <- st_transform(polPac400_025.3,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly025.4 <- st_transform(polPac400_025.4,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly05.1 <- st_transform(polPac400_05.1,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly05.2 <- st_transform(polPac400_05.2,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly05.3 <- st_transform(polPac400_05.3,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly05.4 <- st_transform(polPac400_05.4,
                                 crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly10.1 <- st_transform(polPac400_10.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly10.2 <- st_transform(polPac400_10.2,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly10.3 <- st_transform(polPac400_10.3,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")
POlaea_poly10.4 <- st_transform(polPac400_10.4,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-154.9429 +lat_0=-6.064346")


# project projected to check
ggplot()+
  # geom_sf(data = POlaea_poly01.1, fill = "red", alpha = 0.9)+
  geom_sf(data = POlaea_poly025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = POlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = POlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  # geom_sf(data = POlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
  # geom_sf(data = AOlaea_poly50.1, fill = "#08519C", alpha = 0.9)+
  # geom_sf(data = POlaea_poly01.2, fill = "red", alpha = 0.7)+
  geom_sf(data = POlaea_poly025.2, fill = "#FEEDDE", alpha = 0.7)+
  geom_sf(data = POlaea_poly05.2, fill = "#FDBE85", alpha = 0.7)+
  geom_sf(data = POlaea_poly10.2, fill = "#FD8D3C", alpha = 0.7)+
  # geom_sf(data = POlaea_poly25.2, fill = "#E6550D", alpha = 0.7)+
  # geom_sf(data = AOlaea_poly50.2, fill = "#A63603", alpha = 0.7)+
  # geom_sf(data = POlaea_poly01.3, fill = "red", alpha = 0.4)+
  geom_sf(data = POlaea_poly025.3, fill = "#EDF8E9", alpha = 0.4)+
  geom_sf(data = POlaea_poly05.3, fill = "#BAE4B3", alpha = 0.4)+
  geom_sf(data = POlaea_poly10.3, fill = "#74C476", alpha = 0.4)+
  # geom_sf(data = POlaea_poly25.3, fill = "#31A354", alpha = 0.4)+
  # # geom_sf(data = AOlaea_poly50.3, fill = "#006D2C", alpha = 0.4)+
  # geom_sf(data = POlaea_poly01.4, fill = "red", alpha = 0.4)+
  geom_sf(data = POlaea_poly025.4, fill = "#FFB4CF", alpha = 0.4)+
  geom_sf(data = POlaea_poly05.4, fill = "#FA8DB7", alpha = 0.4)+
  geom_sf(data = POlaea_poly10.4, fill = "#F263A0", alpha = 0.4)+
  # geom_sf(data = POlaea_poly25.4, fill = "#E7298A", alpha = 0.4)+
  # geom_sf(data = AOlaea_poly50.4, fill = "#AB1C65", alpha = 0.4)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7500000, 8000000), ylim = c(-6500000, 7000000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  # coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
  # coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  theme_bw()



##### Southern ####

polSou400_025.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search400_poly02.5_unsmoothed_unprojected.shp")
# polSou500_025.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search500_poly02.5_unsmoothed_unprojected.shp")

polSou400_05.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search400_poly05_unsmoothed_unprojected.shp")
# polSou500_05.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search500_poly05_unsmoothed_unprojected.shp")

polSou400_10.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search400_poly10_unsmoothed_unprojected.shp")
# polSou500_10.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search500_poly10_unsmoothed_unprojected.shp")

polSou400_25.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search400_poly25_unsmoothed_unprojected.shp")
# polSou500_25.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search500_poly25_unsmoothed_unprojected.shp")

polSou400_50.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search400_poly50_unsmoothed_unprojected.shp")
# polSou500_50.1 <- st_read(dsn = "./cluster analysis/ArcGIS files_line density/Polygons from rasters/Southern/UPDATEDJan24/NoClusters_search500_poly50_unsmoothed_unprojected.shp")

# plot to check all saved/opened correctly
ggplot()+
  geom_sf(data = polSou400_025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = polSou400_05.1, fill = "#BDD7E7", alpha = 0.9)+
  geom_sf(data = polSou400_10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = polSou400_25.1, fill = "#3182BD", alpha = 0.9)+
  geom_sf(data = polSou400_50.1, fill = "#08519C", alpha = 0.9)+
  # geom_sf(data = polInd500_025.2, fill = "#FEEDDE", alpha = 0.7)+
  # geom_sf(data = polInd500_05.2, fill = "#FDBE85", alpha = 0.7)+
  # geom_sf(data = polInd500_10.2, fill = "#FD8D3C", alpha = 0.7)+
  # geom_sf(data = polInd500_25.2, fill = "#E6550D", alpha = 0.7)+
  # geom_sf(data = polInd500_50.2, fill = "#A63603", alpha = 0.7)+
  ##
  geom_sf(data = borders, fill = "gray60", alpha = 0.8)+
  # coord_sf(xlim = c(-12000000, 10000000), ylim = c(-900000, 900000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()

# project to Lambert equal areas for smoothing

SOlaea_poly025.1 <- st_transform(polSou400_025.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")
SOlaea_poly05.1 <- st_transform(polSou400_05.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")
SOlaea_poly10.1 <- st_transform(polSou400_10.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")
SOlaea_poly25.1 <- st_transform(polSou400_25.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")
SOlaea_poly50.1 <- st_transform(polSou400_50.1,
                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=0 +lat_0=-90")

ggplot()+
  # geom_sf(data = ocean_Soulaea, fill = "lightskyblue1", alpha = 0.5)+
  # geom_sf(data = SOlaea_poly025.1, fill = "#EFF3FF", alpha = 0.9)+
  geom_sf(data = SOlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  # geom_sf(data = polSou400_10.1, fill = "#6BAED6", alpha = 0.9)+ # jump back to previous script to get correct polygon 10%
  geom_sf(data = SOlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = SOlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
  # geom_sf(data = SOlaea_poly50.1, fill = "#08519C", alpha = 0.9)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  # coord_sf(xlim = c(-7500000, 8000000), ylim = c(-6500000, 7000000))+ # Pacific Ocean projection limits
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  # coord_sf(xlim = c(-5000000, 5000000), ylim = c(-3500000, 6000000))+ # Indian Ocean projection
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

# select colonies that were used in Atlantic analysis only
colonyMollAtl<- colonyMoll %>% 
  filter(colony_name == "AMM" | colony_name ==  "Baccalieu Island" | colony_name == "Bird Island, South Georgia"| 
           colony_name == "Curral Velho" | colony_name == "Eemshaven" | colony_name == "Gough Island" | 
           colony_name == "HOC" | colony_name == "Iles Kerguelen"| colony_name == "Inaccessible Island"| 
           colony_name == "Kidney Island FK" | colony_name == "King George Island (Fildes Peninsula and Ardley Island)" |
           colony_name == "M Clara" | colony_name == "Menorca/Cala Morell" | colony_name == "Na Foradada" | 
           colony_name == "Na Pobra" | colony_name == "New Island" | colony_name == "Raso" | colony_name == "Sand Island" |
           colony_name == "Sandgerdi" | colony_name == "Selvagem" | colony_name == "Skomer"  | colony_name == "Strofades" |
           colony_name == "SVA"| colony_name == "Vila" | colony_name == "ZAC"| colony_name == "Zembra" )

# plot with molleweide projection and colonies
plotAtl1<- ggplot(data = AtlOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothAOlaea_poly05.1, fill= "#b0d8c7")+
  geom_sf(data = smoothAOlaea_poly10.1, fill = "#5bb191")+
  geom_sf(data = smoothAOlaea_poly25.1, fill = "#117457")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AO.10.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AtlOc_moll, fill = "grey")+
  # add all colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # add Atlantic colonies
  geom_sf(data = colonyMollAtl, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-7500000, 3100000), ylim = c(-6800000, 7500000))+
  theme_bw()+
  theme(axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("A")

# cluster 2
plotAtl2 <- ggplot(data = AtlOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothAOlaea_poly05.2, fill= "#f5c1a8")+
  geom_sf(data = smoothAOlaea_poly10.2, fill = "#e4824e")+
  geom_sf(data = smoothAOlaea_poly25.2, fill = "#d95f02")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AO.10.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AtlOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # add Atlantic colonies
  geom_sf(data = colonyMollAtl, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-7500000, 3100000), ylim = c(-6800000, 7500000))+
  theme_bw()+
  theme(axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("B")

# cluster 3
plotAtl3<- ggplot(data = AtlOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothAOlaea_poly05.3, fill= "#c5c5e5")+
  geom_sf(data = smoothAOlaea_poly10.3, fill = "#8e8cca")+
  geom_sf(data = smoothAOlaea_poly25.3, fill = "#55518a")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AO.10.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AtlOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # add Atlantic colonies
  geom_sf(data = colonyMollAtl, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  coord_sf(xlim = c(-7500000, 3100000), ylim = c(-6800000, 7500000))+
  theme_bw()+
  theme(axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("C")

 
# create manual legend
legend = data.frame(x1=c(1,3,1,5,4, 1,3,1,5), x2=c(2,4,3,6,6,2,4,3,6), y1=c(1,1,4,1,3, 1,1,4,1), y2=c(2,2,5,3,5, 2,2,5,3), 
                    t=c('cluster A: 5%','cluster A: 10%','cluster A: 25%','cluster B: 5%','cluster B: 10%','cluster B: 25%','cluster C: 5%','cluster C: 10%','cluster C: 25%'))
plot1 <- ggplot()+
  # create flyway line legend
  geom_line(data=legend, mapping = aes(x = x1, y = y1,  colour = "Flyway"),lwd = 1.5 )+
  scale_colour_manual( name = "",labels = c("Atlantic Ocean Flyway"), values = c("red"))+
  # create contour filled legend
  geom_rect(data=legend, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black") +
  scale_fill_manual(breaks = c('cluster A: 5%','cluster A: 10%','cluster A: 25%','cluster B: 5%','cluster B: 10%','cluster B: 25%','cluster C: 5%','cluster C: 10%','cluster C: 25%'), 
                    name = "Contour", values = c("#b0d8c7", "#5bb191", "#117457", "#f5c1a8", "#e4824e", "#d95f02", "#c5c5e5", "#8e8cca", "#55518a"))+
  theme_bw(base_size = 16) # increase legend font size
  

legend1 <- cowplot::get_legend(plot1)

cowplot::plot_grid(plotAtl1, plotAtl2, plotAtl3, legend1, nrow = 1)

ggsave(plot = last_plot(), "./Figures/Manuscript figures/Atlantic cluster groups_Atlantic colonies only_smaller spacing_dpi600.jpg", dpi = 600, height = 6, width = 16)


# save the flyways shapefiles to separate location
st_write(AO.10.clust1.2.3,"./Global Flyways/Individual flyway shp files/Atlantic Ocean Flyway_search400_poly10_smoothed7.5_projected.shp", driver = "ESRI Shapefile" )

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


#### Southern Ocean - repeat for each  raster (5, 10, 25) ####

# first plot to check if any holes that need filling/ small crumbs to drop
ggplot()+
  # geom_sf(data = SOlaea_poly05.1, fill = "#BDD7E7", alpha = 0.9)+
  # geom_sf(data = SOlaea_poly10.1, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = SOlaea_poly25.1, fill = "#3182BD", alpha = 0.9)+
    ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  # coord_sf(xlim = c(-4000000, 6000000), ylim = c(-7000000, 7000000))+ # Atlantic Ocean projection limits
  theme_bw()


#SOUOC POLYGONS (3 clusters)
# poly05 = no blobs or holes
# poly10 = no crumbs, hole below Atlantic Ocean - fill units = 210,000 km^2
# pol25 = leave all crumbs, no holes

# below is code to drop crumbs and smooth - for each cluster and polygon size at a time
area_thresh <- units::set_units(10000000, km^2)
r_poly_dropped <- smoothr::drop_crumbs(SOlaea_poly10.1, threshold = area_thresh, drop_empty = TRUE) 
#only run next line if holes to fill
r_poly_filled <- smoothr::fill_holes(r_poly_dropped, units::set_units(210000, km^2))

ggplot()+
  # geom_sf(data = r_poly_dropped, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = r_poly_filled, fill = "#6BAED6", alpha = 0.9)+
  ##
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  theme_bw()


SAOlaea_poly05.1 <- SOlaea_poly05.1
SOlaea_poly10.1.drop <- r_poly_filled 
SOlaea_poly25.1 <- SOlaea_poly25.1 

# smooth 
smoothSOlaea_poly05.1 <- smoothr::smooth(SOlaea_poly05.1, method = "ksmooth", smoothness = 7.5)
smoothSOlaea_poly10.1 <- smoothr::smooth(SOlaea_poly10.1.drop, method = "ksmooth", smoothness = 7.5)
smoothSOlaea_poly25.1 <- smoothr::smooth(SOlaea_poly25.1, method = "ksmooth", smoothness = 7.5)

# plot smoothed and unsmoothed to compare
ggplot()+
  geom_sf(data = SOlaea_poly25.1, fill = "red", alpha = 0.9)+
  geom_sf(data = smoothSOlaea_poly25.1, fill= "orange", alpha = 0.7)+
  coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000))+# southern ocean projection
  geom_sf(data = land, fill = "gray60", alpha = 0.8)+
  
  theme_bw()


# save projected smoothed shapefile for each polygon
# latlon <- sf::st_transform(smoothSOlaea_poly05.1, crs = 4326)
plot(smoothSOlaea_poly05.1)
st_write(smoothSOlaea_poly05.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly05_smoothed7.5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly10.1, crs = 4326)
plot(smoothSOlaea_poly10.1)
st_write(smoothSOlaea_poly10.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly10_smoothed7.5_projected.shp", driver = "ESRI Shapefile")

# latlon <- sf::st_transform(smoothSOlaea_poly25.1, crs = 4326)
plot(smoothSOlaea_poly25.1)
st_write(smoothSOlaea_poly25.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly25_smoothed7.5_projected.shp", driver = "ESRI Shapefile")

# open smoothed polygons (if not running full script) - have saved 5 and 7.5 smoothed - check pathway
smoothSOlaea_poly05.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly05_smoothed7.5_projected.shp")
smoothSOlaea_poly10.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly10_smoothed7.5_projected.shp")
smoothSOlaea_poly25.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Southern/UPDATEDJan24/Noclusters_search400_poly25_smoothed7.5_projected.shp")

# check areas of flyway: 5, 10 and 25% contours
# cluster 1 / East Indian Ocean Flyway
units::set_units(st_area(smoothSOlaea_poly05.1), "km^2") # 75,123,564 [km^2]
units::set_units(st_area(smoothSOlaea_poly10.1), "km^2") # 57,420,408 [km^2]
units::set_units(st_area(smoothSOlaea_poly25.1), "km^2") # 19,839,910 [km^2]

# no clusters to union - straight to plots

# plot with SO projection and colonies
# select Southern Ocean colonies used in analysis only
colonySOSou <- colonySO %>% 
  filter(colony_name == "Bird Island, South Georgia" | colony_name == "Iles Crozet" | colony_name == "Iles Kerguelen" |
         colony_name == "Marion Island")

plotSou<- ggplot(data = SOc_laea) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothSOlaea_poly05.1, fill= "#b0d8c7")+
  geom_sf(data = smoothSOlaea_poly10.1, fill = "#5bb191")+
  geom_sf(data = smoothSOlaea_poly25.1, fill = "#117457")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = smoothSOlaea_poly10.1, fill = "NA", colour = "red", lwd = 1.5)+ # plot flyway outline
  geom_sf(data = SOc_laea, fill = "grey")+
  # add colonies
  # geom_sf(data = colonySO, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  # add southern ocean colonies used in analysis only
  geom_sf(data = colonySOSou, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
   coord_sf(xlim = c(-7000000, 7000000), ylim = c(-6000000, 6000000),
           label_axes = list(
    bottom = c("E"), top = c("E"),
    left = c("E"), right = c("E")
  ))+# southern ocean projection
  # coord_sf(xlim = c(-7500000, 3100000), ylim = c(-6800000, 7500000))+
  metR::scale_x_latitude(breaks = c(0, 30, 60, 90,120, 150, 180, -150, -120, -90, -60, -30), 
                         labels = c("0°", "", "60°E", "", "120°E", "", "180°", "", "120°W", "", "60°W", ""))+
  metR::scale_y_longitude(limits = c(-80,0), 
                          breaks = c(0, -10, -20,-30, -40, -50, -60, -70, -80))+
  theme_bw()+
  theme(axis.text=element_text(size=16), plot.title=element_text(size=20), 
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank()
        )


# create manual legend
legend = data.frame(x1=c(1,3,1), x2=c(2,4,3), y1=c(1,1,4), y2=c(2,2,5), 
                    t=c('5%','10%','25%'))
plot1 <- ggplot()+
  # create flyway line legend
  geom_line(data=legend, mapping = aes(x = x1, y = y1,  colour = "Flyway"),lwd = 1.5 )+
  scale_colour_manual( name = "",labels = c("Southern Ocean Flyway"), values = c("red"))+
  # create contour filled legend
  geom_rect(data=legend, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black") +
  scale_fill_manual(breaks = c('5%','10%','25%'), 
                    name = "Contour", values = c("#b0d8c7", "#5bb191", "#117457"))+
  theme_bw(base_size = 14)+ # increase legend font size
  guides(colour = guide_legend(order = 2), # change legend display order
         shape = guide_legend(order = 1))

legend1 <- cowplot::get_legend(plot1)

cowplot::plot_grid(plotSou, legend1, nrow = 1, rel_widths = c(3, 1))


ggsave(plot = last_plot(), "./Figures/Manuscript figures/Southern Ocean_SO colonies only_dpi300.png", dpi = 300, height = 5, width = 10)


# save the flyways shapefiles to separate location
st_write(smoothSOlaea_poly10.1,"./Global Flyways/Individual flyway shp files/Southern Ocean Flyway_search400_poly10_smoothed7.5_projected.shp", driver = "ESRI Shapefile" )
SouFlyway <- st_read("./Global Flyways/Individual flyway shp files/Southern Ocean Flyway_search400_poly10_smoothed7.5_projected.shp")


#### Pacific - repeat for each cluster (4) and raster (2.5, 5, 10) ####

# first plot to check if any holes that need filling/ small crumbs to drop
ggplot()+
  # geom_sf(data = POlaea_poly025.1, fill = "#BDD7E7", alpha = 0.9)+
  # geom_sf(data = POlaea_poly05.1, fill = "#6BAED6", alpha = 0.9)+
  # geom_sf(data = POlaea_poly10.1, fill = "#3182BD", alpha = 0.9)+
  # geom_sf(data = POlaea_poly025.2, fill = "#FDBE85", alpha = 0.7)+
  # geom_sf(data = POlaea_poly05.2, fill = "#FD8D3C", alpha = 0.7)+
  # geom_sf(data = POlaea_poly10.2, fill = "#E6550D", alpha = 0.7)+
  # geom_sf(data = POlaea_poly025.3, fill = "#BAE4B3", alpha = 0.7)+
  # geom_sf(data = POlaea_poly05.3, fill = "#74C476", alpha = 0.7)+
  # geom_sf(data = POlaea_poly10.3, fill = "#31A354", alpha = 0.7)+
  # geom_sf(data = POlaea_poly025.4, fill = "#FFB4CF", alpha = 0.7)+
  # geom_sf(data = POlaea_poly05.4, fill = "#FA8DB7", alpha = 0.7)+
  geom_sf(data = POlaea_poly10.4, fill = "#E7298A", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7600000, 7600000), ylim = c(-6600000, 6800000))+ # Pacific Ocean projection limits
  theme_bw()



# PACOC POLYGONS (4 clusters)
# poly025, clust 1 = 0 drop_crumbs big blob by coast of US, larger than 600,000 - leave there, fill holes60,000km^2
# poly05, clust 1 = no crumbs or holes - all very large areas
# pol10, clust 1 = drop_crumbs 270,000 km^2, no holes
# poly025, clust 2 = no blobs, holes fill 170,000 km^2
# poly05, clust 2 = no blobs, small hole fill 30,000 km^2
# poly10, clust 2 = no blobs, no holes
# poly025, clust 3 =  no blobs, no holes
# poly05, clust 3 = large blob - leave it there, no holes
# poly10, clust 3 = no blobs,  no holes
# poly025, clust 4 = no blobs, no holes
# poly05, clust 4 = no blobs , two holes - fill the smaller 30,000 km^2
# poly10, clust 4 = no blobs or holes

# below is code to drop crumbs and smooth - for each cluster and polygon size at a time
# only run the next two lines to drop blobs/crumbs
area_thresh <- units::set_units(300, km^2) # when no crumbs set threshold low at 50km^2
r_poly_dropped <- smoothr::drop_crumbs(POlaea_poly05.4, threshold = area_thresh, drop_empty = TRUE) #
#only run next line if holes to fill
# r_poly_filled <- smoothr::fill_holes(r_poly_dropped, units::set_units(30000, km^2))
r_poly_filled <- smoothr::fill_holes(POlaea_poly05.4, units::set_units(30000, km^2))


ggplot()+
  # geom_sf(data = r_poly_dropped, fill = "#6BAED6", alpha = 0.9)+
  geom_sf(data = r_poly_filled, fill = "red", alpha = 0.9)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7600000, 7600000), ylim = c(-6600000, 6800000))+ # Pacific Ocean projection limits
  theme_bw()


POlaea_poly025.1.drop <- r_poly_filled
POlaea_poly05.1 <- POlaea_poly05.1 
POlaea_poly10.1.drop <- r_poly_dropped 
POlaea_poly025.2.drop <- r_poly_filled
POlaea_poly05.2.drop <- r_poly_filled
POlaea_poly10.2 <- POlaea_poly10.2 
POlaea_poly025.3 <- POlaea_poly025.3
POlaea_poly05.3 <-  POlaea_poly05.3
POlaea_poly10.3 <- POlaea_poly10.3
POlaea_poly025.4 <- POlaea_poly025.4
POlaea_poly05.4.drop <- r_poly_filled
POlaea_poly10.4 <- POlaea_poly10.4


# smooth 
smoothPOlaea_poly025.1 <- smoothr::smooth(POlaea_poly025.1.drop, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly05.1 <- smoothr::smooth(POlaea_poly05.1, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly10.1 <- smoothr::smooth(POlaea_poly10.1.drop, method = "ksmooth", smoothness = 5)

smoothPOlaea_poly025.2 <- smoothr::smooth(POlaea_poly025.2.drop, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly05.2 <- smoothr::smooth(POlaea_poly05.2.drop, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly10.2 <- smoothr::smooth(POlaea_poly10.2, method = "ksmooth", smoothness = 5)

smoothPOlaea_poly025.3 <- smoothr::smooth(POlaea_poly025.3, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly05.3 <- smoothr::smooth(POlaea_poly05.3, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly10.3 <- smoothr::smooth(POlaea_poly10.3, method = "ksmooth", smoothness = 5)

smoothPOlaea_poly025.4 <- smoothr::smooth(POlaea_poly025.4, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly05.4 <- smoothr::smooth(POlaea_poly05.4.drop, method = "ksmooth", smoothness = 5)
smoothPOlaea_poly10.4 <- smoothr::smooth(POlaea_poly10.4, method = "ksmooth", smoothness = 5)


# plot smoothed and unsmoothed to compare
ggplot()+
  geom_sf(data = POlaea_poly10.4, fill = "red", alpha = 0.9)+
  geom_sf(data = smoothPOlaea_poly10.4, fill= "orange", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7600000, 7600000), ylim = c(-6600000, 6800000))+ # Pacific Ocean projection limits
  theme_bw()

# save smoothed shapefile for each polygon
st_write(smoothPOlaea_poly025.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly025_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly05.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly05_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly10.1, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly10_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")

st_write(smoothPOlaea_poly025.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly025_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly05.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly05_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly10.2, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly10_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")

st_write(smoothPOlaea_poly025.3, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly025_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly05.3, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly05_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly10.3, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly10_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")

st_write(smoothPOlaea_poly025.4, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly025_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly05.4, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly05_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")
st_write(smoothPOlaea_poly10.4, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly10_smoothed5_laeaprojected.shp", driver = "ESRI Shapefile")

# open smoothed polygons (if not running full script) - have saved 5 and 7.5 smoothed - check pathway
smoothPOlaea_poly025.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly025_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly05.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly05_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly10.1 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster1_search400_poly10_smoothed7.5_laeaprojected.shp")

smoothPOlaea_poly025.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly025_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly05.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly05_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly10.2 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster2_search400_poly10_smoothed7.5_laeaprojected.shp")

smoothPOlaea_poly025.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly025_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly05.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly05_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly10.3 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster3_search400_poly10_smoothed7.5_laeaprojected.shp")

smoothPOlaea_poly025.4 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly025_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly05.4 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly05_smoothed7.5_laeaprojected.shp")
smoothPOlaea_poly10.4 <- st_read("C:/Users/Joanne.Morten/OneDrive - BirdLife International/Flyways/Flyways Rproj/cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Cluster4_search400_poly10_smoothed7.5_laeaprojected.shp")


# union Pacific clusters 1, 2 and 3
PO.025.clust1.2 <- st_union(st_make_valid(smoothPOlaea_poly025.1), st_make_valid(smoothPOlaea_poly025.2))
PO.025.clust1.2.3 <- st_union(st_make_valid(PO.025.clust1.2), st_make_valid(smoothPOlaea_poly025.3))
PO.05.clust1.2 <- st_union(st_make_valid(smoothPOlaea_poly05.1), st_make_valid(smoothPOlaea_poly05.2))
PO.05.clust1.2.3 <- st_union(st_make_valid(PO.05.clust1.2), st_make_valid(smoothPOlaea_poly05.3))
PO.10.clust1.2 <- st_union(st_make_valid(smoothPOlaea_poly10.1), st_make_valid(smoothPOlaea_poly10.2))
PO.10.clust1.2.3 <- st_union(st_make_valid(PO.10.clust1.2), st_make_valid(smoothPOlaea_poly10.3))

# plot unioned smoothed polygons
ggplot()+
  geom_sf(data = PO.025.clust1.2.3, fill = "red", alpha = 0.9)+
  geom_sf(data = PO.05.clust1.2.3, fill= "orange", alpha = 0.7)+
  geom_sf(data = PO.10.clust1.2.3, fill= "yellow", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7600000, 7600000), ylim = c(-6600000, 6800000))+ # Pacific Ocean projection limits
  theme_bw()


# smooth again to close gap
xtrasmoothPOpoly025 <- PO.025.clust1.2.3 %>%  
  st_buffer(dist = 600000) %>% 
  st_buffer(dist = -600000)

ggplot()+
  # geom_sf(data = PO.025.clust1.2.3, fill = "green", alpha = 0.9)+
  geom_sf(data = xtrasmoothPOpoly025, fill = "red", alpha = 0.5)+
  geom_sf(data = PO.05.clust1.2.3, fill= "orange", alpha = 0.7)+
  geom_sf(data = PO.10.clust1.2.3, fill= "yellow", alpha = 0.7)+
  ##
  geom_sf(data = land, fill = "NA", alpha = 0.8)+
  coord_sf(xlim = c(-7600000, 7600000), ylim = c(-6600000, 6800000))+ # Pacific Ocean projection limits
  theme_bw()

# save flyway polygon 
st_write(xtrasmoothPOpoly025, "./cluster analysis/ArcGIS files_line density/Smoothed polygons/Pacific/UPDATEDJan24/Unioned Clusters_search400_poly025_smoothed7.5+extrasmooth_laeaprojected.shp", driver = "ESRI Shapefile")

# check areas of unioned 5, 10 and 25% contours
# Pacific Ocean Flyway
# units::set_units(st_area(PO.025.clust1.2.3), "km^2") # 98,932,347 [km^2]
units::set_units(st_area(xtrasmoothPOpoly025), "km^2") # 100,113,612 [km^2]
units::set_units(st_area(PO.05.clust1.2.3), "km^2") #  49,912,068 [km^2]
units::set_units(st_area(PO.10.clust1.2.3), "km^2") #  25,425,143 [km^2]

# West Pacific Ocean Flyway
units::set_units(st_area(smoothPOlaea_poly025.4), "km^2") # 52,032,889 [km^2]
units::set_units(st_area(smoothPOlaea_poly05.4), "km^2") # 44,604,416 [km^2]
units::set_units(st_area(smoothPOlaea_poly10.4), "km^2") # 30,362,293 [km^2]


# select colonies that were used in Pacific analysis only
colonyMollPac<- colonyMoll %>% 
  filter(colony_name == "Alpine, Alaska" | colony_name ==  "Antipodes Island"| colony_name ==   "Aorangi Island"| colony_name ==  "Bird Island, South Georgia"|
           colony_name == "Cabbage Tree Island"| colony_name ==  "Chatham Island"| colony_name ==  "Codfish Island"|  
           colony_name == "French Frigate Shoals" | colony_name =="Henderson Island"| colony_name == "Isla Alejandro Selkirk"| colony_name == "King George Island (Fildes Peninsula and Ardley Island)"| 
           colony_name ==  "Lake Hauroko/Petrel Island"| colony_name == "Little Barrier Island" |
           colony_name == "Mana Island" | colony_name == "Midway Atoll" | colony_name == "Mikura"|  colony_name == "Mokohinau"|
           colony_name ==  "New Caledonia" | colony_name == "North-East Island" | colony_name == "Proclamation Island"| colony_name == "Punakaiki" |  
           colony_name ==  "Red Mercury Island" | colony_name == "Sanganjima Island" | colony_name == "South East Island" | 
           colony_name ==  "Terre Ad<e9>lie" | colony_name ==  "The Forty-fours"  )

# plot with molleweide projection and colonies
plotPac1<- ggplot(data = PacOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothPOlaea_poly025.1, fill= "#b0d8c7")+
  geom_sf(data = smoothPOlaea_poly05.1, fill = "#5bb191")+
  geom_sf(data = smoothPOlaea_poly10.1, fill = "#117457")+
  # geom_sf(data = smoothPOlaea_poly05.4, fill = "NA", colour = "red", lwd = 1.5)+ # west pacific ocean flyway outline
  geom_sf(data = xtrasmoothPOpoly025, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = PacOc_moll, fill = "grey")+
  # add all colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  # add Pacific colonies only
  geom_sf(data = colonyMollPac, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  coord_sf(xlim = c(-5500000, 11000000), ylim = c(-7000000, 7300000))+ # Pacific Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        # axis.text.y = element_text(color = "white"),
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("A")

# cluster 2
plotPac2 <- ggplot(data = PacOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothPOlaea_poly025.2, fill= "#f5c1a8")+
  geom_sf(data = smoothPOlaea_poly05.2, fill = "#e4824e")+
  geom_sf(data = smoothPOlaea_poly10.2, fill = "#d95f02")+
  # geom_sf(data = smoothPOlaea_poly05.4, fill = "NA", colour = "red", lwd = 1.5)+ # west pacific ocean flyway outline
  geom_sf(data = xtrasmoothPOpoly025, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = PacOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  # add Pacific colonies only
  geom_sf(data = colonyMollPac, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  coord_sf(xlim = c(-5500000, 11000000), ylim = c(-7000000, 7300000))+ # Pacific Ocean projection limits
  theme_bw()+
  theme(axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("B")

# cluster 3
plotPac3<- ggplot(data = PacOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothPOlaea_poly025.3, fill= "#c5c5e5")+
  geom_sf(data = smoothPOlaea_poly05.3, fill = "#8e8cca")+
  geom_sf(data = smoothPOlaea_poly10.3, fill = "#55518a")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  # geom_sf(data = smoothPOlaea_poly05.4, fill = "NA", colour = "red", lwd = 1.5)+ # west pacific ocean flyway outline
  geom_sf(data = xtrasmoothPOpoly025, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = PacOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  # add Pacific colonies only
  geom_sf(data = colonyMollPac, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  coord_sf(xlim = c(-5500000, 11000000), ylim = c(-7000000, 7300000))+ # Pacific Ocean projection limits
  theme_bw()+
  theme(
    # axis.text.x = element_text(color="white"), # hide axis labels for panel plot
    #     axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("C")

# cluster 4
plotPac4<- ggplot(data = PacOc_moll) +
  geom_sf(fill = "red")+
  geom_sf(data = smoothPOlaea_poly025.4, fill= "#FFB4CF")+
  geom_sf(data = smoothPOlaea_poly05.4, fill = "#FA8DB7")+
  geom_sf(data = smoothPOlaea_poly10.4, fill = "#E7298A")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = smoothPOlaea_poly05.4, fill = "NA", colour = "red", lwd = 1.5)+
  # geom_sf(data = xtrasmoothPOpoly025, fill = "NA", colour = "red", lwd = 1.5)+ # acific Ocean flyway
  geom_sf(data = PacOc_moll, fill = "grey")+
  # add colonies
  # geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  # add Pacific colonies
  geom_sf(data = colonyMollPac, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
  coord_sf(xlim = c(-5500000, 11000000), ylim = c(-7000000, 7300000))+ # Pacific Ocean projection limits
  theme_bw()+
  theme(
    # axis.text.x = element_text(color="white"), # hide axis labels for panel plot
        axis.text.y = element_text(color = "white"), 
        axis.text=element_text(size=18), plot.title=element_text(size=20))+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  ggtitle("D")




# create manual legend
legend = data.frame(x1=c(1,3,1,5,4, 1,3,1,5, 2,3, 4), x2=c(2,4,3,6,6,2,4,3,6,2, 4, 5), y1=c(1,1,4,1,3, 1,1,4,1, 2, 3, 4), y2=c(2,2,5,3,5, 2,2,5,3, 4, 6, 7), 
                    t=c('cluster A: 2.5%','cluster A: 5%','cluster A: 10%','cluster B: 2.5%','cluster B: 5%','cluster B: 10%','cluster C: 2.5%','cluster C: 5%','cluster C: 10%', 'cluster D: 2.5%','cluster D: 5%','cluster D: 10%'))
plot1 <- ggplot()+
  # create flyway line legend
  geom_line(data=legend, mapping = aes(x = x1, y = y1,  colour = "Flyway"),lwd = 1.5 )+
  scale_colour_manual( name = "",labels = c("Pacific Ocean Flyways"), values = c("red"))+
  # create contour filled legend
  geom_rect(data=legend, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black") +
  scale_fill_manual(breaks = c('cluster A: 2.5%','cluster A: 5%','cluster A: 10%','cluster B: 2.5%','cluster B: 5%','cluster B: 10%','cluster C: 2.5%','cluster C: 5%','cluster C: 10%', 'cluster D: 2.5%','cluster D: 5%','cluster D: 10%'), 
                    name = "Contour", values = c("#b0d8c7", "#5bb191", "#117457", "#f5c1a8", "#e4824e", "#d95f02", "#c5c5e5", "#8e8cca", "#55518a", "#FFB4CF", "#FA8DB7", "#E7298A"))+
  theme_bw(base_size = 16) # increase legend font size


legend1 <- cowplot::get_legend(plot1)

# make blank legend with same dimensions to keep full facet plot aligned
plot2 <- ggplot()+
  # create flyway line legend
  geom_line(data=legend, mapping = aes(x = x1, y = y1,  colour = "Flyway"),lwd = 1.5 )+
  scale_colour_manual( name = "",labels = c("Pacific Ocean Flyways"), values = c("white"))+
  # create contour filled legend
  geom_rect(data=legend, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="white") +
  scale_fill_manual(breaks = c('cluster A: 2.5%','cluster A: 5%','cluster A: 10%','cluster B: 2.5%','cluster B: 5%','cluster B: 10%','cluster C: 2.5%','cluster C: 5%','cluster C: 10%', 'cluster D: 2.5%','cluster D: 5%','cluster D: 10%'), 
                    name = "Contour", values = rep("white", 12))+
  theme(
    # axis.text.x = element_text(color="white"), # hide axis labels for panel plot
    # axis.text.y = element_text(color = "white"), 
    legend.text = element_text(color= "white", size = 16), 
    legend.title = element_text(color = "white", size = 16))#+
  # theme_bw(base_size = 16) # increase legend font size


legend2 <- cowplot::get_legend(plot2)


bottomrow <- cowplot::plot_grid(plotPac3, plotPac4,legend2, nrow = 1, rel_widths = c(2,2,1))
toprow <- cowplot::plot_grid(plotPac1, plotPac2, legend1, nrow =1 , rel_widths = c(2, 2, 1))

cowplot::plot_grid(toprow, bottomrow, nrow = 2)
ggsave(plot= last_plot(), "./Figures/Manuscript figures/Pacific Ocean_to delete.jpg", dpi = 600, height = 10, width = 15)


# save the flyways shapefiles to separate location
st_write(xtrasmoothPOpoly025,"./Global Flyways/Individual flyway shp files/Pacific Ocean Flyway_search400_poly025_smoothed7.5_projected.shp", driver = "ESRI Shapefile" )
st_write(smoothPOlaea_poly05.4, "./Global Flyways/Individual flyway shp files/West Pacific Ocean Flyway_search400_poly5_smoothed7.5_projected.shp", driver = "ESRI Shapefile")
SouFlyway <- st_read("./Global Flyways/Individual flyway shp files/Southern Ocean Flyway_search400_poly10_smoothed7.5_projected.shp")


# how many species and colonies from NZ and surrounding region 

unique(MIG$common_name[which(MIG$colony_name == "New Caledonia"|
                             MIG$colony_name == "Lord Howe Island"|
                               MIG$colony_name == "Aorangi Island"|
                               MIG$colony_name ==  "Cabbage Tree Island" |
                               MIG$colony_name == "Mokohinau" | 
                               MIG$colony_name == "Little Barrier Island"| 
                               MIG$colony_name == "Red Mercury Island" |
                               MIG$colony_name == "Punakaiki" |
                               MIG$colony_name ==  "Mana Island" |
                               MIG$colony_name ==  "Antipodes Island"|
                               MIG$colony_name ==  "Proclamation Island"|
                               MIG$colony_name == "Lake Hauroko/Petrel Island" |
                               MIG$colony_name == "Codfish Island" |
                               MIG$colony_name == "North-East Island" |
                               MIG$colony_name == "Chatham Island" |
                               MIG$colony_name == "The Forty-fours" |
                               MIG$colony_name == "South East Island" )])

##### Plot of all flyways and colonies (MS Fig 1) ####

worldRobin <- st_transform(borders, crs = "+proj=robin") # basemap
colonyRobin <- st_transform(colonyGeom, crs = "+proj=robin") # all colonies

EIndRobin <- st_transform(smoothIOlaea_poly25.1, crs = "+proj=robin")
NIndRobin <- st_transform(smoothIOlaea_poly25.2, crs = "+proj=robin")
AtlRobin <- st_transform(AtlFlyway, crs = "+proj=robin")

SouFlyway <- st_read("./Global Flyways/Individual flyway shp files/Southern Ocean Flyway_search400_poly10_smoothed7.5_projected.shp")
st_crs(SouFlyway)  <- "epsg:6932"
SouRobin <- st_transform(SouFlyway, crs = 4326) %>% 
  st_wrap_dateline(options= c('WRAPDATELINE=YES'))

plot(SouRobin)
# reproject all flyways to atlantic centred molleweide

# INDIAN: molleweide centred at Indian Ocean
target_crs <- st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=0") # Global

# crossing dateline with projected polygons help: https://stackoverflow.com/questions/68278789/how-to-rotate-world-map-using-mollweide-projection-with-sf-rnaturalearth-ggplot
SouMoll <- SouFlyway %>%
  st_make_valid()

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world

offset <- 180 - 179 # Indian centred

polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(6932)

# modify world dataset to remove overlapping portions with world's polygons
Sou2 <- SouMoll %>% st_difference(polygon)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

# Transform
SouOc_moll <- Sou2 %>% st_transform(crs = target_crs)

ggplot(data = SouOc_moll) +
  geom_sf(fill = "red")+
  # add colonies
  geom_sf(data = colonyMoll, fill = "blue", colour = "black", shape = 22, size = 2, alpha = 0.8)+ 
  # geom_sf(data = polygon, fill = "blue") # shows centre line
  coord_sf(xlim = c(-5000000, 4500000), ylim = c(-5500000, 3000000))


ggplot(data = worldRobin) +
  geom_sf(fill = "red")+
  # geom_sf(data = smoothPOlaea_poly025.4, fill= "#FFB4CF")+
  # geom_sf(data = smoothPOlaea_poly05.4, fill = "#FA8DB7")+
  # geom_sf(data = smoothPOlaea_poly10.4, fill = "#E7298A")+
  # geom_sf(data = AO.05.clust1.2.3, fill = "NA", colour = "red", lwd = 1.5)+
  geom_sf(data = AtlRobin, fill = "NA", colour = "blue", lwd = 1.5)+
  geom_sf(data = EIndRobin, fill = "NA", colour = "blue", lwd = 1.5)+
  geom_sf(data = NIndRobin, fill = "NA", colour = "blue", lwd = 1.5)+
  # geom_sf(data = xtrasmoothPOpoly025, fill = "NA", colour = "red", lwd = 1.5)+ # acific Ocean flyway
  # geom_sf(data = PacOc_moll, fill = "grey")+
  # add colonies
  geom_sf(data = colonyMoll, fill = "black", colour = "black", shape = 22, size = 2, alpha = 0.8)+
 
  # coord_sf(xlim = c(-5500000, 11000000), ylim = c(-7000000, 7300000))+ # Pacific Ocean projection limits
  theme_bw()#+
  # theme(
  #   # axis.text.x = element_text(color="white"), # hide axis labels for panel plot
  #   axis.text.y = element_text(color = "white"), 
  #   axis.text=element_text(size=18), plot.title=element_text(size=20))+
  # theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  # ggtitle("D")