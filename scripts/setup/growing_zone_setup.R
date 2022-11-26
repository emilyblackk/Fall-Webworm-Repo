#Emily Black
#26 Sept. 2022
#Assigning growing zones to fall webworm locations

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Setting up R environment and packages

#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "raster", "tmap", "terra", "rgdal", "sf", "stars")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)
#Part 1. Read in and clean data

#NOTE: This is for growing zone setup. Uncomment this code if you 
#need to create the growing zone map again



#Read in the US growing zone raster
#From https://prism.oregonstate.edu/projects/plant_hardiness_zones.php 
# us_zones <- read_sf(dsn="maps/plant_hardiness_zones/phm_us_shp/phm_us_shp.shp")
# crs(us_zones)
# 
# #create column with only zone
# us_zones <- us_zones %>%
#   mutate(zone = sub("\\D*(\\d+).*", "\\1", ZONE))
# #code from: https://stackoverflow.com/questions/23323321/r-extract-first-number-from-string
# 
# #target Raster
# target_rast <- rast(xmin=-130, xmax=-50, 
#                     ymin=25, ymax=50, 
#                     resolution=0.01, crs=crs(us_zones))
# us_zones_raster <- terra::rasterize(us_zones, target_rast, field="zone")
# plot(us_zones_raster)
# 
# #Read in canada temperature map
# canada_temp <- raster("maps/plant_hardiness_zones/canada_tmin.tif")
# 
# #convert canada temps to growing zones
# zone1 <- canada_temp$canada_tmin >-51.1
# zone2 <- canada_temp$canada_tmin > -45.6
# zone3 <- canada_temp$canada_tmin > -40
# zone4 <- canada_temp$canada_tmin > -34.4
# zone5 <- canada_temp$canada_tmin > -28.9
# zone6 <- canada_temp$canada_tmin > -23.3
# zone7 <- canada_temp$canada_tmin > -17.8
# zone8 <- canada_temp$canada_tmin > -12.2
# zone9 <- canada_temp$canada_tmin > -6.7
# zone10 <- canada_temp$canada_tmin > -1.1
# 
# 
# overlay_canada <- rast(mosaic(zone1, zone2, zone3, zone4, zone5, 
#                          zone6, zone7, zone8, zone9,zone10,
#                          fun=sum))
# plot(overlay_canada)
# 
# rm(zone1, zone2, zone3, zone4, zone5, 
#    zone6, zone7, zone8, zone9,zone10)
# 
# canada_zones <- resample(overlay_canada, target_rast, method="near")
# canada_zones[canada_zones==0] = NA
# plot(canada_zones)
# 
# zones <- merge(canada_zones, us_zones_raster)
# plot(zones)
# 
# #Export as raster
# writeRaster(zones, "maps/plant_hardiness_zones.tif")


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 2. Read in the new raster

#read in the raster
zones_readin <- raster("maps/plant_hardiness_zones.tif")
zones_readin[zones_readin==0] = NA


#PLot the raster
tmap_mode("plot")
zones_tmap <- tm_shape(zones_readin, bbox=extent(zones_readin))+
  tm_graticules()+
  tm_raster(style="cat", palette="-RdBu", legend.show=TRUE, title="Growing zones")+
  tm_layout(legend.text.size = 3, legend.title.size=4, legend.outside = TRUE)
zones_tmap
#tmap_save(zones_tmap, "figures/growing_zones_map.pdf")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Assign growing zones to the data



#Assign zones to fall webworm observations
obs <- read.csv("raw_data/110422_raw_data.csv")
head(obs)

coordinates <- obs %>%
  dplyr::select(longitude, latitude)
extract <- terra::extract(zones_readin, coordinates)
obs_zones <- cbind(obs, extract)
colnames(obs_zones) <- c("id", "observed_on", "user_id", "latitude", "longitude", 
                         "field.colour", "field.head.capsule.visible", "growing_zone")

points <- st_as_sf(x = obs_zones, 
                        coords = c("longitude", "latitude"),
                        crs = crs(zones_readin))
plot(points)

write.csv(obs_zones, "mod_data/obs_zones.csv")

