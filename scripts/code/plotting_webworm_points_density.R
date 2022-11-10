

#Part 0. Setting up R environment and packages

#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "raster", "tmap", "terra", "rgdal", "sf", "stars", "smoothr")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)




#Make tmap showing where the points fall on the map 

#Read in the growing zones map
zones_readin <- raster("maps/plant_hardiness_zones.tif")
zones_readin[zones_readin==0] = NA
#plot(zones_readin)

#zones_polygons <- terra::as.polygons(rast(zones_readin), 
                                    #trunc=TRUE, values=TRUE)
#zones_polygons_2 <- simplifyGeom(zones_polygons,
                                # tolerance=0.1, 
                                # preserveTopology=TRUE, makeValid=TRUE)
#plot(zones_polygons_2)
#zones_polygons <- st_as_sf(zones_polygons)
#zones_polygons_2 <- st_simplify(zones_polygons, dTolerance=1)
#back to terra vector
#zones_polygons_3 <- vect(zones_polygons_2)

#Save vectors
#writeVector(zones_polygons_2, "maps/vector_growing_zones.shp", overwrite=TRUE)

#Read in the vector dataset
#zones_polygons_readin <- vect("maps/vector_growing_zones.shp")
#zones_polygons_readin_2 <- simplifyGeom(zones_polygons_readin,
                               #  tolerance=10, preserveTopology=TRUE)
#remove the "crumbs"

#zones_polygons_readin_2 <- st_as_sf(zones_polygons_readin_2)
#remove broken polygons
#zones_polygons_filtered <- zones_polygons_readin_2 %>%
 # filter(!st_is_empty(.))

#area_thresh <- units::set_units(50, km^2)
#zones_polygons_readin_3 <- drop_crumbs(zones_polygons_filtered, threshold=area_thresh)
#plot(zones_polygons_readin_3)


#Add the fall webworm points

#read in fall webworm data
obs <- read.csv("mod_data/obs_zones.csv")

#Clean the data
#Create column with date of observation
obs$date <- as.Date(obs$observed_on, format="%Y-%m-%d")
#Create column with year
obs$year <-format(as.Date(obs$observed_on, format="%Y-%m-%d"), "%Y")
obs <- obs %>%
  dplyr::select(!X)
#Make more readable column names 
colnames(obs) <- c("id", "observed_on", "user_id", "latitude", "longitude", 
                   "colour_morph", "head_capsule", "growing_zone",  "date", "year")

#How many observations in each growing zone?
obs %>%
  group_by(growing_zone)%>%
  summarize(n=n())
#Only 4-9 contain a sufficient number of observations


#Cleaning the data: 
#Include observations with head capsule visible,
#Filter out observations from western North America
#only get dates after 2018
#only get observations with a red or black colour
#only get observations between latitudes 29-45
#Remove NA values
obs <- obs %>% dplyr::filter(head_capsule=="Yes", 
                             date>="2018-01-01",
                             date<="2020-12-31",
                             colour_morph %in% c("Black", "Red")) %>%
                             #growing_zone >=4)%>%
  na.omit()

#Make obs into points

points <- st_as_sf(x = obs, 
                   coords = c("longitude", "latitude"),
                   crs = crs(zones_readin))

points$colour_morph <- as.factor(points$colour_morph)

black_points <- points %>%
  filter(colour_morph=="Black")
red_points <- points %>%
  filter(colour_morph=="Red")

#Make density plot from points - red
red_points <- obs %>%
  filter(colour_morph=="Red")
#Turn into spatial points data frame
xy <- red_points %>%
  dplyr::select(longitude, latitude)

#Make blank target raster
target_rast <- rast(xmin=-130, xmax=-50, 
                    ymin=25, ymax=50, 
                    resolution=0.5, crs="EPSG:4326")

#Rasterize the red points
red_raster <- terra::rasterize(xy, raster(target_rast), fun="count", mask=FALSE)
sum_raster_red <-cellStats(red_raster, 'sum')
red_raster_props <- red_raster/sum_raster_red
plot(red_raster_props)


#Make density plot from points - black
black_points <- obs %>%
  filter(colour_morph=="Black")
#Turn into spatial points data frame
xy <- black_points %>%
  dplyr::select(longitude, latitude)

#Make blank target raster
target_rast <- rast(xmin=-130, xmax=-50, 
                    ymin=25, ymax=50, 
                    resolution=1, crs="EPSG:4326")

#Rasterize the black points
black_raster <- terra::rasterize(xy, raster(target_rast), fun="count", mask=FALSE)
sum_raster_black <-cellStats(black_raster, 'sum')
black_raster_props <- black_raster/sum_raster_black
plot(black_raster_props)





tmap_mode("plot")
zones_tmap_black <- tm_shape(zones_readin)+
  tm_raster(palette="-RdYlBu", n=10, alpha=0.5, style="cat", title=
              "Growing Zone", legend.show=TRUE)+
  tm_graticules(n.y=5, labels.size = 1, col="darkgrey", labels.show=c(FALSE, TRUE))+
  tm_shape(black_raster_props)+
  tm_raster(palette="Greys", alpha=1, style="cont", title=
              "Density of Black Morph Observations", legend.show=TRUE)+
  tm_legend(legend.outside=TRUE, legend.title.size=2, legend.text.size=1.5,
            legend.frame=TRUE)
zones_tmap_black


zones_tmap_red <- tm_shape(zones_readin)+
  tm_raster(palette="-RdYlBu", n=10, alpha=0.5, style="cat", title=
              "Growing Zone", legend.show=TRUE)+
  tm_graticules(n.y=5, labels.size = 1, col="darkgrey", labels.show=c(FALSE, TRUE))+
  tm_shape(red_raster_props)+
  tm_raster(palette="Reds", alpha=1, style="cont", title=
              "Density of Red Morph Observations", legend.show=TRUE)+
  tm_legend(legend.outside=TRUE, legend.title.size=2, legend.text.size=1.5,
            legend.frame=TRUE)
zones_tmap_red



tmap_save(arrange, "figures/colour_morph_distribution_density.pdf", units="in", width=11, height=8.5)

