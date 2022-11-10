#Elevation, Latitude, Longitude, and Growing Zone plots
#Emily Black
#25 October 2022
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra", "elevatr")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 1. Reading and cleaning the data
obs <- read.csv("mod_data/obs_zones.csv")
head(obs)

#Create column with date of observation
obs$date <- as.Date(obs$observed_on, format="%Y-%m-%d")
#Create column with year
obs$year <-format(as.Date(obs$observed_on, format="%Y-%m-%d"), "%Y")
obs$year <- as.numeric(obs$year)
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
table(obs$year, obs$growing_zone)



#Cleaning the data: 
#Include observations with head capsule visible,
#Filter out observations from western North America
#only get dates after 2018
#only get observations with a red or black colour
#only get observations between latitudes 29-45
#Remove NA values
obs <- obs %>% dplyr::filter(head_capsule=="Yes", 
                             date>="2018-01-01", 
                             colour_morph %in% c("Black", "Red"), 
                             year<=2020)%>%
  na.omit()


#How many observations in each year and growing zone?
table(obs$year, obs$growing_zone, obs$colour_morph)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 2. Analyzing latitude, longitude, and growing zone distributions

#check that assumptions are met in the data

#Latitude check
qqnorm(obs$latitude, pch = 1, frame = FALSE)
qqline(obs$latitude, col = "steelblue", lwd = 2)
#all points are falling roughly on the line
#Check variance
obs %>%
group_by(colour_morph) %>%
  summarize(var = var(latitude))
#Similar

#Longitude check
qqnorm(obs$longitude, pch = 1, frame = FALSE)
qqline(obs$longitude, col = "steelblue", lwd = 2)
#There is a steep dropoff in the western population, but the rest is roughly normal
#Check variance
obs %>%
  group_by(colour_morph) %>%
  summarize(var = var(longitude))
#Red has a larger variance, but not by a factor greater than 2

#Growing zone check
qqnorm(obs$growing_zone, pch = 1, frame = FALSE)
qqline(obs$growing_zone, col = "steelblue", lwd = 2)
#I would say roughly normal
#Check the variances
obs %>%
  group_by(colour_morph) %>%
  summarize(var = var(growing_zone))
#Very similar


#Get a summary table for latitude
sum.lat <- obs %>%
  group_by(colour_morph, year) %>%
  summarize(mean_lat= mean(latitude, 
            max_lat = max(latitude), 
            min_lat = min(latitude), 
            sd_lat = sd(latitude)))
sum.lat
#Perform ANOVA for our three groups for latitude
lat.aov <- aov(latitude ~ colour_morph*year, data = obs)
summary(lat.aov)
#Significant effect of colour morph and year, and interaction between colour morph
#and year


#plot the data
lat_plot <- obs %>%
  ggplot(aes(x=as.factor(year), y=latitude, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph))+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Latitude", fill="Colour morph") + 
  theme_classic()+
  theme(axis.text = element_text(size=12), 
        axis.title = element_text(size=16),
        legend.title=element_text(size=16), 
        legend.text = element_text(size=12))
lat_plot


#Perform ANOVA for our three groups for longitude
lon.aov <- aov(longitude ~ colour_morph*year, data = obs)
summary(lon.aov)
#Significant effect of colour morph and year, and interaction between colour morph
#and year


#plot the data
lon_plot <- obs %>%
  ggplot(aes(x=as.factor(year), y=longitude, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph))+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Longitude", fill="Colour morph") + 
  theme_classic()+
  theme(axis.text = element_text(size=12), 
        axis.title = element_text(size=16),
        legend.title=element_text(size=16), 
        legend.text = element_text(size=12))
lon_plot


#Perform ANOVA for our three groups for growing zone
zone.aov <- aov(growing_zone ~ colour_morph*year, data = obs)
summary(zone.aov)
#Significant effect of colour morph and year, and interaction between colour morph
#and year


#plot the data
zone_plot <- obs %>%
  ggplot(aes(x=as.factor(year), y=growing_zone, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph))+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Growing Zone", fill="Colour morph") + 
  theme_classic()
zone_plot


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 3. Analyzing elevation
#reorganize the dataset so coordinates are at the front
obs_reorg <- obs %>%
  dplyr::select(longitude, latitude, everything())
head(obs_reorg)


#Use the get_elev_point function to get elevations
prj_dd <- "EPSG:4326"
obs_elev <- get_elev_point(obs_reorg, prj = prj_dd, src = "epqs")
obs_elev_df <- data.frame(obs_elev)

#Turn all negative values to 0
obs_elev_df$elevation <-  ifelse(obs_elev_df$elevation < 0, 0, obs_elev_df$elevation)

#Check to see if data meets assumptions
#Growing zone check
qqnorm(obs_elev_df$elevation, pch = 1, frame = FALSE)
qqline(obs_elev_df$elevation, col = "steelblue", lwd = 2)
hist(obs_elev_df$elevation)


#make a transformation?
qqnorm(sqrt(obs_elev_df$elevation), pch = 1, frame = FALSE)
#that's better
obs_elev_df <- obs_elev_df %>%
  dplyr::mutate(sqrt_elev = sqrt(abs(elevation)))%>%
  na.omit()

#Check the variances
obs_elev_df %>%
  group_by(colour_morph) %>%
  summarize(var = var(sqrt_elev))
#Very similar

##Perform ANOVA for our three groups for growing zone
elev.aov <- aov(elevation ~ colour_morph*year, data = obs_elev_df)
summary(elev.aov)
#Significant effect of colour morph and year

#Get a summary table for elevation
sum.elev <- obs_elev_df %>%
  group_by(colour_morph, year) %>%
  summarize(mean_elev= mean(elevation), 
                           max_elev = max(elevation), 
                           min_elev = min(elevation), 
                           sd_elev = sd(elevation))
sum.elev

# read in
obs_elev_df <- read.csv("mod_data/observations_with_elevation.csv")

obs_elev_df_2 <- obs_elev_df %>%
  filter(coords.x1 < I(-100)
         )

table(obs_elev_df_2$colour_morph)

#plot the data
elev_plot <- obs_elev_df_2 %>%
  ggplot(aes(x=as.factor(year), y=elevation, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph))+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Elevation (m)", fill="Colour morph") + 
  theme_classic()+
  theme(axis.text = element_text(size=12), 
        axis.title = element_text(size=16),
        legend.title=element_text(size=16), 
        legend.text = element_text(size=12))
elev_plot

elev.aov <- aov(elevation ~ colour_morph*year*coords.x2, data = obs_elev_df_2)
summary(elev.aov)
my.aov2 <- stepAIC(elev.aov)

combined_plot <- grid.arrange(lat_plot, lon_plot, elev_plot, ncol=1)
ggsave("figures/lat_lon_elev_distribution.pdf", combined_plot, 
       width=11, height=8.5, units=c("in"))

#Save the elevation data
write.csv(obs_elev_df, "mod_data/observations_with_elevation.csv")

