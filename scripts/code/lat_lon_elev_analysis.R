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
          "Matching", "gridExtra", "elevatr", "ggpubr", "envalysis")
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
                             year<=2020, 
                             longitude>-100)%>%
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
  summarize(mean = mean(latitude),
    var = var(latitude), 
            sd = sd(latitude))
#Similar

#Longitude check
qqnorm(obs$longitude, pch = 1, frame = FALSE)
qqline(obs$longitude, col = "steelblue", lwd = 2)
#There is a steep dropoff in the western population, but the rest is roughly normal
#Check variance
obs %>%
  group_by(colour_morph) %>%
  summarize(mean = mean(longitude),
            var = var(longitude), 
            sd = sd(longitude))
#Red has a larger variance, but not by a factor greater than 2

#Growing zone check
qqnorm(obs$growing_zone, pch = 1, frame = FALSE)
qqline(obs$growing_zone, col = "steelblue", lwd = 2)
#I would say roughly normal
#Check the variances
obs %>%
  group_by(colour_morph) %>%
  summarize(var = var(growing_zone))

#Get a summary table for latitude
sum.lat <- obs %>%
  group_by(colour_morph, year) %>%
  summarize(
    mean_lat= mean(latitude), 
            max_lat = max(latitude), 
            min_lat = min(latitude), 
            sd_lat = sd(latitude))
sum.lat

#Compare latitude year-over-year
mean.lat.table <- sum.lat %>%
  group_by(colour_morph) %>%
  summarize(mean = mean(mean_lat), sd = mean(sd_lat))
mean.lat.table

red.sum <- sum.lat %>%
  filter(colour_morph=="Red")
black.sum <- sum.lat %>%
  filter(colour_morph=="Black")
lat.stats <- black.sum$mean_lat - red.sum$mean_lat
sd(lat.stats)
mean(lat.stats)
#Perform ANOVA for our three groups for latitude
lat.aov <- aov(latitude ~ colour_morph+ colour_morph:as.character(year), data = obs)
summary(lat.aov)
TukeyHSD(lat.aov)
#Significant effect of colour morph and year, and interaction between colour morph
#and year


#plot the data
lat_plot <- obs %>%
  ggplot(aes(x=as.factor(year), y=latitude, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph))+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  ylim(25, 60)+
  guides(colour="none")+
  labs(x="Year", y="Latitude (°N)", fill="Colour morph") + 
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position = c(0.90, 0.85))
lat_plot


#Perform ANOVA for our three groups for longitude
#Get a summary table for latitude
sum.lon <- obs %>%
  group_by(colour_morph, year) %>%
  summarize(mean_lon= mean(longitude), 
            max_lon = max(longitude), 
            min_lon = min(longitude), 
            sd_lon = sd(longitude))
sum.lon

#Compare lonitude year-over-year
mean.lon.table <- sum.lon %>%
  group_by(colour_morph) %>%
  summarize(mean = mean(mean_lon), sd = mean(sd_lon))
mean.lon.table

red.sum <- sum.lon %>%
  filter(colour_morph=="Red")
black.sum <- sum.lon %>%
  filter(colour_morph=="Black")
lon.stats <- black.sum$mean_lon - red.sum$mean_lon
sd(lon.stats)
mean(lon.stats)


lon.aov <- aov(longitude ~ colour_morph + colour_morph:as.character(year), data = obs)
summary(lon.aov)
TukeyHSD(lon.aov)
#Significant effect of colour morph and year, and interaction between colour morph
#and year


#plot the data
lon_plot <- obs %>%
  ggplot(aes(x=as.factor(year), y=longitude, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph)
              , show.legend = FALSE)+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Longitude (°W)", fill="Colour morph") + 
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
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
# #reorganize the dataset so coordinates are at the front
# obs_reorg <- obs %>%
#   dplyr::select(longitude, latitude, everything())
# head(obs_reorg)
# 
# 
# #Use the get_elev_point function to get elevations
# prj_dd <- "EPSG:4326"
# obs_elev <- get_elev_point(obs_reorg, prj = prj_dd, src = "epqs")
# obs_elev_df <- data.frame(obs_elev)
# 
# #Turn all negative values to 0
# obs_elev_df$elevation <-  ifelse(obs_elev_df$elevation < 0, 0, obs_elev_df$elevation)
# 
# #Check to see if data meets assumptions
# #Growing zone check
# qqnorm(obs_elev_df$elevation, pch = 1, frame = FALSE)
# qqline(obs_elev_df$elevation, col = "steelblue", lwd = 2)
# hist(obs_elev_df$elevation)
# 
# 
# #make a transformation?
# qqnorm(sqrt(obs_elev_df$elevation), pch = 1, frame = FALSE)
# #that's better
# obs_elev_df <- obs_elev_df %>%
#   dplyr::mutate(sqrt_elev = sqrt(abs(elevation)))%>%
#   na.omit()
# 
# #Check the variances
# obs_elev_df %>%
#   group_by(colour_morph) %>%
#   summarize(var = var(sqrt_elev))
# #Very similar

#Read in completed data

obs_elev_df <- read.csv("mod_data/observations_with_elevation.csv")
##Perform ANOVA for our three groups for growing zone
elev.aov <- aov(elevation ~ colour_morph + colour_morph:as.character(year), data = obs_elev_df)
summary(elev.aov)
TukeyHSD(elev.aov)
#Significant effect of colour morph and year

#Get a summary table for elevation
sum.elev <- obs_elev_df %>%
  group_by(colour_morph, year) %>%
  summarize(mean_elev= mean(elevation), 
                           max_elev = max(elevation), 
                           min_elev = min(elevation), 
                           sd_elev = sd(elevation))
sum.elev

red.sum <- sum.elev %>%
  filter(colour_morph=="Red")
black.sum <- sum.elev %>%
  filter(colour_morph=="Black")
elev.stats <- black.sum$mean_elev - red.sum$mean_elev
sd(elev.stats)
mean(elev.stats)
# read in

obs_elev_df_2 <- obs_elev_df %>%
  filter(coords.x1 > -100
         )

table(obs_elev_df_2$colour_morph)

#plot the data


elev_plot <- obs_elev_df_2 %>%
  ggplot(aes(x=as.factor(year), y=elevation, colour=colour_morph)) + 
  geom_violin(trim=TRUE, adjust = .5, aes(fill = colour_morph, colour=colour_morph), 
              show.legend=FALSE)+
  scale_fill_manual(values = c("black", "#9C0260"))+
  scale_colour_manual(values = c("black", "#9C0260"))+
  guides(colour="none")+
  labs(x="Year", y="Elevation (m)", fill="Colour 
morph") +
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14))
elev_plot



elev.aov <- aov(elevation ~ colour_morph*year*coords.x2, data = obs_elev_df_2)
summary(elev.aov)
my.aov2 <- stepAIC(elev.aov)

combined_plot <- grid.arrange(lat_plot, lon_plot, elev_plot, ncol=1)
combined_plot_2 <- plot_grid(lat_plot, lon_plot, elev_plot ,ncol = 1, align = "v", 
                             rel_heights=c(1,1,1.2))
combined_plot_2

ggsave("figures/lat_lon_elev_distribution.pdf", combined_plot_2, 
       width=2500, height=1800, units=c("px"))
#ggsave("figures/lat_lon_elev_distribution.svg", combined_plot_2, 
      # width=2500, height=1800, units=c("px"))

#Save the elevation data
#write.csv(obs_elev_df, "mod_data/observations_with_elevation.csv")



