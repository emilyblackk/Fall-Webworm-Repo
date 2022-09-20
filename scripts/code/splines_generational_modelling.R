#Emily Black
#20 Sept. 2022
#Modelling Fall Webworm Generations from 2018-2020 at different latitudes

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot")
install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 1. Reading and cleaning the data
obs <- read.csv("raw_data/051421_raw_data.csv")
head(obs)

#Create column with date of observation
obs$date <- as.Date(obs$observed_on, format="%Y-%m-%d")
#Create column with year
obs$year <-format(as.Date(obs$observed_on, format="%Y-%m-%d"), "%Y")
#Make more readable column names 
colnames(obs) <- c("id", "observed_on", "user_id", "latitude", "longitude", 
                   "colour_morph", "head_capsule", "date", "year")

#Cleaning the data: 
  #Include observations with head capsule visible,
  #Filter out observations from western North America
#only get dates after 2018
#Remove NA values
obs <- obs %>% dplyr::filter(head_capsule=="Yes", 
                             longitude>-100, 
                             date>="2018-01-01") %>%
  na.omit()

#Create latitude bands
obs <- obs %>%
  mutate(latitude_band = case_when(latitude >= 29 &  latitude<33 ~ "29-33", 
                   latitude >= 33 & latitude <37 ~ "33-37", 
                   latitude >= 37 & latitude < 41 ~ "37-41", 
                   latitude >= 41 & latitude < 45 ~ "41-45"))


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 2. Determining patterns in abundance across dates, colours, and latitudes
abundances <- obs %>%
  group_by(date, latitude_band, colour_morph) %>%
  summarize(worm_count = n())          


#Get relative abundance of worms on each date 
