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
#only get observations with a red or black colour
#only get observations between latitudes 29-45
#Remove NA values
obs <- obs %>% dplyr::filter(head_capsule=="Yes", 
                             longitude>-100, 
                             date>="2018-01-01", 
                             colour_morph %in% c("Black", "Red"), 
                             latitude>=29, 
                             latitude <=45)%>%
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
#Bring back year column
abundances$year <-format(as.Date(abundances$date, format="%Y-%m-%d"), "%Y")
#Add week column
abundances$week <- format(abundances$date, format="%W/%Y")

#generate summary table of webworm abundances per latitude band, year, and colour
summary_table_weeks <- abundances %>%
  group_by(colour_morph, latitude_band, week) %>%
  summarize(week_count = n())
summary_table_weeks$year <-format(as.Date(summary_table_weeks$week, format="%W/%Y"), "%Y")


#generate summary table of webworm abundances per latitude band, year, and colour
summary_table_year <- abundances %>%
  group_by(colour_morph, latitude_band, year) %>%
  summarize(total_count = n())

#Use left-join to add total counts to each observation
abundances_2 <- 
  left_join(summary_table_weeks, summary_table_year,
            by=c("year", "latitude_band", "colour_morph"))

#Get relative abundance on each date
abundances_2 <- abundances_2 %>%
  mutate(rel_count=week_count/total_count)
#Add date column for plotting


#Part 3. Plot the Data
plot <- abundances_2 %>%
  filter(year==2020) %>%
  ggplot(aes(x=week, y=rel_count))+
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5)+
  facet_wrap(vars(latitude_band), nrow=4) +
  ylim(0, 0.3)+
  scale_color_manual(values = c("black", "#9C0260"))+
  labs(x="Date", y="Observations")+
  scale_linetype_manual(values=c("solid", "solid"))+
  geom_smooth(aes(colour=colour_morph, linetype=colour_morph), method="loess", 
              se=TRUE, fill="#34595E", span=0.25, lwd=2.5)+
  theme_classic()
plot