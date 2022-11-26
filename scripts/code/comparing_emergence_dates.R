#Nov 23
#Date Analysis 
#Let's do this thing

#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra", "elevatr", "envalysis")
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

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Get earliest date for each group 
#filter obs to just our growing zones of interest
obs <- obs %>%
  filter(growing_zone >=5, 
         growing_zone <=9)
dates_table <- obs %>%
  group_by(colour_morph, year, growing_zone) %>%
  summarize(earliest_date = min(date), 
            latest_date = max(date))
dates_table

#get difference between dates
black_dates <- dates_table %>%
  filter(colour_morph=="Black")
red_dates <- dates_table %>%
  filter(colour_morph=="Red")
black_dates$earliest_diff_from_red <- as.numeric(difftime(black_dates$earliest_date, 
                                                 red_dates$earliest_date, 
                                               units="days"))

black_dates$latest_diff_from_red <- as.numeric(difftime(black_dates$latest_date, 
                                               red_dates$latest_date, 
                                               units="days"))

#Perform average and statistical tests
dates_summary <- black_dates %>%
  ungroup() %>%
  summarize(mean_earliest = mean(earliest_diff_from_red), 
            mean_latest = mean(latest_diff_from_red), 
            sd_earliest = sd(earliest_diff_from_red), 
            sd_latest = sd(latest_diff_from_red))
dates_summary

#Test for significance of difference
wilcox.test(black_dates$earliest_diff_from_red, mu = 0, alternative = "two.sided")
wilcox.test(black_dates$latest_diff_from_red, mu = 0, alternative = "two.sided")


#test effect of growing zone
earliest_lm <- lm(earliest_diff_from_red ~ as.numeric(growing_zone)+ as.character(year), data=black_dates)
anova(earliest_lm)
#plot to check
black_dates %>%
ggplot(aes(x=as.numeric(growing_zone), y=earliest_diff_from_red))+
  geom_point()

latest_lm <- lm(latest_diff_from_red ~ as.numeric(growing_zone)+
                  as.character(year), data=black_dates)
latest_aov <- anova(latest_lm)
coef(latest_lm)
poly_lm <- lm(latest_diff_from_red ~poly(growing_zone, 2), data=black_dates)
anova(poly_lm)



#plot to check

#Use colourblind friendly palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


black_dates %>%
  ggplot(aes(x=as.numeric(growing_zone), y=latest_diff_from_red, colour=as.character(year)))+
  geom_point()+
  stat_smooth(method='lm', formula = y~poly(x,2), se=FALSE)+
  scale_colour_manual(values=cbPalette)+
  labs(x="Growing Zone", y="Difference in final observation date", colour="Year")+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14))
  