#Emily Black
#20 Sept. 2022
#Modelling Fall Webworm Generations from 2018-2020 at different latitudes

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra")
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
                             growing_zone >=4, 
                             longitude>-100, 
                             year<=2020)%>%
  na.omit()

#Create latitude bands
#obs <- obs %>%
  #mutate(latitude_band = case_when(latitude >= 29 &  latitude<33 ~ "29-33", 
                   #latitude >= 33 & latitude <37 ~ "33-37", 
                   #latitude >= 37 & latitude < 41 ~ "37-41", 
                   #latitude >= 41 & latitude < 45 ~ "41-45"))

#How many observations in each year and growing zone?
table(obs$year, obs$growing_zone, obs$colour_morph)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 2. Determining patterns in abundance across dates, colours, and growing zone
abundances <- obs %>%
<<<<<<< HEAD
  group_by(date,  colour_morph, growing_zone) %>%
  summarize(worm_count = n())   
abundances$growing_zone <- as.character(abundances$growing_zone)


#We have dates where no worms were observed, however our sum function is not counting them
#Create tables with all possible dates, colour, and latitude band
dates_table <- function(x, y) {
 date_seq <- data.frame(seq(ymd("2018-01-01"), ymd("2020-12-31"), by="day"))
 colnames(date_seq) <- c("date")
 colour <- x
<<<<<<< HEAD
 growing_zone <- y
 date_table <- cbind(date = date_seq, colour_morph = colour, growing_zone =growing_zone)
}

#set up tables for black webworm
table_black_4 <- dates_table("Black", "4")
table_black_5 <- dates_table("Black", "5")
table_black_6 <- dates_table("Black", "6")
table_black_7 <- dates_table("Black", "7")
table_black_8 <- dates_table("Black", "8")
table_black_9 <- dates_table("Black", "9")

#Set up tables for red webworms
table_red_4 <- dates_table("Red", "4")
table_red_5 <- dates_table("Red", "5")
table_red_6 <- dates_table("Red", "6")
table_red_7 <- dates_table("Red", "7")
table_red_8 <- dates_table("Red", "8")
table_red_9 <- dates_table("Red", "9")

#Bind all these columns together 
date_list_table <- rbind(table_black_4,table_black_5, 
                         table_black_6, table_black_7, 
                         table_black_8, table_black_9, 
                         table_red_4,table_red_5, 
                         table_red_6, table_red_7, 
                         table_red_8, table_red_9)
#Clean the workspace
rm(table_black_4,table_black_5, 
   table_black_6, table_black_7, 
   table_black_8, table_black_9, 
   table_red_4,table_red_5, 
   table_red_6, table_red_7, 
   table_red_8, table_red_9)

=======
 lat_band <- y
 date_table <- cbind(date = date_seq, colour_morph = colour, latitude_band =lat_band)
}

#set up tables for black webworm
table_black_29 <- dates_table("Black", "29-33")
table_black_33 <- dates_table("Black", "33-37")
table_black_37 <- dates_table("Black", "37-41")
table_black_41 <- dates_table("Black", "41-45")
#Set up tables for red webworms
table_red_29 <- dates_table("Red", "29-33")
table_red_33 <- dates_table("Red", "33-37")
table_red_37 <- dates_table("Red", "37-41")
table_red_41 <- dates_table("Red", "41-45")

#Bind all these columns together 
date_list_table <- rbind(table_black_29,table_black_33, 
                         table_black_37, table_black_41, 
                         table_red_29,table_red_33, 
                         table_red_37, table_red_41)
#Clean the workspace
rm(table_black_29,table_black_33, 
   table_black_37, table_black_41, 
   table_red_29,table_red_33, 
   table_red_37, table_red_41)
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220

#Join the abundance table with the date list table to get full set of observations
abundances_complete <- left_join(date_list_table, abundances) 
#Replace NA with 0
  abundances_complete[is.na(abundances_complete)] <- 0




#Get relative abundance of worms on each date 
#Bring back year column
abundances_complete$year <-format(as.Date(abundances_complete$date, format="%Y-%m-%d"), "%Y")
#Add week  and month column
abundances_complete$week <- lubridate::week(abundances_complete$date)
<<<<<<< HEAD
#abundances_complete$month <- lubridate::month(abundances_complete$date)
=======
abundances_complete$month <- lubridate::month(abundances_complete$date)
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220


#generate summary table of webworm abundances_complete per latitude band, year, and colour
summary_table_weeks <- abundances_complete %>%
<<<<<<< HEAD
  group_by(colour_morph, growing_zone, year, week) %>%
=======
  group_by(colour_morph, latitude_band, year, month, week) %>%
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220
  summarize(week_count = sum(worm_count))


#Use left-join to add total counts to each observation
#get relative counts
#scale red and black data so peaks can appear
abundances_2 <- summary_table_weeks %>%
<<<<<<< HEAD
group_by(colour_morph, growing_zone, year) %>%
  mutate(scaled_count = 
           rescale(week_count, to = c(0, 1), from = range(week_count)))
#Add date column for plotting
abundances_2$date <- lubridate::ymd("2018-01-01") + lubridate::weeks(abundances_2$week - 1) + 
  lubridate::years(as.numeric(abundances_2$year) - 2018)
=======
group_by(colour_morph, latitude_band, year) %>%
  mutate(scaled_count = 
           rescale(week_count, to = c(0, 1), from = range(week_count)))
#Add date column for plotting
abundances_2$date <- as.Date(make_datetime(year=abundances_2$year)+
weeks((abundances_2$week-1)))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

library(stats)
#Part 3. Identify local maxima for ggplot
abundances_2 <- abundances_2 %>% 
  group_by(colour_morph, latitude_band, year) %>%
  mutate(smooth_y = predict(loess(scaled_count~as.numeric(date), span=0.1), 
                            newdata=as.numeric(date)))
identify_maxima <- function(x, y) {
#identify maxima
#From https://stackoverflow.com/questions/65442224/find-all-local-maxima-of-a-geom-smooth-curve-in-r-ggplot
# Run length encode the sign of difference
rle <- rle(diff(as.vector(x)) > 0)
# Calculate startpoints of runs
starts <- cumsum(rle$lengths) - rle$lengths + 1
# Take the points where the rle is FALSE (so difference goes from positive to negative) 
maxima_id <- starts[!rle$values]
maximum <- y[maxima_id]
}
maxima_black_29 <- abundances_2 %>%
  filter(colour_morph == "Black", latitude_band == "41-45")
 maxima_black_29 <-  identify_maxima (maxima_black_29$smooth_y, 
                                      as.numeric(maxima_black_29$date))
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220

#Check number of observations in each group
summary_table <- abundances_2 %>%
  group_by (year, growing_zone, colour_morph) %>%
  summarize(n=sum(week_count))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

library(stats)
#Part 3. Identify local maxima for ggplot
#abundances_2 <- abundances_2 %>% 
  #group_by(colour_morph, growing_zone, year) %>%
  #mutate(smooth_y = predict(loess(scaled_count~as.numeric(date), span=0.1), 
                        #    newdata=as.numeric(date)))
#identify_maxima <- function(x, y) {
#identify maxima
#From https://stackoverflow.com/questions/65442224/find-all-local-maxima-of-a-geom-smooth-curve-in-r-ggplot
# Run length encode the sign of difference
#rle <- rle(diff(as.vector(x)) > 0)
# Calculate startpoints of runs
#starts <- cumsum(rle$lengths) - rle$lengths + 1
# Take the points where the rle is FALSE (so difference goes from positive to negative) 
#maxima_id <- starts[!rle$values]
#maximum <- y[maxima_id]
#}
#maxima_black_29 <- abundances_2 %>%
 # filter(colour_morph == "Black", latitude_band == "41-45")
# maxima_black_29 <-  identify_maxima (maxima_black_29$smooth_y, 
                                  #    as.numeric(maxima_black_29$date))
#

#Part 3. Plot the Data

#Factor the latitude bands so they are arranged North to South
<<<<<<< HEAD
abundances_2$growing_zone = factor(abundances_2$growing_zone, 
                                 levels=c('4', '5', '6', '7', '8', '9'))
plot <- abundances_2 %>%
  ggplot(aes(x=date, y=scaled_count, group=colour_morph))+
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5)+
  facet_grid(rows=vars(growing_zone)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Growing zones", breaks = NULL, labels = NULL), 
                    limits=(-0.05:1)) +
=======
abundances_2$lat_factor = factor(abundances_2$latitude_band, 
                                 levels=c('41-45','37-41','33-37','29-33'))
plot <- abundances_2 %>%
  ggplot(aes(x=date, y=scaled_count))+
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5)+
  facet_wrap(vars(lat_factor), nrow=4) +
  ylim(0,1)+
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220
  scale_color_manual(values = c("black", "#9C0260"))+
  labs(x="Date", y="Observations")+
  scale_linetype_manual(values=c("solid", "solid"))+
  geom_smooth(aes(colour=colour_morph, linetype=colour_morph 
                  ), method="loess", span=0.1)+
<<<<<<< HEAD
 # geom_line(aes(colour=colour_morph))+
=======
>>>>>>> 4003b07ab1fc806d8a9d5a67ded78b0e90bf5220
  theme_classic()
plot


#export the data
write.csv(abundances_2, "mod_data/cleaned_weekly_obs_102322.csv", row.names=FALSE)
ggsave("figures/abundance_plot.png", plot=plot, 
       width=2100, height=1080, units=c("px"))
