#Nov 23
#Date Analysis 
#Let's do this thing

#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra", "elevatr", "envalysis", "broom", "effectsize")
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

#Filter observations
obs <- obs %>% dplyr::filter(head_capsule=="Yes", 
                             date>="2018-01-01", 
                             date<="2020-12-31",
                             colour_morph %in% c("Black", "Red"), 
                             growing_zone >=5, 
                             growing_zone <=9,
                             longitude>-100, 
                             year<=2020)%>%
  na.omit()


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Get earliest date for each group 

#Get earliest 10% of dates for each year and growing zone

#Make a function
earliest_10 <- function(x,y) {
 obs2 <- obs %>%
   filter(growing_zone==y, 
          year==x)
 obs2 <- obs2 %>%
   mutate(julian_day = yday(date))
 obs_red <- obs2 %>%
   filter(colour_morph=="Red")
 obs_black <- obs2 %>%
   filter(colour_morph=="Black")
 quantile_red_10 <- as.numeric(quantile(obs_red$julian_day, 0.1))
 quantile_black_10 <- as.numeric(quantile(obs_black$julian_day, 0.1))
red_below_10 <- obs_red %>%
  filter(julian_day <=quantile_red_10)
black_below_10 <- obs_black %>%
  filter(julian_day <=quantile_black_10)
#compare julian days
# t.test <- t.test(red_below_10$julian_day, black_below_10$julian_day, 
#                  var.equal=TRUE)
# t.test.results <- data.frame(tidy(t.test)) %>%
#   dplyr::select(estimate, statistic, p.value, parameter)
# colnames(t.test.results) <- c("days_black_earlier", "statistic", 
#                               "p_value", "df")
# year <- x
# growing_zone <- y
# t.test.results <- cbind(year, growing_zone, t.test.results)
below_10 <- rbind(black_below_10, red_below_10)

}

#Perform for all years and growing zones
compare_earliest_first <- earliest_10(2020, 5)
compare_earliest <- rbind(earliest_10(2020, 6), compare_earliest_first)
compare_earliest <- rbind(earliest_10(2020, 7), compare_earliest)
compare_earliest <- rbind(earliest_10(2020, 8), compare_earliest)
compare_earliest <- rbind(earliest_10(2020, 9), compare_earliest)
compare_earliest <- rbind(earliest_10(2019, 5), compare_earliest)
compare_earliest <- rbind(earliest_10(2019, 6), compare_earliest)
compare_earliest <- rbind(earliest_10(2019, 7), compare_earliest)
compare_earliest <- rbind(earliest_10(2019, 8), compare_earliest)
compare_earliest <- rbind(earliest_10(2019, 9), compare_earliest)
compare_earliest <- rbind(earliest_10(2018, 5), compare_earliest)
compare_earliest <- rbind(earliest_10(2018, 6), compare_earliest)
compare_earliest <- rbind(earliest_10(2018, 7), compare_earliest)
compare_earliest <- rbind(earliest_10(2018, 8), compare_earliest)
compare_earliest <- rbind(earliest_10(2018, 9), compare_earliest)

#Make summary table of earliest date
earliest_date_summary <- compare_earliest %>%
  group_by(growing_zone, colour_morph)%>%
  summarize(mean_yday = mean(julian_day), 
            sd_yday = sd(julian_day), 
            n_yday = n(), 
            se = sd_yday/sqrt(n_yday))
earliest_date_summary

#Earliest date stats
mean_diff_earliest <- compare_earliest %>%
  group_by(colour_morph) %>%
  summarize(mean = mean(julian_day))
mean_diff_earliest
sd_pooled(julian_day~colour_morph, data=compare_earliest)

#Anova for earliest dates
compare_earliest$growing_zone <- as.numeric(compare_earliest$growing_zone)
compare_earliest$year <- as.character(compare_earliest$year)
earliest_lm <- lm(julian_day~colour_morph + colour_morph:growing_zone + 
                    colour_morph:growing_zone:as.character(year),
                   data=compare_earliest)
coef(earliest_lm)
earliest_aov <- anova(earliest_lm)
earliest_aov


#Plot the data
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


compare_earliest_plot <- earliest_date_summary %>%
  ggplot(aes(x=as.numeric(growing_zone), y=mean_yday, colour=as.factor(colour_morph), 
             linetype = colour_morph))+
  geom_point(aes(colour = colour_morph), size=4, show.legend=FALSE)+
  ylim(90, 210)+
  geom_line(aes(colour=colour_morph), fill = "#34595E", level=0.95, 
              lwd=1.5, alpha=0.3, se=FALSE, show.legend=FALSE)+
    geom_errorbar(aes(ymin =(mean_yday - se), ymax = (mean_yday+se)), 
                  width=0.25, show.legend=FALSE, linewidth=1, linetype="solid")+
  scale_color_manual(values = c("black", "#9C0260"))+
  annotate("text", x=9.2, y=200, label= "B", size=10)+
  labs(x="Plant hardiness zone", y="First 10%
  of observation dates", colour="Colour morph", 
      )+
theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14))
compare_earliest_plot   



#Do for latest dates

latest_90 <- function(x,y) {
  obs2 <- obs %>%
    filter(growing_zone==y, 
           year==x)
  obs2 <- obs2 %>%
    mutate(julian_day = yday(date))
  obs_red <- obs2 %>%
    filter(colour_morph=="Red")
  obs_black <- obs2 %>%
    filter(colour_morph=="Black")
  quantile_red_90 <- as.numeric(quantile(obs_red$julian_day, 0.9))
  quantile_black_90 <- as.numeric(quantile(obs_black$julian_day, 0.9))
  red_below_90 <- obs_red %>%
    filter(julian_day >=quantile_red_90)
  black_below_90 <- obs_black %>%
    filter(julian_day >=quantile_black_90)
  #compare julian days
  # t.test <- t.test(red_below_10$julian_day, black_below_10$julian_day, 
  #                  var.equal=TRUE)
  # t.test.results <- data.frame(tidy(t.test)) %>%
  #   dplyr::select(estimate, statistic, p.value, parameter)
  # colnames(t.test.results) <- c("days_black_earlier", "statistic", 
  #                               "p_value", "df")
  # year <- x
  # growing_zone <- y
  # t.test.results <- cbind(year, growing_zone, t.test.results)
  below_90 <- rbind(black_below_90, red_below_90)
  
}

#Perform for all years and growing zones
compare_latest_first <- latest_90(2020, 5)
compare_latest <- rbind(latest_90(2020, 6), compare_latest_first)
compare_latest <- rbind(latest_90(2020, 7), compare_latest)
compare_latest <- rbind(latest_90(2020, 8), compare_latest)
compare_latest <- rbind(latest_90(2020, 9), compare_latest)
compare_latest <- rbind(latest_90(2019, 5), compare_latest)
compare_latest <- rbind(latest_90(2019, 6), compare_latest)
compare_latest <- rbind(latest_90(2019, 7), compare_latest)
compare_latest <- rbind(latest_90(2019, 8), compare_latest)
compare_latest <- rbind(latest_90(2019, 9), compare_latest)
compare_latest <- rbind(latest_90(2018, 5), compare_latest)
compare_latest <- rbind(latest_90(2018, 6), compare_latest)
compare_latest <- rbind(latest_90(2018, 7), compare_latest)
compare_latest <- rbind(latest_90(2018, 8), compare_latest)
compare_latest <- rbind(latest_90(2018, 9), compare_latest)

#Make summary table of latest date
latest_date_summary <- compare_latest %>%
  group_by(growing_zone, colour_morph)%>%
  summarize(mean_yday = mean(julian_day), 
            sd_yday = sd(julian_day), 
            n_yday = n(), 
            se = sd_yday/sqrt(n_yday))
latest_date_summary


#latest date stats
mean_diff_latest <- compare_latest %>%
  group_by(colour_morph) %>%
  summarize(mean = mean(julian_day))
mean_diff_latest
sd_pooled(julian_day~colour_morph, data=compare_latest)

#Anova for latest dates
compare_latest$growing_zone <- as.numeric(compare_latest$growing_zone)
compare_latest$year <- as.character(compare_latest$year)
latest_lm <- lm(julian_day~colour_morph + colour_morph:growing_zone + 
                  colour_morph:growing_zone:year,
                  data=compare_latest)
coef(latest_lm)
latest_aov <- aov(latest_lm)
summary(latest_aov)
stats::TukeyHSD(latest_aov)


compare_latest_plot <- latest_date_summary %>%
  ggplot(aes(x=as.numeric(growing_zone), y=mean_yday, colour=as.factor(colour_morph), 
             linetype = colour_morph))+
  geom_point(aes(colour = colour_morph), size=4)+
  geom_line(aes(colour=colour_morph), fill = "#34595E", level=0.95, 
            lwd=1.5, alpha=0.3, se=FALSE, show.legend=FALSE)+
  ylim(230, 350)+
  geom_errorbar(aes(ymin =(mean_yday - se), ymax = (mean_yday+se)), 
                width=0.25, show.legend=FALSE, linewidth=1, linetype="solid")+
  scale_color_manual(values = c("black", "#9C0260"))+
  annotate("text", x=9.2, y=345, label= "A", size=10)+
  labs(x="Plant hardiness zone", y="Last 10%
   of observation dates", colour="Colour morph", 
  )+
  theme_publish()+
  theme(text = element_text(size=14), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        axis.title= element_text(size=16, face="bold"), 
        axis.text = element_text(size=14), 
        legend.position = c(0.80, 0.85), 
        strip.text= element_text(size = 14), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
  )
compare_latest_plot   



# compare_latest_plot <- compare_latest %>%
#   ggplot(aes(x=as.numeric(growing_zone), y=diff_date, colour=as.character(year)))+
#   geom_point(show.legend=FALSE)+
#   stat_smooth(method='lm', formula = y~x, se=FALSE, 
#               show.legend=FALSE)+
#   scale_colour_manual(values=cbPalette)+
#   annotate("text", x=9.2, y=35, label= "B", size=10)+
#   labs(x="Plant hardiness zone", y="Difference between last
# 10% of dates", colour="Year")+
#   theme_publish()+
#   theme(text = element_text(size=14), 
#         axis.text = element_text(size=14), 
#         axis.title= element_text(size=16, face="bold"), 
#         legend.text = element_text(size=14), 
#         legend.title = element_text(size=14, face="bold"), 
#         strip.text= element_text(size = 14))

combined_plot_2 <- plot_grid(compare_latest_plot , compare_earliest_plot, ncol = 1, 
                             align = "v")
                             
combined_plot_2
  

ggsave("figures/first_last_date_plot.pdf", combined_plot_2, 
       width=2500, height=1800, units=c("px"))