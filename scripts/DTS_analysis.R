#Test the anderson-darling and other methods of testing the distributions


#Emily Black
#25 October 2022
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Setting up R environment and packages

#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra", "twosamples")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 1. Prepare the data for Survival Analysis 
abundances_2 <- read.csv("mod_data/cleaned_weekly_obs_102322.csv")
head(abundances_2)
#Remove zone 4, not enough data
abundances_2 <- abundances_2 %>%
  filter(!growing_zone=="4")


#make sure dates read as date in file
abundances_2$date <- as.Date(abundances_2$date)

worms.lo <- loess(scaled_count~as.numeric(date), abundances_2)

abundances_2 <- abundances_2 %>% 
  group_by(colour_morph, growing_zone, year) %>%
  mutate(smooth_y = predict(loess(scaled_count~as.numeric(date), span=0.05), 
                            newdata=as.numeric(date))) 
#make all negative values are 0
abundances_2$smooth_y[abundances_2$smooth_y<0] <- 0

#Get cumulative distribution functions
#See cumulative density plots
abundances_2 <- abundances_2 %>%
  group_by(year, colour_morph, growing_zone) %>%
  mutate(csum = cumsum(smooth_y)/sum(smooth_y))



#Remove all rows with zeroes in the cumulative distributions
remove_first_last_zero <- function(x,y) {
  data <- abundances_2 %>%
    filter(year==x, growing_zone==y, colour_morph=="Black")
  #get black dates
  data_2 <- data[ data$csum != 0, ]
  data_3 <- data_2 %>%
    mutate(inv_csum =csum-1)
  data_3 <- data_3[ data_3$inv_csum != 0, ]
  
  #get red dates
  data_red <- abundances_2 %>%
    filter(year==x, growing_zone==y, colour_morph=="Red")
  data_2_red <- data_red[ data_red$csum != 0, ]
  data_3_red <- data_2_red %>%
    mutate(inv_csum =csum-1)
  data_3_red <- data_3_red[ data_3_red$inv_csum != 0, ]
  
  min_dates <- c(min(data_3$date), min(data_3_red$date))
  max_dates <- c(max(data_3$date), max(data_3_red$date))
  
  overall_min <- min(min_dates)
  overall_max <- max(max_dates)
  
  #Select the final dataset
  truncated_data <- abundances_2 %>%
    filter(year==x, growing_zone==y,
           date>=as.Date(c(overall_min)-7) & date <=as.Date(c(overall_max))+7)
  
  
  
}

dts_dataset <- remove_first_last_zero("2018", "5")
#dts_dataset <- rbind(remove_first_last_zero("2019", "4"), dts_dataset)
#dts_dataset <- rbind(remove_first_last_zero("2020", "4"), dts_dataset)
#dts_dataset <- rbind(remove_first_last_zero("2018", "5"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2019", "5"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2020", "5"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2018", "6"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2019", "6"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2020", "6"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2018", "7"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2019", "7"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2020", "7"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2018", "8"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2019", "8"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2020", "8"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2018", "9"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2019", "9"), dts_dataset)
dts_dataset <- rbind(remove_first_last_zero("2020", "9"), dts_dataset)

plot_3 <- dts_dataset %>%
  ggplot(aes(x=date, y=csum, group=colour_morph))+
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5)+
  facet_grid(rows=vars(growing_zone)) +
  ylim(0,1)+
  scale_color_manual(values = c("black", "#9C0260"))+
  #geom_line(aes(colour=colour_morph))+
  labs(x="Date", y="Observations")+
  scale_linetype_manual(values=c("solid", "solid"))+
  geom_smooth(aes(colour=colour_morph, linetype=colour_morph 
  ), method="loess", span=0.05)+
  theme_classic()
plot_3



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2. Testing The Cramer-von-Mis test on the data
#Compare the distributions of the red and black from zone 5, year 2022


set.seed(111)

#Perform DTS test
dts_test <- function(x,y) {
  
test_data <- dts_dataset %>%
    filter(year==x, growing_zone==y)
  
red_test <- test_data %>%
    filter(colour_morph=="Red")
black_test <- test_data %>%
    filter(colour_morph=="Black")
  
numbers <- as.numeric(twosamples::dts_test(black_test$csum, red_test$csum, 
                               nboots = 2000,keep.boots = T, keep.samples = F))
numbers <- data.frame(numbers)


p_value <- numbers[2,]
test_statistic <- numbers[1,]

sample <- data.frame(x,y, test_statistic, p_value)

}

dts_5_2018 <- dts_test("2018", "5")

#Perform dts test on all growing zones and years
dts_results <- rbind(dts_5_2018, dts_test("2018", 6))
dts_results <- rbind(dts_results, dts_test("2018", 7))
dts_results <- rbind(dts_results, dts_test("2018", 8))
dts_results <- rbind(dts_results, dts_test("2018", 9))
dts_results <- rbind(dts_results, dts_test("2019", 5))
dts_results <- rbind(dts_results, dts_test("2019", 6))
dts_results <- rbind(dts_results, dts_test("2019", 7))
dts_results <- rbind(dts_results, dts_test("2019", 8))
dts_results <- rbind(dts_results, dts_test("2019", 9))
dts_results <- rbind(dts_results, dts_test("2020", 5))
dts_results <- rbind(dts_results, dts_test("2020", 6))
dts_results <- rbind(dts_results, dts_test("2020", 7))
dts_results <- rbind(dts_results, dts_test("2020", 8))
dts_results <- rbind(dts_results, dts_test("2020", 9))


#plot the results of the dts test
generational_overlap_plots <- 
  dts_results %>%
  ggplot(aes(x=as.numeric(y), y=(test_statistic), colour=x, group=x)) +
  geom_point() +
  stat_summary(aes(y =(test_statistic),group=1), fun.y=mean, colour="Black", size=2,
               geom="line",group=1)+
  ylim(0,3)+
  geom_line()+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red")+
  labs(x="Growing Zone", y="Weighted area between curves", colour="Year")+
  theme_classic()+
  theme( 
    axis.title = element_text(size=12, face="bold"), 
    title=element_text(size=12, face="bold")) 
generational_overlap_plots
