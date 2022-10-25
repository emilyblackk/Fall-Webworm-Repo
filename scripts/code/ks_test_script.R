#Perform KS Analysis on Webworms
#Emily Black, 12 October 2022



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

#Part 1. Prepare the data for KS analysis
abundances_2 <- read.csv("mod_data/cleaned_weekly_obs_102322.csv")
head(abundances_2)
#make sure dates read as date in file
abundances_2$date <- as.Date(abundances_2$date)

#To test the curves of the data rather than the individual points, I will use the 
#predict function
worms.lo <- loess(scaled_count~as.numeric(date), abundances_2)

abundances_2 <- abundances_2 %>% 
  group_by(colour_morph, growing_zone, year) %>%
  mutate(smooth_y = predict(loess(scaled_count~as.numeric(date), span=0.05), 
                            newdata=as.numeric(date))) 
#make all negative values are 0
abundances_2$smooth_y[abundances_2$smooth_y<0] <- 0
  
#Make sure prediction went ok
plot_2 <- abundances_2 %>%
  ggplot(aes(x=date, y=smooth_y, group=colour_morph))+
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5)+
  facet_grid(rows=vars(growing_zone)) +
  ylim(-0.5,1)+
  scale_color_manual(values = c("black", "#9C0260"))+
  labs(x="Date", y="Observations")+
  scale_linetype_manual(values=c("solid", "solid"))+
  geom_smooth(aes(colour=colour_morph, linetype=colour_morph 
  ), method="loess", span=0.05)+
  theme_classic()
plot_2

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

ks_dataset <- remove_first_last_zero("2018", "4")
ks_dataset <- rbind(remove_first_last_zero("2019", "4"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "4"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2018", "5"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2019", "5"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "5"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2018", "6"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2019", "6"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "6"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2018", "7"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2019", "7"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "7"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2018", "8"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2019", "8"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "8"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2018", "9"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2019", "9"), ks_dataset)
ks_dataset <- rbind(remove_first_last_zero("2020", "9"), ks_dataset)

plot_3 <- ks_dataset %>%
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



#Make a function to generate ks_test p-value
#make a blank dataframe
df <- tibble(year = character(), 
             growing_zone = character(), 
             statistic = as.numeric(),
             p_value = as.numeric())
#Generate function that outputs test statistic, p-value
ks_test <- function(x,y) {
  ks_data <- ks_dataset %>%
    filter(year==x, growing_zone==y) 
  red_abundance <- ks_data %>%
    filter(colour_morph=="Red")
  black_abundance <- ks_data %>%
    filter(colour_morph=="Black")
  ks_test <- ks.boot(red_abundance$smooth_y, black_abundance$smooth_y)
  p_value <- ks_test$ks.boot.pvalue
  statistic <- as.numeric(ks_test$ks$statistic)
  year <- x
  growing_zone <- y
  sample <- data.frame(year,growing_zone, statistic, p_value)
  
}



#Run the function on all combinations of years and growing zones, add to table
df <- rbind(ks_test("2018", "4"), df)
df <- rbind(ks_test("2019", "4"), df)
df <- rbind(ks_test("2020", "4"), df)
df <- rbind(ks_test("2018", "5"), df)
df <- rbind(ks_test("2019", "5"), df)
df <- rbind(ks_test("2020", "5"), df)
df <- rbind(ks_test("2018", "6"), df)
df <- rbind(ks_test("2019", "6"), df)
df <- rbind(ks_test("2020", "6"), df)
df <- rbind(ks_test("2018", "7"), df)
df <- rbind(ks_test("2019", "7"), df)
df <- rbind(ks_test("2020", "7"), df)
df <- rbind(ks_test("2018", "8"), df)
df <- rbind(ks_test("2019", "8"), df)
df <- rbind(ks_test("2020", "8"), df)
df <- rbind(ks_test("2018", "9"), df)
df <- rbind(ks_test("2019", "9"), df)
df <- rbind(ks_test("2020", "9"), df)

#Adjust p values using bonferroni
df <- df %>%
  mutate(adjusted_p_value = p.adjust(p_value, method="bonferroni", n=n()))


#Plot the results
df_2 <- df %>%
  filter(!year=="2018")

generational_overlap_plots <- 
  df %>%
  ggplot(aes(x=as.numeric(growing_zone), y=(adjusted_p_value), colour=year, group=year)) +
  geom_point() +
  stat_summary(aes(y =(adjusted_p_value),group=1), fun.y=mean, colour="Black", size=2,
               geom="line",group=1)+
  ylim(0,1)+
  geom_line()+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red")+
  labs(x="Growing Zone", y="Significance of difference between 
       red and black generations", colour="Year")+
  theme_classic()+
  theme( 
    axis.title = element_text(size=12, face="bold"), 
    title=element_text(size=12, face="bold")) 
generational_overlap_plots


grid.arrange(plot_3, generational_overlap_plots)

#Save the plot
ggsave("figures/ks_test_results.png", plot=generational_overlap_plots, 
       width=1900, height=1080, units=c("px"))
