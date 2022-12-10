#Perform DTS Analysis 
#Emily Black
#25 October 2022
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Setting up R environment and packages

#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "scales", "stats", "cowplot", "ggpmisc", "lubridate", 
          "Matching", "gridExtra", "lme4", "twosamples", "envalysis", "ggpmisc")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 1. Prepare the data for DTS Analysis 
abundances_2 <- read.csv("mod_data/cleaned_weekly_obs_120222.csv")
head(abundances_2)
#Remove zone 4, not enough data
abundances_2 <- abundances_2 %>%
  filter(!growing_zone=="4")


#make sure dates read as date in file
abundances_2$date <- as.Date(abundances_2$date)

#Get cumulative distribution functions
#See cumulative density plots
abundances_2 <- abundances_2 %>%
  group_by(year, colour_morph, growing_zone) %>%
  mutate(csum = cumsum(scaled_count)/sum(scaled_count))



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
  geom_point(aes(colour=colour_morph),position="jitter", alpha=0.5, size=2)+
  facet_grid(rows=vars(growing_zone)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5))+
  scale_color_manual(values = c("black", "#9C0260"))+
  #geom_line(aes(colour=colour_morph))+
  labs(x="Date", y="Cumulative distribution of observations", group = "Colour morph")+
  scale_linetype_manual(values=c("solid", "solid"))+
  geom_line(aes(colour=colour_morph, linetype=colour_morph), linewidth=0.5 )+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        legend.position = c(.1,.95))
plot_3

ggsave("figures/cdf_plot.pdf", plot=plot_3, 
       width=2500, height=1800, units=c("px"), bg = "white")



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2. Perform DTS Analysis

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

colnames(dts_results) <- c("year", "growing_zone", "test_statistic", "p_value")
#plot the results of the dts test
generational_overlap_plots <- 
  dts_results %>%
  ggplot(aes(x=as.numeric(growing_zone), y=(test_statistic), colour=year, group=year)) +
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

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
dts_results$growing_zone <- as.numeric(dts_results$growing_zone)
dts_results$year <- as.character(dts_results$year)
#add term with growing zone squared 
dts_results <- dts_results %>%
  mutate(growing_zone2 = growing_zone^2)

#Part 3. Perform Regression Analysis 
dts_results$year <- as.character(dts_results$year)
dts_model_poly <-lm(test_statistic ~ growing_zone + growing_zone2 + year + growing_zone:year + 
                      growing_zone2:year,
                 data=dts_results)
summary(dts_model_poly)
coef(dts_model_poly)
anova <- anova(dts_model_poly)
anova
#Make table with f-values and p values
variable_names <- rownames(anova)
dts_model_values <- as.data.frame(anova, row.names=rownames(anova))
dts_model_values <- cbind(variable_names, dts_model_values)


dts_model_linear <-lm(test_statistic ~ growing_zone + year + 
                        growing_zone:year,
                 data=dts_results)
summary(dts_model_linear)
anova_linear <- anova(dts_model_linear)
anova_linear
variable_names <- rownames(anova_linear)
dts_model_values <- as.data.frame(anova_linear, row.names=rownames(anova_linear))
dts_model_values <- cbind(variable_names, dts_model_values)
write_csv(as.data.frame(dts_model_values), "mod_data/dts_regression_stats.csv")


library(AICcmodavg)
aictab(cand.set = list(dts_model_poly, dts_model_linear), modnames=c("poly", "linear"))
#The linear model fits better according to the AIC

#Use colourblind friendly palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


generational_overlap_plots_2 <- 
  dts_results %>%
  ggplot(aes(x=as.numeric(growing_zone), y=(test_statistic), colour=year, group=year)) +
  geom_point(size=4) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, 
              se=FALSE, linetype="81")+
  #geom_smooth(method="lm",show.legend=TRUE, se = FALSE, 
 # size=1, linetype="81")+
  ylim(0,2.5)+
  scale_colour_manual(values=cbPalette)+
  labs(x="Plant hardiness zone", y="Difference between cumulative distribution 
functions of morphs", colour="Year")+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        legend.position = c(.2,.85))
generational_overlap_plots_2

ggsave("figures/dts_plot_quadratic.pdf", plot=generational_overlap_plots_2, 
       width=2500, height=1800, units=c("px"), bg = "white")

generational_overlap_plots_3 <- 
  dts_results %>%
  ggplot(aes(x=as.numeric(growing_zone), y=(test_statistic), colour=year, group=year)) +
  geom_point(size=4) +
  geom_smooth(method="lm",show.legend=TRUE, se = FALSE, 
   size=1, linetype="81")+
  ylim(0,3)+
  scale_colour_manual(values=cbPalette)+
  labs(x="Plant hardiness zone", y="Difference between cumulative distribution 
functions of morphs", colour="Year")+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        legend.position = c(.2,.85))
generational_overlap_plots_3
ggsave("figures/dts_plot_linear.pdf", plot=generational_overlap_plots_3, 
       width=2500, height=1800, units=c("px"), bg = "white")
