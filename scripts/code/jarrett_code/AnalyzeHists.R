rm(list=ls())

library(lattice)
library(caret)
library(MASS)
library(ggplot2)

#Emily's packages
#load relevant libraries for script
pkgs <- c("tidyverse", "raster", "tmap", "terra", "rgdal", "sf", "stars", "envalysis")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)


datakeep = read.csv("~/Work and Career/Fall Webworm/R Code - Post Submission/Fall-Webworm-Repo/mod_data/jarrett_data/ColourData.csv")
head(datakeep)

datakeep$year <-format(as.Date(datakeep$observed_on, format="%Y-%m-%d"), "%Y")
datakeep$year <- as.numeric(datakeep$year)
datakeep = subset(datakeep, datakeep$longitude > -100) #Remove data <= -100 long
datakeep = subset(datakeep, datakeep$year >= 2018)


#assign to growing zones
#read in the raster
zones_readin <- raster("maps/plant_hardiness_zones.tif")
zones_readin[zones_readin==0] = NA


coordinates <- datakeep %>%
  dplyr::select(longitude, latitude)
growing_zones <- terra::extract(zones_readin, coordinates)
datakeep2 <- cbind(datakeep, growing_zones)
datakeep2 <- datakeep2 %>%
  filter(growing_zones>=5)

summary(datakeep2)

#get the data breakdown
nrow(datakeep2)
datakeep2 %>%
  filter(field.colour=="Black")%>%
  count()
datakeep2 %>%
  filter(field.colour=="Red")%>%
  count()





dataParam <- preProcess(datakeep2[,6:23], method = c("center", "scale")) #Normalizing data
normData <- predict(dataParam, datakeep2[,6:23])

datakeep2[,6:23] = normData #Swapping datakeep data with normalized data

wormspca = prcomp(normData)  #Running pca on normalized data

trainingData = datakeep2[,5:23]
model = lda(field.colour~., data = trainingData) #Training lda model
scaling <- as.data.frame(model$scaling) %>%
  mutate(absolute_LD1  = abs(LD1))%>%
  arrange(desc(absolute_LD1))
scaling
write.csv(scaling, "mod_data/LD1_scaling.csv")

lda.data = cbind(datakeep2, predict(model)$x) #Adding LD1 data

# Band = rep(NA, nrow(lda.data)) #Spliting data into latitudinal bands
# for(i in 1:nrow(lda.data)){
#   if(lda.data$latitude[i] >= 29 & lda.data$latitude[i] < 33){
#     Band[i] = "29-33"
#   }
#   if(lda.data$latitude[i] >= 33 & lda.data$latitude[i] < 37){
#     Band[i] = "33-37"
#   }
#   if(lda.data$latitude[i] >= 37 & lda.data$latitude[i] < 41){
#     Band[i] = "37-41"
#   }
#   if(lda.data$latitude[i] >= 41 & lda.data$latitude[i] < 45){
#     Band[i] = "41-45"
#   }
# }

# lda.data = data.frame(lda.data, Band) #Adding band data to datakeep

#get data in bands 5 to 9

#Set growing zones as factors
lda.data$growing_zones <- as.factor(lda.data$growing_zones)
boxplot <- ggplot(lda.data, aes(x=growing_zones, y=LD1, fill=field.colour)) + #Plotting bands as boxplot
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#818181", "#9c2265", "#56B4E9"))+
  labs(x="Plant hardiness zone", y="LD1", fill="Colour morph")+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=12), 
        legend.text = element_text(size=12), 
        legend.position = c(0.85, 0.1))
boxplot
ggsave("figures/colour_analysis_boxplot.png", plot=boxplot, 
       width=3500, height=2800, units=c("px"), bg = "white")


#Create column with year
FinalDF <- lda.data
FinalDF$year <-format(as.Date(FinalDF$observed_on, format="%Y-%m-%d"), "%Y")
FinalDF$year <- as.numeric(FinalDF$year)

band.aov = aov(LD1 ~ as.character(year) + as.character(year):field.colour + field.colour+growing_zones+ 
                 field.colour:growing_zones, data = FinalDF) #ANOVA by bands
summary(band.aov)


group.colours = c(Black = "#000000", Red = "#9C0260") #Plot LD1 vs latitude for each colour morph (29-45)
linear_plot <- ggplot(data = FinalDF, aes(x = as.numeric(as.character(growing_zones)), y = LD1, 
                                          colour = field.colour, linetype=as.character(year)))+
  #geom_point(aes(colour = field.colour, shape=as.character(year)))+
  geom_smooth(aes(colour=field.colour), method = "lm", fill = "#000000", 
              level=0.95, 
              lwd=1.5, alpha=0.15)+
  stat_summary(aes(shape=as.character(year), colour=field.colour),
               fill="black",
    geom = "point",
    fun.y = "mean",
    size = 4, 
    stroke=1.5)+
  coord_cartesian(xlim = c(5,9),
                  ylim = c(-1,2.5))+
  xlab("Plant hardiness zone")+
  ylab("Linear discriminant 1")+
  labs(color="Colour morph", linetype="Year", shape="Mean LD1")+
  #facet_wrap(vars(year))+
  #geom_vline(xintercept = 41, linetype = "dotted") +
  scale_shape_manual(name = "Year", values = c(1, 5, 4))+
  scale_color_manual(values = c("#000000", "#9C0260"))+
  scale_fill_manual(values = c("#000000", "#9C0260"))+
  scale_linetype_manual(values = c("solid", "longdash", "dotdash"))+
  theme_publish()+
  theme(text = element_text(size=14), 
        axis.text = element_text(size=14), 
        axis.title= element_text(size=16, face="bold"), 
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14, face="bold"), 
        strip.text= element_text(size = 14), 
        legend.position = c(0.75, 0.90),
        legend.key.width=unit(2,"cm"))
linear_plot

ggsave("figures/colour_analysis_linear.pdf", plot=linear_plot, 
       width=2500, height=1800, units=c("px"), bg = "white")



Red = FinalDF[which(FinalDF$field.colour == "Red"),]
Black = FinalDF[which(FinalDF$field.colour == "Black"),]
Black41 = Black[which(Black$latitude < 41),]

RedFit = lm(LD1 ~ as.numeric(growing_zones)*as.character(year), data = Red) #Within red morph regression
anova(RedFit)
summary(RedFit)
BlackFit = lm(LD1 ~ as.numeric(growing_zones)+ as.numeric(growing_zones):as.character(year), data = Black) #Within black morph regression
anova(BlackFit)
summary(BlackFit)

Black41Fit = lm(LD1 ~ latitude, data = Black41) #Within black morph <41 lat regression


Volt = rep(NA, nrow(Black))
for(i in 1:nrow(Black)){
  if(Black$latitude[i] >= 29 & Black$latitude[i] < 41){
    Volt[i] = "29-41"
  }
  if(Black$latitude[i] >= 41){
    Volt[i] = ">41"
  }
}

VoltDF = data.frame(Black, Volt) #Dataframe where >41 and <41 lat black morphs are labelled separately

band.aov = aov(LD1 ~ Volt, data = VoltDF) #ANOVA on volt df groups
summary(band.aov)

