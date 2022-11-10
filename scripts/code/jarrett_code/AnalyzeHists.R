library(lattice)
library(caret)
library(MASS)
library(ggplot2)

setwd("C:/Carabid_Data/Webworm/Histograms")

datakeep = read.csv("webworms.csv")

datakeep = subset(datakeep, datakeep$longitude > -100) #Remove data <= -100 long

dataParam <- preProcess(datakeep[,5:22], method = c("center", "scale")) #Normalizing data
normData <- predict(dataParam, datakeep[,5:22])

datakeep[,5:22] = normData #Swapping datakeep data with normalized data

wormspca = prcomp(normData)  #Running pca on normalized data

trainingData = datakeep[,4:22]
model = lda(field.colour~., data = trainingData) #Training lda model

lda.data = cbind(datakeep, predict(model)$x) #Adding LD1 data

Band = rep(NA, nrow(lda.data)) #Spliting data into latitudinal bands
for(i in 1:nrow(lda.data)){
  if(lda.data$latitude[i] >= 29 & lda.data$latitude[i] < 33){
    Band[i] = "29-33"
  }
  if(lda.data$latitude[i] >= 33 & lda.data$latitude[i] < 37){
    Band[i] = "33-37"
  }
  if(lda.data$latitude[i] >= 37 & lda.data$latitude[i] < 41){
    Band[i] = "37-41"
  }
  if(lda.data$latitude[i] >= 41 & lda.data$latitude[i] < 45){
    Band[i] = "41-45"
  }
}

lda.data = data.frame(lda.data, Band) #Adding band data to datakeep

FinalDF = subset(lda.data, !is.na(lda.data$Band)) #Removing data that belongs to no bands


ggplot(FinalDF, aes(x=Band, y=LD1, fill=field.colour)) + #Plotting bands as boxplot
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values=c("#818181", "#9c2265", "#56B4E9")) 


band.aov = aov(LD1 ~ field.colour*Band, data = FinalDF) #ANOVA by bands
summary(band.aov)
latinband.aov = aov(LD1 ~ field.colour*latitude, data = FinalDF) #ANOVA by latitude (29-45)
summary(latinband.aov)
rawlat.aov = aov(LD1 ~ field.colour*latitude, data = lda.data) #ANOVA by latitude (entire range)
summary(rawlat.aov)


group.colours = c(Black = "#000000", Red = "#9C0260") #Plot LD1 vs latitude for each colour morph (29-45)
ggplot(data = FinalDF, aes(x = latitude, y = LD1, color = field.colour))+
  geom_smooth(aes(group = field.colour), method = "loess", fill = "#34595E")+
  coord_cartesian(xlim = c(29,45),
                  ylim = c(-1,2.5))+
  xlab("Latitude")+
  ylab("Linear Discriminant 1")+
  geom_vline(xintercept = 41, linetype = "dotted") +
  scale_color_manual(values = c("#000000", "#9C0260"))+
  scale_fill_manual(values = c("#000000", "#9C0260"))+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12, family = "serif"),
    axis.title = element_text(size = 16, family = "serif"),
    axis.title.y = element_text(vjust = 0.5, family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "serif"),
    legend.position = c(0.85, 0.1))




Red = FinalDF[which(FinalDF$field.colour == "Red"),]
Black = FinalDF[which(FinalDF$field.colour == "Black"),]
Black41 = Black[which(Black$latitude < 41),]

RedFit = lm(LD1 ~ latitude, data = Red) #Within red morph regression
BlackFit = lm(LD1 ~ latitude, data = Black) #Within black morph regression
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

