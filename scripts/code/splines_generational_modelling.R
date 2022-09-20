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

#Part 1. Reading and cleaning the data
obs <- read.csv("raw_data/051421_raw_data.csv")
head(obs)
