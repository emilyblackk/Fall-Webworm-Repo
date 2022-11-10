library(e1071)
exhists = vector(mode = "list", length = length(hists))

for(i in 1:length(exhists)){ #Populating list of dataframes with histogram data
  exhists[[i]] = rep(c(0:255), hists[[i]][,1])
  exhists[[i]] = data.frame(exhists[[i]])
  for(j in 2:3){
    exhists[[i]][,j] = rep(c(0:255), hists[[i]][,j])
  }
}

for(i in 1:length(exhists)){ #Populating datakeep with histogram summary data
  for(j in 0:2){
    datakeep[i,5+(6*j)] = mean(exhists[[i]][,j+1])
    datakeep[i,6+(6*j)] = sd(exhists[[i]][,j+1])
    datakeep[i,7+(6*j)] = min(exhists[[i]][,j+1])
    datakeep[i,8+(6*j)] = max(exhists[[i]][,j+1])
    datakeep[i,9+(6*j)] = kurtosis(exhists[[i]][,j+1])
    datakeep[i,10+(6*j)] = skewness(exhists[[i]][,j+1])
  }
}

colours = c("Red", "Green", "Blue")
cnames = c(colnames(datakeep[1:4]))
for(i in 0:2){ #Naming columns according to their colour channel
  cnames[5+(6*i)] = paste0(colours[i+1], "Mean")
  cnames[6+(6*i)] = paste0(colours[i+1], "SD")
  cnames[7+(6*i)] = paste0(colours[i+1], "Min")
  cnames[8+(6*i)] = paste0(colours[i+1], "Max")
  cnames[9+(6*i)] = paste0(colours[i+1], "Kurt")
  cnames[10+(6*i)] = paste0(colours[i+1], "Skew")
}

colnames(datakeep) = cnames

write.csv(datakeep, "webworms.csv")