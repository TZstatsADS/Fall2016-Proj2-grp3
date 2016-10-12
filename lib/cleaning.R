library(dplyr)


locations <- read.csv("locations.csv")
signs <- read.csv("signs.csv",stringsAsFactors = F, na.strings = "NA")
psign <- cbind(signs[,2],signs[,6])
csign <- cbind(psign[,1],strsplit(as.character(psign[,2]),' '))

ssign = vector()
for (i in 1:nrow(csign)) {
  ssign[i] = pmatch("NO",csign[[i,2]])
  } 

nsign = vector()
for (i in 1:length(ssign)) {
  if (is.na(ssign[i]) == FALSE) {
    nsign = rbind(nsign,csign[i,])
  }
}

nsign_d = as.data.frame(nsign) %>% unique()
