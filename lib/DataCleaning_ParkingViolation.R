library(ggmap)

raw <- read.csv('/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Yaqing_Parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv')
data <- raw[c('Violation.County','Street.Name','Intersecting.Street','Violation.Time')]
data$Location <- paste(data$Street.Name, data$Intersecting.Street, data$Violation.County,sep=' ')
data$Street.Name <- NULL
data$Intersecting.Street <- NULL
data$Violation.County <- NULL

data$Time <- substring(data$Violation.Time,1,4)
data$AM.PM <- substring(data$Violation.Time,5,5)
data$Violation.Time <- NULL


data[,'Lon'] <- NA
data[,'Lat'] <- NA

data[c(1:10),c('Lon','Lat')] <- geocode(data[c(1:10),'Location'])

write.csv(data, file = '/Users/YaqingXie/Desktop/3-Applied Data Science/Fall2016-Proj2-grp3/data/Yaqing_Parking/Part_of_Parking_Violations_Issued_-_Fiscal_Year_2017.csv')

