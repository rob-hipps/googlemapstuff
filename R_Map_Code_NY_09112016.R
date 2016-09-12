library(ggmap)

#Fatal Crash data from http://www-fars.nhtsa.dot.gov/States/StatesCrashesAndAllVictims.aspx
map <- read.csv(file.choose(), header=T)
map$County <- as.character(map$County)

#get lat/long values 
geocoded <- as.numeric(geocode(map$County))
geocodemap <- cbind(map,geocoded)

#plot data on google map through api in R
map<-get_map(location='new york state', zoom=6, maptype='roadmap') 
ggmap(map)+geom_point(aes(x=lon, y=lat, size=(X2014)), data=geocodemap, alpha=.5)
