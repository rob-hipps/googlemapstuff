library(ggmap)
library(dplyr)
library(data.table)

#cambridge crash data
crashes <- read.csv(file.choose(), header=T)
crashes$lat <- crashes$Latitude
crashes$lon <- crashes$Longitude



#plot data on google map through api in R
map <- get_map(location='cambridge, ma', zoom=13, maptype='roadmap') 

ggmap(map) +
    geom_point(data = crashes, aes(x = lon, y = lat, fill = "red", alpha = 0.8,size=(Day.Of.Week)), size = 1, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)


#Route map
route_df <- as.data.route(from = "cambridge, ma",
                  to = "adams,ma",
                  structure = "route",
                  output = "all")

#Make list of turn lat long's available in a list for extraction.
df <- data.frame(matrix(unlist(route_df), nrow=132, byrow=T))


my_map <- get_map("massachusetts", zoom = 7)

ggmap(my_map) +
    geom_path(aes(x = lon, y = lat), color = "red", size = 1.5,
              data = route_df, lineend = "round")

map <- qmap('cambridge, ma', zoom = 13, maptype = 'roadmap')


#plot the density map
map + stat_density2d(
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..*2), 
    size = 2, bins = 5, data = crashes, geom = "polygon") +
    scale_fill_gradient(low = "black", high = "red")


#Plotly with toggle
plot_ly(crashes, lon = Longitude, lat = Latitude, text = Date.Time,
        marker = Crash.Number, type = 'scattergeo', locationmode = 'USA-states') 


#Geocoding in R

x <- crashes[1:20,]
geocoded <- geocode(paste(x$Steet.Name, x$Location, sep = " ", collapse = NULL))
geocoded

#plotly map of geocoded addresses
plot_ly(geocoded, lat = lat, lon = lon,
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers')


#Start looking at indexing days of week in terms of crashes
number <- ddply(crashes,c("Day.Of.Week") ,summarise,
                number = length(Day.Of.Week))
number


#seems like the bottleneck is to get the information from the route and apply it to a model. 