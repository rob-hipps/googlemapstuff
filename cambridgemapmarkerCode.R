library(ggmap)
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)
library(plotly)

#cambridge crash data
crashes <- read.csv(file.choose(), header=T)
crashes$lat <- crashes$Latitude
crashes$lon <- crashes$Longitude



#plot data on google map through api in R
map <- get_map(location='iowa', zoom=7, maptype='roadmap') 

ggmap(map) +
    geom_point(data = crashes, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)


#A good example is noralk, ia to ames, ia

safetyRating <- function(point_a,point_b){
#Route map
route_df <- route(from = sprintf("%s",point_a),
                  to = sprintf("%s",point_b),
                  structure = "route",
                  output="all")

# Custom decode function
# Taken from http://s4rdd.blogspot.com/2012/12/google-maps-api-decoding-polylines-for.html

decodeLine <- function(encoded){
    require(bitops)
    
    vlen <- nchar(encoded)
    vindex <- 0
    varray <- NULL
    vlat <- 0
    vlng <- 0
    
    while(vindex < vlen){
        vb <- NULL
        vshift <- 0
        vresult <- 0
        repeat{
            if(vindex + 1 <= vlen){
                vindex <- vindex + 1
                vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
            }
            
            vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
            vshift <- vshift + 5
            if(vb < 32) break
        }
        
        dlat <- ifelse(
            bitAnd(vresult, 1)
            , -(bitShiftR(vresult, 1)+1)
            , bitShiftR(vresult, 1)
        )
        vlat <- vlat + dlat
        
        vshift <- 0
        vresult <- 0
        repeat{
            if(vindex + 1 <= vlen) {
                vindex <- vindex+1
                vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
            }
            
            vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
            vshift <- vshift + 5
            if(vb < 32) break
        }
        
        dlng <- ifelse(
            bitAnd(vresult, 1)
            , -(bitShiftR(vresult, 1)+1)
            , bitShiftR(vresult, 1)
        )
        vlng <- vlng + dlng
        
        varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
    }
    coords <- data.frame(varray)
    names(coords) <- c("lat", "lon")
    coords
}

route_df <- decodeLine( route_df$routes[[1]]$overview_polyline$points )

m <- leaflet() %>% addTiles()
m <- m %>% addPolylines(route_df$lon, route_df$lat, fill = FALSE)
m <- m %>% addPopups(route_df$lon[1], route_df$lat[1], 'Origin')
m <- m %>% addPopups(route_df$lon[length(route_df$lon)], 
                    route_df$lat[length(route_df$lon)], 'Destination')

#return(m)

crashesalongroute <- merge(route_df,iowacrashes, by=c("lat","lon"))
return(crashesalongroute)

}

my_map <- get_map("iowa", zoom = 10)

ggmap(my_map) +
    geom_path(aes(x = lon, y = lat), color = "red", size = 1.5,
              data = route_df, lineend = "round")

map <- qmap('iowa', zoom = 7, maptype = 'roadmap')


#plot the density map
map + stat_density2d(
    aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..*2), 
    size = 2, bins = 100, data = crashes, geom = "polygon") +
    scale_fill_gradient(low = "black", high = "red")


#Plotly with toggle
plot_ly(crashes, lon = Longitude, lat = Latitude, text = Date.Time,
        marker = Crash.Number, type = 'scattergeo', locationmode = 'USA-states') 


#Geocoding in R

x <- crashes[1:20,]
geocoded <- geocode(paste(x$Steet.Name, x$Location, sep = " ", collapse = NULL))
geocoded

#plotly map of geocoded addresses
plot_ly(crashes, lat = lat, lon = lon,
        type = 'scattergeo', locationmode = 'USA-states', mode = 'markers')


#Start looking at indexing days of week in terms of crashes
number <- ddply(crashes,c("CRASH_DAY") ,summarise,
                number = length(CRASH_KEY))
number

ggplot(data=number, aes(x=CRASH_DAY, y=number)) +
    geom_line() +
    geom_point()

#seems like the bottleneck is to get the information from the route and apply it to a model. 

#get data in
iowacrashes <- read.csv(file.choose(),header=T)
iowaroute <- read.csv(file.choose(),header=T)

#trim lats and longs to make them join-able
iowacrashes$Latitude = substr(iowacrashes$Latitude,1,nchar(iowacrashes$Latitude)-3)
iowacrashes$Longitude = substr(iowacrashes$Longitude,1,nchar(iowacrashes$Longitude)-3)

#bit lazy but makes the join easier
iowacrashes$lat <- as.numeric(iowacrashes$Latitude)
iowacrashes$lon <- as.numeric(iowacrashes$Longitude)



#merge the sets
crashesalongroute <- merge(route_df,iowacrashes, by=c("lat","lon"))

#examine the crashes that have occurred along the route
crashesalongroute

crashtable <- ddply(crashesalongroute, "CRASH_KEY", summarise,
                       totalcost = sum(PROPDMG))
                       

ggplot(crashtable, aes(x=totalcost)) +
    geom_histogram(binwidth=.5, colour="black", fill="white")


c <- crashes[sample(nrow(crashes), 1000), ]

c$PROPDMG <- as.numeric(c$PROPDMG)
c$PROPDMG <- as.numeric(c$PROPDMG)

plot_ly(
    data= c, x = CRASH_MONTH, y = PROPDMG,
    color = PROPDMG, size = PROPDMG, type = "scatter", mode = "bars"
)


plot_ly(data=c,type="scattergeo", lon = lon, lat = lat, marker = list(color=CRASH_MONTH),mode="markers", size = PROPDMG)

crashsub <- subset(crashes, crashes$PROPDMG < 16000)
ggplot(crashsub, aes(x=PROPDMG, colour=CRASH_DAY)) + geom_density()
