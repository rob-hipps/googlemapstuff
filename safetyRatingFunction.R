library(ggmap)
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)
library(plotly)
library(leaflet)

#Read in crash data
crashes <- read.csv(file.choose(), header=T)

#shorten lat/lon numerials by 3
crashes$Latitude = substr(crashes$Latitude,1,nchar(crashes$Latitude)-3)
crashes$Longitude = substr(crashes$Longitude,1,nchar(crashes$Longitude)-3)

#make lat/lon data numeric
crashes$lat <- as.numeric(crashes$Latitude)
crashes$lon <- as.numeric(crashes$Longitude)


safetyRating <- function(point_a,point_b){
    #Route map
    route_df <- route(from = sprintf("%s",point_a),
                      to = sprintf("%s",point_b),
                      structure = "route",
                      output="all")
    
    
    # Custom list extraction
    # Supplied by http://s4rdd.blogspot.com/2012/12/google-maps-api-decoding-polylines-for.html
    
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
    
    safetyMap <- leaflet() %>% addTiles()
    safetyMap <- safetyMap %>% addPolylines(route_df$lon, route_df$lat, fill = FALSE)
    safetyMap <- safetyMap %>% addPopups(route_df$lon[1], route_df$lat[1], 'Start')
    safetyMap <- safetyMap %>% addPopups(route_df$lon[length(route_df$lon)], 
                         route_df$lat[length(route_df$lon)], 'End')
    
    #return(safetyMap)
    
    crashesalongroute <- merge(route_df,crashes, by=c("lat","lon"))
    #return(crashesalongroute,safetyMap)
    
    rating <- sum(crashesalongroute$INJURIES + crashesalongroute$RDTYP + crashesalongroute$CRASH_DAY)
    
    SR <- sprintf("The safety rating for this route is %s", rating)
    
 returns <- list(safetyMap, crashesalongroute, SR)
 return(returns)
    
}