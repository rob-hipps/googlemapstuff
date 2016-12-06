library(ggmap)
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)
library(plotly)
library(leaflet)
library(caret)

#Read in crash data
crashes <- read.csv(file.choose(), header=T)
numeric_crashes <- Filter(is.numeric, crashes)

#shorten lat/lon numerials by 3
numeric_crashes$Latitude = substr(numeric_crashes$Latitude,1,nchar(numeric_crashes$Latitude)-3)
numeric_crashes$Longitude = substr(numeric_crashes$Longitude,1,nchar(numeric_crashes$Longitude)-3)

#make lat/lon data numeric
numeric_crashes$lat <- as.numeric(numeric_crashes$Latitude)
numeric_crashes$lon <- as.numeric(numeric_crashes$Longitude)


model <- train(PROPDMG ~ .,
               data = numeric_crashes, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               #tunelength = 15,
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5),# Use 5 folds for cross-validation
               importance = TRUE
)

varImp(model)


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
    
    crashesalongroute <- merge(route_df,numeric_crashes, by=c("lat","lon"))
    #return(crashesalongroute,safetyMap)
    
    rating <- sum((crashesalongroute$INJURIES*.006779) + (crashesalongroute$VEHICLES*.008381) + (crashesalongroute$MININJURY*.005461)
                  +(crashesalongroute$MAJINJURY*.004148))
    
    SR <- sprintf("The safety rating for this route is %s", rating)
    
 returns <- list(safetyMap, crashesalongroute, SR)
 return(returns)
    
}

safetyRating("windsor heights, ia", "lovington, ia")
