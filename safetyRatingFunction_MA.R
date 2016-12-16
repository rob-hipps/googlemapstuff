library(ggmap)
library(dplyr)
library(ggplot2)
library(leaflet)
library(caret)

#Read in crash data
crashes <- read.csv("Crashes.csv", header=T)
crashes$Object.1 <- as.numeric(crashes$Object.1)
crashes$Object.2 <- as.numeric(crashes$Object.2)
crashes$Day.Of.Week <- as.numeric(crashes$Day.Of.Week)
numeric_crashes <- Filter(is.numeric, crashes)
d <- as.Date(crashes$Date.Time)
df <- data.frame(date = d,
                 year = as.numeric(format(d, format = "%Y")),
                 month = as.numeric(format(d, format = "%m")),
                 day = as.numeric(format(d, format = "%d")))

numeric_crashes_dt <- cbind(numeric_crashes,df)
numeric_crashes_dt$date <- NULL

numeric_crashes_dt$lat <- format(round(numeric_crashes_dt$Latitude, 5), nsmall = 5)
numeric_crashes_dt$lon <- format(round(numeric_crashes_dt$Longitude, 5), nsmall = 5)


#make lat/lon data numeric
numeric_crashes_dt$lat <- as.numeric(numeric_crashes_dt$lat)
numeric_crashes_dt$lon <- as.numeric(numeric_crashes_dt$lon)
numeric_crashes_dt$crash_num <- 1

x <- ddply(numeric_crashes_dt, c("lat", "lon"), summarise,
           number_of_crashes = length(crash_num)
           )

numeric_crashes_dt <- merge(numeric_crashes_dt, x, by=c("lat", "lon"))
numeric_crashes_dt$crash_num <- NULL

preProcValues <- preProcess(numeric_crashes_dt, method = "bagImpute")
numeric_crashes_dt <- predict(preProcValues, numeric_crashes_dt)
numeric_crashes_dt$year <- as.integer(numeric_crashes_dt$year)
numeric_crashes_dt$day <- as.integer(numeric_crashes_dt$day)
numeric_crashes_dt$month <- as.integer(numeric_crashes_dt$month)

model <- train(number_of_crashes ~ .,
               data = numeric_crashes_dt, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               #tunelength = 15,
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5),# Use 5 folds for cross-validation
               importance = TRUE
)

varImp(model)


safetyRating <- function(point_a,point_b, month_int){
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
    
    crashesalongroute <- merge(route_df,numeric_crashes_dt, by=c("lat","lon"))
    crashesalongroute <- subset(crashesalongroute, month == month_int)
    #return(crashesalongroute,safetyMap)
    
    rating <- sum((crashesalongroute$Object.2*.0055738) + (crashesalongroute$Object.1*.0035094) + (crashesalongroute$year*.0017704)
                  +(crashesalongroute$Day.Of.Week*.004900))
    SR <- sprintf("The safety rating for this route is %s", rating)
    
 returns <- list(safetyMap, SR)
 return(returns)
    
}

safetyRating("157 Berkeley Street, Boston, MA", "2072 Massachusetts Ave, Cambridge, MA", month_int = 12)
safetyRating("157 Berkeley Street, Boston, MA", "2072 Massachusetts Ave, Cambridge, MA", month_int = 1)
