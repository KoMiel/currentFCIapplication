
# Part 1: Open hydrosheds and find the connection between the segments #####

library(sf)
library(rgdal)
library(readxl)
library(data.table)
library(elevatr)

setwd("../data/HydroRIVERS_v10_na_shp") #data directory
hydrosheds <- read_sf(dsn = ".", layer = "HydroRIVERS_v10_na") #read hydrosheds
setwd("..") #reset directory

data <- fread(file = 'ohio_freshwater.csv')

# remove incomplete cases
data <- na.omit(data, cols = c("SYCode", "DraAre", "LatituTemp", "LongitTemp", "Latitu", "Longit", "Channe", "Cover", "GraMet", "Pool", "Riffle", "Ripari", "Substr", "COD", "SpeCon", "CaCO3", "P", "TKN", "TDS", "TSS", "PAFInd"))

# format transformation
datapoints <- st_as_sf(data, coords = c("LongitTemp", "LatituTemp")) # convert to simple features
st_crs(datapoints) <- 4326 # set crs to WGS84

# set coordinates
coordinates <- data.frame(datapoints$Longit, datapoints$Latitu)
crs <- st_crs(datapoints)

# get the elevation at the measurement locations
elevationPoints <- get_elev_point(coordinates, prj = crs$proj4string, src = "aws")$elevation
datapoints$Elev <- elevationPoints

# transform to cartesian coordinates
hydrosheds <- st_transform(hydrosheds, crs = 32717) # convert to cartesian coordinates (UTM 17S)
datapoints <- st_transform(datapoints, crs = 32717) # convert measurement stations to cartesian coordinates, too

# from hydrosheds, extract the part that belongs to the measurement region
box <- st_bbox(datapoints) + c(-50000,-50000, 50000, 50000)
hydrosheds_cropped <- st_crop(x = hydrosheds, box) # cut the part out where there are actually measurement stations

# empty data table for river segments
frame <- data.table(matrix(NA_real_, nrow = 100000, ncol = 5)) # 100000 is just a large number so that we don't run out of space; data.table for runtime
names(frame) <- c("StartX", "StartY", "EndX", "EndY", "Segment") # rename columns, segments tells us which the original segment was in river data to reassemble smaller segments later

counter <- 1 # counter to store all line segments in data.table

pb <- txtProgressBar(min = 1, max = length(hydrosheds_cropped$geometry)) # progress bar

# loop over all simple feature structures (lines and multilines) and convert them to single lines; store in dataframe

for (i in 1:length(hydrosheds_cropped$geometry)) {
  setTxtProgressBar(pb, i)
  coordinates <- st_coordinates(hydrosheds_cropped$geometry[i])
  for (j in 1:(nrow(coordinates)-1)) {
    frame[counter, StartX := coordinates[j,1]]
    frame[counter, StartY := coordinates[j,2]]
    frame[counter, EndX := coordinates[j+1,1]]
    frame[counter, EndY := coordinates[j+1,2]]
    frame[counter, Segment := i]
    counter <- counter + 1
  }
}

frame <- frame[complete.cases(frame),] # remove the rows that were not needed

pb <- txtProgressBar(min = 1, max = nrow(frame)) # new progress bar

# loop over all single line segments and find their connections (which segment connects to which other segment); store these connections in data.table (downstream -> upstream)

for (i in 1:nrow(frame)) {
  setTxtProgressBar(pb, i)
  x <- frame$StartX[i]
  y <- frame$StartY[i]
  connection <- which(frame$EndX == x & frame$EndY == y)
  if(length(connection) > 0) {
    for (j in 1:length(connection)) {
      frame[i, paste0('connection', toString(j)) := connection[j]]
    }
  }
}

pb <- txtProgressBar(min = 1, max = length(hydrosheds_cropped$geometry)) # new progress bar

# loop over all line segments, link them to "old" (larger) segments again and find the connection to other "old" segments

for (i in 1:length(hydrosheds_cropped$geometry)) {
  
  setTxtProgressBar(pb, i)
  
  segment_frame <- frame[frame$Segment == i]
  connected_segments <- c(segment_frame$connection1, segment_frame$connection2, segment_frame$connection3)
  connected_segments <- connected_segments[!is.na(connected_segments)]
  connected_segments <- frame$Segment[connected_segments]
  connected_segments <- unique(connected_segments)
  connected_segments <- connected_segments[which(connected_segments != i)]
  
  if(length(connected_segments) > 0) {
    for (j in 1:length(connected_segments)) {
      hydrosheds_cropped[i, paste0('connection', toString(j))] <- connected_segments[j]
    }
  }
}

hydrosheds_cropped["Associated"] <- 0 # variable for the number of measurements stations per segment (1 if there is one station in the segment, 2 for two, ...)

pb <- txtProgressBar(min = 1, max = nrow(datapoints)) # new progress bar

# loop over all measurement stations and connect them to a single segment (the one with the closest distance)

for (i in 1:nrow(datapoints)) {
  
  setTxtProgressBar(pb, i)
  
  segment <- datapoints$geometry[i]
  distances <- st_distance(hydrosheds_cropped$geometry, segment) # find distances to all segments
  
  datapoints$segment[i] <- which.min(distances) # save all information
  datapoints$distance[i] <- min(distances)
  datapoints$volume[i] <- hydrosheds_cropped[which.min(distances),]$DIS_AV_CMS
  
  hydrosheds_cropped$Associated[which.min(distances)] <- hydrosheds_cropped$Associated[which.min(distances)] + 1
  
}

saveRDS(hydrosheds_cropped, file = "hydroshedsRiverSegments.rds")
saveRDS(datapoints, file = "hydroshedsMeasurementStations.rds")
