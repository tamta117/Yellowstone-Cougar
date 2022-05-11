#generate random spatial points

#load library
library(here)
library(tidyverse)
library(sp)
library(raster)
library(dplyr)

#load data
odata<-read.csv(here("data/all_elev.csv"))%>%
  distinct(kill_num,.keep_all = TRUE)

#### make spatial points without designated vertices ####
## THIS IS ONLY A TEST. WE ARE NOT USING THIS.
sample(x = 44.86298:45.07584, size  = 664)
x<-runif(664, min=44.86298, max=45.07584)
y<-runif(664, min=-110.8111, max=-110.1557)
lat<-as.data.frame(x)
long<-as.data.frame(y)
long.lat<-bind_cols(lat,long)
ggplot(long.lat)+
  geom_jitter(aes(x=x,y=y))

#### make spatial polygon with designated vertices ####
# assign vertices
y.coords<-c(44.8629883907557,44.8772172328409,44.8779809962684,
            44.9474364036878,45.0435623871759,45.0503022836054,
            45.0758268653013,45.0537289631273,44.8629883907557)
x.coords<-c(-110.155727473923,-110.412155356569,-110.571689414361,
            -110.700167303798,-110.811111726929,-110.799101679801,
            -110.74696126922,-110.654769528016,-110.155727473923)

# make polygon from vertices
poly1<-Polygon(cbind(x.coords,y.coords))

# convert it to Polygon class
firstPoly <- Polygons(list(poly1), ID = "A")
str(firstPoly,1)

# convert to spatial polygon
firstSpatialPoly <- SpatialPolygons(list(firstPoly))

# preliminary map of polygon
mapview(firstSpatialPoly)

#### generate random points ####
points<-spsample(firstSpatialPoly,n=664,"random")

# make into data frame and export
random.points<-as.data.frame(points)
colnames(random.points)<-c('long','lat')
write.csv(random.points,here("data/random.points.csv"))

#### compare with odata ####
compare.points<-odata%>%
  dplyr::select(long,lat)
match<-semi_join(random.points,compare.points,by=c("long","lat"))
#OMG THERE'S NO MATCHES PRAISE THE LORD JESUS CHRIST

#### map random points for confirmation ####
random.map<-random.points

# convert data frame back into spatial points
coordinates(random.map) <- random.map[, c('long', 'lat')]
proj4string(random.map) <- CRS('+proj=longlat +datum=WGS84')

# map
mapview(random.map,map.types = c("Esri.WorldImagery"))
