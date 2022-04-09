#map coordinates
library(sp)
library(rgdal)
library(mapview)
library(here)

#make utm numeric
full_dir$utm_e<-as.numeric(full_dir$utm_e,replace=FALSE)
full_dir$utm_n<-as.numeric(full_dir$utm_n,replace=FALSE)

#define coordinates
coordinates(full_dir) <- full_dir[, c('utm_e', 'utm_n')]

#assign crs project
proj4string(full_dir) <- CRS('+proj=utm +zone=12 +datum=WGS84')

#transform to latitude/longtitude
map <- spTransform(full_dir, CRSobj = CRS('+proj=longlat +datum=WGS84'))

#convert to data frame
map <- as.data.frame(map)

#rename columns
names(map)[(ncol(map)-1):ncol(map)] <- c('long', 'lat')

#save as csv
write.csv(map,here("data/full_dir.csv"))

#redefine coordinates
coordinates(map) <- map[, c('long', 'lat')]

#assign crs project
proj4string(map) <- CRS('+proj=longlat +datum=WGS84')

#plot
mapview(map)
mapview(map, map.types = c("Esri.WorldImagery"))
(m1<-mapview(map, zcol="species", map.types = c("Esri.WorldImagery")))

#save
mapshot(m1, file = here("figures/map.png"))
mapshot(m1, url = paste0(getwd(), "figures/map.html"))
