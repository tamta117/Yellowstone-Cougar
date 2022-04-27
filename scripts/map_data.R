#map coordinates
library(sp)
library(rgdal)
library(mapview)
library(here)
library(RColorBrewer)
library(elevatr)
library(tidyverse)

#make utm numeric
all$utm_e<-as.numeric(all$utm_e,replace=FALSE)
all$utm_n<-as.numeric(all$utm_n,replace=FALSE)

#define coordinates
coordinates(all) <- all[, c('utm_e', 'utm_n')]

#assign crs project
proj4string(all) <- CRS('+proj=utm +zone=12 +datum=WGS84')

#transform to latitude/longtitude
map <- spTransform(all, CRSobj = CRS('+proj=longlat +datum=WGS84'))

#convert to data frame
map <- as.data.frame(map)

#rename columns
names(map)[(ncol(map)-1):ncol(map)] <- c('long', 'lat')

#save as csv
write.csv(map,here("data/all.csv"))

#redefine coordinates
coordinates(map) <- map[, c('long', 'lat')]

#assign crs project
proj4string(map) <- CRS('+proj=longlat +datum=WGS84')

#plot
mapview(map)
mapview(map, map.types = c("Esri.WorldImagery"))
(m1<-mapview(map, zcol="species", map.types = c("Esri.WorldImagery"),
             col.regions=brewer.pal(9,"Set1")))
(m2<-mapview(map, zcol="season", map.types = c("Esri.WorldImagery"),
             col.regions=brewer.pal(4,"Set1")))

#save
mapshot(m1, url = paste0(getwd(), "/figures/map.html"))

#fetch elevation data
dir<-read_csv(here("data/all.csv"))
elev<-dir%>%
  select(long,lat)
prj_dd="EPSG:4326"
examp_sp <- SpatialPoints(elev, proj4string = CRS(prj_dd))
elev_a <- get_elev_point(examp_sp, prj = prj_dd, src = "epqs")
elev_a <- as.data.frame(elev_a)
colnames(elev_a)<-c("elevation","unit","long","lat")

#bind with dir
dir_elev<-left_join(dir,elev_a)%>%
  select(-unit)
dir_elev<-distinct(dir_elev)
write.csv(dir_elev,here("data/all_elev.csv"))
coord<-dir_elev%>%
  select(long,lat)
write.csv(coord,here("data/coord.csv"))

#prepare
dir_map<-dir_elev%>%
  subset(dir_elev$season=="winter")
dir_map<-dir_map%>%
  subset(species=="mule deer" | species =="elk")
  
coordinates(dir_map) <- dir_map[, c('long', 'lat')]
proj4string(dir_map) <- CRS('+proj=longlat +datum=WGS84')

#map
mapview(dir_map, zcol="elevation", map.types = c("Esri.WorldImagery"),
             col.regions=brewer.pal(9,"YlGnBu"))+
    mapview(dir_map, zcol="species", map.types = c("Esri.WorldImagery"))

mapshot(m3, url = paste0(getwd(), "/figures/map2.html"))
#show in new window to export