#preliminary model v2 with actual random points

library(here)
library(tidyverse)
library(elevatr)

#load data
odata<-read.csv(here("data/all_elev.csv"))%>%
  distinct(kill_num,.keep_all = TRUE)%>%
  mutate(kill=1)
random<-read.csv(here("data/random.points.csv"))%>%
  select(-X)

#get elevation data for random points
prj_dd="EPSG:4326"
examp_sp <- SpatialPoints(random, proj4string = CRS(prj_dd))
random.elev <- get_elev_point(examp_sp, prj = prj_dd, src = "epqs")
random.elev <- as.data.frame(random.elev)
colnames(random.elev)<-c("elevation","unit","long","lat")
random.elev<-random.elev%>%
  mutate(kill=0)

#join data
bind<-bind_rows(odata,random.elev)%>%
  dplyr::select(kill,species,elevation)

#scale
bind.scale <- scale(bind[3])
bind.mod <- data.frame("kill" = bind$kill, "species" = bind$species,
                  bind.scale)

#filter for species
elk<-bind.mod%>%
  filter(species=="elk" | kill == 0)
mule<-bind.mod%>%
  filter(species=="mule deer" | kill == 0)

#null model
m.null <- glm(kill ~ 1, family = binomial, data=bind.mod)
summary(m.null)

#actual model
elk.mod<-glm(kill~elevation, data=elk, family=binomial)
summary(elk.mod)
mule.mod<-glm(kill~elevation, data=mule, family=binomial)
summary(mule.mod)