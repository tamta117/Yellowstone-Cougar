#preliminary model v2 with actual random points

library(here)
library(tidyverse)
library(elevatr)
library(ggplot2)

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

#plot elk model
elk.plot <- data.frame(elevation=seq(min(elk$elevation), max(elk$elevation),
                             len=500))
elk.plot$pre = predict(elk.mod, elk.plot, type="response")
elk.plot$min<-elk.plot$pre-0.16
elk.plot$max<-elk.plot$pre+0.16
ggplot(elk.plot, aes(x=elevation, y=pre)) + 
  geom_line()+
  ylab("Predicted probability \n of elk kill")+
  xlab("Elevation (m)")

#plot mule deer model
mule.plot <- data.frame(elevation=seq(min(mule$elevation), max(mule$elevation),
                                     len=500))
mule.plot$pre = predict(mule.mod, mule.plot, type="response")
# mule.plot$min<-mule.plot$pre-0.1906
# mule.plot$max<-mule.plot$pre+0.1906
ggplot(mule.plot, aes(x=elevation, y=pre)) +
  geom_smooth(size=1.5,color="black")+
  geom_ribbon(aes(ymin=min,ymax=max),linetype=2,alpha=0.1)+
  ylab("Predicted probability \n of mule deer kill")+
  xlab("Elevation (m)")

#plot joined model
mule.plot$group<-"1"
elk.plot$group<-"2"
bind.plot<-rbind(mule.plot,elk.plot)
ggplot(bind.plot, aes(x=elevation, y=pre, col=group)) +
  #geom_ribbon(aes(ymin=min,ymax=max), fill="grey70")+
  geom_smooth(size=1.5)+
  xlab("Elevation (m)")+
  ylab("Predicted probability of kill")+
  scale_color_manual(values = c("#39638b","#26a185"), name = "Species",
                     labels=c("Mule deer","Elk"))
