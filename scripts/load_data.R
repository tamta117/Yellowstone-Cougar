#reformat og datasets
library(here)
library(tidyverse)

#load data
c19_dir<-read_excel(here("data/2019_Carcass_Data_Marzluff.xlsx"))
c15_dir<-read_excel(here("data/CougarKills_2015.2017.xlsx"))

#filter for coug and reformat data
c19<-c19_dir%>%
  subset(c19_dir$`KILL TYPE`=="COUGAR KILL")%>%
  select(`Kill #`,SPECIES,DOD,`GROUND EAST`,`GROUND NORTH`,
         `AGE CLASS`,`WOLVES PRESENT`)
colnames(c19)<-c("kill_num","species","dod","utm_e","utm_n","age","id")
c19$utm_e<-as.character(c19$utm_e)
c19$utm_n<-as.character(c19$utm_n)

c15<-c15_dir%>%
  select(`Observation ID`,`Prey Species`,`Date of Death`,
         `UTM Easting`,`UTM Northing`,Age,)
colnames(c15)<-c("kill_num","species","dod","utm_e","utm_n","age")
c15$kill_num<-as.character(c15$kill_num)

#combine data tables
c15_19<-bind_rows(c15,c19)
c15_19<-c15_19[complete.cases(c15_19[,1:6]), ] #remove NA
