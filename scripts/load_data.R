#reformat og datasets
library(here)
library(tidyverse)
library(lubridate)
library(dplyr)

#load data
c19_dir<-read_excel(here("data/2019_Carcass_Data_Marzluff.xlsx"))
c15_dir<-read_excel(here("data/CougarKills_2015.2017.xlsx"))
stats_dir<-read_csv(here("data/cougardemostats.csv"))

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
c15_19$dod<-parse_date_time(c15_19$dod,orders = c('ymd','dmy'),tz="")
c15_19<-c15_19%>%
  mutate(year=year(dod),
         month=month(dod),
         day=day(dod),
         jday=yday(dod))%>%
  mutate(study_period=case_when(
    between(jday,321,356)~"EW 2019",
    between(jday,356,365)~"MW 2019",
    between(jday,1,26)~"MW 2019",
    between(jday,26,63)~"LW 2019",
    between(jday,64,166)~"SS 2019"
  ))
c15_19$id<-gsub("MTN LION", "", as.character(c15_19$id))
c15_19$id<-gsub("MT LION", "", as.character(c15_19$id))
c15_19$id<-gsub("MT. LION", "", as.character(c15_19$id))

#reformat demographics data
stats<-stats_dir%>%
  select(ID,Sex,`Age Class`,`Reproductive Status`,`Study Period`,Year)%>%
  mutate(Sex=ifelse(Sex=="Male","M","F"))
colnames(stats)<-c("id","coug_sex","coug_age","kits","study_period","year")

#join demographics and full data
c15_19$id<-gsub(" ", "", as.character(c15_19$id)) #remove space from id
full_dir<-left_join(c15_19,stats)%>%
  mutate(species=case_when(
    species=="Elk"~"elk",
    species=="Deer (unknown)"~"unk deer",
    species=="Deer (mule)"~"mule deer",
    species=="Deer (whitetailed)"~"wt deer",
    species=="Porcupine"~"porcupine",
    species=="Bighorn sheep"~"bh sheep",
    species=="Other"~"other",
    species=="MULE DEER"~"mule deer",
    species=="ELK"~"elk",
    species=="PORCUPINE"~"porcupine"))%>%
  mutate(age=case_when(
    age=="Calf/fawn"~"fawn",
    age=="Unknown"~"unk",
    age=="Yearling"~"yearling",
    age=="Old adult (10+ yrs)"~"old adult",
    age=="Adult"~"adult",
    age=="FAWN"~"fawn",
    age=="ADULT"~"adult",
    age=="YEARLING"~"yearling",
    age=="CALF"~"fawn"
  ))
full_dir$utm_e<-gsub("0532908", "532908",
                    as.character(full_dir$utm_e))
#still need to define early/mid/late winter
