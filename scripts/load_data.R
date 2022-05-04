#reformat og datasets
library(here)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)

#load data
c19_dir<-read_excel(here("data/2019_Carcass_Data_Marzluff.xlsx"))
c15_dir<-read_excel(here("data/CougarKills_2015.2017.xlsx"))
stats_dir<-read_csv(here("data/cougardemostats.csv"))
kill_dir<-read_excel(here("data/kills2009-2021.xlsx"))

#filter for coug and reformat data
c19<-c19_dir%>%
  subset(c19_dir$`KILL TYPE`=="COUGAR KILL")%>%
  select(`Kill #`,SPECIES,DOD,`GROUND EAST`,`GROUND NORTH`,
         `WOLVES PRESENT`)
colnames(c19)<-c("kill_num","species","dod","utm_e","utm_n","id")
c19$utm_e<-as.character(c19$utm_e)
c19$utm_n<-as.character(c19$utm_n)

c15<-c15_dir%>%
  select(`Observation ID`,`Prey Species`,`Date of Death`,
         `UTM Easting`,`UTM Northing`)
colnames(c15)<-c("kill_num","species","dod","utm_e","utm_n")
c15$kill_num<-as.character(c15$kill_num)

kill<-kill_dir%>%
  subset(kill_dir$`KILL TYPE`=="COUGAR KILL")%>%
  select(Kill,SPECIES,DOD,`GROUND EAST`,`GROUND NORTH`,
         `WOLVES PRESENT`)%>%
  mutate(id=ifelse(`WOLVES PRESENT`=="M211 (MT. LION)","M211",""))%>%
  select(-`WOLVES PRESENT`)
colnames(kill)<-c("kill_num","species","dod","utm_e","utm_n","id")
kill$utm_e<-as.character(kill$utm_e)
kill$utm_n<-as.character(kill$utm_n)

#combine data tables
all<-bind_rows(c15,c19,kill)
all<-all[complete.cases(all[,1:5]), ] #remove NA
all$dod<-gsub("2107", "2017",as.character(all$dod))
all$dod<-parse_date_time(all$dod,orders = c('ymd','dmy'),tz="")
all$species<-tolower(all$species)
all.full<-all%>%
  mutate(year=year(dod),
         month=month(dod),
         day=day(dod),
         jday=yday(dod))%>%
  mutate(species=ifelse(species=="deer (mule)","mule deer",species))%>%
  mutate(season=case_when(
    between(jday,355,365)~"winter",
    between(jday,1,79)~"winter",
    between(jday,80,171)~"spring",
    between(jday,172,265)~"summer",
    between(jday,266,354)~"fall"))%>%
  filter(species=="mule deer" | species=="elk")%>%
  filter(season=="winter")
all.full$id<-gsub("MTN LION", "", as.character(all.full$id))
all.full$id<-gsub("MT LION", "", as.character(all.full$id))
all.full$id<-gsub("MT. LION", "", as.character(all.full$id))

#reformat demographics data
stats<-stats_dir%>%
  select(ID,Sex)%>%
  mutate(Sex=ifelse(Sex=="Male","M","F"))
colnames(stats)<-c("id","coug_sex")
stats<-distinct(stats,id,.keep_all = TRUE)

#join demographics and full data
all.full$id<-gsub(" ", "", as.character(all.full$id)) #remove space from id
full_dir<-left_join(all.full,stats)
full_dir$utm_e<-gsub("0532908", "532908",
                    as.character(full_dir$utm_e))
