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
  # mutate(species=case_when(
  #   species=="Elk"~"elk",
  #   species=="Deer (unknown)"~"unk deer",
  #   species=="Deer (mule)"~"mule deer",
  #   species=="Deer (whitetailed)"~"wt deer",
  #   species=="Porcupine"~"other",
  #   species=="Bighorn sheep"~"bh sheep",
  #   species=="Other"~"other",
  #   species=="MULE DEER"~"mule deer",
  #   species=="ELK"~"elk",
  #   species=="PORCUPINE"~"other",
  #   species=="MARMOT"~"other",
  #   species=="RED FOX"~"other",
  #   species=="DEER"~"unk deer",
  #   species=="COTTONTAIL RABBIT"~"other",
  #   species=="WHITE-TAILED DEER"~"wt deer",
  #   species=="GROUSE"~"other",
  #   species=="YELLOW-BELLIED MARMOT"~"other",
  #   species=="PRONGHORN"~"pronghorn",
  #   species=="COYOTE"~"other",
  #   species=="COUGAR"~"other",
  #   species=="DEER SPP"~"unk deer",
  #   species=="BEAVER"~"other",
  #   species=="BIGHORN SHEEP"~"bh sheep",
  #   species=="MOUNTAIN GOAT"~"other",
  #   species=="UNKNOWN"~"unk",
  #   species=="GROUND SQIRREL"~"other",
  #   species=="FOX"~"other",
  #   species=="YELLOW-RUMPED WARBLER"~"other",
  #   species=="UNKNOWN SMALL MAMMAL"~"other",
  #   species=="MOUNTAIN COTTONTAIL"~"other"))%>%
  # mutate(age=case_when(
  #   age=="Calf/fawn"~"young",
  #   age=="Unknown"~"unk",
  #   age=="Yearling"~"yearling",
  #   age=="Old adult (10+ yrs)"~"old adult",
  #   age=="Adult"~"adult",
  #   age=="FAWN"~"young",
  #   age=="ADULT"~"adult",
  #   age=="YEARLING"~"yearling",
  #   age=="CALF"~"young",
  #   age=="OLD ADULT"~"old adult",
  #   age=="UNKNOWN"~"unk",
  #   age=="CALF/FAWN"~"young",
  #   age=="KID"~"young",
  #   age=="LAMB"~"young",
  #   age=="KIT"~"young",
  #   age=="PUP"~"young"))%>%
  mutate(season=case_when(
    between(jday,355,365)~"winter",
    between(jday,1,79)~"winter",
    between(jday,80,171)~"spring",
    between(jday,172,265)~"summer",
    between(jday,266,354)~"fall"))%>%
  filter(species=="mule deer" | species=="elk")%>%
  filter(season=="winter")
  # mutate(study_period=case_when(
  #   between(jday,321,356)~"EW 2019",
  #   between(jday,356,365)~"MW 2019",
  #   between(jday,1,26)~"MW 2019",
  #   between(jday,26,63)~"LW 2019",
  #   between(jday,64,166)~"SS 2019"
  # ))
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
  # mutate(species=case_when(
  #   species=="Elk"~"elk",
  #   species=="Deer (unknown)"~"unk deer",
  #   species=="Deer (mule)"~"mule deer",
  #   species=="Deer (whitetailed)"~"wt deer",
  #   species=="Porcupine"~"porcupine",
  #   species=="Bighorn sheep"~"bh sheep",
  #   species=="Other"~"other",
  #   species=="MULE DEER"~"mule deer",
  #   species=="ELK"~"elk",
  #   species=="PORCUPINE"~"porcupine"))%>%
  # mutate(age=case_when(
  #   age=="Calf/young"~"young",
  #   age=="Unknown"~"unk",
  #   age=="Yearling"~"yearling",
  #   age=="Old adult (10+ yrs)"~"old adult",
  #   age=="Adult"~"adult",
  #   age=="young"~"young",
  #   age=="ADULT"~"adult",
  #   age=="YEARLING"~"yearling",
  #   age=="CALF"~"young"
  # ))
full_dir$utm_e<-gsub("0532908", "532908",
                    as.character(full_dir$utm_e))