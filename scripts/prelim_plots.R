#preliminary plots

dir<-read_csv(here("data/all.csv"))
species<-dir%>%
  subset(dir$species=="elk" | dir$species=="mule deer" |
           dir$species=="")%>%
  group_by(season,species)%>%
  summarize(nobs=n())

age<-dir%>%
  subset(dir$species=="elk" | dir$species=="mule deer")%>%
  group_by(season,species,age)%>%
  summarize(nobs=n())

age_jday<-dir%>%
  subset(dir$species=="elk" | dir$species=="mule deer")%>%
  group_by(jday,age,season)%>%
  summarize(nobs=n())

ggplot(age)+
  geom_bar(aes(x=season,y=nobs,fill=species),stat="identity",
           position=position_dodge(0.9))

ggplot(age)+
  geom_bar(aes(x=season,y=nobs,fill=age),stat="identity",
           position=position_dodge(0.9))

ggplot(age_jday)+
  geom_bar(aes(x=jday,y=nobs,fill=age),stat="identity",
              position=position_dodge(0.9))

ggplot(age_jday%>%
         filter(season=="winter"))+
  geom_tile(aes(x=age,y=jday,fill=nobs),stat="identity",
           position=position_dodge(0.9))
