#comparing manual and auto detections

# load packages
source("scripts/install_packages_function.R")# this loads a function to install/load packages
lp(pck="tidyverse")# installs/loads tidyverse
lp(pck="Rraven")
lp(pck="lubridate")
lp(pck="readxl")

#load data
#snake island dataset
si<-imp_raven(path="wdata",#where to find the file
              files="SnakeIslandDectionsToCheck.txt_uptodate.txt", #which file to upload
              all.data = TRUE)# tells imp_raven to bring in all of the columns


#lions bay dataset
lb<-imp_raven(path="wdata",
              files="LionsBayDectionsToCheck.txt_uptodate.txt",
              all.data = TRUE)


#look to see if there are values of class & man.class that need to be corrected
table(si$`Manual Class`)

#there are three observations with no value in the snake island dataset - have Ariel look at these

table(lb$`Manual Class`)
#lions bay is all good

#organize data so that we have how many manual fish calls and how many automatically detected fish calls 
# there are per minute

si2<-si%>%# creates an object called si2 and tells R we want to store the output there
  separate(`Begin File`,into=c("st","dt","ext"),sep=c(-16,-4))%>%#split the begin file variable into 3 columns - the soundtrap ID, the date time info, and the file extension
  mutate(dt=ymd_hms(dt),
         call.dt=dt+`File Offset (s)`,
         yr=year(call.dt),
         mnth=month(call.dt),
         d=day(call.dt),
         hr=hour(call.dt),
         mins=minute(call.dt),
         auto=ifelse(Class=="FS",1,0),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins)%>%
  summarize(auto.calls=sum(auto),man.calls=sum(man))

lb2<-lb%>%
  separate(`Begin File`,into=c("st","dt","ext"),sep=c(-16,-4))%>%
  mutate(dt=ymd_hms(dt),
         call.dt=dt+`File Offset (s)`,
         yr=year(call.dt),
         mnth=month(call.dt),
         d=day(call.dt),
         hr=hour(call.dt),
         mins=minute(call.dt),
         auto=ifelse(Class=="FS",1,0),
         man=ifelse(`Manual Class`=="F",1,0))%>%
  group_by(yr,mnth,d,hr,mins)%>%
  summarize(auto.calls=sum(auto),man.calls=sum(man))

#bring in and organize SPL
#Note: need to calculate SPL for snake island still

lb.spl<-read_xlsx("odata/Lion's Bay_SPL_60sec.xlsx")%>%
  mutate(yr=year(Date),
         mnth=month(Date),
         d=day(Date),
         hr=hour(Time),
         mins=minute(Time))%>%
  select(yr,mnth,d,hr,mins,spl.fish=`20-100Hz`,spl.low=`100-1000Hz`,
         spl.mid=`1-10kHz`,spl.high=`10-48kHz`,spl.broad=`20Hz-48kHz`)

#SPL for snake island
si.spl<-read_rds("odata/snake_island_spl.rds")

#join spl data to call data

lb2<-left_join(lb2,lb.spl)

si2<-left_join(si2,si.spl)

# make plots
theme_set(theme_bw())
theme_update(panel.grid=element_blank())


#Snake Island
(sip<-ggplot(data=si2)+
  geom_jitter(aes(x=man.calls,y=auto.calls,color=spl.broad),size=2,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1))+
    scale_color_viridis_c())

#Lions Bay
(lbp<-ggplot(data=lb2)+
    geom_jitter(aes(x=man.calls,y=auto.calls,color=spl.broad),size=2,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1))+
    scale_color_viridis_c())

# looks like it might work to correct by broadband spl
summary(lm(man.calls~auto.calls+spl.broad,data=si2))
si2$corrected.fish.calls<-predict(lm(man.calls~auto.calls+spl.broad,data=si2))

# see if this worked

(sip2<-ggplot(data=si2)+
    geom_jitter(aes(x=corrected.fish.calls,y=auto.calls,color=spl.broad),size=2,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1))+
    scale_color_viridis_c())


(lbp3<-ggplot(data=lb2)+
    geom_jitter(aes(x=corrected.fish.calls,y=man.calls,color=spl.broad),size=2,alpha=.5)+
    geom_abline(aes(intercept=0,slope=1))+
    scale_color_viridis_c())


# yeah that works pretty well
<<<<<<< HEAD


# distribution plot (average call and average detection duration)
ggplot(data=si,aes(`Delta Time (s)`,fill=`Manual Class`))+
  geom_density(alpha=.5)+
  facet_grid(rows=vars(`Manual Class`))+
  xlim(0,0.5)

# bar graph 
DTPLot<-si%>%
  group_by(`Manual Class`)%>%
  summarise(MDT=mean(`Delta Time (s)`))
ggplot(data=DTPLot,aes(x=`Manual Class`,y=MDT,fill=`Manual Class`))+
  geom_bar(stat='identity')
=======
>>>>>>> 467f943ac4f541fa1c94e21dc8accd8c10c337d1
