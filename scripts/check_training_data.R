# Function to compare training dataset with key

#load packages
library(tidyverse)
library(Rraven)

#bring in datasets
ttall<-imp_raven(path="odata",name.from.file = T,all.data = T,ext.case = "lower")[,-24]

unique(ttall$selec.file)

ttcheck<-ttall%>%
  filter(selec.file=="1342218252.190411100155.wav.txt")%>%
  select(Selection,`Begin File`,`Begin Time (s)`,`End Time (s)`,`Delta Time (s)`,`Low Freq (Hz)`,
         `High Freq (Hz)`, `File Offset (s)`,SPL,Class,Type,comments)

key<-ttall%>%
  filter(selec.file=="training_table_key.txt")%>%
  select(`File Offset (s)`,`Delta Time (s)`,`Low Freq (Hz)`,`High Freq (Hz)`,detector.class=Class,
         m.class=`Manual Class`,m.type=`Manual Type`)%>%
  mutate(detector.class=case_when(
    detector.class=="FS"~"F",
    detector.class=="NN"~"N"),
    m.class=case_when(
      m.class=="FS"~"F",
      m.class=="F"~"F",
      m.class=="N"~"N"))

# check to make sure no typos that would cause a mismatch
unique(ttcheck$Class)
unique(ttcheck$Type)
unique(key$m.class)
unique(key$detector.class)
unique(key$m.type)

#create a joined dataset and look at matches
tt<-left_join(ttcheck,key)%>%
  filter(!is.na(m.class))%>%
  mutate(
    Class=ifelse(Type=="N","N","F"),
    m.match=case_when(
    Class==m.class~"match",
    Class=="F" & m.class=="FS"~'match',
    Class=="N" & m.class=="FS"~'match',
    Class!=m.class~"no.match"),
    d.match=case_when(
      Class==detector.class~"match",
      Class=="F" & detector.class=="FS"~'match',
      Class=="N" & detector.class=="FS"~'match',
      Class!=detector.class~"no.match"),
    t.match=case_when(
      Type==m.type~"match",
      Type=="N" & m.type==""~'match',
      Type!=m.type~"no.match"),    
    dm.match=case_when(
        m.class==detector.class~"match",
        m.class=="F" & detector.class=="FS"~'match',
        m.class=="N" & detector.class=="FS"~'match',
        m.class!=detector.class~"no.match"),
    cm=paste0(Class,".",m.class),
    cm=factor(cm,levels=c("F.F","N.N","F.N","N.F")))

# Look at how we match up

ttfsum<-tt%>%
  group_by(Class,m.class,detector.class)%>%
  summarize(ncalls=n())

ggplot(data=ttfsum)+
  geom_bar(aes(x=Class,y=ncalls),stat="identity",position = position_dodge())+
  facet_grid(~m.class)+
  ggtitle("You vs me")+
  xlab("What you called it")+
  ylab("number of calls")


ggplot(data=ttfsum)+
  geom_bar(aes(x=Class,y=ncalls),stat="identity",position = position_dodge())+
  facet_grid(m.class~detector.class)+
  ggtitle("What the detector called it")+
  xlab("What you called it")+
  ylab("number of calls")

# write out datasets to look at
tt.dNsNyF<-tt%>%
  filter(detector.class=="N")%>%
  filter(m.class=="N")%>%
  filter(Class=="F")%>%
  mutate(`Begin Path`=paste0("D:/RCA_IN/April_July2019/amplified_10/",`Begin File`))

write.table(tt.dNsNyF,"wdata/detectorNstephNyouY.txt",sep = "\t", row.names = FALSE, quote = FALSE)

tt.nomatch<-tt%>%
  filter(m.match=="no.match")%>%
  filter(!Selection %in% tt.dNsNyF$Selection)%>%
  mutate(`Begin Path`=paste0("D:/RCA_IN/April_July2019/amplified_10/",`Begin File`))

write.table(tt.nomatch,"wdata/nomatch.txt",sep = "\t", row.names = FALSE, quote = FALSE)





