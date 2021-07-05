#this is a script that makes a function to change file paths in raven selection tables
#written by Stephanie K. Archer

# load (install if necessary) required packages

if(!require(Rraven))install.package("Rraven");library(Rraven)
if(!require(warbleR))install.packages("warbleR");library(warbleR)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# now write the function

change.path<-function(inpath,soundpath,outpath){
  rvn.dat <- imp_raven(warbler.format =  FALSE, all.data=TRUE,path = inpath)
  if(nchar(rvn.dat$`Begin File`[1])!=nchar(rvn.dat$`Begin Path`[1])){
    rvn.dat<-rvn.dat%>%
      separate(`Begin Path`,into=c("pre","Begin Path"),sep=-nchar(rvn.dat$`Begin File`[1]))%>%
      select(-pre)
  }
  rvn.dat<-rvn.dat%>%
    mutate(`Begin Path`=paste0(soundpath,"/",`Begin Path`))
  x<-unique(rvn.dat$selec.file)
  for(i in 1:length(x)){
  write.table(rvn.dat%>%
                filter(selec.file==x[i])%>%
                select(-selec.file),file = paste0(outpath,"/",x[i],"_uptodate.txt"), 
              sep = "\t", row.names = FALSE, quote = FALSE)
  }
}


