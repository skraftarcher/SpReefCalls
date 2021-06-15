#this is a script that makes a function to change file paths in raven selection tables
#written by Stephanie K. Archer

# load (install if necessary) required packages

if(!require(Rraven))install.package("Rraven");library(Rraven)
if(!require(warbleR))install.packages("warbleR");library(warbleR)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# now write the function

change.path<-function(inpath,soundpath,outpath){
  rvn.dat <- imp_raven(warbler.format =  TRUE, path = inpath)
  if(grepl("/",rvn.dat$sound.files[1],fixed=TRUE)){
    rvn.dat<-rvn.dat%>%
      separate(sound.files,into=c("pre","sound.files"),sep="/")
  }
  exp_raven(rvn.dat2, sound.file.path = soundpath, path = outpath)
}


