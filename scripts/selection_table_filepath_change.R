#this is a script that makes a function to change file paths in raven selection tables
#written by Stephanie K. Archer

# load (install if necessary) required packages

if(!require(Rraven))install.package("Rraven");library(Rraven)
if(!require(warbleR))install.packages("warbleR");library(warbleR)

# now write the function

change.path<-function(inpath,soundpath,outpath){
  rvn.dat <- imp_raven(warbler.format =  TRUE, path = inpath)
  exp_raven(rvn.dat, sound.file.path = soundpath, path = outpath)
}


