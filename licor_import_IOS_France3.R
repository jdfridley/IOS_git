#create master licor dataset
#NSF IOS project
#Fridley Jan 2022

#library(data.table)
library(readxl)
library(stringr)
library(googlesheets4)

#sample master downloaded from google
master = read_sheet("https://docs.google.com/spreadsheets/d/1QLL5AUeP-fHar0HRfDjDP0Mw25FvowqidiCyEbBA5og/edit#gid=0")
#master = read.csv("/Users/fridley/Documents/academic/projects/IOS_FranceJapan/NSF-IOS spreadsheet_Jan24.csv")
str(master)
unique(master$Species)
unique(master$Sampling.Date)
unique(master$Site.final)

#folder of licor files ***use the current France folder on as-fridley (correct files and filenames)
folder.local = "/Users/fridley/Documents/academic/projects/IOS_FranceJapan/licor_files/LICOR data files_France/" #downloaded after Robert's fixes, 2-8-22
files = list.files(folder.local)
length(files)

#compare files to spreadsheet filenames
ena.labels = master$LICOR.file.in.handENA[master$Region=="France"]
  #length = 150
#xls files saved as xlsx files; add 'x' to end of spreadsheet label names
#for(i in 1:length(ena.labels)) {
#  if(str_sub(ena.labels[i],start=-3)  == "xls") ena.labels[i] = paste0(ena.labels[i],"x")
#}
setdiff(ena.labels,files)
  #all good

#file output types:
#1. 8 .xls files (do separately)
#2. tab-delim text files

#output
out = NULL

#loop over each file, examine, and add to 'out'
for(i in i:length(files)) {

#sample information to include in licor dataset
date = master$Sampling.Date[master$LICOR.file.in.handENA==files[i]]
site = master$Site.final[master$LICOR.file.in.handENA==files[i]]
species = master$Species[master$LICOR.file.in.handENA==files[i]]
code = master$Species.abbreviation[master$LICOR.file.in.handENA==files[i]]

if(str_sub(files[i],start=-3)  == "csv") { #csv files
dat = read.table(paste0(folder.local,files[i]),header=F,blank.lines.skip=F,sep="\t")
start = which(substr(dat[,1],1,3)=="Obs") - 1
dat = read.csv(paste0(folder.local,files[i]),header=T,skip=start)
dat = dat[(!is.na(dat$FTime)),] #remove rows of no data
dat$filename = files[i] #append column of filename
dat$date = date
dat$site = site
dat$species = species
dat$sppcode = code
#names(dat) = names(out)
par(mfrow=c(1,2))
plot(dat$PARi,dat$Photo,pch=19,cex=1.3,main="A-q")
plot(dat$Ci,dat$Photo,pch=19,cex=1.3,main="A-Ci")
mtext(files[i],at=-300,line=0)
out = rbind(out,dat)
readline() #uncomment if you want to inspect each curve upon import
}
  
if(gregexpr(".",files[i],fixed=T)<0) { #tab delim text files
  dat = read.table(paste0(folder.local,files[i]),header=F,blank.lines.skip=F,sep="\t")
  start = which(substr(dat[,1],1,3)=="Obs") - 1
  dat = read.csv(paste0(folder.local,files[i]),header=T,skip=start,sep="\t")
  dat = dat[(!is.na(dat$FTime)),] #remove rows of no data
  dat$filename = files[i] #append column of filename
  dat$date = date
  dat$site = site
  dat$species = species
  dat$sppcode = code
  names(dat) = names(out)
  par(mfrow=c(1,2))
  plot(dat$PARi,dat$Photo,pch=19,cex=1.3,main="A-q")
  plot(dat$Ci,dat$Photo,pch=19,cex=1.3,main="A-Ci")
  mtext(files[i],at=-300,line=0)
  out = rbind(out,dat)
  readline() #uncomment if you want to inspect each curve upon import
}

if(str_sub(files[i],start=-4) == "xlsx" | str_sub(files[i],start=-4) == "xls" ) { #excel files
dat = read_excel(paste0(folder.local,files[i]),col_names=FALSE)
start = which(dat[,1]=="Obs") - 1
dat = as.data.frame(read_excel(paste0(folder.local,files[i]),col_names=TRUE,skip=start))
dat = dat[-1,]
dat = dat[(!is.na(dat$FTime)),] #remove rows of no data
dat[,c(1,3:58)] = apply(dat[,c(1,3:58)], 2, function(x) as.numeric(as.character(x))) #change character to numeric (all except HHMMSS)
dat$filename = files[i] #append column of filename
dat$date = date
dat$site = site
dat$species = species
dat$sppcode = code
par(mfrow=c(1,2))
plot(dat$PARi,dat$Photo,pch=19,cex=1.3,main="A-q")
plot(dat$Ci,dat$Photo,pch=19,cex=1.3,main="A-Ci")
mtext(files[i],at=-300,line=0)
out = rbind(out,dat)
readline() #uncomment if you want to inspect each curve upon import
}
}

#write.csv(out,file="/Users/fridley/Documents/academic/projects/IOS_FranceJapan/licor_files/ENA_licor.csv")
