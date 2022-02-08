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

#folder of licor files
folder.local = "/Users/fridley/Documents/academic/projects/IOS_FranceJapan/licor_files/LICOR data files_ENA/" #downloaded after Robert's fixes, 2-8-22
files = list.files(folder.local)
length(files)

#compare files to spreadsheet filenames
ena.labels = master$LICOR.file.in.hand[master$Region=="ENA"]
  #length = 150
setdiff(ena.labels,files)
files[is.element(files,master$LICOR.file.in.hand)==F]
  # these names were fixed on 2-8-22: "210808_s27_bifro.xls"          "combined_190625_S6_Bethu.xlsx" "combined_190625_S6_ceorb.xlsx"
  #ALL GOOD, make sure to ignore names that start with 'MISSING'

#file output types:
#1. 2019 csv files: standard csv with no headers
#2. 2021 text files (2): 6800 output, tab delim, investigate
#3. 2000 text files (8): 6400 output, tab delim, std header info
#the rest are apparently all the same:
#4. 2019 xls (many): 6400 output, pain with header info and remarks to omit
#5. 2000 xls (many): 6400 output, pain with header info and remarks to omit
#6. 2001 xls (~10): 6400 output, pain with header info and remarks to omit
#7. "rob" - (many, 2021?): 6400 output, pain with header info and remarks to omit
#8. "combined" - (8, 2019): 6400 output, pain with header info and remarks to omit


#output
out = NULL

#loop over each file, examine, and add to 'out'
for(i in 1:length(files)) {

#sample information to include in licor dataset
date = master$Sampling.Date[master$LICOR.file.in.hand==files[i]]
site = master$Site.final[master$LICOR.file.in.hand==files[i]]
species = master$Species[master$LICOR.file.in.hand==files[i]]
code = master$Species.abbreviation[master$LICOR.file.in.hand==files[i]]

if(str_sub(files[i],start=-3)  == "csv") { #csv files
dat = read.table(paste0(folder.local,files[i]),header=F,blank.lines.skip=F,sep="\t")
start = which(substr(dat[,1],1,3)=="Obs") - 1
dat = read.csv(paste0(folder.local,files[i]),header=T,skip=start)
dat = dat[(!is.na(dat$Time)),] #remove rows of no data
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
#readline() #uncomment if you want to inspect each curve upon import
}
  
if(str_sub(files[i],start=-4) == "xlsx" | str_sub(files[i],start=-4) == "xls" ) { #excel files
dat = read_excel(paste0(folder.local,files[i]),col_names=FALSE)
start = which(dat[,1]=="Obs") - 1
dat = as.data.frame(read_excel(paste0(folder.local,files[i]),col_names=TRUE,skip=start))
dat = dat[(!is.na(dat$Time)),] #remove rows of no data
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
#readline() #uncomment if you want to inspect each curve upon import
}
}

#write.csv(out,file="/Users/fridley/Documents/academic/projects/IOS_FranceJapan/licor_files/Japan_licor.csv")
