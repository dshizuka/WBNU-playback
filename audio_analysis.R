#importing and analyzing audio data

#R packages to load
library(lubridate)
library(stringr)

#file organization
#path names to february selection tables
list.files("Data/February audio data", full.names=T)

#import just the first data
feb.data1=read.table(list.files("Data/February audio data", full.names=T)[1], sep="\t", header=T)

feb.data1

#now import all the february selection tables using lapply()
feb.data=lapply(list.files("Data/February audio data", full.names=T), function(x) read.table(x, sep="\t", header=T))

feb.data

#calculate number of selections
no.bouts=sapply(feb.data, function(x) nrow(x)-1)

#extract metadata from file names
feb_filenames=list.files("Data/February audio data")
trial=str_sub(feb_filenames, start=1, end=2)
month=str_sub(feb_filenames, start=4, end=6)
treatment=str_sub(feb_filenames, start=8, end=9)

dat=data.frame(trial, month, treatment, no.bouts)
dat

write.csv(dat,"feb_data.csv")
