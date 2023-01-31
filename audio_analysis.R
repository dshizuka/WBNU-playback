#importing and analyzing audio data

#R packages to load
library(lubridate)
library(stringr)
library(tidyverse)

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
feb_filename_short=str_sub(feb_filenames, start=1, end=9)

febdat=data.frame(filename=feb_filename_short, no.bouts=no.bouts)
febdat

### now do the same thing with november data
nov.data=lapply(list.files("Data/November audio data", full.names=T), function(x) read.table(x, sep="\t", header=T))
nov.no.bouts=sapply(feb.data, function(x) nrow(x)-1)
nov_filename_short=str_sub(list.files("Data/November audio data"), start=1, end=9)
novdat=data.frame(filename=nov_filename_short, no.bouts=nov.no.bouts)
novdat

audio.data=bind_rows(febdat, novdat)

#now import behavior data and then combine it with the audio data
behavior.data=read.csv("Data/Thesis behavior data combined.csv", na.strings="N/A")
behavior.data

#merge the behavior and audio data
global.data=left_join(behavior.data, audio.data, by=c("Audio.code"="filename"))

global.data
#write.csv(dat,"feb_data.csv")

names(global.data)
global.data$Treatment
