#importing and analyzing audio data

#R packages to load
library(lubridate)
library(stringr)
library(tidyverse)

#file organization
#path names to february selection tables
list.files("Data/audio data_DS", full.names=T)


#now import all the february selection tables using lapply()
dat.list=lapply(list.files("Data/audio data_DS", full.names=T), function(x) read.table(x, sep="\t", header=T))

#calculate number of selections
no.bouts=sapply(dat.list, function(x) nrow(x)-1)

#extract metadata from file names
filenames=list.files("Data/audio data_DS")
filename_short=str_sub(filenames, start=1, end=9)

#calculate mean bout length for all data after removing the first line
mean.bout=sapply(dat.list, function(x) mean(x$End.Time..s.[-1]-x$Begin.Time..s.[-1]))
sum.dat=list()
tot.notes=vector(length=length(dat.list))
for(i in 1:length(dat.list)){
  sum.dat[[i]]=as.data.frame(dat.list[[i]] %>% group_by(Call) %>% summarise(n.notes=sum(Note.Number, na.rm=T)) %>% mutate(trial=filename_short[i]) %>% pivot_wider(id_cols=trial,names_from=Call, values_from=n.notes))
  if(is.null(sum.dat[[i]]$double)==F) sum.dat[[i]]$double=sum.dat[[i]]$double/2
  tot.notes[i]=sum(sum.dat[[i]][1,2:ncol(sum.dat[[i]])], na.rm=T)
}
sum.dat
tot.notes


audio.dat=data.frame(filename=filename_short, no.bouts=no.bouts, mean.bout.length=mean.bout, tot.notes=tot.notes)
audio.dat


#now import behavior data and then combine it with the audio data
behavior.data=read.csv("Data/Thesis behavior data combined.csv", na.strings="N/A")
behavior.data

#merge the behavior and audio data
global.data=left_join(behavior.data, audio.dat, by=c("Audio.code"="filename"))

global.data


names(global.data)
global.data$Treatment
global.data$HD
global.data$VD
global.data$no.bouts

#changing vertical Y/N data into numeric to look at significance
DICDV_1=global.data$DICDV
DICDV_1[DICDV_1=="Y"]=1
DICDV_1[DICDV_1=="N"]=0
DICDV_1=as.numeric(DICDV_1)
DICDV_1

DIAH_1=global.data$DIAH
DIAH_1[DIAH_1=="Y"]=1
DIAH_1[DIAH_1=="N"]=0
DIAH_1=as.numeric(DIAH_1)
DIAH_1
#sum(DDIAH~global.data$Treatment)


#ANOVA for HD
fit_HD=aov(HD~Treatment, data=global.data)
fit_VD=aov(VD~Treatment, data=global.data)
fit_bout=aov(no.bouts~Treatment, data=global.data)
fit_DICDV=aov(DICDV_1~Treatment, data=global.data)
fit_DIAH=aov(DIAH_1~Treatment, data=global.data)
fit_boutlength=aov(mean.bout.length~Treatment, data=global.data)
summary(fit_HD)
summary(fit_VD)
summary(fit_bout)
summary(fit_DICDV)
summary(fit_DIAH)
summary(fit_boutlength)

#Post-hoc comparisons (Tukey Honest Significant Differences test)
TukeyHSD(fit_HD)
TukeyHSD(fit_VD)
TukeyHSD(fit_bout)
TukeyHSD(fit_DICDV)
TukeyHSD(fit_DIAH)
TukeyHSD(fit_boutlength)

#Boxplot for HD, base R way:
boxplot(HD~Treatment, data=global.data)

#Boxplot for HD, ggplot way:
p=ggplot(data=global.data, aes(x=Treatment, y=tot.notes)) +
  geom_boxplot() +
  theme_classic() 
p

fit_notes=aov(tot.notes~Treatment, data=global.data)
summary(fit_notes)
TukeyHSD(fit_notes)

plot(global.data$HD, global.data$tot.notes, xlim=c(0,20), col=factor(global.data$Treatment))
