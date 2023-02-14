#importing and analyzing audio data

#R packages to load
library(lubridate)
library(stringr)
library(tidyverse)

#file organization
#path names to february selection tables
list.files("Data/audio data_new", full.names=T)


#now import all the february selection tables using lapply()
dat.list=lapply(list.files("Data/audio data_new", full.names=T), function(x) read.table(x, sep="\t", header=T, na.strings=c("NA")))

#calculate number of selections
no.bouts=sapply(dat.list, function(x) nrow(x)-1)

#extract metadata from file names
filenames=list.files("Data/audio data_new")
filename_short=str_sub(filenames, start=1, end=9)

#note types that exist
notetypes=c("quank", "double", "rapid", "wurp", "squeak")

#calculate mean bout length for all data after removing the first line
mean.bout=sapply(dat.list, function(x) mean(x$End.Time..s.[-1]-x$Begin.Time..s.[-1]))
sum.dat=list()
tot.notes=vector(length=length(dat.list))

for(i in 1:length(dat.list)){
  dat.list[[i]]$filename=filename_short[i]
}

dat.comb=bind_rows(dat.list)
dat.comb$Call=gsub("Quank", "quank", dat.comb$Call) #replace capitalization
dat.comb$Call=gsub("Double", "double", dat.comb$Call)
dat.comb$Call=gsub("Squeak", "squeak", dat.comb$Call)
dat.comb$Call=gsub("quank ", "quank", dat.comb$Call)
dat.comb$Call=gsub("double ", "double", dat.comb$Call)
table(dat.comb$Call)

#gather all the data into a clean dataset
audio.dat=dat.comb %>% group_by(filename, Call) %>% summarise(n=sum(Note.Number)) %>% pivot_wider(id_cols=filename,names_from=Call, values_from=n) %>% replace_na(list(double=0, quank=0, wurp=0, rapid=0, squeak=0)) %>% select(-Playback) %>% mutate(double=double/2)

audio.dat


#now import behavior data and then combine it with the audio data
behavior.data=read.csv("Data/Thesis behavior data combined.csv", na.strings="N/A")
behavior.data

#merge the behavior and audio data
global.data=left_join(behavior.data, audio.dat, by=c("Audio.code"="filename"))
global.data=global.data %>% mutate(season=(month(mdy(DATE))>5)+1) %>% mutate(tot.notes=rowSums(.[14:18])) %>% mutate(tot.quanks=rowSums(.[c(14, 15,17)]))
global.data

write.csv(global.data, "global.data_230214.csv")

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

#Boxplot for HD, looking at seasonal difference
p=ggplot(data=global.data, aes(x=Treatment, y=HD)) +
  geom_boxplot() +
  facet_wrap(~season) +
  theme_classic() 
p

fit_HD=aov(HD~Treatment+factor(season), data=global.data)
summary(fit_HD)
TukeyHSD(fit_HD)

#looking at number of quanks produced by treatment and season
p=ggplot(data=global.data, aes(x=Treatment, y=tot.quanks)) +
  geom_boxplot() +
  facet_wrap(~season) +
  theme_classic() 
p

fit_quanks=aov(tot.quanks~Treatment+factor(season), data=global.data )
summary(fit_quanks)
TukeyHSD(fit_quanks)

fit_quanks_s1=aov(tot.quanks~Treatment, data=global.data %>% filter(season==1))
summary(fit_quanks_s1)
TukeyHSD(fit_quanks_s1)


fit_quanks_s2=aov(tot.quanks~Treatment, data=global.data %>% filter(season==2))
summary(fit_quanks_s2)
TukeyHSD(fit_quanks_s2)

fit_quanks=aov(tot.quanks~Treatment, data=global.data )
summary(fit_quanks)
TukeyHSD(fit_quanks)
#looking at number of quanks produced by treatment and season
p=ggplot(data=global.data, aes(x=Treatment, y=double)) +
  geom_boxplot() +
  facet_wrap(~season) +
  theme_classic() 
p


#is there a relationship between approaching and vocalizing

plot(tot.quanks~HD, data=global.data, xlim=c(0,20), col=season, pch=19)
