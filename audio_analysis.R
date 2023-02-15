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

#calculate bout lengths for a trial
feb.data[[1]]$End.Time..s.-feb.data[[1]]$Begin.Time..s.

#calculate mean bout lengths for a trial
mean(feb.data[[1]]$End.Time..s.-feb.data[[1]]$Begin.Time..s.)

#calculate mean bout length for all data after removing the first line
mean.bout.feb=sapply(feb.data, function(x) mean(x$End.Time..s.[-1]-x$Begin.Time..s.[-1]))

#extract metadata from file names
feb_filenames=list.files("Data/February audio data")
feb_filename_short=str_sub(feb_filenames, start=1, end=9)

febdat=data.frame(filename=feb_filename_short, no.bouts=no.bouts, mean.bout.length=mean.bout.feb)
febdat

### now do the same thing with november data
nov.data=lapply(list.files("Data/November audio data", full.names=T), function(x) read.table(x, sep="\t", header=T))
nov.no.bouts=sapply(nov.data, function(x) nrow(x)-1)
mean.bout.nov=sapply(nov.data, function(x) mean(x$End.Time..s.[-1]-x$Begin.Time..s.[-1]))
nov_filename_short=str_sub(list.files("Data/November audio data"), start=1, end=9)
novdat=data.frame(filename=nov_filename_short, no.bouts=nov.no.bouts, mean.bout.length=mean.bout.nov)
novdat

audio.data=bind_rows(febdat, novdat)

#now import behavior data and then combine it with the audio data
behavior.data=read.csv("Data/Thesis behavior data combined.csv", na.strings="N/A")
behavior.data

#merge the behavior and audio data
global.data=left_join(behavior.data, audio.data, by=c("Audio.code"="filename"))
global.data=global.data %>% mutate(season=(month(mdy(DATE))>5)+1)

global.data
#write.csv(dat,"feb_data.csv")

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
fit_HD=aov(HD~Treatment+factor(season), data=global.data)
fit_VD=aov(VD~Treatment+factor(season), data=global.data)
fit_bout=aov(no.bouts~Treatment+factor(season), data=global.data)
fit_DICDV=aov(DICDV_1~Treatment, data=global.data)
fit_DIAH=aov(DIAH_1~Treatment, data=global.data)
fit_boutlength=aov(mean.bout.length~Treatment+factor(season), data=global.data)
summary(fit_HD)
summary(fit_VD)
summary(fit_bout)
summary(fit_DICDV)
summary(fit_DIAH)
summary(fit_boutlength)

fit_bout=aov(no.bouts~Treatment+factor(season), data=global.data)
summary(fit_bout)

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
p=ggplot(data=global.data, aes(x=Treatment, y=HD)) +
  geom_boxplot() +
  theme_classic() 
p

#playing around with different ggplots:
q=ggplot(data=global.data, aes(x=HD, fill=Treatment)) +
  geom_histogram(alpha=0.7, position='identity', color="black") +
  scale_fill_manual(values=c("red", "yellow", "blue")) +
  facet_grid(rows=vars(Treatment)) +
  theme_classic()
q

ggplot(data=global.data, aes(x=HD, fill=Treatment)) +
  geom_density(alpha=0.5) +
  theme_classic()

ggplot(data=global.data, aes(x= Treatment, y=HD, fill=Treatment)) +
  geom_violin() +
  scale_fill_brewer(palette="Blues") +
  ylab("Horizontal Approach Distance")

HDplot=ggplot(data=global.data, aes(x= Treatment, y=HD, fill=Treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette="RdYlBu") +
  ylab("Horizontal Approach Distance (m)")

HDplot

HDplot + scale_x_discrete(name ="Treatment", 
                     limits=c("Control","Low","High"))


boxplot(mean.bout.length~Treatment, data=global.data)

