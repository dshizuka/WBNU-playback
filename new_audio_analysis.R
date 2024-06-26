#importing and analyzing audio data

#R packages to load
library(lubridate)
library(stringr)
library(tidyverse)

#file organization
#path names to february selection tables
#list.files("Data/2024 audio data", full.names=T)


#now import all the february selection tables using lapply()
dat.list=lapply(list.files("Data/2024 audio data", full.names=T), function(x) read.table(x, sep="\t", header=T, na.strings="NA"))

#dat.list

#calculate number of selections
no.bouts=sapply(dat.list, function(x) nrow(x)-1)
no.bouts

#for each sound, calculate note rate

for(j in 1:length(dat.list)){
  names(dat.list[[j]])=c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.", "call", "note.number")
}

dat.list=lapply(dat.list, function(x) {
  x$note.number[which(x$call=="double"|x$call=="Double")]=x$note.number[which(x$call=="double"|x$call=="Double")]/2
  x$rate=x$note.number/(x$End.Time..s.-x$Begin.Time..s.)
  x})

#extract metadata from file names
filenames=list.files("Data/2024 audio data")
filename_short=str_sub(filenames, start=1, end=16)

#note types that exist
notetypes=c("quank", "double", "rapid", "wurp", "squeak")

for(i in 1:length(dat.list)){
  if(nrow(dat.list[[i]])==0) next else dat.list[[i]]$filename=filename_short[i]
}

#this snip of code will tell us which dataset in the list is empty
which(sapply(dat.list, nrow)==0)

#if no.bouts=== -1 


#now we can tell the bind_rows() function to ignore the dataset that is empty when combining
dat.comb=bind_rows(dat.list[-which(sapply(dat.list, nrow)==0)
])


dat.comb=dat.comb %>% mutate(call=str_replace_all(call, c("Quank"="quank", "Double"="double", "Squeak"="squeak", "quank "="quank", "double "="double")))

#this creates a dataframe with each trial as rows, and then number of bouts of each call type as columns
n_bouts_each=dat.comb %>% group_by(filename, call) %>% summarise(n.bouts=n()) %>% mutate(call=paste(call, "_bout", sep="")) %>% pivot_wider(id_cols=filename, names_from=call, values_from = n.bouts)


#gather all the data into a clean dataset
#added number of bouts of each call type in the global data.
audio.dat=dat.comb %>% group_by(filename, call) %>% summarise(n=sum(note.number), avg.rate=mean(rate)) %>% pivot_wider(id_cols=filename,names_from=call, values_from=c(n, avg.rate)) %>% replace_na(list(n_double=0, n_quank=0, n_wurp=0, n_rapid=0, n_squeak=0)) %>% replace_na(list(avg.rate_double=0, avg.rate_quank=0, avg.rate_wurp=0, avg.rate_rapid=0, avg.rate_squeak=0)) %>% left_join(n_bouts_each) %>% mutate(total_notes=n_double+n_quank+n_rapid+n_squeak+n_wurp)

audio.dat 

quankrate.dat=dat.comb%>% mutate(str_replace_all(call, c("rapid"="quank", "double"="quank"))) %>% filter(call=="quank") %>% group_by(filename) %>% summarise(avg.quankrate=mean(rate)) 

audio.dat = audio.dat %>% left_join(., quankrate.dat)
audio.dat



#now import behavior data and then combine it with the audio data
behavior.data=read.csv("Data/2024 behavior data combined.csv", na.strings="N/A")
behavior.data



#merge the behavior and audio data
global.data=left_join(behavior.data, audio.dat, by=c("Recording"="filename"))
global.data=global.data %>% mutate(tot.notes=rowSums(.[c(16, 17, 18, 19, 20)])) %>% mutate(tot.quanks=rowSums(.[c(16, 17, 18)]))

global.data

write.csv(global.data, "global.data_240426.csv")



names(global.data)
# global.data$Treatment
# global.data$HD
# global.data$VD
# global.data$V.app.d
#global.data$no.bouts


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
#sum(DIAH~global.data$Treatment)

ggplot(data=global.data, aes(x=Treatment, y=avg.rate_quank)) +
  geom_boxplot() +
  theme_classic()

fit=aov(avg.rate_quank~Treatment, data=global.data)
summary(fit)
TukeyHSD(fit)

#ANOVA for HD
fit_HD=aov(HD~Treatment, data=global.data)
fit_VD=aov(VD~Treatment, data=global.data)
#fit_bout=aov(no.bouts~Treatment, data=global.data)
fit_DICDV=aov(DICDV_1~Treatment, data=global.data)
fit_DIAH=aov(DIAH_1~Treatment, data=global.data)
summary(fit_HD)
summary(fit_VD)
summary(fit_bout)
summary(fit_DICDV)
summary(fit_DIAH)

fit=glm(tot.notes~Treatment, data=global.data)
summary(fit)

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
  theme_classic()
p

#Boxplot for HD, looking at seasonal difference
V=ggplot(data=global.data, aes(x=Treatment, y=VD)) +
  geom_boxplot() +
  theme_classic()
V

p=ggplot(data=global.data, aes(x=Treatment, y=n_quank)) +
  geom_point(color=gray(0.3, 0.5)) +
  theme_classic()
p

p=ggplot(data=global.data, aes(x=N_Dnotes, y=n_quank)) +
  geom_point() +
  theme_classic()
p

p=ggplot(data=global.data, aes(x=Treatment, y=tot.quanks)) +
  geom_point() +
  theme_classic()
p

fit_nquank=aov(n_quank~Treatment, data=global.data)
summary(fit_nquank)
TukeyHSD(fit_nquank)

fit_quankbout=aov(quank_bout~Treatment, data=global.data)
summary(fit_quankbout)
TukeyHSD(fit_quankbout)



#fit_HD=aov(HD~Treatment+factor(season), data=global.data)
#summary(fit_HD)
#TukeyHSD(fit_HD)

#fit_bout=aov(no.bouts~Treatment+factor(season), data=global.data)

#TukeyHSD(fit_HD)

#looking at number of quanks produced by treatment and season
p=ggplot(data=global.data, aes(x=Treatment, y=tot.notes)) +
  geom_boxplot() +
  theme_classic() 
p

fit_quanks=aov(tot.quanks~Treatment, data=global.data )
summary(fit_quanks)
TukeyHSD(fit_quanks)



fit_quanks=aov(tot.quanks~Treatment, data=global.data )
summary(fit_quanks)
TukeyHSD(fit_quanks)

fit_quankrate_season=aov(avg.quankrate~Treatment+factor(season), data=global.data)
summary(fit_quankrate_season)

fit_quankrate_seas=aov(mean.bout.length~Treatment+factor(season), data=global.data)
summary(fit_quankrate_seas)

fit_quankrate=aov(avg.quankrate~Treatment, data=global.data)
summary(fit_quankrate)
TukeyHSD(fit_quankrate)

#looking at number of quanks produced by treatment and season
p=ggplot(data=global.data, aes(x=Treatment, y=double)) +
  geom_boxplot() +
  theme_classic() 
p


#is there a relationship between approaching and vocalizing

plot(tot.quanks~HD, data=global.data, xlim=c(0,20), col=season, pch=19)

###PCA

names(global.data)

use.data=global.data %>% select(Recording, Treatment, HD, VD, tot.quanks, n_quank, quank_bout) %>% drop_na()

pr=princomp(~tot.quanks + n_quank + quank_bout + HD + VD, data=use.data)
biplot(pr)

pr$loadings
str(pr)
pr$scores

pc1=pr$scores[,1]
use.data$pc1=pc1

p=ggplot(data=use.data, aes(x=Treatment, y=pc1)) +
  geom_point() +
  theme_classic() 
p
