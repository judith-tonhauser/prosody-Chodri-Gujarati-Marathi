# Prosody of Chodri, Gujarati, Marathi
# f0 analysis: time series (GAMM), L/H

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.r")

# load packages
library(tidyverse)
library(lmerTest)

theme_set(theme_bw())

# GAMM analysis ====

## preprocessing for GAMM analysis ----
# This analysis is based on f0 values extracted every 10ms in the subject and the predicate
# The f0 contour is plotted and modeled for the subject and the predicate together
# Praat script: MAR-pitch-over-time.praat

# read in the data
dc <- read.csv("../data/Chodri-pitch-by-time.csv")
dg <- read.csv("../data/Gujarati-pitch-by-time.csv")
dm <- read.csv("../data/Marathi-pitch-by-time.csv")

nrow(dc) #19408
nrow(dg) #20760
nrow(dm) #17067

# add ".wav" to File names so that code below works
dc$File <- paste(dc$File,".wav",sep="")
dg$File <- paste(dg$File,".wav",sep="")
dm$File <- paste(dm$File,".wav",sep="")

length(unique(dc$File)) #170
length(unique(dg$File)) #186
length(unique(dm$File)) #175

d <- bind_rows(dc,dm,dg)
length(unique(d$File)) #531 
table(d$File)

# exclude disfluent utterances
excluded <- read.csv("../data/excluded.csv")
d <- droplevels(subset(d,!d$File %in% excluded$File))
nrow(excluded) #16
length(unique(d$File)) #528 (168 C + 186 G + 174 M)
table(d$File %in% excluded$File) # no disfluent utterances remain

# exclude utterances with utterance-internal pauses

# Chodri
pauseCHO <- read.csv(file="../data/Chodri-pauses.csv", header=TRUE, sep=",")
pauseCHO
nrow(pauseCHO) #16

d <- droplevels(subset(d, !(d$File %in% pauseCHO$File)))
length(unique(d$File)) #512 (16 Chodri utterances with internal pause excluded)

# Gujarati
pauseGUJ <- read.csv(file="../data/Gujarati-pauses.csv", header=TRUE, sep=",")
pauseGUJ
nrow(pauseGUJ) #18

d <- droplevels(subset(d, !(d$File %in% pauseGUJ$File)))
length(unique(d$File)) #494 (18 Gujarati utterances with internal pause excluded)

# Marathi
pauseMAR <- read.csv(file="../data/Marathi-pauses.csv", header=TRUE, sep=",")
pauseMAR
nrow(pauseMAR) #10

d <- droplevels(subset(d, !(d$File %in% pauseMAR$File)))
length(unique(d$File)) #484 (10 Marathi utterances with internal pause excluded)
nrow(d) #52090

# create additional columns

# language
d$language <- ifelse(grepl("CHO-",as.character(d$File)),"Chodri",
                     ifelse(grepl("MAR-",as.character(d$File)),"Marathi",
                            ifelse(grepl("GUJ-",as.character(d$File)),"Gujarati",
                                   "OTHER")))
table(d$language)

# talker
table(d$File)
d$talker <- d$File
d$talker <- gsub("CHO-","", d$talker)
d$talker <- gsub("MAR-","", d$talker)
d$talker <- gsub("GUJ-","", d$talker)
d$talker <- gsub("-target[0-9]+.wav","", d$talker)
table(d$talker)
d$talker <- as.factor(as.character(d$talker))

# utterance
d$utt <- d$File
d$utt <- gsub("CHO-","", d$utt)
d$utt <- gsub("MAR-","", d$utt)
d$utt <- gsub("GUJ-","", d$utt)
d$utt <- gsub("P[0-9]+-","", d$utt)
d$utt <- gsub(".wav","", d$utt)
table(d$utt)

# sentence

# target 1: SubjAP (Ananyaa naangarNi 
# target 8: VerbAP (Ananyaa naangarNi

# target 2: VerbLE (Laavanya vyaayaam) 
# target 5: SubjLE (Laavanya vyaayaam)

# target 3: SubjAW (Ananyaa nindaNi 
# target 6: VerbAW (Ananyaa nindaNi 

# target 4: VerbAR (Ananyaa aaraam)
# target 13: SubjAR (Ananyaa aaraam)

# target 7: SubjLR (Laavanya aaraam)
# target 10: VerbLR (Laavanya aaraam)

# target 9: SubjLW (Laavanya nindaNi 
# target 16: VerbLW (Laavanya nindaNi

# target 11: SubjAE (Ananyaa vyaayaam)
# target 14: VerbAE (Ananyaa vyaayaam)

# target 12: VerbLP (Laavanya naangarNi 
# target 15: SubjLP (Laavanya naangarNi 


# target 1 = 8
# target 2 = 5
# target 3 = 6
# target 4 = 13
# target 7 = 10
# target 9 = 16
# target 11 = 14
# target 12 = 15

d$sentence <- d$utt
d$sentence <- ifelse(d$utt == "target1" | d$utt == "target8", "sentence18",
                     ifelse(d$utt == "target2" | d$utt == "target5", "sentence25",
                            ifelse(d$utt == "target3" | d$utt == "target6", "sentence36",
                                   ifelse(d$utt == "target4" | d$utt == "target13", "sentence413",
                                          ifelse(d$utt == "target7" | d$utt == "target10", "sentence710",
                                                 ifelse(d$utt == "target9" | d$utt == "target16", "sentence916",
                                                        ifelse(d$utt == "target11" | d$utt == "target14", "sentence1114", "sentence1215")))))))
table(d$sentence)

# focus condition (subject focus, verb focus)
d$cond <- ifelse(d$utt == "target1" | d$utt == "target3" | 
                   d$utt == "target5" | d$utt == "target7" |
                   d$utt == "target9" | d$utt == "target11" |
                   d$utt == "target13" | d$utt == "target15", "subjFoc", "predFoc")
d$cond <- as.factor(d$cond)
table(d$cond)

# grammatical function
table(d$label)
d$gf <- ifelse(d$label == "laavanya" | d$label == "ananyaa", "subject","predicate")
d$gf <- as.factor(d$gf)
table(d$gf)

# unique utterances per language
length(unique(d[d$language == "Chodri",]$File)) #152 (168 - 16 utterances with internal pause)
length(unique(d[d$language == "Gujarati",]$File)) #168 (186 - 18 utterances with internal pause)
length(unique(d[d$language == "Marathi",]$File)) #164 (174 - 10 utterances with internal pause)

# plot the extracted raw f0 points of the utterances to see whether good f0 values 
# were extracted

table(d$sentence)

# # Chodri
# 
# ggplot(data=d[d$sentence == "sentence1114" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence1114.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence18" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence18.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence36" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence36.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence710" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence710.pdf",height=14,width=14)
# # P1 and P6 predicate focus pitch halving
# 
# ggplot(data=d[d$sentence == "sentence1215" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence1215.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence25" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence25.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence413" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence413.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence916" & d$language == "Chodri",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/CHO-raw-f0-contour-sentence916.pdf",height=14,width=14)
# 
# # Gujarati
# 
# ggplot(data=d[d$sentence == "sentence1114" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence1114.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence18" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence18.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence36" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence36.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence710" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence710.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence1215" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence1215.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence25" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence25.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence413" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence413.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence916" & d$language == "Gujarati",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/GUJ-raw-f0-contour-sentence916.pdf",height=14,width=14)
# 
# # Marathi
# 
# ggplot(data=d[d$sentence == "sentence1114" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence1114.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence18" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence18.pdf",height=14,width=14)
# #P6 subjFoc pitch halving
# 
# ggplot(data=d[d$sentence == "sentence36" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence36.pdf",height=14,width=14)
# #P4 subjFoc pitch halving
# 
# ggplot(data=d[d$sentence == "sentence710" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence710.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence1215" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence1215.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence25" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence25.pdf",height=14,width=14)
# 
# ggplot(data=d[d$sentence == "sentence413" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence413.pdf",height=14,width=14)
# #P8 subjFoc pitch halving
# 
# ggplot(data=d[d$sentence == "sentence916" & d$language == "Marathi",], aes(x=time, y=f0)) +
#   geom_point(na.rm=TRUE,size=.1) +
#   facet_grid(talker ~ cond, scales = "free", space = "fixed") +
#   xlab("time (ms)") +
#   ylab("f0 (Hz)")
# ggsave("../graphs/raw-f0-contours/MAR-raw-f0-contour-sentence916.pdf",height=14,width=14)

# 3 MAR utterances in subjFoc condition had pitch-halving, not excluded

# normalize the f0 values by time and talker
# such that the subject goes from 0 to .5 and the predicate goes from .5 to 1
# in each language, subjects and predicates may be of different lengths, which is
# why decision was made to not normalize so that entire utt goes from 0 to 1

# find the first and last time of each subject and predicate utterance
minT = aggregate(time ~ File + label, data=d, FUN=min)
colnames(minT)[ncol(minT)] = 'time.min'
head(minT)

maxT = aggregate(time ~ File + label, data=d, FUN=max)
colnames(maxT)[ncol(maxT)] = 'time.max'
head(maxT)

# merge the first and last time to one data frame
minmaxT = merge(minT,maxT,by=c('File','label'))
head(minmaxT)

length(unique(minmaxT$File)) #484

# add first and last time info to data
d = merge(d,minmaxT,by=c('File','label'))
head(d)
nrow(d) #52090

table(d$gf) # only subject and predicate accentual phrases

# normalize each time point such that subject times go from 0 to .5 and
# predicate times go from .5 to 1
d$total.ap.time <- d$time.max - d$time.min 

d$Time <- ifelse(d$gf == "subject", 
                 (d$time - d$time.min) / ((d$total.ap.time)*2), # *2 to get to .5 instead of 1 for subject AP
                 ((d$time - d$time.min) / ((d$total.ap.time)*2))+.5) # +.5 to go from .5 to 1 for predicate AP

head(d)
nrow(d) #52090

# min and max times per subject and predicate (to see if normalization worked so far; not done yet)
min(d[d$gf == "subject",]$Time) #0
max(d[d$gf == "subject",]$Time) #.5
min(d[d$gf == "predicate",]$Time) #.5
max(d[d$gf == "predicate",]$Time) #1

# for the subject times that are .5, subtract .0001ms to make them precede min of predicate
d$Time <- ifelse(d$gf == "subject" & d$Time == 0.5, 0.49999, d$Time)
max(d[d$gf == "subject",]$Time)   #.49999   

# plots to make sure normalization worked
par(mfrow=c(1,2))
boxplot(d[d$gf == "subject",]$Time, main='Normalized subject AP time')
boxplot(d[d$gf == "predicate",]$Time, main='Normalized predicate AP time')

# normalize f0

# find f0 mean and sd of each talker in each language
f0mean = aggregate(f0 ~ talker + language, data=d, FUN=mean)
colnames(f0mean)[ncol(f0mean)] = 'f0.mean'
f0mean
f0sd = aggregate(f0 ~ talker + language, data=d, FUN=sd)
colnames(f0sd)[ncol(f0sd)] = 'f0.sd'
f0sd
f0 = merge(f0mean, f0sd,by=c("talker","language"))
f0

# save the f0 mean and sd for each talker for LH analysis 
write.csv(f0, "../data/f0-mean-sd-by-talker.csv", row.names=FALSE)

# merge this talker f0 mean and sd with data 
d = merge(d, f0, by=c("talker","language"))
head(d)

# z-transform f0
d$f0.z = (d$f0 - d$f0.mean) / d$f0.sd
head(d)

# plots to check if normalization worked
par(mfrow=c(1,2))
boxplot(d$f0,main='Original f0')
boxplot(d$f0.z,main='Z-transformed f0 by talker') 

ggplot(d, aes(x=talker, y=f0.z)) + 
  geom_boxplot() +
  facet_wrap(~ language) 

# change outliers to NA: f0.z values below -3 and above 3
nrow(d) #52090
nrow(d[d$f0.z > 3 & !is.na(d$f0.z),]) #128
nrow(d[d$f0.z < -3 & !is.na(d$f0.z),]) #653
# only 781 / 52090 = .0149, that is .02% of data points are excluded this way

d$f0.z[d$f0.z > 3] <- NA
d$f0.z[d$f0.z <= -3] <- NA
nrow(d)

sapply(d,function(x) sum(is.na(x))) #4372 NA f0.z

ggplot(d, aes(x=talker, y=f0.z)) + 
  geom_boxplot() +
  facet_wrap(~ language) 

# how many utterances per language?
length(unique(d$File)) #484
length(unique(d[d$language == "Chodri",]$File)) #152
length(unique(d[d$language == "Gujarati",]$File)) #168
length(unique(d[d$language == "Marathi",]$File)) #164

nrow(d) #52090 data points
nrow(d[d$language == "Chodri",]) #17508
nrow(d[d$language == "Gujarati",]) #18662
nrow(d[d$language == "Marathi",]) #15920

# save the preprocessed data
write.csv(d, "../data/f0-time-series-cleaned.csv")
nrow(d) #52090

## Figures 3 and 4 ----

# Fig 3: unnormalized f0 values over unnormalized time for sample utterance

ggplot(data=d[d$sentence == "sentence36" & d$talker == "P7" & d$cond == "predFoc" & d$language == "Marathi",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  #facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time") + 
  ylab("f0") 
ggsave("../graphs/MAR-unnormalized-example.pdf",height=2,width=6.2)

# Fig 4: normalized f0 values over normalized time for sample utterance

ggplot(data=d[d$sentence == "sentence36" & d$talker == "P7" & d$cond == "predFoc" & d$language == "Marathi",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  #facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("normalized f0") 
ggsave("../graphs/MAR-f0z-example.pdf",height=2,width=6.2)


## load clean data ----
d = read.csv("../data/f0-time-series-cleaned.csv")
nrow(d) #52090

## GAMM analysis ----

# load required
library <- c("mgcv","itsadug")

# restrict analyses to utterances with LH on predicate 
# 493 total utterances with LH predicates
c.pred.LH <- read.csv(file="../data/Chodri-pred-LH.csv", header=TRUE, sep=",")
nrow(c.pred.LH) #167
g.pred.LH <- read.csv(file="../data/Gujarati-pred-LH.csv", header=TRUE, sep=",")
nrow(g.pred.LH) #173
m.pred.LH <- read.csv(file="../data/Marathi-pred-LH.csv", header=TRUE, sep=",")
nrow(m.pred.LH) #153

# drop the predicates that don't have LH
d <- droplevels(subset(d,!(d$gf == "predicate" & !(d$File %in% c.pred.LH$File | d$File %in% g.pred.LH$File | d$File %in% m.pred.LH$File))))
nrow(d) #50573

# check that we have the right amount of files
length(unique(d[d$language == "Chodri",]$File)) #152
length(unique(d[d$language == "Gujarati",]$File)) #168
length(unique(d[d$language == "Marathi",]$File)) #164

# transform the data so that the subject/predicate combination is a time series

# find the start events for each utterance in each language
d <- start_event(d, event=c("File","utt"))
head(d)
summary(d)

str(d$cond)
str(d$talker)
str(d$sentence)
d$talker <- as.factor(d$talker)
d$cond <- as.factor(d$cond)
d$sentence <- as.factor(d$sentence)

# rename condition levels for plotting 
d$cond <- fct_recode(d$cond, subject_focus = "subjFoc", predicate_focus = "predFoc")
table(d$cond)

# set reference level for cond to "subject_focus"
levels(d$cond)
d$cond <- relevel(d$cond, "subject_focus")

# for binary and contrast models
d$IsPredFocus <- (d$cond == "predicate_focus")*1
d$IsPredFocusO <- as.ordered(d$IsPredFocus)
contrasts(d$IsPredFocusO) <- 'contr.treatment'
table(d$IsPredFocus)
table(d$IsPredFocusO)

#### Chodri ----

# determine residuals, to correct for autocorrelation
c.tmp <-  bam(f0.z ~ cond # fixed effect
              + s(Time, by = cond, bs='ad') # smooth for fixed effect
              + s(Time, talker, by=cond, bs = "re") # talker random effect
              + s(Time, talker, by=cond, bs = "fs", m = 1) # talker slope
              + s(Time, sentence, by=cond, bs = "re") # sentence random effect 
              + s(Time, sentence, by=cond, bs = "fs", m = 1), # sentence slope
              data = d[d$language == "Chodri",], discrete = T, nthreads = 2)
summary(c.tmp)

macf <- acf_resid(c.tmp)
macf

# to fix autocorrelation
rhoval <- macf[2]
rhoval # .51

# model predicting f0 from condition and Time by condition (fixed effects) and
# random effects (with slopes) for talker and sentence
c <- bam(f0.z ~ cond # fixed effect
         + s(Time, by = cond, bs='ad', k=60) # smooth for fixed effect
         + s(Time, talker, by=cond, bs = "re", k=60) # talker random effect
         + s(Time, talker, by=cond, bs = "fs", m = 1, k=60) # talker slope
         + s(Time, sentence, by=cond, bs = "re", k=60) # sentence random effect 
         + s(Time, sentence, by=cond, bs = "fs", m = 1, k=60), # sentence slope
         data = d[d$language == "Chodri",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$language == "Chodri",]$start.event)
#sum.m <- summary(m)
#sum.m

# check to make sure that k-index isn't lower than 1 and associated p-value small,
# and that edf isn't too close to k' (suggests model needs to be more complex)
gam.check(c)
k.check(c, subsample=5000, n.rep=400)
# k = 60

# plots
pdf(file = "../graphs/Chodri-GAMM-1.pdf",width = 4, height = 4)
plot_smooth(c, 
            view='Time', # name of smooth to be displayed
            plot_all ="cond", # list of predictors to be plotted
            rm.ranef=T, # remove random effects
            rug=F, # F: don't plot x-axis ticks
            hide.label=T, # hide the label (put in caption instead)
            col=c('grey','black'),
            legend_plot_all = NULL,
            xlab = "normalized time",
            ylab = "fitted values for normalized f0")
dev.off()
pdf(file = "../graphs/Chodri-GAMM-2.pdf",width = 4, height = 4)
plot_diff(c, 
          view = "Time",
          rm.ranef=T,
          print.summary = F,
          hide.label = T,
          main = "",
          comp = list(cond = c("subject_focus","predicate_focus")),
          xlab = "normalized time",
          ylab = "estimated difference in normalized f0")
dev.off()

#### Gujarati ----

# determine residuals, to correct for autocorrelation
g.tmp <-  bam(f0.z ~ cond # fixed effect
              + s(Time, by = cond, bs='ad') # smooth for fixed effect
              + s(Time, talker, by=cond, bs = "re") # talker random effect
              + s(Time, talker, by=cond, bs = "fs", m = 1) # talker slope
              + s(Time, sentence, by=cond, bs = "re") # sentence random effect 
              + s(Time, sentence, by=cond, bs = "fs", m = 1), # sentence slope
              data = d[d$language == "Gujarati",], discrete = T, nthreads = 2)
summary(g.tmp)

macf <- acf_resid(g.tmp)
macf

# to fix autocorrelation
rhoval <- macf[2]
rhoval # .25

# model predicting f0 from condition and Time by condition (fixed effects) and
# random effects (with slopes) for talker and sentence
g <- bam(f0.z ~ cond # fixed effect
         + s(Time, by = cond, bs='ad', k=70) # smooth for fixed effect
         + s(Time, talker, by=cond, bs = "re", k=70) # talker random effect
         + s(Time, talker, by=cond, bs = "fs", m = 1, k=70) # talker slope
         + s(Time, sentence, by=cond, bs = "re", k=70) # sentence random effect 
         + s(Time, sentence, by=cond, bs = "fs", m = 1, k=70), # sentence slope
         data = d[d$language == "Gujarati",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$language == "Gujarati",]$start.event)
#sum.m <- summary(m)
#sum.m

# check to make sure that k-index isn't lower than 1 and associated p-value small,
# and that edf isn't too close to k' (suggests model needs to be more complex)
gam.check(g)
k.check(g, subsample=5000, n.rep=400)
# k = 70 (k-index still below 1)

# plots
pdf(file = "../graphs/Gujarati-GAMM-1.pdf",width = 4, height = 4)
plot_smooth(g, 
            view='Time', # name of smooth to be displayed
            plot_all ="cond", # list of predictors to be plotted
            rm.ranef=T, # remove random effects
            rug=F, # F: don't plot x-axis ticks
            hide.label=T, # hide the label (put in caption instead)
            col=c('grey','black'),
            legend_plot_all = NULL,
            xlab = "normalized time",
            ylab = "fitted values for normalized f0")
dev.off()
pdf(file = "../graphs/Gujarati-GAMM-2.pdf",width = 4, height = 4)
plot_diff(g, 
          view = "Time",
          rm.ranef=T,
          print.summary = F,
          hide.label = T,
          main = "",
          comp = list(cond = c("subject_focus","predicate_focus")),
          xlab = "normalized time",
          ylab = "estimated difference in normalized f0")
dev.off()

#### Marathi ----

# determine residuals, to correct for autocorrelation
m.tmp <-  bam(f0.z ~ cond # fixed effect
              + s(Time, by = cond, bs='ad') # smooth for fixed effect
              + s(Time, talker, by=cond, bs = "re") # talker random effect
              + s(Time, talker, by=cond, bs = "fs", m = 1) # talker slope
              + s(Time, sentence, by=cond, bs = "re") # sentence random effect 
              + s(Time, sentence, by=cond, bs = "fs", m = 1), # sentence slope
              data = d[d$language == "Marathi",], discrete = T, nthreads = 2)
summary(m.tmp)

macf <- acf_resid(m.tmp)
macf

# to fix autocorrelation
rhoval <- macf[2]
rhoval # .9

# model predicting f0 from condition and Time by condition (fixed effects) and
# random effects (with slopes) for talker and sentence
m <- bam(f0.z ~ cond # fixed effect
         + s(Time, by = cond, bs='ad', k=30) # smooth for fixed effect
         + s(Time, talker, by=cond, bs = "re", k=30) # talker random effect
         + s(Time, talker, by=cond, bs = "fs", m = 1, k=30) # talker slope
         + s(Time, sentence, by=cond, bs = "re", k=30) # sentence random effect 
         + s(Time, sentence, by=cond, bs = "fs", m = 1, k=30), # sentence slope
         data = d[d$language == "Marathi",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$language == "Marathi",]$start.event)
#sum.m <- summary(m)
#sum.m

# check to make sure that k-index isn't lower than 1 and associated p-value small,
# and that edf isn't too close to k' (suggests model needs to be more complex)
gam.check(m)
k.check(m, subsample=5000, n.rep=400)
# k = 30

# plots
pdf(file = "../graphs/Marathi-GAMM-1.pdf",width = 4, height = 4)
plot_smooth(m, 
            view='Time', # name of smooth to be displayed
            plot_all ="cond", # list of predictors to be plotted
            rm.ranef=T, # remove random effects
            rug=F, # F: don't plot x-axis ticks
            hide.label=T, # hide the label (put in caption instead)
            col=c('grey','black'),
            legend_plot_all = NULL,
            xlab = "normalized time",
            ylab = "fitted values for normalized f0")
dev.off()
pdf(file = "../graphs/Marathi-GAMM-2.pdf",width = 4, height = 4)
plot_diff(m, 
          view = "Time",
          rm.ranef=T,
          print.summary = F,
          hide.label = T,
          main = "",
          comp = list(cond = c("subject_focus","predicate_focus")),
          xlab = "normalized time",
          ylab = "estimated difference in normalized f0")
dev.off()

# fit binary model to investigate whether non-linear difference is significant 
mb.tmp <- bam(f0.z ~ s(Time) 
          + s(Time, by = IsPredFocus) 
          + s(Time, talker, by=IsPredFocus, bs = "re")
          + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1)
          + s(Time, sentence, by=IsPredFocus, bs = "re")
          + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1),
          data = d[d$language == "Marathi",], discrete = T, nthreads = 2)

macf <- acf_resid(mb.tmp)
(rhoval <- macf[2]) #0.90

mb <- bam(f0.z ~ s(Time, k=50) 
          + s(Time, by = IsPredFocus, k=30)
          + s(Time, talker, by=IsPredFocus, bs = "re", k=30)
          + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1, k=30)
          + s(Time, sentence, by=IsPredFocus, bs = "re", k=30)
          + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1, k=30),
           data = d[d$language == "Marathi",], discrete = T, nthreads = 2, rho=rhoval, 
           AR.start=d[d$language == "Marathi",]$start.event)
sum.mb <- summary(mb)
sum.mb
# s(Time):IsPredFocus          8.855e+00   9.821  2.468 0.00677 ** 
# the non-linear pattern for the difference between the two conditions is significant

# fit ordered factor model to investigate whether intercept and/or non-linear difference are different
mo.tmp <- bam(f0.z ~ IsPredFocusO 
           + s(Time) 
           + s(Time, by = IsPredFocusO) 
           + s(Time, talker, by=IsPredFocus, bs = "re")
           + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1)
           + s(Time, sentence, by=IsPredFocus, bs = "re")
           + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1),
           data = d[d$language == "Marathi",], discrete = T, nthreads = 2)
macf <- acf_resid(mo.tmp)
rhoval <- macf[2] 
rhoval #0.90

mo <- bam(f0.z ~ IsPredFocusO 
          + s(Time, k=30) 
          + s(Time, by = IsPredFocusO, k=30) 
          + s(Time, talker, by=IsPredFocus, bs = "re", k=30)
          + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1, k=30)
          + s(Time, sentence, by=IsPredFocus, bs = "re", k=30)
          + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1, k=30),
          data = d[d$language == "Marathi",], discrete = T, nthreads = 2, rho=rhoval, 
           AR.start=d[d$language == "Marathi",]$start.event)
sum.mo <- summary(mo)
sum.mo
# IsPredFocusO1  IsPredFocusO1  0.03649    0.06755   0.540    0.589
# s(Time):IsPredFocusO1        7.932e+00   8.947   2.716 0.00392 ** 
# no difference in intercept
# non-linear difference

# LH analysis ====

## preprocessing for L and H analysis ----
# Praat scripts: CHO-LH-extract-f0.praat, GUJ-LH-extract-f0.praat, MAR-LH-extract-f0.praat

# read in the relevant data
dc = read.csv("../data/Chodri-LHvalues.csv")
dg = read.csv("../data/Gujarati-LHvalues.csv")
dm = read.csv("../data/Marathi-LHvalues.csv")

length(unique(dc$File)) #171
length(unique(dg$File)) #186
length(unique(dm$File)) #175

str(dc) # f0 read as character, not numeric because some --undefined--
str(dg)
str(dm)

# add ".wav" to File names 
dc$File <- paste(dc$File,".wav",sep="")
dg$File <- paste(dg$File,".wav",sep="")
dm$File <- paste(dm$File,".wav",sep="")

# 12 Files with undefined f0 values in Chodri
dc[dc$f0 == "--undefined--",]$File

dc$gf <- ifelse(dc$label == "laavanya" | dc$label == "ananyaa" | 
                  dc$label == "laa" | dc$label == "van" |
                  dc$label == "ya" | dc$label == "a" |
                  dc$label == "nan" | dc$label == "yaa", "subject", 
                ifelse(dc$label == "aaraam" | dc$label == "kasrat" |
                         dc$label == "nedvaanu" | dc$label == "kheDvaanu" |
                         dc$label == "aa" | dc$label == "raam" |
                         dc$label == "kas" | dc$label == "rat" |
                         dc$label == "ned" | dc$label == "vaa" | dc$label == "nu" |
                         dc$label == "kheD","predicate", "other"))
table(dc$gf)

dc[dc$f0 == "--undefined--" & dc$labelPoint == "L",]$File
dc[dc$f0 == "--undefined--" & dc$gf == "subject",]$File

# Chodri: fix the undefined f0 values
dc$f0 <- as.numeric(as.character(dc$f0))
table(is.na(dc$f0)) #12 NA

str(dc)


# [1] "CHO-P1-target8.wav"  L on subject
dc[dc$File == "CHO-P1-target8.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 178.3
# [2] "CHO-P10-target12.wav" L on subject
dc[dc$File == "CHO-P10-target12.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 125.8
# [3] "CHO-P10-target5.wav"  L on subject
dc[dc$File == "CHO-P10-target5.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 117.5
# [4] "CHO-P11-target1.wav"  L on subject
dc[dc$File == "CHO-P11-target1.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 100.4
# [5] "CHO-P11-target10.wav" L on subject
dc[dc$File == "CHO-P11-target10.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 99.4
# [6] "CHO-P11-target16.wav" L on subject
dc[dc$File == "CHO-P11-target16.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 82.6
# [7] "CHO-P11-target2.wav"  L on subject
dc[dc$File == "CHO-P11-target2.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 93.3
# [8] "CHO-P11-target3.wav" L on subject
dc[dc$File == "CHO-P11-target3.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 96
# [9] "CHO-P11-target8.wav" L on subject
dc[dc$File == "CHO-P11-target8.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 102.2
# [10] "CHO-P3-target5.wav"  H on predicate
dc[dc$File == "CHO-P3-target5.wav" & dc$labelPoint == "H" & dc$gf == "predicate",]$f0 <- 149.1
# [11] "CHO-P5-target16.wav" L on subject
dc[dc$File == "CHO-P5-target16.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 104.6
# [12] "CHO-P5-target9.wav" L on subject
dc[dc$File == "CHO-P5-target9.wav" & dc$labelPoint == "L" & dc$gf == "subject",]$f0 <- 98.3

table(is.na(dc$f0)) #no NA

# remove gf info from Chodri again
dc <- dc %>%
  select(-gf)

names(dc)

# bind the data
d <- bind_rows(dc,dg,dm)
length(unique(d$File)) #532

table(is.na(d$f0)) #no NA

# exclude disfluent utterances
excluded <- read.csv("../data/excluded.csv")
d <- droplevels(subset(d,!d$File %in% excluded$File))
nrow(excluded) #16
length(unique(d$File)) #528 = 532 - 16
table(d$File %in% excluded$File) # only FALSE, so all good
nrow(d) #2077

### add columns 

# language
d$language <- ifelse(grepl("CHO-",as.character(d$File)),"Chodri",
                     ifelse(grepl("MAR-",as.character(d$File)),"Marathi",
                            ifelse(grepl("GUJ-",as.character(d$File)),"Gujarati",
                                   "OTHER")))
table(d$language)

# talker
table(d$File)
d$talker <- d$File
d$talker <- gsub("CHO-","", d$talker)
d$talker <- gsub("MAR-","", d$talker)
d$talker <- gsub("GUJ-","", d$talker)
d$talker <- gsub("-target[0-9]+.wav","", d$talker)
table(d$talker)
d$talker <- as.factor(as.character(d$talker))

# utterance
d$utt <- d$File
d$utt <- gsub("CHO-","", d$utt)
d$utt <- gsub("MAR-","", d$utt)
d$utt <- gsub("GUJ-","", d$utt)
d$utt <- gsub("P[0-9]+-","", d$utt)
d$utt <- gsub(".wav","", d$utt)
table(d$utt)

# focus condition (subject focus, predicate focus)
d$condition <- ifelse(d$utt == "target1" | d$utt == "target3" | 
                        d$utt == "target5" | d$utt == "target7" |
                        d$utt == "target9" | d$utt == "target11" |
                        d$utt == "target13" | d$utt == "target15", "subjFoc", "predFoc")
table(d$condition)



# grammatical function (subject, verb)
d$gf <- ifelse(d$label == "laavanya" | d$label == "ananyaa" | 
                 d$label == "laa" | d$label == "van" |
                 d$label == "ya" | d$label == "a" |
                 d$label == "nan" | d$label == "yaa", "subject", 
               ifelse(d$label == "aaraam" | d$label == "vyaayaam" |
                        d$label == "nindaNi" | d$label == "naangarNi" |
                        d$label == "aa" | d$label == "raam" |
                        d$label == "vyaa" | d$label == "yaam" |
                        d$label == "nin" | d$label == "da" | d$label == "Ni" |
                        d$label == "naan" | d$label == "gar" |
                        d$label == "aaraam" | d$label == "nindvaanu" |
                        d$label == "vyaayaam" | d$label == "kheDvaanu" |
                        d$label == "aa" | d$label == "raam" |
                        d$label == "nind" | d$label == "vaa" |
                        d$label == "nu" | d$label == "vyaa" | d$label == "yaam" |
                        d$label == "kheD" | 
                        d$label == "aaraam" | d$label == "nedvaanu" |
                        d$label == "kasrat" | d$label == "kheDvaanu" |
                        d$label == "aa" | d$label == "raam" |
                        d$label == "ned" | d$label == "vaa" |
                        d$label == "nu" | d$label == "kas" | d$label == "rat" |
                        d$label == "kheD" | d$label == "vaa" | d$label == "nu","predicate", "other"))
table(d$gf)
#other predicate   subject 
#5      1016      1056 

# check that we have the right amount of files
length(unique(d[d$language == "Chodri",]$File)) #168
length(unique(d[d$language == "Gujarati",]$File)) #186
length(unique(d[d$language == "Marathi",]$File)) #174

length(unique(d$File)) #528

# check that all 528 subjects have a L and a H on it
table(d[d$gf == "subject",]$labelPoint) #528 H and L
table(d[d$label == "ananyaa",]$labelPoint) #264 
table(d[d$label == "laavanya",]$labelPoint) #264

# how many predicates with LH?
table(d[d$gf == "predicate",]$labelPoint)
# H   L 
# 489 527

# 493 total utterances with LH predicates
c.pred.LH <- read.csv(file="../data/Chodri-pred-LH.csv", header=TRUE, sep=",")
nrow(c.pred.LH) #167
g.pred.LH <- read.csv(file="../data/Gujarati-pred-LH.csv", header=TRUE, sep=",")
nrow(g.pred.LH) #173
m.pred.LH <- read.csv(file="../data/Marathi-pred-LH.csv", header=TRUE, sep=",")
nrow(m.pred.LH) #153

# drop the predicates that don't have LH
d <- droplevels(subset(d,!(d$gf == "predicate" & !(d$File %in% c.pred.LH$File | d$File %in% g.pred.LH$File | d$File %in% m.pred.LH$File))))
nrow(d) #2042

# how many predicates with LH?
table(d[d$gf == "predicate",]$labelPoint)
# H   L 
# 488 493

table(is.na(d$f0)) #no NA

# why are there predicates with a L but not a H?
#View(d[d$gf == "predicate",])

library(reshape2)
wide <- dcast(d, File + label + language ~ labelPoint, value.var="f0")
head(wide)
#View(wide)
table(is.na(wide$L)) # 5 NA
table(is.na(wide$H)) # 5 NA
# NA introduced in Chodri whenever H is realized on "kaam"
d$label <- trimws(d$label)
d[d$label == "kaam",]$File # 5 files with kaam

tmp <- c("CHO-P1-target12.wav","CHO-P1-target15.wav","CHO-P1-target8.wav","CHO-P10-target8.wav","CHO-P4-target1.wav")

# exclude the predicates and "kaam" of these 5 Chodri utterances
d <- droplevels(subset(d,!(d$File %in% tmp & (d$gf == "predicate" | d$gf == "other"))))
nrow(d) #2032

table(d$gf)

# check that we have the right amount of files
length(unique(d[d$language == "Chodri",]$File)) #168 (5 predicates removed but not subjects)
length(unique(d[d$language == "Gujarati",]$File)) #186
length(unique(d[d$language == "Marathi",]$File)) #174

# how many predicates with LH? (523 utterances, but not all have LH on predicate)
# 488 utterances with LH on predicate (not kaam)
table(d[d$gf == "predicate",]$labelPoint)
#H   L 
#488 488 

# how many subject APs per language?
length(unique(d[d$gf == "subject",]$File)) #528
length(unique(d[d$language == "Chodri" & d$gf == "subject",]$File)) #168
length(unique(d[d$language == "Gujarati" & d$gf == "subject",]$File)) #186
length(unique(d[d$language == "Marathi" & d$gf == "subject",]$File)) #174

# how many predicate APs per language?
length(unique(d[d$gf == "predicate",]$File)) #488
length(unique(d[d$language == "Chodri" & d$gf == "predicate",]$File)) #162
length(unique(d[d$language == "Gujarati" & d$gf == "predicate",]$File)) #173
length(unique(d[d$language == "Marathi" & d$gf == "predicate",]$File)) #153

# normalize time for the L and H times

# plots
par(mfrow=c(1,2))
boxplot(d[d$labelPoint == "L",]$time, main='Time of the L')
boxplot(d[d$labelPoint == "H",]$time, main='Time of the H')
boxplot(d$begTimeLabel, main='Beginnings of words')
boxplot(d$endTimeLabel, main='Ends of words')

# word duration
d$wordDur <- d$endTimeLabel - d$begTimeLabel
boxplot(d$wordDur, main='word durations')
min(d$wordDur)

# normalize each temporal location of L and H: 
# (time of L/H - first time of word)/(last time of word - first time of word)
names(d)
head(d)
d$Time <- 0
d$Time <- (d$time - d$begTimeLabel) / d$wordDur
boxplot(d$Time, main='normalized times')
boxplot(d[d$labelPoint == "L",]$Time, main='normalized times of L')
boxplot(d[d$labelPoint == "H",]$Time, main='normalized times of H')

# normalize f0 values

# load f0 mean and sd data by talker 
f0.mean.sd <- read.csv("../data/f0-mean-sd-by-talker.csv")
f0.mean.sd

# merge this talker f0 mean and sd with data 
d = merge(d, f0.mean.sd, by=c("talker","language"))
head(d)

# z-transform f0
d$f0.z = (d$f0 - d$f0.mean) / d$f0.sd

# plots
par(mfrow=c(1,2))
boxplot(d$f0,main='Original f0')
boxplot(d$f0.z,main='Z-transformed f0 by talker') 

# change outliers to NA: f0.z values below -3 and above 3
nrow(d) #2032
nrow(d[d$f0.z > 3 & !is.na(d$f0.z),]) #4
nrow(d[d$f0.z < -3 & !is.na(d$f0.z),]) #21
# 25 f0 values out of 2032

# remove expressions (not files!) with outlier f0 values
outliers <- droplevels(subset(d, d$f0.z > 3 | d$f0.z < -3))
nrow(outliers) #25
#View(outliers)
outliers$item <- paste(outliers$File,outliers$gf,sep="-")
head(outliers)

d$item <- paste(d$File,d$gf,sep="-")
d <- droplevels(subset(d, !d$item %in% outliers$item))
nrow(d) #1984

# how many subject APs per language?
length(unique(d[d$gf == "subject",]$File)) #512
length(unique(d[d$language == "Chodri" & d$gf == "subject",]$File)) #160
length(unique(d[d$language == "Gujarati" & d$gf == "subject",]$File)) #183
length(unique(d[d$language == "Marathi" & d$gf == "subject",]$File)) #169

# how many predicate APs per language?
length(unique(d[d$gf == "predicate",]$File)) #480
length(unique(d[d$language == "Chodri" & d$gf == "predicate",]$File)) #158
length(unique(d[d$language == "Gujarati" & d$gf == "predicate",]$File)) #171
length(unique(d[d$language == "Marathi" & d$gf == "predicate",]$File)) #151

# save the preprocessed data
write.csv(d, "../data/f0-L-H-cleaned.csv")
nrow(d) #1984

## load clean data ----
d = read.csv("../data/f0-L-H-cleaned.csv")
nrow(d) #1984

## L and H analysis ----

# f0 range
range <- dcast(d, File + label + language ~ labelPoint, value.var="f0.z")
head(range)
  
range$range <- 0
range$range <- range$H - range$L

colnames(range)[colnames(range) == 'H'] <- 'f0.z.H'
colnames(range)[colnames(range) == 'L'] <- 'f0.z.L'
head(range)

# now merge range info with d to calculate slope
d = merge(d, range, by=c('File','label','language'))
head(d)

# slope = range / difference in time between L and H
diff <- dcast(d, File + label + language ~ labelPoint, value.var="Time")
head(diff)
diff$diff <- 0
diff$diff <- diff$H - diff$L

colnames(diff)[colnames(diff) == 'H'] <- 'Time.H'
colnames(diff)[colnames(diff) == 'L'] <- 'Time.L'
head(diff)

# now merge range info with dm to calculate slope
d = merge(d, diff, by=c('File','label','language'))
head(d)

# slope 
d$slope <- d$range / (d$diff / d$wordDur)

# normalized temporal distance between the time of the L and the time of the H
d$distance <- d$diff / d$wordDur

head(d)
names(d)

### plots ----

# f0 of L by condition and grammatical function
agr = d %>%
  filter(labelPoint == "L") %>%
  group_by(condition,gf,language) %>%
  summarise(meanf0=mean(f0.z),ci.low=ci.low(f0.z),ci.high=ci.high(f0.z)) %>%
  mutate(YMin=meanf0-ci.low,YMax=meanf0+ci.high)
agr

# relevel factors for visualization
agr$gf <-factor(agr$gf, levels=c("subject","predicate"))
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))

ggplot(agr, aes(x=gf,y=meanf0,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean normalized f0 of L") +
  xlab("Accentual phrase") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)) +
  theme(legend.position="top")
ggsave("../graphs/f0-of-L-by-condition.pdf",height=4,width=9)

# f0 of H by condition and grammatical function
agr = d %>%
  filter(labelPoint == "H") %>%
  group_by(condition,gf,language) %>%
  summarise(meanf0=mean(f0.z),ci.low=ci.low(f0.z),ci.high=ci.high(f0.z)) %>%
  mutate(YMin=meanf0-ci.low,YMax=meanf0+ci.high)
agr

# relevel factors for visualization
agr$gf <-factor(agr$gf, levels=c("subject","predicate"))
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))


ggplot(agr, aes(x=gf,y=meanf0,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean normalized f0 of H") +
  xlab("Accentual phrase") + 
theme(axis.text=element_text(size=14),
      axis.title=element_text(size=14),
      axis.text.x = element_text(size = 14),
      strip.text.x = element_text(size = 14),
      legend.text=element_text(size=14),
      legend.title=element_text(size=14)) +
  theme(legend.position="top")
ggsave("../graphs/f0-of-H-by-condition.pdf",height=4,width=9)

# slope by condition and grammatical function
agr = d %>%
  group_by(condition,gf,language) %>%
  summarise(meanSlope=mean(slope),ci.low=ci.low(slope),ci.high=ci.high(slope)) %>%
  mutate(YMin=meanSlope-ci.low,YMax=meanSlope+ci.high)
agr

# relevel factors for visualization
agr$gf <-factor(agr$gf, levels=c("subject","predicate"))
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))

ggplot(agr, aes(x=gf,y=meanSlope,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean slope") +
  xlab("Accentual phrase") + 
theme(axis.text=element_text(size=14),
      axis.title=element_text(size=14),
      axis.text.x = element_text(size = 14),
      strip.text.x = element_text(size = 14),
      legend.text=element_text(size=14),
      legend.title=element_text(size=14)) +
  theme(legend.position="top")
ggsave("../graphs/slope-by-condition.pdf",height=4,width=9)

### models----
names(d)
nrow(d) #1984

# change characters to factor
d$gf <- as.factor(d$gf)
with(d, levels(gf)) #predicate
d$condition <- as.factor(d$condition)
with(d, levels(condition)) #predFoc

# f0 on L

# Chodri
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Chodri" & d$gf == "subject",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "L" & d$language == "Chodri" & d$gf == "subject",],  REML=F)
summary(m)
  
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "L" & d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)

# Gujarati
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Gujarati" & d$gf == "subject",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "L" & d$language == "Gujarati" & d$gf == "subject",],  REML=F)
summary(m)
# conditionsubjFoc   0.10081    0.05841 169.90775   1.726   0.0862 .

m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Gujarati" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "L" & d$language == "Gujarati" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m)
# conditionsubjFoc  -0.12760    0.05617 157.16818  -2.271   0.0245 *

# Marathi
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Marathi" & d$gf == "subject",],  REML=F)
summary(m)
# conditionsubjFoc  0.21817    0.08287 10.81785   2.632  0.02360 *

m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "L" & d$language == "Marathi" & d$gf == "predicate",],  REML=F)
summary(m)

# f0 on H

# Chodri
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Chodri" & d$gf == "subject",],  REML=F)
summary(m)
# conditionsubjFoc  0.21586    0.08338 11.61195   2.589  0.02425 *

m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "H" & d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)

# Gujarati
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Gujarati" & d$gf == "subject",],  REML=F)
summary(m)

m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Gujarati" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(f0.z ~ condition + (1|label) + (1|talker), 
         data = d[d$labelPoint == "H" & d$language == "Gujarati" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m)
# conditionsubjFoc  -0.2379     0.0706 156.8973  -3.370 0.000946 ***

# Marathi
m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Marathi" & d$gf == "subject",],  REML=F)
summary(m)

m = lmer(f0.z ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$labelPoint == "H" & d$language == "Marathi" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m)
# conditionsubjFoc  -0.9006     0.1725 10.3068  -5.220 0.000353 ***

# slope
head(d)

# Chodri
m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Chodri" & d$gf == "subject",],  REML=F)
summary(m)

m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) #singular fit warning because effect of condition on talker is very small

m = lmer(slope ~ condition + (1|label) + (1|talker), 
         data = d[d$language == "Chodri" & d$gf == "predicate",],  REML=F)
summary(m)
# conditionsubjFoc  -0.1844     0.0743 302.5722  -2.481  0.01363 *

# Gujarati
m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Gujarati" & d$gf == "subject",],  REML=F)
summary(m)

m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Gujarati" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m) 

# Marathi
m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Marathi" & d$gf == "subject",],  REML=F)
summary(m)

m = lmer(slope ~ condition + (1|label) + (1+condition|talker), 
         data = d[d$language == "Marathi" & d$gf == "predicate",],  REML=F)
summary(m)
ranef(m)
# conditionsubjFoc  -0.7045     0.1351 10.5969  -5.216 0.000324 ***

# temporal alignment of L and H ----

# plot temporal alignment of L by condition and grammatical function
table(d$talker)
head(d)

agr = d %>%
  filter(labelPoint == "L") %>%
  group_by(condition,gf,language) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$gf <- as.factor(agr$gf)
agr$gf <- relevel(agr$gf, ref = "subject")

ggplot(agr, aes(x=condition,y=meanTime,fill=gf)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ gf) +
  ylab("Normalized temporal location of L") +
  xlab("Condition")
ggsave(f="../graphs/MAR-temporal-alignment-of-L-all-words-by-condition.pdf",height=3.5,width=8)

# plot temporal distance between L and H by condition and grammatical function

agr = d %>%
  group_by(cond,gf) %>%
  summarise(meanDistance=mean(distance),ci.low=ci.low(distance),ci.high=ci.high(distance)) %>%
  mutate(YMin=meanDistance-ci.low,YMax=meanDistance+ci.high)
agr
dodge=position_dodge(.9)

ggplot(agr, aes(x=cond,y=meanDistance)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ gf) +
  ylab("Normalized temporal distance between L and H") +
  xlab("Condition")
ggsave(f="../graphs/MAR-temporal-distance-between-L-and-H-by-condition.pdf",height=3.5,width=8)

# plot temporal alignment of L by talker and grammatical function
table(d$talker)
head(d)

agr = d %>%
  filter(labelPoint == "L") %>%
  group_by(talker,gf) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$gf <- as.factor(agr$gf)
agr$gf <- relevel(agr$gf, ref = "subject")

agr$talker.L <- factor(agr$talker, levels=unique(agr$talker[order(agr$gf,agr$meanTime)]), ordered=TRUE)
agr$talker.L

ggplot(agr, aes(x=talker.L,y=meanTime,fill=gf)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  ylab("Normalized temporal location of L") +
  xlab("Condition")
ggsave(f="../graphs/MAR-temporal-alignment-of-L-all-words.pdf",height=3.5,width=8)

# plot temporal alignment of H by talker and grammatical function
table(d$talker)
head(d)

agr = d %>%
  filter(labelPoint == "H") %>%
  group_by(talker,gf) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$gf <- as.factor(agr$gf)
agr$gf <- relevel(agr$gf, ref = "subject")

agr$talker.H <- factor(agr$talker, levels=unique(agr$talker[order(agr$gf,agr$meanTime)]), ordered=TRUE)
agr$talker.H

ggplot(agr, aes(x=talker.H,y=meanTime,fill=gf)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  ylab("Normalized temporal location of L") +
  xlab("Condition")
ggsave(f="../graphs/MAR-temporal-alignment-of-H-all-words.pdf",height=3.5,width=8)


# preparatory analysis: pitch range for each talker ----
# determine the pitch range of each talker to extract f0 values for GAMM analysis
# Praat script: utt-f0-min-max-mean.praat

# read in the data
dc = read.csv("../data/Chodri-f0values.csv")
dg = read.csv("../data/Gujarati-f0values.csv")
dm = read.csv("../data/Marathi-f0values.csv")

nrow(dc) #176
nrow(dg) #192
nrow(dm) #176 (= 11 talkers x 16 utts)

summary(dc)
length(unique(dc$File)) #176
summary(dg)
length(unique(dg$File)) #192
summary(dm)
length(unique(dm$File)) #176


# add ".wav" to File names so that code below works
dc$File <- paste(dc$File,".wav",sep="")
dg$File <- paste(dg$File,".wav",sep="")
dm$File <- paste(dm$File,".wav",sep="")

table(dc$File)
table(dg$File)
table(dm$File)

# add talker column 

dc$talker <- dc$File
dc$talker <- gsub("CHO-","", dc$talker)
dc$talker <- gsub("-target[0-9]+.wav","", dc$talker)
table(dc$talker)

dg$talker <- dg$File
dg$talker <- gsub("MAR-","", dg$talker)
dg$talker <- gsub("-target[0-9]+.wav","", dg$talker)
table(dg$talker)

dm$talker <- dm$File
dm$talker <- gsub("MAR-","", dm$talker)
dm$talker <- gsub("-target[0-9]+.wav","", dm$talker)
table(dm$talker)

# for each talker, get utterance mean f0 min, mean f0 max and mean f0 and sd
# to determine best range for f0 value extraction for GAMM analysis

# Chodri
agr_c = dc %>%
  group_by(talker) %>%
  summarise(meanf0min = mean(uttf0min), f0min.sd = sd(uttf0min),meanf0=mean(uttf0mean), meanf0max = mean(uttf0max),f0max.sd = sd(uttf0max))
agr_c

# talker meanf0min f0min.sd meanf0 meanf0max f0max.sd
# 50-350
# 3 P11         76.1     1.98   100.      145.     16.4
# 7 P5          91.7     4.44   108.      143.     45.9
# 1 P1         109.     40.5    178.      223.     11.1

# 75-400
# 2 P10        110.     12.4    131.      180.     74.7
# 5 P3         110.      8.34   135.      188.     61.0
# 4 P12        136.     42.6    210.      258.     15.0
# 8 P6         127.     48.3    226.      272.     11.4
# 10 P8         111.     24.2    170.      215.     77.1
# 11 P9         124.     36.4    202.      251.     67.1

# 100-500
# 9 P7         182.     35.5    246.      379.    112. 
# 6 P4         163.     60.5    238.      351.     72.7

# Gujarathi
agr_g = dg %>%
  group_by(talker) %>%
  summarise(meanf0min = mean(uttf0min), f0min.sd = sd(uttf0min),meanf0=mean(uttf0mean), meanf0max = mean(uttf0max),f0max.sd = sd(uttf0max))
agr_g

# talker meanf0min f0min.sd meanf0 meanf0max f0max.sd
# 50-350
# 1 P1          80.1    3.46   105.       125.     4.84
# 4 P12         75.7    0.881   97.9      119.     6.17
# 10 P7          88.6    5.82   122.       147.     6.86

# 50-400
# 3 P11         86.4   19.0    179.       253.    64.6 
# 9 P6          80.4    4.01   118.       178.    94.0
# 6 P3          89.6   13.7    211.       295.    53.9 
# 12 P9          93.6   26.1    209.       325.   108.

#75-400
# 2 P10        105.     1.81   126.       149.     7.11
# 5 P2         112.    44.3    199.       274.    85.7 
# 7 P4         146.    64.0    232.       285.     8.25
# 8 P5         102.    27.5    204.       258.     7.59
# 11 P8         114.    38.8    191.       224.     6.66


# Marathi
agr_m = dm %>%
  group_by(talker) %>%
  summarise(meanf0min = mean(uttf0min), f0min.sd = sd(uttf0min),meanf0=mean(uttf0mean), meanf0max = mean(uttf0max),f0max.sd = sd(uttf0max))
agr_m

# talker     meanf0min f0min.sd meanf0 meanf0max f0max.sd
#50-350
# 6 P4          97.3    24.9    208.      274.    10.6 
# 7 P5          97.8    21.4    144.      192.    51.3
# 9 P7          91.2     2.14   125.      168.     6.58
# 10 P8          97.9    14.3    127.      166.    21.9 
# 11 P9          93.8     2.67   130.      163.     7.21

#75-400 
# 1 P1         129.     31.5    202.      256.    14.0 
# 8 P6         106.     20.5    167.      211.    20.0 
# 3 P11        105.     33.2    182.      240.    66.1 
# 5 P3         173.     39.8    231.      305.    17.5 

#75-500
# 2 P10         85.9     9.38   158.      421.   146.
# 4 P2         145.     41.5    235.      304.    63.5 

# get impression for each language of f0 range and sd

# Chodri
agr_c = dc %>%
  summarise(meanf0min = mean(uttf0min), meanf0max = mean(uttf0max), meanRange = mean(uttf0max-uttf0min))
agr_c
# meanf0min meanf0max meanRange
# 121.8019  236.8462  115.0443

# Gujarati
agr_g = dg %>%
  summarise(meanf0min = mean(uttf0min), meanf0max = mean(uttf0max), meanRange = mean(uttf0max-uttf0min))
agr_g
#meanf0min meanf0max meanRange
# 97.90171  219.2376  121.3359

# Marathi
agr_m = dm %>%
  summarise(meanf0min = mean(uttf0min), meanf0max = mean(uttf0max), meanRange = mean(uttf0max-uttf0min))
agr_m
# meanf0min meanf0max meanRange
# 111.0743   245.449  134.3747


