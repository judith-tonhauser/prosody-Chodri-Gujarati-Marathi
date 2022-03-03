# Prosody of Chodri, Gujarati, Marati
# duration 
# Praat script: duration.praat

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.r")

# load packages
library(tidyverse)
library(lmerTest)

theme_set(theme_bw())

# read in the data
dc = read.csv("../data/Chodri-durations.csv")
dg = read.csv("../data/Gujarati-durations.csv")
dm = read.csv("../data/Marathi-durations.csv")

#File: language-talker-targetUtterance
#totalDuration: duration (ms) of the entire utterance
#label: expression
#duration: duration of the expression

# expected number of Files (= utterances)
length(unique(dc$File)) #176 = 11 talkers x 16 utterances (one talker excluded)
length(unique(dg$File)) #192 = 12 talkers x 16 utterances
length(unique(dm$File)) #176 = 11 talkers x 16 utterances

# preprocessing ----

# exclude utterances with utterance-internal pauses

# Chodri

pauseCHO <- read.csv(file="../data/Chodri-pauses.csv", header=TRUE, sep=",")
pauseCHO
nrow(pauseCHO) #16

dc <- droplevels(subset(dc, !(dc$File %in% pauseCHO$File)))
length(unique(dc$File)) #160

# utterance where predicate had 0 was removed as part of removal of utterances with pauses
# d3[d3$File == "CHO-P1-target1.wav",]$label
# d1[d1$File == "CHO-P1-target1.wav",]$Contour

# exclude 6 utterances with "kaam" where predicate+kaam jointly realize LH
#dc <- droplevels(subset(dc,dc$File %in% d1[d1$kaamContour != "LH",]$File))
#length(unique(dc$File)) #146 (152 - 6)

# Marathi
pauseMAR <- read.csv(file="../data/Marathi-pauses.csv", header=TRUE, sep=",")
pauseMAR
nrow(pauseMAR) #10

dm <- droplevels(subset(dm, !(dm$File %in% pauseMAR$File)))
length(unique(dm$File)) #166

# Gujarati
pauseGUJ <- read.csv(file="../data/Gujarati-pauses.csv", header=TRUE, sep=",")
pauseGUJ
nrow(pauseGUJ) #18

dg <- droplevels(subset(dg, !(dg$File %in% pauseGUJ$File)))
length(unique(dg$File)) #174

# exclude 6 kaam utterances where predicate+kaam jointly realize LH (doesn't happen in Gujarati)
#d3 <- droplevels(subset(d3,d3$File %in% d1[d1$kaamContour != "LH",]$File))
#length(unique(d3$File)) #168

### merge the data from the three languages
summary(dc)
summary(dm)
summary(dg)

d <- bind_rows(dc,dm,dg)
length(unique(d$File)) #500 = (176 + 192 + 176) - 16 - 10 - 18 = 544 - 45

# exclude disfluent utterances
excluded <- read.csv("../data/excluded.csv")
d <- droplevels(subset(d,!d$File %in% excluded$File))
nrow(excluded) #16
length(unique(d$File)) #484 = 500 - 16
table(d$File %in% excluded$File) # only FALSE, so all good

# trim spaces from the labels
table(d$label)
d$label <- trimws(d$label)

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

# code whether the label is for a word or a syllable (needed for plots)
d$WorS <- ifelse(d$label == "laavanya" | d$label == "ananyaa" |
                   d$label == "aaraam" | d$label == "vyaayaam" |
                   d$label == "naangarNi" | d$label == "nindaNi" |
                   d$label == "aaraam" | d$label == "nedvaanu" |
                   d$label == "kasrat" | d$label == "kheDvaanu" |
                   d$label == "kheDvaanu" | d$label == "nindvaanu","word",
                 ifelse(d$label == "laa" | d$label == "van" |
                          d$label == "ya" | d$label == "a" |
                          d$label == "nan" | d$label == "yaa" |
                          d$label == "aa" | d$label == "raam" |
                          d$label == "vyaa" | d$label == "yaam" |
                          d$label == "nin" | d$label == "da" | d$label == "Ni" |
                          d$label == "naan" | d$label == "gar" |
                          d$label == "aa" | d$label == "raam" |
                          d$label == "ned" | d$label == "vaa" |
                          d$label == "nu" | d$label == "kas" | d$label == "rat" |
                          d$label == "kheD" | d$label == "vaa" | d$label == "nu" |
                          d$label == "aa" | d$label == "raam" |
                          d$label == "vyaa" | d$label == "yaam" |
                          d$label == "kheD" | d$label == "vaa" | d$label == "nu" |
                          d$label == "nind","syl","other"))

table(d$WorS)

# normalize duration 
d$durationN <- d$duration / d$totalDuration

# for the models
# add information on whether the expression is realized with LH or 0 (since that influences duration)
# all subjects are realized with LH, so only info for predicates needed

# these CSV files contain those Files where the predicate has a LH
predToneC <- read.csv(file="../data/Chodri-pred-LH.csv", header=TRUE, sep=",")
predToneM <- read.csv(file="../data/Marathi-pred-LH.csv", header=TRUE, sep=",")
predToneG <- read.csv(file="../data/Gujarati-pred-LH.csv", header=TRUE, sep=",")

d$Tone <- "0"
d$Tone[d$gf == "subject"] <- "LH"
d$Tone[d$gf == "predicate" & d$File %in% predToneC$File] <- "LH"
d$Tone[d$gf == "predicate" & d$File %in% predToneM$File] <- "LH"
d$Tone[d$gf == "predicate" & d$File %in% predToneG$File] <- "LH"

table(d$gf,d$Tone) #all subjects LH

# save the preprocessed data
write.csv(d, "../data/duration-merged-and-cleaned.csv")
nrow(d) #5440

# check number of utterances per language
length(unique(d[d$language == "Chodri",]$File)) #152
length(unique(d[d$language == "Gujarati",]$File)) #168
length(unique(d[d$language == "Marathi",]$File)) #164

# load clean data ----
d = read.csv("../data/duration-merged-and-cleaned.csv")
nrow(d) #5440

# plots ----

## Fig: mean normalized duration by accentual phrase
agr = d %>%
  filter(gf == "subject" | gf == "predicate") %>%
  filter(WorS == "word") %>%
  group_by(condition,gf,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr

# relevel factors for visualization
agr$gf <-factor(agr$gf, levels=c("subject","predicate"))
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))

ggplot(agr, aes(x=gf,y=dur,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean normalized duration") +
  xlab("Accentual phrase") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)) +
  theme(legend.position="top")
ggsave("../graphs/duration-by-condition.pdf",height=4,width=9)


# models ----
names(d)
str(d$duration)
str(d$label)
d$label <- as.factor(d$label)
str(d$talker)
str(d$condition)
d$condition <- as.factor(d$condition)
str(d$Tone)
d$Tone <- as.factor(d$Tone)

table(d$gf)

#### duration of subjects
# Tone omitted as fixed effect because always LH 

d$condition <- relevel(d$condition, ref = "predFoc")

# Predict normalized duration of the subject from condition (Chodri)
m.1c = lmer(durationN ~ condition + (1|label) + (1+condition|talker), 
           data = d[d$language == "Chodri" & d$gf == "subject" & d$WorS == "word",])
summary(m.1c)
ranef(m.1c) #singular fit warning because effect of condition on talker is very small

# model reported in the paper
m.1c2 = lmer(durationN ~ condition + (1|label) + (1|talker), 
            data = d[d$language == "Chodri" & d$gf == "subject" & d$WorS == "word",])
summary(m.1c2) #no singular fit warning
# conditionsubjFoc   0.01093    0.00585 139.54974   1.868 0.063808 .  

# Predict normalized duration of the subject from condition (Gujarati)
m.1g = lmer(durationN ~ condition + (1|label) + (1+condition|talker), 
            data = d[d$language == "Gujarati" & d$gf == "subject" & d$WorS == "word",])
summary(m.1g)
ranef(m.1g) #singular fit warning because effect of condition on talker is very small

# model reported in the paper
m.1g2 = lmer(durationN ~ condition + (1|label) + (1|talker), 
             data = d[d$language == "Gujarati" & d$gf == "subject" & d$WorS == "word",])
summary(m.1g2) #no singular fit warning
# conditionsubjFoc 1.437e-02  7.543e-03 1.530e+02   1.905   0.0586 .

# Predict normalized duration of the subject from condition (Marathi)
m.1m = lmer(durationN ~ condition + (1|label) + (1+condition|talker), 
            data = d[d$language == "Marathi" & d$gf == "subject" & d$WorS == "word",])
summary(m.1m)
ranef(m.1m) #singular fit warning because effect of condition on talker is very small

# model reported in the paper
m.1m2 = lmer(durationN ~ condition + (1|label) + (1|talker), 
             data = d[d$language == "Marathi" & d$gf == "subject" & d$WorS == "word",])
summary(m.1m2) #no singular fit warning
# conditionsubjFoc 1.203e-02  5.045e-03 1.511e+02   2.385   0.0183 *

### duration of predicates
# Tone included as fixed effect in Gujarati and Marathi because some have LH, others have 0
table(d$gf,d$Tone)

d$condition <- relevel(d$condition, ref = "subjFoc")

# Predict normalized duration of the predicate from condition (Chodri)
m.1c = lmer(durationN ~ condition + (1|label) + (1+condition|talker), 
            data = d[d$language == "Chodri" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1c) # singular fit warning
ranef(m.1c)

# model reported in paper
m.1c2 = lmer(durationN ~ condition + (1|label) + (1|talker), 
            data = d[d$language == "Chodri" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1c2) 
# conditionpredFoc 8.290e-03  3.216e-03 1.374e+02   2.578    0.011 *  

# Predict normalized duration of the predicate from condition (Gujarati)
m.1g = lmer(durationN ~ condition * Tone + (1|label) + (1+condition|talker), 
            data = d[d$language == "Gujarati" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1g) # no singular fit warning
ranef(m.1g) 

m.1g2 = lmer(durationN ~ condition * Tone + (1|label) + (1|talker), 
             data = d[d$language == "Gujarati" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1g2) 

# model reported in the paper
m.1g3 = lmer(durationN ~ condition + Tone + (1|label) + (1|talker), 
             data = d[d$language == "Gujarati" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1g3) 
# conditionpredFoc 9.493e-03  3.374e-03 1.523e+02   2.813  0.00555 ** 

m.1g4 = lmer(durationN ~ condition + (1|label) + (1|talker), 
             data = d[d$language == "Gujarati" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1g4)

anova(m.1g2,m.1g3) #model with interaction not significantly better
anova(m.1g3,m.1g4) #model with Tone not significantly better

# Predict normalized duration of the predicate from condition (Marathi)
m.1m = lmer(durationN ~ condition * Tone + (1|label) + (1+condition|talker), 
            data = d[d$language == "Marathi" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1m) # singular fit warning
ranef(m.1m)

m.1m2 = lmer(durationN ~ condition * Tone + (1|label) + (1|talker), 
             data = d[d$language == "Marathi" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1m2) 

# model reported in paper
m.1m3 = lmer(durationN ~ condition + Tone + (1|label) + (1|talker), 
             data = d[d$language == "Marathi" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1m3) 
# conditionpredFoc 1.463e-02  3.751e-03 1.492e+02   3.900 0.000145 ***

m.1m4 = lmer(durationN ~ condition + (1|label) + (1|talker), 
             data = d[d$language == "Marathi" & d$gf == "predicate" & d$WorS == "word",])
summary(m.1m4) 

anova(m.1m2,m.1m3) #model with interaction not significantly better
anova(m.1m3,m.1m4) #model with Tone not significantly better

# older plots and analyses ----

## plot mean normalized duration of the subject syllables
table(d$WorS)

agr = d %>%
  filter(WorS == "syl" & gf == "subject") %>%
  group_by(condition,label,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr

# relevel factors for visualization
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))

ggplot(agr, aes(x=label,y=dur,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean normalized duration") +
  xlab("Syllable") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)) +
  theme(legend.position="top")

## plot mean normalized duration of the predicate syllables
table(d$WorS)

agr = d %>%
  filter(WorS == "syl" & gf == "predicate") %>%
  group_by(condition,label,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr

# relevel factors for visualization
agr = agr %>%
  mutate(condition=recode(condition, subjFoc = "subject focus", predFoc = "predicate focus"))
agr$condition <-factor(agr$condition, levels=c("subject focus","predicate focus"))

ggplot(agr, aes(x=label,y=dur,fill=condition)) +
  scale_fill_grey() +
  geom_bar(stat="identity",width = 0.8,position=position_dodge(.9),color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9)) +
  facet_grid(. ~ language) +
  ylab("Mean normalized duration") +
  xlab("Syllable") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)) +
  theme(legend.position="top")
