# Bhili-Kandeshi prosody project: Gujarati analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.r")

# load packages
library(tidyverse)
library(xtable)
library(lmerTest)
library(mgcv) 
library(itsadug)

theme_set(theme_bw())

# contour analysis (d1) ----
# Praat script: contours.praat

# read in data
d1 = read.csv("../../production-exp-data/Gujarati-contours.csv")
nrow(d1) #192 (12 talkers x 16 utterances)

head(d1)
summary(d1)
length(unique(d1$File)) #192

# exclude disfluent utterances
table(d1$Words)
d1$File[grepl("disfluent",d1$Words)] #6 utterances
d1 <- droplevels(subset(d1,!grepl("disfluent",d1$Words)))
nrow(d1) #186

# add columns 

# language
d1$language <- ifelse(grepl("GUJ-",as.character(d1$File)),"GUJ","OTHER")
table(d1$language)

# talker
d1$talker <- d1$File
d1$talker <- gsub("GUJ-","", d1$talker)
d1$talker <- gsub("-target[0-9]+.wav","", d1$talker)
table(d1$talker)
d1$talker <- as.factor(as.character(d1$talker))

# utterance
d1$utt <- d1$File
d1$utt <- gsub("GUJ-","", d1$utt)
d1$utt <- gsub("P[0-9]+-","", d1$utt)
d1$utt <- gsub(".wav","", d1$utt)
table(d1$utt)

# focus condition (subject focus, predicate focus)
d1$cond <- ifelse(d1$utt == "target1" | d1$utt == "target3" | 
                   d1$utt == "target5" | d1$utt == "target7" |
                   d1$utt == "target9" | d1$utt == "target11" |
                   d1$utt == "target13" | d1$utt == "target15", "subjFoc", "predFoc")
table(d1$cond)

d1$sentence <- d1$utt
d1$sentence <- ifelse(d1$utt == "target1" | d1$utt == "target8", "sentence18",
                     ifelse(d1$utt == "target2" | d1$utt == "target5", "sentence25",
                            ifelse(d1$utt == "target3" | d1$utt == "target6", "sentence36",
                                   ifelse(d1$utt == "target4" | d1$utt == "target13", "sentence413",
                                          ifelse(d1$utt == "target7" | d1$utt == "target10", "sentence710",
                                                 ifelse(d1$utt == "target9" | d1$utt == "target16", "sentence916",
                                                        ifelse(d1$utt == "target11" | d1$utt == "target14", "sentence1114", "sentence1215")))))))
table(d1$sentence)

head(d1)

# some sanity checks

# any missing values for any of the columns?
sapply(d1,function(x) sum(is.na(x)))

# how many unique entries are there in each column?
sapply(d1, function(x) length(unique(x)))

# what are the utterances
table(d1$Words)

# fix mislabeled contours (have pause after subject but LH annotation on subject)
str(d1$Contour)
d1$Contour <- as.character(d1$Contour)
# do all the utterances with pauses have % in the tier 1 annotation?
table(d.pause$ContourS) #2 utterances that have pause but not %
d1[d1$File == "GUJ-P2-target13.wav",]$Contour <- "L-H% LH LH L-L%" #instead of LH LH LH L-L%
d1[d1$File == "GUJ-P2-target3.wav",]$Contour <- "L-H% LH 0 0 L-L%" #instead of LH LH 0 0 L-L%

# simplified contour: merge all utterances ending in L-H\% and L-L\% to L and all utterances ending in 
# H-L% into H
table(d1$Contour)
d1$ContourS <- d1$Contour
# trim spaces
d1$ContourS <- trimws(d1$ContourS)
# reduce
d1$ContourS <- gsub("L-L\\%","L", d1$ContourS)
d1$ContourS <- gsub("0 L-H\\%","0 L", d1$ContourS)
d1$ContourS <- gsub("LH L-H\\%","LH L", d1$ContourS)
d1$ContourS <- gsub("LH LH L-H\\%","LH LH L", d1$ContourS)
d1$ContourS <- gsub("H-L\\%","H", d1$ContourS)
table(d1$ContourS)

# super simplified contour: also ignore whether LH or L-H% on subject and predicate
d1$ContourSS <- d1$ContourS
d1$ContourSS <- gsub("L-H\\%","LH", d1$ContourS)
table(d1$ContourSS)

# subset of utterances where predicate has LH 
length(unique(d1$File)) #186
pred.LH <- d1[grep("LH LH 0|LH LH LH",d1$ContourSS),]
head(pred.LH)
length(unique(pred.LH$File)) #173 (only 13 utterancse with dephrased predicate)
write.csv(pred.LH, "../data/GUJ-pred-LH.csv")

# subset of utterances where auxiliary has LH 
length(unique(d1$File)) #186
table(d1$ContourSS)
aux.LH <- d1[grep("LH LH LH H|0 0 LH L|LH LH LH L|LH 0 LH L|LH LH 0 LH L|LH LH LH L",d1$ContourSS),]
length(unique(aux.LH$File)) #120
write.csv(aux.LH, "../data/GUJ-aux-LH.csv")

# what's the final tone?
table(d1$Contour)
d1$finalTone <- d1$Contour
# trim spaces
d1$finalTone <- trimws(d1$finalTone)
d1$finalTone <- gsub("L-H\\% 0","", d1$finalTone)
d1$finalTone <- gsub("L-H\\% LH","", d1$finalTone)
d1$finalTone <- gsub("LH","", d1$finalTone)
d1$finalTone <- gsub("0","", d1$finalTone)
d1$finalTone <- trimws(d1$finalTone)
table(d1$finalTone)

# utterance-final tone by condition
table <- table(d1$finalTone,d1$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-utterance-final-tone-by-condition.tex")

# what are the contours that occur in the utterances?
table <- table(d1$ContourS,d1$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-simplified-contour-by-condition.tex")


# distribution of pauses, by condition
# does a pause after the subject occur more frequently in one condition?
d.pause <- droplevels(subset(d1,grepl("<p>",d1$Words),drop=FALSE))
d.pause
nrow(d.pause) #18
write.csv(d.pause, "../data/GUJ-pause.csv")

table <- table(d.pause$ContourS,d.pause$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-pause-after-subject-by-condition.tex")

# do all the utterances with pauses have % in the tier 1 annotation?
table(d.pause$ContourS) #2 utterances that have pause but not % (fixed above)
#d.pause[d.pause$ContourS == "LH LH LH L",]$File #GUJ-P2-target13.wav
#d.pause[d.pause$ContourS == "LH LH 0 0 L",]$File #GUJ-P2-target3.wav

# how often is there dephrasing in each condition?
table <- table(d1$ContourSS,d1$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-super-simplified-contour-by-condition.tex")

# realization of utterances with "kaam" by condition
table(d1$sentence,d1$Words)

# sentences with kaam: sentence916,sentence1215,sentence36,sentence18 (4 of 8)
# 4 sentences because 2 predicates that are combined with 2 different subjects

# how many sentences with "kaam"?
nrow(d1[(d1$sentence == "sentence916" | d1$sentence == "sentence1215" 
         | d1$sentence == "sentence36" | d1$sentence == "sentence18"),]) 
#94 (12 talkers x 4 sentences x 2 prosody = 98, minus disfluent utterances)

# contour of 94 sentences with "kaam" by condition
table <- table(d1[(d1$sentence == "sentence916" 
                   | d1$sentence == "sentence1215" 
                   | d1$sentence == "sentence36" 
                   | d1$sentence == "sentence18"),]$ContourSS,d1[(d1$sentence == "sentence916" 
                                                                  | d1$sentence == "sentence1215" 
                                                                  | d1$sentence == "sentence36" 
                                                                  | d1$sentence == "sentence18"),]$cond)


table

d1$kaamContour <- d1$ContourSS #initialize kaamContour
table(d1$kaamContour)
# 5 slots on tier 3: these utterances all have "kaam"
d1$kaamContour <- gsub("LH LH 0 LH L", "LH 0", d1$kaamContour)
d1$kaamContour <- gsub("LH 0 0 LH L", "0 0", d1$kaamContour)
d1$kaamContour <- gsub("LH 0 0 LH H", "0 0", d1$kaamContour)
d1$kaamContour <- gsub("LH LH 0 LH H", "LH 0", d1$kaamContour)
d1$kaamContour <- gsub("LH LH 0 LH L", "LH 0", d1$kaamContour)
d1$kaamContour <- gsub("LH LH 0 0 H", "LH 0", d1$kaamContour)
d1$kaamContour <- gsub("LH LH 0 0 L", "LH 0", d1$kaamContour)
d1$kaamContour <- gsub("LH LH LH 0 L", "LH LH", d1$kaamContour)
d1$kaamContour <- gsub("LH LH LH 0 H", "LH LH", d1$kaamContour)
d1$kaamContour <- gsub("LH LH LH LH L", "LH LH", d1$kaamContour)
d1$kaamContour <- gsub("LH LH LH LH H", "LH LH", d1$kaamContour)
d1$kaamContour <- gsub("LH 0 0 0 L", "0 0", d1$kaamContour)
# 4 slots on tier 3: these utterances may have "kaam", only change those that have it
d1[(d1$sentence == "sentence916" 
    | d1$sentence == "sentence1215" 
    | d1$sentence == "sentence36" 
    | d1$sentence == "sentence18"),]$kaamContour <- gsub("LH 0 LH L", "0", d1[(d1$sentence == "sentence916" 
                                                                                | d1$sentence == "sentence1215" 
                                                                                | d1$sentence == "sentence36" 
                                                                                | d1$sentence == "sentence18"),]$kaamContour)

d1[(d1$sentence == "sentence916" 
    | d1$sentence == "sentence1215" 
    | d1$sentence == "sentence36" 
    | d1$sentence == "sentence18"),]$kaamContour <- gsub("LH LH 0 H", "LH", d1[(d1$sentence == "sentence916" 
                                                                                | d1$sentence == "sentence1215" 
                                                                                | d1$sentence == "sentence36" 
                                                                                | d1$sentence == "sentence18"),]$kaamContour)
d1[(d1$sentence == "sentence916" 
    | d1$sentence == "sentence1215" 
    | d1$sentence == "sentence36" 
    | d1$sentence == "sentence18"),]$kaamContour <- gsub("LH LH 0 L", "LH", d1[(d1$sentence == "sentence916" 
                                                                                | d1$sentence == "sentence1215" 
                                                                                | d1$sentence == "sentence36" 
                                                                                | d1$sentence == "sentence18"),]$kaamContour)

d1[(d1$sentence == "sentence916" 
    | d1$sentence == "sentence1215" 
    | d1$sentence == "sentence36" 
    | d1$sentence == "sentence18"),]$kaamContour <- gsub("LH LH LH L", "LH", d1[(d1$sentence == "sentence916" 
                                                                                 | d1$sentence == "sentence1215" 
                                                                                 | d1$sentence == "sentence36" 
                                                                                 | d1$sentence == "sentence18"),]$kaamContour)
d1[(d1$sentence == "sentence916" 
    | d1$sentence == "sentence1215" 
    | d1$sentence == "sentence36" 
    | d1$sentence == "sentence18"),]$kaamContour <- gsub("LH LH LH H", "LH", d1[(d1$sentence == "sentence916" 
                                                                                 | d1$sentence == "sentence1215" 
                                                                                 | d1$sentence == "sentence36" 
                                                                                 | d1$sentence == "sentence18"),]$kaamContour)
# remove spaces at end and beginning
d1$kaamContour <- trimws(d1$kaamContour)

table(d1$kaamContour)

# contour of predicate + kaam of sentences with kaam
table <- table(d1[(d1$sentence == "sentence916" 
                   | d1$sentence == "sentence1215" 
                   | d1$sentence == "sentence36" 
                   | d1$sentence == "sentence18"),]$kaamContour,d1[(d1$sentence == "sentence916" 
                                                                    | d1$sentence == "sentence1215" 
                                                                    | d1$sentence == "sentence36" 
                                                                    | d1$sentence == "sentence18"),]$cond)
table 
#94 entries (good because 94 utterances with kaam)

print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-kaam-super-simplified-contour-by-condition.tex")

# for duration analysis: create info about kaam with LH or 0
names(d1)
table(d1$kaamContour)

kaamLHLH <- droplevels(subset(d1,d1$kaamContour == "LH LH"))
nrow(kaamLHLH) #38
write.csv(kaamLHLH, "../data/GUJ-kaamLHLH.csv")

# ...contour analysis: models ----
# make new columns

d1$predLH <- ifelse(d1$File %in% pred.LH$File,"1","0")
table(d1$predLH)
d1$predLH <- as.factor(d1$predLH)

d1$auxLH <- ifelse(d1$File %in% aux.LH$File,"1","0")
table(d1$auxLH)
d1$auxLH <- as.factor(d1$auxLH)

d1$auxLHpredLH <- ifelse(d1$File %in% pred.LH$File & d1$File %in% aux.LH$File, "1","0")
table(d1$auxLHpredLH)
d1$auxLHpredLH <- as.factor(d1$auxLHpredLH)

# logistic regression models

# predict dephrasing of predicate over all utterances
m.pred <- glmer(predLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1)
summary(m.pred) #does not converge
m.pred.2 <- glmer(predLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1)
summary(m.pred.2) #does not converge
m.pred.3 <- glmer(predLH ~ cond + (1|talker),family=binomial(link='logit'),data=d1)
summary(m.pred.3)

# predict dephrasing of auxiliary over all utterances
m.aux <- glmer(auxLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1)
summary(m.aux) #high correlation
m.aux.2 <- glmer(auxLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1)
summary(m.aux.2)

# predict dephrasing of auxiliary over all utterances with LH on predicate
m.aux2 <- glmer(auxLHpredLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1[d1$predLH == "1",])
summary(m.aux2) #high correlation
m.aux2.2 <- glmer(auxLHpredLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1[d1$predLH == "1",])
summary(m.aux2.2)

# by-talker plots for dephrasing of predicate
ggplot(data=d1, aes(x=talker, fill=predLH)) +
  geom_bar(stat="count") +
  facet_grid(. ~ cond)
ggsave("../graphs/GUJ-dephrasing-pred-by-condition.pdf",height=6,width=8)

# by-talker plots for dephrasing of aux in utterances with LH on predicate
ggplot(data=d1[d1$predLH == "1",], aes(x=talker, fill=auxLHpredLH)) +
  geom_bar(stat="count") +
  facet_grid(. ~ cond)
ggsave("../graphs/GUJ-dephrasing-aux-by-condition.pdf",height=6,width=8)


# prosodic system (d2) ----
# Praat script: extract-labels-tier4.praat

# the data here consist of tier 4 annotations of the L and the H of the 
# subject and the predicate
d2 = read.csv("../../production-exp-data/Gujarati-LH-syllables.csv")
nrow(d2) #731 (192 files x about 3.8 tones on tier 4)
head(d2)
summary(d2)
length(unique(d2$File)) #186 (6 disfluent utterances do not have annotations on tier 4)

# exclude disfluent files
d2 <- droplevels(subset(d2,d2$File != "GUJ-P2-target1.wav" 
                        & d2$File != "GUJ-P2-target14.wav" 
                        & d2$File != "GUJ-P2-target5.wav" 
                        & d2$File != "GUJ-P2-target11.wav"
                        & d2$File != "GUJ-P2-target2.wav"
                        & d2$File != "GUJ-P2-target9.wav"))

length(unique(d2$File)) #good, still 186

table(d2$Word)
table(d2$Syllable)
# trim spaces
d2$Syllable <- trimws(d2$Syllable)
d2$Word <- trimws(d2$Word)

### add columns 

# language
d2$language <- ifelse(grepl("GUJ-",as.character(d2$File)),"GUJ","OTHER")
table(d2$language)

# talker
d2$talker <- d2$File
d2$talker <- gsub("GUJ-","", d2$talker)
d2$talker <- gsub("-target[0-9]+.wav","", d2$talker)
table(d2$talker)
d2$talker <- as.factor(as.character(d2$talker))

# utterance
d2$utt <- d2$File
d2$utt <- gsub("GUJ-","", d2$utt)
d2$utt <- gsub("P[0-9]+-","", d2$utt)
d2$utt <- gsub(".wav","", d2$utt)
table(d2$utt)

# focus condition (subject focus, predicate focus)
d2$cond <- ifelse(d2$utt == "target1" | d2$utt == "target3" | 
                   d2$utt == "target5" | d2$utt == "target7" |
                   d2$utt == "target9" | d2$utt == "target11" |
                   d2$utt == "target13" | d2$utt == "target15", "subjFoc", "predFoc")
table(d2$cond)

# grammatical function (subject, verb)
d2$gf <- ifelse(d2$Word == "laavanya" | d2$Word == "ananyaa" | 
                  d2$Syllable == "laa" | d2$Syllable == "van" |
                  d2$Syllable == "ya" | d2$Syllable == "a" |
                  d2$Syllable == "nan" | d2$Syllable == "yaa", "subject", 
                ifelse(d2$Word == "aaraam" | d2$Word == "nindvaanu" |
                         d2$Word == "vyaayaam" | d2$Word == "kheDvaanu" |
                         d2$Syllable == "aa" | d2$Syllable == "raam" |
                         d2$Syllable == "nind" | d2$Syllable == "vaa" |
                         d2$Syllable == "nu" | d2$Syllable == "vyaa" | d2$Syllable == "yaam" |
                         d2$Syllable == "kheD","predicate", "other"))
table(d2$gf)
table(d2$Syllable,d2$gf)

head(d2)

# does each subject have a L and a H on it? (all subjects have LH)
# there are 186 subjects (because 186 files), each should have L and H
table(d2[d2$gf == "subject",]$Tone) #186
table(d2[d2$Word == "ananyaa",]$Tone) #93
table(d2[d2$Word == "laavanya",]$Tone) #93

# exclude predicates with 0 from count (these have L annotation!)
table(d2$gf,d2$Syllable)
table(d2$gf)
table(d2$Word)

pred.LH <- read.csv(file="../data/GUJ-pred-LH.csv", header=TRUE, sep=",")
nrow(pred.LH) #173

table(d1$ContourSS) # 13 utterances where predicate has 0
length(unique(d2[!(d2$File %in% pred.LH$File),]$File)) #13
length(unique(d2[d2$File %in% pred.LH$File,]$File)) #173
length(unique(pred.LH$File)) #173 utterances with LH on predicate
 
d2.predLH <- d2
d2.predLH <- droplevels(subset(d2.predLH, !(d2.predLH$gf == "predicate" & !d2.predLH$File %in% pred.LH$File)))
length(unique(d2.predLH$File)) #173
length(unique(d2.predLH[d2.predLH$gf == "predicate",]$File)) #173

# does each predicate with a LH (186-13 = 173) have a L and a H on it?
table(d2.predLH$gf)
table(d2.predLH[d2.predLH$gf == "predicate",]$Tone) #173

# which syllables is the L realized on?
syl.L <- droplevels(subset(d2.predLH,d2.predLH$Tone == "L"))
table <- table(syl.L$Word)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-words-total.tex")

table <- table(syl.L$Syllable)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-syllables-of-L.tex")

# which syllables is the H realized on?
syl.H <- droplevels(subset(d2.predLH,d2.predLH$Tone == "H"))
table(syl.H$Word)
table <- table(syl.H$Syllable)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/GUJ-syllables-of-H.tex")

# for the 3-syllable words, for how many is the L on the 1st syl and the H on the 3rd syl?
# this provides evidence for the L_a H_a vs the L*+H hypotheses
# 3-syllable words: ananyaa, laavanya, kheDvaanu, nindvaanu
head(d2.predLH)
library(reshape2)
LH.syls <- dcast(d2.predLH, File + Word ~ Tone, value.var="Syllable")
head(LH.syls)

# subset to the 3-syllable words
table(LH.syls$Word)
LH.3 <- droplevels(subset(LH.syls,LH.syls$Word == "ananyaa" | LH.syls$Word == "laavanya" 
                          | LH.syls$Word == "kheDvaanu" | LH.syls$Word == "nindvaanu"))
nrow(LH.3) #272 (93 ananyaa, 93 laavanya, 40 kheDvaanu, 46 nindvaanu)
head(LH.3)

t <- table(LH.3$L,LH.3$H)
t
t2 <- prop.table(t,1)
t2 %>% `*`(100) %>% round(2)

# duration analysis (d3) ----
# Praat script: duration.praat

d3 = read.csv("../../production-exp-data/Gujarati-durations.csv")
nrow(d3)
head(d3)
summary(d3)
length(unique(d3$File)) #192 = 12 talkers x 16 utterances

# remove spaces from labels
d3$label <- trimws(d3$label)

# exclude disfluent files
d3 <- droplevels(subset(d3,d3$File != "GUJ-P2-target1.wav" 
                        & d3$File != "GUJ-P2-target14.wav" 
                        & d3$File != "GUJ-P2-target5.wav" 
                        & d3$File != "GUJ-P2-target11.wav"
                        & d3$File != "GUJ-P2-target2.wav"
                        & d3$File != "GUJ-P2-target9.wav"))

length(unique(d3$File)) #186

# exclude utterances with utterance-internal pause
table(d3$label)
length(unique(d3$File)) #186
pauseGUJ <- read.csv(file="../data/GUJ-pause.csv", header=TRUE, sep=",")
pauseGUJ
nrow(pauseGUJ) #18
str(pauseGUJ$File)
str(d3$File)

d3 <- droplevels(subset(d3, !(d3$File %in% pauseGUJ$File)))
length(unique(d3$File)) #168

# exclude 6 kaam utterances where predicate+kaam jointly realize LH (doesn't happen in Gujarati)
#d3 <- droplevels(subset(d3,d3$File %in% d1[d1$kaamContour != "LH",]$File))
#length(unique(d3$File)) #168

### add columns 

# language
d3$language <- ifelse(grepl("GUJ-",as.character(d3$File)),"GUJ","OTHER")
table(d3$language)

# talker
d3$talker <- d3$File
d3$talker <- gsub("GUJ-","", d3$talker)
d3$talker <- gsub("-target[0-9]+.wav","", d3$talker)
table(d3$talker)
d3$talker <- as.factor(as.character(d3$talker))

# utterance
d3$utt <- d3$File
d3$utt <- gsub("GUJ-","", d3$utt)
d3$utt <- gsub("P[0-9]+-","", d3$utt)
d3$utt <- gsub(".wav","", d3$utt)
table(d3$utt)

# focus condition (subject focus, predicate focus)
d3$cond <- ifelse(d3$utt == "target1" | d3$utt == "target3" | 
                   d3$utt == "target5" | d3$utt == "target7" |
                   d3$utt == "target9" | d3$utt == "target11" |
                   d3$utt == "target13" | d3$utt == "target15", "subjFoc", "predFoc")
table(d3$cond)

table(d3$label)

# grammatical function (subject, predicate)
d3$gf <- ifelse(d3$label == "laavanya" | d3$label == "ananyaa" | 
                  d3$label == "laa" | d3$label == "van" |
                  d3$label == "ya" | d3$label == "a" |
                  d3$label == "nan" | d3$label == "yaa", "subject", 
                ifelse(d3$label == "aaraam" | d3$label == "nindvaanu" |
                         d3$label == "vyaayaam" | d3$label == "kheDvaanu" |
                         d3$label == "aa" | d3$label == "raam" |
                         d3$label == "nind" | d3$label == "vaa" |
                         d3$label == "nu" | d3$label == "vyaa" | d3$label == "yaam" |
                         d3$label == "kheD","predicate", "other"))
table(d3$gf)

# word or syllable
d3$WorS <- ifelse(d3$label == "laavanya" | d3$label == "ananyaa" | 
                   d3$label == "aaraam" | d3$label == "vyaayaam" |
                   d3$label == "kheDvaanu" | d3$label == "nindvaanu","word", 
                 ifelse(d3$label == "laa" | d3$label == "van" |
                          d3$label == "ya" | d3$label == "a" |
                          d3$label == "nan" | d3$label == "yaa" |
                          d3$label == "aa" | d3$label == "raam" |
                          d3$label == "vyaa" | d3$label == "yaam" |
                          d3$label == "kheD" | d3$label == "vaa" | d3$label == "nu" |
                          d3$label == "nind","syl","other"))

table(d3$WorS)

# normalized duration
d3$durationN <- d3$duration / d3$totalDuration

# all subjects have LH on them, so we don't need to consider the presence/absence
# of a tone in the models

# for predicates and kaam, make a new column that captures whether they have LH or 0

predTone <- read.csv(file="../data/GUJ-pred-LH.csv", header=TRUE, sep=",")
nrow(predTone) #173
kaamTone <- read.csv(file="../data/GUJ-kaamLHLH.csv", header=TRUE, sep=",")
nrow(kaamTone) #38

d3$Tone <- "0"
d3$Tone[d3$gf == "subject"] <- "LH"
d3$Tone[d3$gf == "predicate" & d3$File %in% predTone$File] <- "LH"
d3$Tone[d3$label == "kaam" & d3$File %in% kaamTone$File] <- "LH"

table(d3$gf,d3$Tone) #all subjects LH

# some sanity checks

# to determine if there are missing values for any of the columns
sapply(d3,function(x) sum(is.na(x)))

# how many unique entries are there in each column?
sapply(d3, function(x) length(unique(x)))

# ..duration: plots ----

# duration of subject words and syllables by condition
table(d3$gf,d3$language)

agr = d3 %>%
  filter(gf == "subject") %>%
  group_by(cond,label,language) %>%
  summarise(dur=mean(duration),ci.low=ci.low(duration),ci.high=ci.high(duration)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
dodge=position_dodge(.9)

ggplot(agr, aes(x=label,y=dur,fill=cond)) +
  scale_fill_grey() +
  #scale_fill_manual(values=c("brown", "blue")) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  ylab("Duration (ms)")+
  xlab("Subjects: words and syllables") 
ggsave("../graphs/GUJ-subject-duration-by-condition.pdf",height=4,width=10)

# normalized duration of subject words and syllables by condition
agr = d3 %>%
  filter(gf == "subject") %>%
  group_by(cond,label,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
dodge=position_dodge(.9)

ggplot(agr, aes(x=label,y=dur,fill=cond)) +
  scale_fill_grey() +
  #scale_fill_manual(values=c("brown", "blue")) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  ylab("Normalized duration (ms)")+
  xlab("Subjects: words and syllables") 
ggsave("../graphs/GUJ-subject-durationN-by-condition.pdf",height=4,width=10)

# Konstanz talk plot: normalized duration of subjects
names(d3)
table(d3$cond,d3$gf)
max(d3$durationN)

agr = d3 %>%
  filter(WorS == "word" & gf == "subject") %>%
  group_by(cond,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
str(agr$cond)
agr$cond <- as.factor(agr$cond)
dodge=position_dodge(.9)

levels(agr$cond) <- c('pre-focal','in focus')
agr$cond <- relevel(agr$cond, ref = "in focus")
agr

ggplot(agr, aes(x=cond,y=dur,fill=cond)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0,0.35)) +
  #ylab("Mean normalized duration")+
  theme(axis.title.y=element_blank()) +
  xlab("Condition") 
ggsave("/Users/tonhauser.1/Dropbox/Konstanz2018-talk/graphs/GUJ-duration-subj.pdf",height=3,width=2)

# Konstanz talk plot: normalized duration of predicate words
names(d3)
table(d3$cond,d3$gf)
max(d3$durationN)

agr = d3 %>%
  filter(WorS == "word" & gf == "predicate") %>%
  group_by(cond,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
str(agr$cond)
agr$cond <- as.factor(agr$cond)
dodge=position_dodge(.9)

levels(agr$cond) <- c('in focus','post-focal')
agr

ggplot(agr, aes(x=cond,y=dur,fill=cond)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0,0.3)) +
  #ylab("Mean normalized duration")+
  theme(axis.title.y=element_blank()) +
  xlab("Condition") 
ggsave("/Users/tonhauser.1/Dropbox/Konstanz2018-talk/graphs/GUJ-duration-pred.pdf",height=3,width=2)


# duration of verb words and syllables by condition
agr = d3 %>%
  filter(gf == "predicate") %>%
  group_by(cond,label,language) %>%
  summarise(dur=mean(duration),ci.low=ci.low(duration),ci.high=ci.high(duration)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
dodge=position_dodge(.9)

ggplot(agr, aes(x=label,y=dur,fill=cond)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  ylab("Duration (ms)") +
  xlab("Predicates: words and syllables") 
ggsave("../graphs/GUJ-predicate-duration-by-condition.pdf",height=4,width=10)

# normalized duration of verb words and syllables by condition
agr = d3 %>%
  filter(gf == "predicate") %>%
  group_by(cond,label,language) %>%
  summarise(dur=mean(durationN),ci.low=ci.low(durationN),ci.high=ci.high(durationN)) %>%
  mutate(YMin=dur-ci.low,YMax=dur+ci.high)
agr
dodge=position_dodge(.9)

ggplot(agr, aes(x=label,y=dur,fill=cond)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ language) +
  ylab("Duration (ms)") +
  xlab("Predicates: words and syllables") 
ggsave("../graphs/GUJ-predicate-durationN-by-condition.pdf",height=4,width=10)

# ..duration: models ----
names(d3)
str(d3$duration)
str(d3$label)
d3$label <- as.factor(d3$label)
str(d3$talker)
str(d3$cond)
d3$cond <- as.factor(d3$cond)
str(d3$Tone)
d3$Tone <- as.factor(d3$Tone)

table(d3$gf)

#### duration of subjects
# Tone omitted as fixed effect because always LH 

# Is the raw duration of the subject (word) predicted by condition?
table(d3[d3$gf == "subject" & d3$WorS == "word",]$label)

m.1 = lmer(duration ~ cond + (1|label) + (1+cond|talker), 
           data = d3[d3$gf == "subject" & d3$WorS == "word",])
summary(m.1)

# Is the raw duration of the stressed syllable of the subjects predicted by condition?
m.2 = lmer(duration ~ cond + (1|label) + (1+cond|talker), 
           data= d3[d3$label == "laa" | d3$label == "nan",])
summary(m.2)
anova(m.2)

# Is the raw duration of the unstressed syllables of the subjects predicted by condition?
m.3 = lmer(duration ~ cond + (1|label) + (1+cond|talker), 
             data=d3[d3$label == "a" | d3$label == "yaa" | d3$label == "van" | d3$label == "ya",])
summary(m.3)

# Is the normalized duration of the subject (word) predicted by condition?
m.1n = lmer(durationN ~ cond + (1|label) + (1+cond|talker), 
           data = d3[d3$gf == "subject" & d3$WorS == "word",])
summary(m.1n)
anova(m.1n)

# Is the normalized duration of the stressed syllable of the subjects predicted by condition?
m.2n = lmer(durationN ~ cond + (1|label) + (1+cond|talker), 
           data= d3[d3$label == "laa" | d3$label == "nan",])
summary(m.2n)
anova(m.2n)

# Is the normalized duration of the unstressed syllables of the subjects predicted by condition?
m.3n = lmer(durationN ~ cond + (1|label) + (1+cond|talker), 
           data=d3[d3$label == "a" | d3$label == "yaa" | d3$label == "van" | d3$label == "ya",])
summary(m.3n)
anova(m.3n)

### duration of predicates
# Tone included as fixed effect because not all predicates have LH
table(d3$gf,d3$Tone)

# Is the raw duration of the predicate (word) predicted by condition?
m.1 = lmer(duration ~ cond + Tone + (1|label) + (1+cond|talker), 
           data = d3[d3$gf == "predicate" & d3$WorS == "word",])
summary(m.1)
anova(m.1)

# Is the raw duration of the stressed syllables of the predicates predicted by condition?
m.2 = lmer(duration ~ cond + Tone + (1|label) + (1+cond|talker), 
           data= d3[d3$label == "naan" | d3$label == "nin" | d3$label == "yaam" | d3$label == "raam",])
summary(m.2)
anova(m.2)

# Is the raw duration of the unstressed syllables of the predicates predicted by condition?
m.3 = lmer(duration ~ cond + Tone + (1|label) + (1+cond|talker), 
           data=d3[d3$label == "aa" | d3$label == "vyaa" | d3$label == "Ni" | d3$label == "da" | d3$label == "gar",])
summary(m.3)
anova(m.3)

# Is the normalized duration of the predicate (word) predicted by condition?
d3$cond <- relevel(d3$cond, ref = "subjFoc")

m.1n = lmer(durationN ~ cond + (1|label) + (1+cond|talker), 
           data = d3[d3$gf == "predicate" & d3$WorS == "word",])
summary(m.1n)

# Is the normalized duration of the stressed syllables of the predicates predicted by condition?
m.2n = lmer(durationN ~ cond + Tone + (1|label) + (1+cond|talker), 
           data= d3[d3$label == "naan" | d3$label == "nin" | d3$label == "yaam" | d3$label == "raam",])
summary(m.2n)
anova(m.2n)

# Is the normalized duration of the unstressed syllables of the predicates predicted by condition?
m.3n = lmer(durationN ~ cond + Tone + (1|label) + (1+cond|talker), 
           data=d3[d3$label == "aa" | d3$label == "vyaa" | d3$label == "Ni" | d3$label == "da" | d3$label == "gar",])
summary(m.3n)
anova(m.3n)

## kaam

# Is the raw duration of kaam predicted by condition? model includes tone info
m.kaam = lmer(duration ~ cond + Tone + (1+cond|talker), 
              data=d3[d3$label == "kaam",])
summary(m.kaam)
anova(m.kaam)

# Is the normalized duration of the predicate (word) predicted by condition?
m.kaam.n = lmer(durationN ~ cond + Tone + (1+cond|talker), 
                data = d3[d3$label == "kaam",])
summary(m.kaam.n)

# GAMM preparation (d4)  ----
# determine pitch range for each talker to extract f0 values for GAMM analysis
# Praat script: utt-f0-min-max-mean.praat

d4 = read.csv("../../production-exp-data/Gujarati-f0values.csv")
nrow(d4) #192 (= 12 talkers x 16 utts)
head(d4)
summary(d4)

table(d4$File)
# add ".wav" to File names
d4$File <- paste(d4$File,".wav",sep="")

### add columns 

# talker
d4$talker <- d4$File
d4$talker <- gsub("GUJ-","", d4$talker)
d4$talker <- gsub("-target[0-9]+.wav","", d4$talker)
table(d4$talker)

# for each talker, get utterance mean f0 min, mean f0 max and mean f0 and sd
# to determine range for f0 value extraction for GAMM analysis
agr = d4 %>%
  group_by(talker) %>%
  summarise(meanf0min = mean(uttf0min), f0min.sd = sd(uttf0min),meanf0=mean(uttf0mean), meanf0max = mean(uttf0max),f0max.sd = sd(uttf0max))
agr

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

# get impression for each language of f0 range and sd
agr = d4 %>%
  summarise(meanf0min = mean(uttf0min), meanf0max = mean(uttf0max), meanRange = mean(uttf0max-uttf0min))
agr

# GAMM analysis (d5) ----
# This analysis is based on f0 values extracted every 10ms in the subject and the predicate
# The f0 contour is plotted and modeled for the subject and the predicate together
# Praat script: GUJ-pitch-over-time.praat

d5 <- read.csv("../../production-exp-data/Gujarati-pitch-by-time.csv")
head(d5)
nrow(d5) #20760

# add ".wav" to File names
table(d5$File)
d5$File <- paste(d5$File,".wav",sep="")

length(unique(d5$File)) #186 (the 6 disfluent-marked utterances have no subject and predicate in tier 1)

# exclude utterances

# exclude disfluent files
d5 <- droplevels(subset(d5,d5$File != "GUJ-P2-target1.wav" 
                        & d5$File != "GUJ-P2-target14.wav" 
                        & d5$File != "GUJ-P2-target5.wav" 
                        & d5$File != "GUJ-P2-target11.wav"
                        & d5$File != "GUJ-P2-target2.wav"
                        & d5$File != "GUJ-P2-target9.wav"))

length(unique(d5$File)) # good, still 186

# exclude 18 utterances with utterance-internal pause
table(d5$label)
length(unique(d5$File)) #186
pauseGUJ <- read.csv(file="../data/GUJ-pause.csv", header=TRUE, sep=",")
pauseGUJ
nrow(pauseGUJ) #18
str(pauseGUJ$File)
str(d5$File)

d5 <- droplevels(subset(d5, !(d5$File %in% pauseGUJ$File)))
length(unique(d5$File)) #168 

nrow(d5) #18662

# create additional columns

# language
d5$language <- ifelse(grepl("GUJ-",as.character(d5$File)),"GUJ","OTHER")
d5$language <- as.factor(d5$language)
table(d5$language)

# talker
d5$talker <- d5$File
d5$talker <- gsub("GUJ-","", d5$talker)
d5$talker <- gsub("-target[0-9]+","", d5$talker)
d5$talker <- gsub(".wav","", d5$talker)
d5$talker <- as.factor(d5$talker)
table(d5$talker)

# utterance
d5$utt <- d5$File
d5$utt <- gsub("GUJ-","", d5$utt)
d5$utt <- gsub("P[0-9]+-","", d5$utt)
d5$utt <- gsub(".wav","", d5$utt)
d5$utt <- as.factor(d5$utt)
table(d5$utt)

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

d5$sentence <- d5$utt
d5$sentence <- ifelse(d5$utt == "target1" | d5$utt == "target8", "sentence18",
                     ifelse(d5$utt == "target2" | d5$utt == "target5", "sentence25",
                            ifelse(d5$utt == "target3" | d5$utt == "target6", "sentence36",
                                   ifelse(d5$utt == "target4" | d5$utt == "target13", "sentence413",
                                          ifelse(d5$utt == "target7" | d5$utt == "target10", "sentence710",
                                                 ifelse(d5$utt == "target9" | d5$utt == "target16", "sentence916",
                                                        ifelse(d5$utt == "target11" | d5$utt == "target14", "sentence1114", "sentence1215")))))))
table(d5$sentence)

# focus condition (subject focus, verb focus)
d5$cond <- ifelse(d5$utt == "target1" | d5$utt == "target3" | 
                   d5$utt == "target5" | d5$utt == "target7" |
                   d5$utt == "target9" | d5$utt == "target11" |
                   d5$utt == "target13" | d5$utt == "target15", "subjFoc", "predFoc")
d5$cond <- as.factor(d5$cond)
table(d5$cond)

# grammatical function
table(d5$label)
d5$gf <- ifelse(d5$label == "kaam", "kaam",
               ifelse(d5$label == "laavanya" | d5$label == "ananyaa", "subject","predicate"))
d5$gf <- as.factor(d5$gf)
table(d5$gf)

# plot raw f0 values over time to see if one talker's values were particularly badly extracted
table(d5$sentence)

ggplot(data=d5[d5$sentence == "sentence1114",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence1114.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence18",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence18.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence36",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence36.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence710",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence710.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence1215",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence1215.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence25",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence25.pdf",height=14,width=14)
# P8 predFoc

ggplot(data=d5[d5$sentence == "sentence413",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence413.pdf",height=14,width=14)
#P6 predFoc

ggplot(data=d5[d5$sentence == "sentence916",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/GUJ-raw-f0-contour-sentence916.pdf",height=14,width=14)

# normalize the f0 values by time and talker
# s.t. subject goes from 0 to .5 and predicate goes from .5 to 1, kaam excluded

# find the first and last time of each subject and predicate utterance
minT = aggregate(time ~ File + label, data=d5, FUN=min)
colnames(minT)[ncol(minT)] = 'time.min'
head(minT)

maxT = aggregate(time ~ File + label, data=d5, FUN=max)
colnames(maxT)[ncol(maxT)] = 'time.max'
head(maxT)

# merge the first and last time to one data frame
minmaxT = merge(minT,maxT,by=c('File','label'))
head(minmaxT)
table(minmaxT$label) #kaam still present

# add first and last time info to data
d5 = merge(d5,minmaxT,by=c('File','label'))
head(d5)

# now we know what the min and max time of the subject and the predicate is (and kaam)

# remove rows pertaining to "kaam"
d5 <- droplevels(subset(d5,d5$gf != "kaam"))
length(unique(d5$File)) # still 168
table(d5$gf)

# normalize each time such that subject goes from 0 to .499 and
# predicate from .5 to 1 
d5$Time <- 0
d5$Time <- ifelse(d5$gf == "subject", 
                  (d5$time - d5$time.min) / ((d5$time.max - d5$time.min)*2), 
                  ((d5$time - d5$time.min) / ((d5$time.max - d5$time.min)*2))+.5)

head(d5)
sapply(d5,function(x) sum(is.na(x))) #1630 missing f0 values
nrow(d5) #16443

# min and max times per subject and predicate (to see if normalization worked so far; not done yet)
min(d5[d5$gf == "subject",]$Time) #0
max(d5[d5$gf == "subject",]$Time) #.5
min(d5[d5$gf == "predicate",]$Time) #.5
max(d5[d5$gf == "predicate",]$Time) #1

# for the subject times that are .5, subtract 1ms to make them precede min of predicate
d5$Time2 <- d5$Time
d5$Time2 <- ifelse(d5$gf == "subject" & d5$Time == 0.5, 0.499, d5$Time)
max(d5[d5$gf == "subject",]$Time2)   #.499   

# now that it worked, set Time to Time2
d5$Time <- d5$Time2

# plots
boxplot(d5$time, main='Original time')

boxplot(d5[d5$gf == "subject",]$Time, main='Normalized time')
boxplot(d5[d5$gf == "predicate",]$Time, main='Normalized time')

# normalize f0

# find f0 mean and sd of each talker
f0mean = aggregate(f0 ~ talker, data=d5, FUN=mean)
colnames(f0mean)[ncol(f0mean)] = 'f0.mean'
f0sd = aggregate(f0 ~ talker, data=d5, FUN=sd)
colnames(f0sd)[ncol(f0sd)] = 'f0.sd'
f0 = merge(f0mean, f0sd, by='talker')
f0

# save the f0 mean and sd for each talker for LH analysis 
write.csv(f0, "../data/GUJ-f0-mean-sd-by-talker.csv", row.names=FALSE)

# merge this talker f0 mean and sd with data 
d5 = merge(d5, f0, by='talker')
head(d5)

# z-transform f0
d5$f0.z = (d5$f0 - d5$f0.mean) / d5$f0.sd
head(d5)

# plots
par(mfrow=c(1,2))
boxplot(d5$f0,main='Original f0')
boxplot(d5$f0.z,main='Z-transformed f0 by talker') 

ggplot(d5, aes(x=talker, y=f0.z)) + 
  geom_boxplot()

# change outliers to NA: f0.z values below -3 and above 3
nrow(d5) #16443
nrow(d5[d5$f0.z > 3 & !is.na(d5$f0.z),]) #14
nrow(d5[d5$f0.z < -3 & !is.na(d5$f0.z),]) #235
# very few data points are changed

d5$f0.z[d5$f0.z > 3] <- NA
d5$f0.z[d5$f0.z < -3] <- NA
nrow(d5)

sapply(d5,function(x) sum(is.na(x))) #1089

ggplot(d5, aes(x=talker, y=f0.z)) + 
  geom_boxplot()

# plot normalized f0 values over time

ggplot(data=d5[d5$sentence == "sentence1114",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence1114.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence18",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z)")
ggsave("../graphs/GUJ-f0z-contour-sentence18.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence36",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence36.pdf",height=14,width=14)


ggplot(data=d5[d5$sentence == "sentence710",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence710.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence1215",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence1215.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence25",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence25.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence413",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence413.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence916",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/GUJ-f0z-contour-sentence916.pdf",height=14,width=14)

# transform the data so that the subject/predicate combination is a time series

# start getting the data in the right shape for GAMM
d5 <- start_event(d5, event=c("File","utt"))
head(d5)
summary(d5)

str(d5$cond)
str(d5$talker)
str(d5$sentence)
d5$sentence <- as.factor(d5$sentence)

# first model to establish residuals
m.tmp <-  bam(f0.z ~ s(Time, by = cond, bs='ad') + cond + s(Time, talker, by=cond, 
              bs = "fs", m = 1) + s(Time, sentence, by=cond, bs = "fs", m = 1), 
              data = d5, discrete = T, nthreads = 2)
macf <- acf_resid(m.tmp)
(rhoval <- macf[2]) #0.87

m <- bam(f0.z ~ s(Time, by = cond, bs='ad') + cond + s(Time, talker, by=cond, 
          bs = "fs", m = 1) + s(Time, sentence, by=cond, bs = "fs", m = 1), 
         data = d5, discrete = T, nthreads = 2, 
          rho=rhoval, AR.start=d5$start.event)
(smry <- summary(m))

# plot 
pdf("../graphs/GUJ-GAM-subjPred.pdf",width = 8, height = 4)
par(mfrow=c(1,2))
plot_smooth(m, view='Time',plot_all='cond',rm.ranef=T, rug=F, main='Utterances by condition')
plot_diff(m, view = "Time",rm.ranef=T,comp = list(cond = c("subjFoc","predFoc")), 
          main = "Difference subjFoc - predFoc",ylim = c(-2,2))
dev.off()

# not significant so remainder of analysis not performed

# # binary model showing significance of difference
# d5$IsPredFocus <- (d5$cond == 'predFoc')*1
# 
# mb.tmp <- bam(f0.z ~ s(Time) + s(Time, by = IsPredFocus) 
#           + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1)
#           + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1),
#           data = d5, discrete = T, nthreads = 2)
# 
# macf <- acf_resid(mb.tmp)
# (rhoval <- macf[2]) #0.93
# 
# mb <- bam(f0.z ~ s(Time) + s(Time, by = IsPredFocus) 
#           + s(Time, talker, by=IsPredFocus, bs = "fs", m = 1)
#           + s(Time, sentence, by=IsPredFocus, bs = "fs", m = 1),
#            data = d5, discrete = T, nthreads = 2, rho=rhoval, 
#            AR.start=d5$start.event)
# (smry.mb <- summary(mb))
# 
# # ordered factor model
# d5$IsPredFocusO <- as.ordered(d5$IsPredFocus)
# contrasts(d5$IsPredFocusO) <- 'contr.treatment'
# 
# # ordered factor model showing significance of difference separately for constant difference and non-linear difference
# mo.tmp <- bam(f0.z ~ s(Time) + s(Time, by = IsPredFocusO) 
#            + IsPredFocusO 
#            +  s(Time, talker, by=IsPredFocusO, bs = "fs", m = 1)
#            +  s(Time, sentence, by=IsPredFocusO, bs = "fs", m = 1), 
#            data = d5, discrete = T, nthreads = 2)
# macf <- acf_resid(mo.tmp)
# (rhoval <- macf[2]) #0.93
# 
# mo <- bam(f0.z ~ s(Time) + s(Time, by = IsPredFocusO) 
#            + IsPredFocusO 
#            +  s(Time, talker, by=IsPredFocusO, bs = "fs", m = 1)
#            +  s(Time, sentence, by=IsPredFocusO, bs = "fs", m = 1), 
#            data = d5, discrete = T, nthreads = 2, rho=rhoval, 
#            AR.start=d5$start.event)
# (smry.mo <- summary(mo))
# 
# # do the same on the subset of data where the predicate has a LH
# tmp <- d5
# predTone <- read.csv(file="../data/GUJ-pred-LH.csv", header=TRUE, sep=",")
# nrow(predTone) #153
# head(predTone)
# 
# table(tmp$gf)
# 
# tmp <- droplevels(subset(tmp,tmp$File %in% predTone$File))
# table(tmp$gf)
# 
# d5.LH <- tmp
# 
# # first model to establish residuals
# m.tmp <-  bam(f0.z ~ s(Time, by = cond, bs='ad') + cond 
#               + s(Time, talker, by=cond, bs = "fs", m = 1) 
#               + s(Time, sentence, by=cond, bs = "fs", m = 1), 
#               data = d5.LH, discrete = T, nthreads = 2)
# macf <- acf_resid(m.tmp)
# (rhoval <- macf[2]) #0.9
# 
# m <- bam(f0.z ~ s(Time, by = cond, bs='ad') + cond 
#          + s(Time, talker, by=cond, bs = "fs", m = 1) 
#          + s(Time, sentence, by=cond, bs = "fs", m = 1), 
#          data = d5.LH, discrete = T, nthreads = 2, 
#          rho=rhoval, AR.start=d5.LH$start.event)
# (smry <- summary(m))
# 
# # plot 
# pdf("../graphs/GUJ-GAM-subjPredLH.pdf",width = 8, height = 4)
# par(mfrow=c(1,2))
# plot_smooth(m, view='Time',plot_all='cond',rm.ranef=T, rug=F, main='Utterances by condition')
# plot_diff(m, view = "Time",rm.ranef=T,comp = list(cond = c("subjFoc","predFoc")), 
#           main = "Focus difference (LH on predicate)",ylim = c(-2,2))
# dev.off()

# Analysis of L and H: f0 and temporal alignment (d6) ----
# Praat script: GUJ-LH-extract-f0.praat

d6 = read.csv("../../production-exp-data/Gujarati-LHvalues.csv")
nrow(d6)
head(d6)
summary(d6)
length(unique(d6$File)) #186 (192-6, no LH annotations for 6 disfluent files)

# trim spaces
d6$label <- trimws(d6$label)

# add ".wav" to File names
d6$File <- paste(d6$File,".wav",sep="")
table(d6$File)

### add columns 

# language
d6$language <- ifelse(grepl("GUJ-",as.character(d6$File)),"GUJ","OTHER")
table(d6$language)

# talker
d6$talker <- d6$File
d6$talker <- gsub("GUJ-","", d6$talker)
d6$talker <- gsub("-target[0-9]+.wav","", d6$talker)
table(d6$talker)
d6$talker <- as.factor(as.character(d6$talker))

# utterance
d6$utt <- d6$File
d6$utt <- gsub("GUJ-","", d6$utt)
d6$utt <- gsub("P[0-9]+-","", d6$utt)
d6$utt <- gsub(".wav","", d6$utt)
table(d6$utt)

# focus condition (subject focus, predicate focus)
d6$cond <- ifelse(d6$utt == "target1" | d6$utt == "target3" | 
                    d6$utt == "target5" | d6$utt == "target7" |
                    d6$utt == "target9" | d6$utt == "target11" |
                    d6$utt == "target13" | d6$utt == "target15", "subjFoc", "predFoc")
table(d6$cond)

# grammatical function (subject, verb)
d6$gf <- ifelse(d6$label == "laavanya" | d6$label == "ananyaa" | 
                  d6$label == "laa" | d6$label == "van" |
                  d6$label == "ya" | d6$label == "a" |
                  d6$label == "nan" | d6$label == "yaa", "subject", 
                ifelse(d6$label == "aaraam" | d6$label == "vyaayaam" |
                         d6$label == "nindvaanu" | d6$label == "kheDvaanu" |
                         d6$label == "aa" | d6$label == "raam" |
                         d6$label == "vyaa" | d6$label == "yaam" |
                         d6$label == "nind" | d6$label == "vaa" | d6$label == "nu" |
                         d6$label == "kheD","predicate", "other"))
table(d6$gf) 

# exclude disfluent files
d5 <- droplevels(subset(d5,d5$File != "GUJ-P2-target1.wav" 
                        & d5$File != "GUJ-P2-target14.wav" 
                        & d5$File != "GUJ-P2-target5.wav" 
                        & d5$File != "GUJ-P2-target11.wav"
                        & d5$File != "GUJ-P2-target2.wav"
                        & d5$File != "GUJ-P2-target9.wav"))

length(unique(d6$File)) #good, still 186

pred.LH <- read.csv(file="../data/GUJ-pred-LH.csv", header=TRUE, sep=",")
nrow(pred.LH) #173

d6 <- droplevels(subset(d6, !(d6$gf == "predicate" & !d6$File %in% pred.LH$File)))
length(unique(d6$File)) #186
length(unique(d6[d6$gf == "predicate",]$File)) #173

names(d6)
# does each subject have a L and a H on it? (all subjects have LH)
# there are 186 subjects (because 186 files), each should have L and ah
table(d6[d6$gf == "subject",]$labelPoint) #186
table(d6[d6$label == "ananyaa",]$labelPoint) #93
table(d6[d6$label == "laavanya",]$labelPoint) #93

# does each predicate with a LH have a L and a H on it?
table(d6[d6$gf == "predicate",]$labelPoint) #173

# exclude rows that are about "kaam"
table(d6$gf) #no kaam have LH annotations

d6.predLH <- d6

# fix the undefined f0 values
head(d6.predLH)
table(d6.predLH$f0) # no undefined f0 values
str(d6.predLH$f0)

# normalize time for the L and H times

# plots
par(mfrow=c(1,2))
boxplot(d6.predLH[d6.predLH$labelPoint == "L",]$time, main='Time of the L')
boxplot(d6.predLH[d6.predLH$labelPoint == "H",]$time, main='Time of the H')
boxplot(d6.predLH$begTimeLabel, main='Beginnings of words')
boxplot(d6.predLH$endTimeLabel, main='Ends of words')

# word duration
d6.predLH$wordDur <- d6.predLH$endTimeLabel - d6.predLH$begTimeLabel
boxplot(d6.predLH$wordDur, main='word durations')
min(d6.predLH$wordDur)

# normalize each temporal location of L and H: 
# (time of L/H - first time of word)/(last time of word - first time of word)
names(d6.predLH)
head(d6.predLH)
d6.predLH$Time <- 0
d6.predLH$Time <- (d6.predLH$time - d6.predLH$begTimeLabel) / d6.predLH$wordDur
boxplot(d6.predLH$Time.n, main='normalized times')
boxplot(d6.predLH[d6.predLH$labelPoint == "L",]$Time.n, main='normalized times of L')
boxplot(d6.predLH[d6.predLH$labelPoint == "H",]$Time.n, main='normalized times of H')

names(d6.predLH)
# normalize f0 values

# load f0 mean and sd data by talker from GAMM analysis
f0 <- read.csv("../data/GUJ-f0-mean-sd-by-talker.csv")
f0

# merge this talker f0 mean and sd with data 
d6.predLH = merge(d6.predLH, f0, by='talker')
head(d6.predLH)

# z-transform f0
table(d6.predLH$f0)
str(d6.predLH$f0)
d6.predLH$f0.z = (d6.predLH$f0 - d6.predLH$f0.mean) / d6.predLH$f0.sd
head(d6.predLH)

# plots
par(mfrow=c(1,2))
boxplot(d6.predLH$f0,main='Original f0')
boxplot(d6.predLH$f0.z,main='Z-transformed f0 by talker') 

# f0 range
head(d6.predLH)
library(reshape2)
range <- dcast(d6.predLH, File + label ~ labelPoint, value.var="f0.z")
head(range)
  
names(range)
range$range <- 0
range$range <- range$H - range$L
boxplot(range$range,main='Normalized range')

colnames(range)[colnames(range) == 'H'] <- 'f0.z.H'
colnames(range)[colnames(range) == 'L'] <- 'f0.z.L'

# now merge range info with d6 to calculate slope
d6.predLH = merge(d6.predLH, range, by=c('File','label'))
head(d6.predLH)

# slope = range / difference in time between L and H
diff <- dcast(d6.predLH, File + label ~ labelPoint, value.var="Time")
head(diff)
diff$diff <- 0
diff$diff <- diff$H - diff$L
boxplot(diff$diff.n,main='Normalized time difference between L and H')

colnames(diff)[colnames(diff) == 'H'] <- 'Time.n.H'
colnames(diff)[colnames(diff) == 'L'] <- 'Time.n.L'

# now merge range info with data to calculate slope
d6.predLH = merge(d6.predLH, diff, by=c('File','label'))
head(d6.predLH)

# slope 
d6.predLH$slope <- d6.predLH$range / (d6.predLH$diff / d6.predLH$wordDur)

# normalized temporal distance between the time of the L and the time of the H
d6.predLH$distance <- d6.predLH$diff / d6.predLH$wordDur
boxplot(d6.predLH$wordDur,main='Raw duration of the word')

head(d6.predLH)

# mean time of H of predicate from beginning of utterance (all participants)
agr = d6.predLH %>%
  filter(labelPoint == "H" & gf == "predicate") %>%
  summarise(meanTime=mean(Time))
agr # .74

### ..LH analysis plots ----

# normalized f0 of L by condition and grammatical function
ggplot(d6.predLH[d6.predLH$labelPoint == "L",], aes(x=cond,y=f0.z)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of L") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-f0z-of-L-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 of H by condition and grammatical function
ggplot(d6.predLH[d6.predLH$labelPoint == "H",], aes(x=cond,y=f0.z)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of H") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-f0z-of-H-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 range by condition and grammatical function
ggplot(d6.predLH, aes(x=cond,y=range)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of range") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-LHrange-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 slope by condition and grammatical function
ggplot(d6.predLH, aes(x=cond,y=slope)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of slope") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-LHslope-by-condition-and-gf.pdf",height=3.5,width=8)

# plot temporal alignment of L by condition and grammatical function
table(d6.predLH$talker)
head(d6.predLH)

agr = d6.predLH %>%
  filter(labelPoint == "L") %>%
  group_by(cond,gf) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$gf <- as.factor(agr$gf)
agr$gf <- relevel(agr$gf, ref = "subject")

ggplot(agr, aes(x=cond,y=meanTime,fill=gf)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ gf) +
  ylab("Normalized temporal location of L") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-temporal-alignment-of-L-all-words-by-condition.pdf",height=3.5,width=8)

# plot temporal alignment of H by condition and grammatical function
table(d6.predLH$talker)
head(d6.predLH)

agr = d6.predLH %>%
  filter(labelPoint == "H") %>%
  group_by(cond,gf) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$gf <- as.factor(agr$gf)
agr$gf <- relevel(agr$gf, ref = "subject")

ggplot(agr, aes(x=cond,y=meanTime,fill=gf)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ gf) +
  ylab("Normalized temporal location of L") +
  xlab("Condition")
ggsave(f="../graphs/GUJ-temporal-alignment-of-H-all-words-by-condition.pdf",height=3.5,width=8)

# plot temporal distance between L and H by condition and grammatical function

agr = d6.predLH %>%
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
ggsave(f="../graphs/GUJ-temporal-distance-between-L-and-H-by-condition.pdf",height=3.5,width=8)

# plot temporal alignment of L by talker and grammatical function
table(d6.predLH$talker)
head(d6.predLH)

agr = d6.predLH %>%
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
ggsave(f="../graphs/GUJ-temporal-alignment-of-L-all-words.pdf",height=3.5,width=8)

# plot temporal alignment of H by talker and grammatical function
table(d6.predLH$talker)
head(d6.predLH)

agr = d6.predLH %>%
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
ggsave(f="../graphs/GUJ-temporal-alignment-of-H-all-words.pdf",height=3.5,width=8)

# plot temporal alignment of H of predicate by talker (for Konstanz talk)
table(d6.predLH$talker)
head(d6.predLH)

agr = d6.predLH %>%
  filter(labelPoint == "H" & gf == "predicate") %>%
  group_by(talker) %>%
  summarise(meanTime=mean(Time),ci.low=ci.low(Time),ci.high=ci.high(Time)) %>%
  mutate(YMin=meanTime-ci.low,YMax=meanTime+ci.high)
agr
dodge=position_dodge(.9)

agr$talker.H <- factor(agr$talker, levels=unique(agr$talker[order(agr$meanTime)]), ordered=TRUE)
agr$talker.H

ggplot(agr, aes(x=talker.H,y=meanTime)) +
  scale_fill_grey() +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_text(x=3, y=.90, label="Marathi mean") +
  geom_hline(yintercept=.88,color = "red") + # Marathi
  geom_text(x=2.9, y=.84, label="Chodri mean") +
  geom_hline(yintercept=.82,color = "blue") + # Chodri
  geom_text(x=2.9, y=.76, label="Gujarati mean") +
  geom_hline(yintercept=.74,color = "black") + # Gujarati
  ylab("Time of H from beginning of utterance") +
  xlab("Gujarati participant")
ggsave(f="../graphs/GUJ-temporal-alignment-of-H-of-predicate.pdf",height=3.5,width=5)

#### ..LH analysis models ----
# set reference levels for initial analyses
d6.predLH$gf <- as.factor(d6.predLH$gf)
with(d6.predLH, levels(gf)) #predicate
d6.predLH$cond <- as.factor(d6.predLH$cond)
with(d6.predLH, levels(cond)) #predFoc

# L
m.L = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.L)
m.Ls = lmer(f0.z ~ cond + gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.Ls)
anova(m.L,m.Ls) #model with interaction is better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.L2 = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.L2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# H
m.H = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.H)
m.Hs = lmer(f0.z ~ cond + gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.Hs)
anova(m.H,m.Hs) #model with interaction is better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.H2 = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.H2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# range
m.range = lmer(range ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.range)
m.rangeS = lmer(range ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.rangeS)
anova(m.range,m.rangeS) #model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.rangeS2 = lmer(range ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.rangeS2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# slope
m.slope = lmer(slope ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.slope)
m.slopeS = lmer(slope ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.slopeS)
anova(m.slope,m.slopeS) #model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.slopeS2 = lmer(slope ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.slopeS2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# temporal location of L
m.locL = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locL)
m.locLS = lmer(Time ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locLS)
anova(m.locL,m.locLS) # model with interaction is better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.locL2 = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locL2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# temporal location of H
m.locH = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.locH)
m.locHS = lmer(Time ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.locHS) # condition is not significant
anova(m.locH,m.locHS) # model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.locH2 = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.locH2) # condition is not significant
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# temporal distance between L and H
m.dist = lmer(distance ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.dist)
m.distS = lmer(distance ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.distS)
anova(m.dist,m.distS) # model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.dist2 = lmer(distance ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.dist2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")
