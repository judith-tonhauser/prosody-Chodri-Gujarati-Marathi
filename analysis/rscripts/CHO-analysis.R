# Bhili-Kandeshi prosody project: Chodri analysis

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
d1 = read.csv("../../production-exp-data/Chodri-contours.csv")
nrow(d1) #176

head(d1)
summary(d1)
length(unique(d1$File)) #176 = 11 talkers x 16 utterances

# exclude disfluent utterances
table(d1$Words)
d1$File[grepl("disfluent",d1$Words)] #6 utterances
d1 <- droplevels(subset(d1,!grepl("disfluent",d1$Words)))
nrow(d1) #170

# also exclude P11-target13: should have said "ananyaa" but said "laavanya"
d1 <- droplevels(subset(d1,d1$File != "CHO-P11-target13.wav"))
nrow(d1) #169

# add columns 

# language
d1$language <- ifelse(grepl("CHO-",as.character(d1$File)),"CHO","OTHER")
table(d1$language)

# talker
d1$talker <- d1$File
d1$talker <- gsub("CHO-","", d1$talker)
d1$talker <- gsub("-target[0-9]+.wav","", d1$talker)
table(d1$talker)
d1$talker <- as.factor(as.character(d1$talker))

# utterance
d1$utt <- d1$File
d1$utt <- gsub("CHO-","", d1$utt)
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
d1[d1$File == "CHO-P12-target15.wav",]$Contour <- "L-H% LH 0 LH H-L%" #instead of LH LH 0 LH H-L%
d1[d1$File == "CHO-P7-target11.wav",]$Contour <- "L-H% LH LH L-H%" #instead of LH LH LH L-H%

# simplified contour: merge all utterances ending in L-H\% and L-L\% to L and all utterances ending in 
# H-L% into H
table(d1$Contour)
d1$ContourS <- d1$Contour
d1$ContourS <- gsub("L-L\\%","L", d1$ContourS)
d1$ContourS <- gsub("0 L-H\\%","0 L", d1$ContourS)
d1$ContourS <- gsub("0 LH L-H\\%","0 LH L", d1$ContourS)
d1$ContourS <- gsub("LH LH L-H\\%","LH LH L", d1$ContourS)
d1$ContourS <- gsub("H-L\\%","H", d1$ContourS)
table(d1$ContourS)
# trim spaces
d1$ContourS <- trimws(d1$ContourS)
table(d1$ContourS)

# super simplified contour: also ignore whether LH or L-H% on subject and predicate
d1$ContourSS <- d1$ContourS
d1$ContourSS <- gsub("L-H\\%","LH", d1$ContourS)
table(d1$ContourSS)

# subset of utterances where predicate has LH 
length(unique(d1$File)) #169
pred.LH <- d1[grep("LH LH LH|LH LH 0|LHLH LH 0",d1$ContourSS),]
head(pred.LH)
length(unique(pred.LH$File)) #168 (only 1 utterance with dephrased predicate)
write.csv(pred.LH, "../data/CHO-pred-LH.csv")

# subset of utterances where auxiliary has LH 
length(unique(d1$File)) #169
table(d1$ContourSS)
aux.LH <- d1[grep("LH L$|LH H$",d1$ContourSS),]
head(pred.LH)
length(unique(aux.LH$File)) #163 = 169-2-3-1
write.csv(aux.LH, "../data/CHO-aux-LH.csv")

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
      file = "../tables/CHO-utterance-final-tone-by-condition.tex")

# what are the contours that occur in the utterances?
table <- table(d1$ContourS,d1$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-simplified-contour-by-condition.tex")

# distribution of pauses, by condition
# does a pause after the subject occur more frequently in one condition?
d.pause <- droplevels(subset(d1,grepl("<p>",d1$Words),drop=FALSE))
d.pause
nrow(d.pause) #17
write.csv(d.pause, "../data/CHO-pause.csv")

table <- table(d.pause$ContourS,d.pause$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-pause-after-subject-by-condition.tex")

# how often is there dephrasing in each condition?
table <- table(d1$ContourSS,d1$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-super-simplified-contour-by-condition.tex")

# realization of utterances with "kaam" by condition
table(d1$sentence,d1$Words)

# sentences with kaam: sentence916,sentence1215,sentence36,sentence18 (4 of 8)
# 4 sentences because 2 predicates that are combined with 2 different subjects

# how many sentences with "kaam"?
nrow(d1[(d1$sentence == "sentence916" | d1$sentence == "sentence1215" 
         | d1$sentence == "sentence36" | d1$sentence == "sentence18"),]) 
#83 (11 talkers x 4 sentences x 2 prosody = 88, minus disfluent utterances)

# contour of 83 sentences with "kaam" by condition
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
d1$kaamContour <- gsub("LHLH LH 0 LH L", "LH 0", d1$kaamContour)
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
# 4 slots on tier 3: these utterances may have "kaam", only change those that have it
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
#83 entries (good because 83 utterances with kaam)

print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-kaam-super-simplified-contour-by-condition.tex")

# for duration analysis: create info about kaam with LH or 0
names(d1)
table(d1$kaamContour)

kaamLHLH <- droplevels(subset(d1,d1$kaamContour == "LH LH"))
nrow(kaamLHLH) #31
write.csv(kaamLHLH, "../data/CHO-kaamLHLH.csv")

# ...contour analysis: models ----
# not done for CHO because hardly any dephrasing
# make new columns

# d1$predLH <- ifelse(d1$File %in% pred.LH$File,"1","0")
# table(d1$predLH)
# d1$predLH <- as.factor(d1$predLH)
# 
# d1$auxLH <- ifelse(d1$File %in% aux.LH$File,"1","0")
# table(d1$auxLH)
# d1$auxLH <- as.factor(d1$auxLH)
# 
# d1$auxLHpredLH <- ifelse(d1$File %in% pred.LH$File & d1$File %in% aux.LH$File, "1","0")
# table(d1$auxLHpredLH)
# d1$auxLHpredLH <- as.factor(d1$auxLHpredLH)
# 
# # logistic regression models
# 
# # predict dephrasing of predicate over all utterances
# m.pred <- glmer(predLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1)
# summary(m.pred) 
# m.pred.2 <- glmer(predLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1)
# summary(m.pred.2) 
# 
# # predict dephrasing of auxiliary over all utterances
# m.aux <- glmer(auxLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1)
# summary(m.aux)
# m.aux.2 <- glmer(auxLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1)
# summary(m.aux.2) 
# 
# # predict dephrasing of auxiliary over all utterances with LH on predicate
# m.aux2 <- glmer(auxLHpredLH ~ cond + (1|sentence) + (1+cond|talker),family=binomial(link='logit'),data=d1[d1$predLH == "1",])
# summary(m.aux2)
# m.aux2.2 <- glmer(auxLHpredLH ~ cond + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d1[d1$predLH == "1",])
# summary(m.aux2.2)
# 
# # by-talker plots for dephrasing of predicate
# ggplot(data=d1, aes(x=talker, fill=predLH)) +
#   geom_bar(stat="count") +
#   facet_grid(. ~ cond)
# ggsave("../graphs/CHO-dephrasing-pred-by-condition.pdf",height=6,width=8)
# 
# # by-talker plots for dephrasing of aux in utterances with LH on predicate
# ggplot(data=d1[d1$predLH == "1",], aes(x=talker, fill=auxLHpredLH)) +
#   geom_bar(stat="count") +
#   facet_grid(. ~ cond)
# ggsave("../graphs/CHO-dephrasing-aux-by-condition.pdf",height=6,width=8)


# prosodic system (d2) ----
# Praat script: extract-labels-tier4.praat

# the data here consist of tier 4 annotations of the L and the H of the 
# subject and the predicate
d2 = read.csv("../../production-exp-data/Chodri-LH-syllables.csv")
nrow(d2) #681 (171 files x about 3.98 tones on tier 4)
head(d2)
summary(d2)
length(unique(d2$File)) #171 (some disfluent files have annotations)

# exclude disfluent files
d2 <- droplevels(subset(d2,d2$File != "CHO-P1-target4.wav" 
                        & d2$File != "CHO-P3-target15.wav" 
                        & d2$File != "CHO-P6-target12.wav" 
                        & d2$File != "CHO-P7-target15.wav"
                        & d2$File != "CHO-P7-target16.wav"
                        & d2$File != "CHO-P7-target6.wav"))

# also exclude P11-target13: should have said "ananyaa" but said "laavanya"
d2 <- droplevels(subset(d2,d2$File != "CHO-P11-target13.wav"))
nrow(d2)
length(unique(d2$File)) #169 (176 - 7 disfluent utterances)

# exclude 6 kaam utterances where predicate+kaam jointly realize LH
d2 <- droplevels(subset(d2,d2$File %in% d1[d1$kaamContour != "LH",]$File))
length(unique(d2$File)) #163 (169 - 6)

# exclude utterance with LHL-H% on subject (find the utterance in d1)
d1[grepl("LHL-H\\% LH",d1$Contour),]$File #"CHO-P6-target8.wav"

d2 <- droplevels(subset(d2,d2$File != "CHO-P6-target8.wav"))
length(unique(d2$File)) #162

table(d2$Word)
table(d2$Syllable)
# trim spaces
d2$Syllable <- trimws(d2$Syllable)
d2$Word <- trimws(d2$Word)

### add columns 

# language
d2$language <- ifelse(grepl("CHO-",as.character(d2$File)),"CHO","OTHER")
table(d2$language)

# talker
d2$talker <- d2$File
d2$talker <- gsub("CHO-","", d2$talker)
d2$talker <- gsub("-target[0-9]+.wav","", d2$talker)
table(d2$talker)
d2$talker <- as.factor(as.character(d2$talker))

# utterance
d2$utt <- d2$File
d2$utt <- gsub("CHO-","", d2$utt)
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
                ifelse(d2$Word == "aaraam" | d2$Word == "nedvaanu" |
                         d2$Word == "kasrat" | d2$Word == "kheDvaanu" |
                         d2$Syllable == "aa" | d2$Syllable == "raam" |
                         d2$Syllable == "ned" | d2$Syllable == "vaa" |
                         d2$Syllable == "nu" | d2$Syllable == "kas" | d2$Syllable == "rat" |
                         d2$Syllable == "kheD" | d2$Syllable == "vaa" | d2$Syllable == "nu","predicate", "other"))
table(d2$gf)
table(d2$Syllable,d2$gf)

head(d2)

# does each subject have a L and a H on it? (all subjects have LH)
# there are 162 subjects (because 162 files), each should have L and ah
table(d2[d2$gf == "subject",]$Tone) #162
table(d2[d2$Word == "ananyaa",]$Tone) #80
table(d2[d2$Word == "laavanya",]$Tone) #82

# exclude predicates with 0 from count
table(d1$ContourSS) # only 1 utterance where predicate has 0
d1[d1$ContourSS == "LH 0 0 LH L",]$File #CHO-P1-target1.wav

d2[d2$File == "CHO-P1-target1.wav",]$Word #utterance is still part of d2, but only with subject because there's no
# L and H annotation for the predicate (which is 0), so nothing needs to be excluded
length(unique(d2$File)) #162

# 162 files, each has subject, but only 161 predicates
d2.predLH <- d2

# does each predicate with a LH (total = 161) have a L and a H on it?
table(d2.predLH[d2.predLH$gf == "predicate",]$Tone) #161

# which syllables is the L realized on?
syl.L <- droplevels(subset(d2.predLH,d2.predLH$Tone == "L"))
table <- table(syl.L$Word)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-words-total.tex")

table <- table(syl.L$Syllable)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-syllables-of-L.tex")

# which syllables is the H realized on?
syl.H <- droplevels(subset(d2.predLH,d2.predLH$Tone == "H"))
table(syl.H$Word)
table <- table(syl.H$Syllable)
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-syllables-of-H.tex")

# for the 3-syllable words, for how many is the L on the 1st syl and the H on the 3rd syl?
# this provides evidence for the L_a H_a vs the L*+H hypotheses
# 3-syllable words: ananyaa, laavanya, kheDvaanu, nedvaanu
head(d2.predLH)
library(reshape2)
LH.syls <- dcast(d2.predLH, File + Word ~ Tone, value.var="Syllable")
head(LH.syls)

# subset to the 3-syllable words
table(LH.syls$Word)
LH.3 <- droplevels(subset(LH.syls,LH.syls$Word == "ananyaa" | LH.syls$Word == "laavanya" 
                          | LH.syls$Word == "kheDvaanu" | LH.syls$Word == "nedvaanu"))
nrow(LH.3) #251 (87 ananyaa, 87 laavanya, 39 naangarNi, 38 nindaNi)
head(LH.3)

t <- table(LH.3$L,LH.3$H)
t
t2 <- prop.table(t,1)
t2 %>% `*`(100) %>% round(2)



# duration analysis (d3) ----
# Praat script: duration.praat

d3 = read.csv("../../production-exp-data/Chodri-durations.csv")
nrow(d3)
head(d3)
summary(d3)
length(unique(d3$File)) #176 = 11 talkers x 16 utterances

# remove spaces from labels
d3$label <- trimws(d3$label)

# exclude the 6 disfluent utterances 
d3 <- droplevels(subset(d3,d3$File != "CHO-P1-target4.wav" 
                        & d3$File != "CHO-P3-target15.wav" 
                        & d3$File != "CHO-P6-target12.wav" 
                        & d3$File != "CHO-P7-target15.wav"
                        & d3$File != "CHO-P7-target16.wav"
                        & d3$File != "CHO-P7-target6.wav"))

# also exclude P11-target13: should have said "ananyaa" but said "laavanya"
d3 <- droplevels(subset(d3,d3$File != "CHO-P11-target13.wav"))
nrow(d3)

length(unique(d3$File)) #169

# exclude utterance with LHL-H% on subject
d3 <- droplevels(subset(d3,d3$File != "CHO-P6-target8.wav"))
length(unique(d3$File)) #168

# exclude utterances with utterance-internal pause
table(d3$label)
length(unique(d3$File)) #168
pauseCHO <- read.csv(file="../data/CHO-pause.csv", header=TRUE, sep=",")
pauseCHO
nrow(pauseCHO) #17
str(pauseCHO$File)
str(d3$File)

d3 <- droplevels(subset(d3, !(d3$File %in% pauseCHO$File)))
length(unique(d3$File)) #152

# utterance where predicate had 0 was removed as part of removal of utterances with pauses
# d3[d3$File == "CHO-P1-target1.wav",]$label
# d1[d1$File == "CHO-P1-target1.wav",]$Contour

# exclude 6 kaam utterances where predicate+kaam jointly realize LH
d3 <- droplevels(subset(d3,d3$File %in% d1[d1$kaamContour != "LH",]$File))
length(unique(d3$File)) #146 (152 - 6)

### add columns 

# language
d3$language <- ifelse(grepl("CHO-",as.character(d3$File)),"CHO","OTHER")
table(d3$language)

# talker
d3$talker <- d3$File
d3$talker <- gsub("CHO-","", d3$talker)
d3$talker <- gsub("-target[0-9]+.wav","", d3$talker)
table(d3$talker)
d3$talker <- as.factor(as.character(d3$talker))

# utterance
d3$utt <- d3$File
d3$utt <- gsub("CHO-","", d3$utt)
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

d3$gf <- ifelse(d3$label == "laavanya" | d3$label == "ananyaa" | 
                  d3$label == "laa" | d3$label == "van" |
                  d3$label == "ya" | d3$label == "a" |
                  d3$label == "nan" | d3$label == "yaa", "subject", 
                ifelse(d3$label == "aaraam" | d3$label == "nedvaanu" |
                         d3$label == "kasrat" | d3$label == "kheDvaanu" |
                         d3$label == "aa" | d3$label == "raam" |
                         d3$label == "ned" | d3$label == "vaa" |
                         d3$label == "nu" | d3$label == "kas" | d3$label == "rat" |
                         d3$label == "kheD" | d3$label == "vaa" | d3$label == "nu","predicate", "other"))
table(d3$gf)

table(d2$Syllable,d2$gf)

# word or syllable
d3$WorS <- ifelse(d3$label == "laavanya" | d3$label == "ananyaa" | 
                   d3$label == "aaraam" | d3$label == "kasrat" |
                   d3$label == "kheDvaanu" | d3$label == "nedvaanu","word", 
                 ifelse(d3$label == "laa" | d3$label == "van" |
                          d3$label == "ya" | d3$label == "a" |
                          d3$label == "nan" | d3$label == "yaa" |
                          d3$label == "aa" | d3$label == "raam" |
                          d3$label == "kas" | d3$label == "rat" |
                          d3$label == "kheD" | d3$label == "vaa" | d3$label == "nu" |
                          d3$label == "ned","syl","other"))

table(d3$WorS)

# is the distribution of "ha" versus "hase" predicted by condition?
table(d3$label)
table <- table(d3[d3$label == "karti ha" | d3$label == "karti hase",]$label,d3[d3$label == "karti ha" | d3$label == "karti hase",]$cond)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/CHO-harti-ha-hase-by-condition.tex")

# normalized duration
d3$durationN <- d3$duration / d3$totalDuration

# all subjects and predicates have LH on them, so we don't need to consider the presence/absence
# of a tone in the models (unlike in Marathi)

# predTone <- read.csv(file="../data/CHO-pred-LH.csv", header=TRUE, sep=",")
# nrow(predTone) #153
# 
# d3$Tone <- "0"
# d3$Tone[d3$gf == "subject"] <- "LH"
# d3$Tone[d3$gf == "predicate" & d3$File %in% predTone$File] <- "LH"
# 
# table(d3$gf,d3$Tone) #all subjects LH

# add column Tone that captures whether kaam is realized with LH or 0
head(d3)

# these are the 31 utterances in which kaam has LH (rather than 0)
kaamTone <- read.csv(file="../data/CHO-kaamLHLH.csv", header=TRUE, sep=",")
nrow(kaamTone) #31, i.e., most predicates realized with LH
length(unique(kaamTone$File)) #31

length(unique(d3[d3$label == "kaam",]$File)) #78

d3$Tone <- "0"
d3[d3$gf == "subject",]$Tone <- "LH"
d3[d3$gf == "predicate",]$Tone <- "LH"
d3[d3$label == "kaam" & d3$File %in% kaamTone$File,]$Tone <- "LH"

table(d3$label,d3$Tone) # all subjects and all predicates have LH 
#31 of 78 kaam with LH

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
ggsave("../graphs/CHO-subject-duration-by-condition.pdf",height=4,width=10)

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
ggsave("../graphs/CHO-subject-durationN-by-condition.pdf",height=4,width=10)

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
  ylab("Mean normalized duration") +
  #theme(axis.title.y=element_blank()) +
  xlab("Condition") 
ggsave("/Users/tonhauser.1/Dropbox/Konstanz2018-talk/graphs/CHO-duration-subj.pdf",height=3,width=2)

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
  ylab("Mean normalized duration")+
  #theme(axis.title.y=element_blank()) +
  xlab("Condition") 
ggsave("/Users/tonhauser.1/Dropbox/Konstanz2018-talk/graphs/CHO-duration-pred.pdf",height=3,width=2)


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
ggsave("../graphs/CHO-predicate-duration-by-condition.pdf",height=4,width=10)

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
ggsave("../graphs/CHO-predicate-durationN-by-condition.pdf",height=4,width=10)

# ..duration: models ----
names(d3)
str(d3$duration)
str(d3$label)
d3$label <- as.factor(d3$label)
str(d3$talker)
str(d3$cond)
d3$cond <- as.factor(d3$cond)
#str(d3$Tone)
#d3$Tone <- as.factor(d3$Tone)

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
# Tone not included as fixed effect because all predicates have LH
table(d3$gf,d3$Tone)

# Is the raw duration of the predicate (word) predicted by condition?
m.1 = lmer(duration ~ cond + (1|label) + (1+cond|talker), 
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

d4 = read.csv("../../production-exp-data/Chodri-f0values.csv")
nrow(d4) #176 (= 11 talkers x 16 utts)
head(d4)
summary(d4)
length(unique(d4$File)) #176

table(d4$File)
# add ".wav" to File names
d4$File <- paste(d4$File,".wav",sep="")

### add columns 

# talker
d4$talker <- d4$File
d4$talker <- gsub("CHO-","", d4$talker)
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

# get impression for each language of f0 range and sd
agr = d4 %>%
  summarise(meanf0min = mean(uttf0min), meanf0max = mean(uttf0max), meanRange = mean(uttf0max-uttf0min))
agr

# GAMM analysis (d5) ----
# This analysis is based on f0 values extracted every 10ms in the subject and the predicate
# The f0 contour is plotted and modeled for the subject and the predicate together
# Praat script: CHO-pitch-over-time.praat

d5 <- read.csv("../../production-exp-data/Chodri-pitch-by-time.csv")
head(d5)
nrow(d5) #19408

# add ".wav" to File names
table(d5$File)
d5$File <- paste(d5$File,".wav",sep="")

length(unique(d5$File)) #170 (not 176 because disfluent-marked utterances have no subject and predicate in tier 1)

# exclude utterances

# exclude the 6 disfluent-marked utterances 
d5 <- droplevels(subset(d5,d5$File != "CHO-P1-target4.wav" 
                        & d5$File != "CHO-P3-target15.wav" 
                        & d5$File != "CHO-P6-target12.wav" 
                        & d5$File != "CHO-P7-target15.wav"
                        & d5$File != "CHO-P7-target16.wav"
                        & d5$File != "CHO-P7-target6.wav"))

length(unique(d5$File)) # still 170

# also exclude P11-target13: should have said "ananyaa" but said "laavanya"
d5 <- droplevels(subset(d5,d5$File != "CHO-P11-target13.wav"))
nrow(d5)
length(unique(d5$File)) #169

# exclude utterance with LHL-H% on subject
d5 <- droplevels(subset(d5,d5$File != "CHO-P6-target8.wav"))
length(unique(d5$File)) #168

# exclude 17 utterances with utterance-internal pause
table(d5$label)
length(unique(d5$File)) #168
pauseCHO <- read.csv(file="../data/CHO-pause.csv", header=TRUE, sep=",")
pauseCHO
nrow(pauseCHO) #17
str(pauseCHO$File)
str(d5$File)

d5 <- droplevels(subset(d5, !(d5$File %in% pauseCHO$File)))
length(unique(d5$File)) #152 (so only 16 utterances excluded: that's because utterance with LHL-H% on 
# subject is an utterance with an utterance-internal pause)

# utterance where predicate had 0 was removed as part of removal of utterances with pauses
# d3[d3$File == "CHO-P1-target1.wav",]$label
# d1[d1$File == "CHO-P1-target1.wav",]$Contour

# exclude 6 kaam utterances where predicate+kaam jointly realize LH
d5 <- droplevels(subset(d5,d5$File %in% d1[d1$kaamContour != "LH",]$File))
length(unique(d5$File)) #146 (152 - 6)

nrow(d5) #16779

# create additional columns

# language
d5$language <- ifelse(grepl("CHO-",as.character(d5$File)),"CHO","OTHER")
d5$language <- as.factor(d5$language)
table(d5$language)

# talker
d5$talker <- d5$File
d5$talker <- gsub("CHO-","", d5$talker)
d5$talker <- gsub("-target[0-9]+","", d5$talker)
d5$talker <- gsub(".wav","", d5$talker)
d5$talker <- as.factor(d5$talker)
table(d5$talker)

# utterance
d5$utt <- d5$File
d5$utt <- gsub("CHO-","", d5$utt)
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
ggsave("../graphs/CHO-raw-f0-contour-sentence1114.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence18",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence18.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence36",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence36.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence710",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence710.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence1215",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence1215.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence25",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence25.pdf",height=14,width=14)
# P8 predFoc

ggplot(data=d5[d5$sentence == "sentence413",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence413.pdf",height=14,width=14)
#P6 predFoc

ggplot(data=d5[d5$sentence == "sentence916",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/CHO-raw-f0-contour-sentence916.pdf",height=14,width=14)

# 2 utterances in predFoc condition had pitch-halving, not excluded

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
length(unique(d5$File)) # still 146
table(d5$gf)

# normalize each time such that subject goes from 0 to .499 and
# predicate from .5 to 1 
d5$Time <- 0
d5$Time <- ifelse(d5$gf == "subject", 
                  (d5$time - d5$time.min) / ((d5$time.max - d5$time.min)*2), 
                  ((d5$time - d5$time.min) / ((d5$time.max - d5$time.min)*2))+.5)

head(d5)
sapply(d5,function(x) sum(is.na(x))) #1630 missing f0 values
nrow(d5) #14992

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
write.csv(f0, "../data/CHO-f0-mean-sd-by-talker.csv", row.names=FALSE)

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
nrow(d5) #14992
nrow(d5[d5$f0.z > 10 & !is.na(d5$f0.z),]) #3
nrow(d5[d5$f0.z > 3 & !is.na(d5$f0.z),]) #96
nrow(d5[d5$f0.z < -3 & !is.na(d5$f0.z),]) #202
# very few data points are changed

d5$f0.z[d5$f0.z > 3] <- NA
d5$f0.z[d5$f0.z < -3] <- NA
nrow(d5)

sapply(d5,function(x) sum(is.na(x))) #1928

ggplot(d5, aes(x=talker, y=f0.z)) + 
  geom_boxplot()

# plot normalized f0 values over time

ggplot(data=d5[d5$sentence == "sentence1114",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence1114.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence18",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z)")
ggsave("../graphs/CHO-f0z-contour-sentence18.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence36",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence36.pdf",height=14,width=14)


ggplot(data=d5[d5$sentence == "sentence710",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence710.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence1215",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence1215.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence25",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence25.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence413",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence413.pdf",height=14,width=14)

ggplot(data=d5[d5$sentence == "sentence916",], aes(x=Time, y=f0.z)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(talker ~ cond, scales = "free", space = "fixed") +
  xlab("normalized time") + 
  ylab("f0.z")
ggsave("../graphs/CHO-f0z-contour-sentence916.pdf",height=14,width=14)

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
(rhoval <- macf[2]) #0.79

m <- bam(f0.z ~ s(Time, by = cond, bs='ad') + cond + s(Time, talker, by=cond, 
          bs = "fs", m = 1) + s(Time, sentence, by=cond, bs = "fs", m = 1), 
         data = d5, discrete = T, nthreads = 2, 
          rho=rhoval, AR.start=d5$start.event)
(smry <- summary(m))

# plot 
pdf("../graphs/CHO-GAM-subjPred.pdf",width = 8, height = 4)
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
# predTone <- read.csv(file="../data/CHO-pred-LH.csv", header=TRUE, sep=",")
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
# pdf("../graphs/CHO-GAM-subjPredLH.pdf",width = 8, height = 4)
# par(mfrow=c(1,2))
# plot_smooth(m, view='Time',plot_all='cond',rm.ranef=T, rug=F, main='Utterances by condition')
# plot_diff(m, view = "Time",rm.ranef=T,comp = list(cond = c("subjFoc","predFoc")), 
#           main = "Focus difference (LH on predicate)",ylim = c(-2,2))
# dev.off()

# Analysis of L and H: f0 and temporal alignment (d6) ----
# Praat script: CHO-LH-extract-f0.praat

d6 = read.csv("../../production-exp-data/Chodri-LHvalues.csv")
nrow(d6)
head(d6)
summary(d6)
length(unique(d6$File)) #171

# trim spaces
d6$label <- trimws(d6$label)

# add ".wav" to File names
d6$File <- paste(d6$File,".wav",sep="")
table(d6$File)

### add columns 

# language
d6$language <- ifelse(grepl("CHO-",as.character(d6$File)),"CHO","OTHER")
table(d6$language)

# talker
d6$talker <- d6$File
d6$talker <- gsub("CHO-","", d6$talker)
d6$talker <- gsub("-target[0-9]+.wav","", d6$talker)
table(d6$talker)
d6$talker <- as.factor(as.character(d6$talker))

# utterance
d6$utt <- d6$File
d6$utt <- gsub("CHO-","", d6$utt)
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
                ifelse(d6$label == "aaraam" | d6$label == "kasrat" |
                         d6$label == "nedvaanu" | d6$label == "kheDvaanu" |
                         d6$label == "aa" | d6$label == "raam" |
                         d6$label == "kas" | d6$label == "rat" |
                         d6$label == "ned" | d6$label == "vaa" | d6$label == "nu" |
                         d6$label == "kheD","predicate", "other"))
table(d6$gf) # 8 kaam?

# exclude the 6 disfluent utterances 
d6 <- droplevels(subset(d6,d6$File != "CHO-P1-target4.wav" 
                        & d6$File != "CHO-P3-target15.wav" 
                        & d6$File != "CHO-P6-target12.wav" 
                        & d6$File != "CHO-P7-target15.wav"
                        & d6$File != "CHO-P7-target16.wav"
                        & d6$File != "CHO-P7-target6.wav"))

length(unique(d6$File)) #170 (so one disfluent utterance had L and H annotations)

# also exclude P11-target13: should have said "ananyaa" but said "laavanya"
d6 <- droplevels(subset(d6,d6$File != "CHO-P11-target13.wav"))
nrow(d6)
length(unique(d6$File)) #169

table(d6$gf)

# exclude subject of utterance with LHL-H% on subject
d6[d6$File == "CHO-P6-target8.wav",]$label #ananyaa
d6 <- droplevels(subset(d6, !(d6$File == "CHO-P6-target8.wav" & d6$label == "ananyaa")))
length(unique(d6$File)) #169 (169 predicates, 168 subjects)

# exclude predicate of utterance with 0 on predicate
d6[d6$File == "CHO-P1-target1.wav",]$label # nothing needs to be removed: no L and H on predicate

# exclude predicates of 6 kaam utterances where predicate+kaam jointly realize LH

d6 <- droplevels(subset(d6,!(d6$File %in% d1[d1$kaamContour == "LH",]$File & d6$gf == "predicate")))
length(unique(d6$File)) #169 (163 predicates, 168 subjects)

names(d6)
# does each subject have a L and a H on it? (all subjects have LH)
# there are 163 subjects (because 163 files), each should have L and ah
table(d6[d6$gf == "subject",]$labelPoint) #168
table(d6[d6$label == "ananyaa",]$labelPoint) #84
table(d6[d6$label == "laavanya",]$labelPoint) #84

# exclude rows that are about "kaam"
table(d6$gf) #5 other
d6[d6$gf == "other",]$label #5 kaam
d6 <- droplevels(subset(d6,d6$label != "kaam"))
length(unique(d6$File)) #169 (163 predicates, 168 subjects)

# exclude predicates with 0 from plotting and analysis: already excluded
table(d6$gf) 
table(d6$label)
table(d6$gf,d6$label)

d6.predLH <- d6
#d6.predLH <- droplevels(subset(d6.predLH,!(d6.predLH$gf == "predicate" & !(d6.predLH$File %in% pred.LH$File))))
table(d6.predLH$gf,d6.predLH$label)

# does each predicate with a LH have a L and a H on it?
table(d6.predLH[d6.predLH$gf == "predicate",]$labelPoint) #162 (1 appears to be missing, can't figure it out)

# fix the undefined f0 values
head(d6.predLH)
table(d6.predLH$f0) # 12 undefined f0 values
d6.predLH$f0 <- as.numeric(as.character(d6.predLH$f0))

d6.predLH[d6.predLH$f0 == "--undefined--" & d6.predLH$labelPoint == "L",]$File
d6.predLH[d6.predLH$f0 == "--undefined--" & d6.predLH$gf == "subject",]$File

# [1] "CHO-P1-target8.wav"  L on subject
d6.predLH[d6.predLH$File == "CHO-P1-target8.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 178.3
# [2] "CHO-P10-target12.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P10-target12.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 125.8
# [3] "CHO-P10-target5.wav"  L on subject
d6.predLH[d6.predLH$File == "CHO-P10-target5.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 117.5
# [4] "CHO-P11-target1.wav"  L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target1.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 100.4
# [5] "CHO-P11-target10.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target10.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 99.4
# [6] "CHO-P11-target16.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target16.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 82.6
# [7] "CHO-P11-target2.wav"  L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target2.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 93.3
# [8] "CHO-P11-target3.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target3.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 96
# [9] "CHO-P11-target8.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P11-target8.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 102.2
# [10] "CHO-P3-target5.wav"  H on predicate
d6.predLH[d6.predLH$File == "CHO-P3-target5.wav" & d6.predLH$labelPoint == "H" & d6.predLH$gf == "predicate",]$f0 <- 149.1
# [11] "CHO-P5-target16.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P5-target16.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 104.6
# [12] "CHO-P5-target9.wav" L on subject
d6.predLH[d6.predLH$File == "CHO-P5-target9.wav" & d6.predLH$labelPoint == "L" & d6.predLH$gf == "subject",]$f0 <- 98.3

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
boxplot(d6.predLH$Time, main='normalized times')
boxplot(d6.predLH[d6.predLH$labelPoint == "L",]$Time, main='normalized times of L')
boxplot(d6.predLH[d6.predLH$labelPoint == "H",]$Time, main='normalized times of H')

# normalize f0 values

# load f0 mean and sd data by talker from GAMM analysis
f0 <- read.csv("../data/CHO-f0-mean-sd-by-talker.csv")
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
boxplot(diff$diff,main='Normalized time difference between L and H')

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
table(d6.predLH$Time)
table(d6.predLH$distance,d6.predLH$cond)

# mean time of H of predicate from beginning of utterance (all participants)
agr = d6.predLH %>%
  filter(labelPoint == "H" & gf == "predicate") %>%
  summarise(meanTime=mean(Time))
agr # .82


### ..LH analysis plots ----

# normalized f0 of L by condition and grammatical function
ggplot(d6.predLH[d6.predLH$labelPoint == "L",], aes(x=cond,y=f0.z)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of L") +
  xlab("Condition")
ggsave(f="../graphs/CHO-f0z-of-L-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 of H by condition and grammatical function
ggplot(d6.predLH[d6.predLH$labelPoint == "H",], aes(x=cond,y=f0.z)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of H") +
  xlab("Condition")
ggsave(f="../graphs/CHO-f0z-of-H-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 range by condition and grammatical function
ggplot(d6.predLH, aes(x=cond,y=range)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of range") +
  xlab("Condition")
ggsave(f="../graphs/CHO-LHrange-by-condition-and-gf.pdf",height=3.5,width=8)

# normalized f0 slope by condition and grammatical function
ggplot(d6.predLH, aes(x=cond,y=slope)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  facet_grid(. ~ gf) +
  ylab("Normalized f0 of slope") +
  xlab("Condition")
ggsave(f="../graphs/CHO-LHslope-by-condition-and-gf.pdf",height=3.5,width=8)

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
ggsave(f="../graphs/CHO-temporal-alignment-of-L-all-words-by-condition.pdf",height=3.5,width=8)

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
ggsave(f="../graphs/CHO-temporal-alignment-of-H-all-words-by-condition.pdf",height=3.5,width=8)

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
ggsave(f="../graphs/CHO-temporal-distance-between-L-and-H-by-condition.pdf",height=3.5,width=8)


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
ggsave(f="../graphs/CHO-temporal-alignment-of-L-all-words.pdf",height=3.5,width=8)

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
ggsave(f="../graphs/CHO-temporal-alignment-of-H-all-words.pdf",height=3.5,width=8)

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
anova(m.L,m.Ls)

# H
m.H = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.H)
m.Hs = lmer(f0.z ~ cond + gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.Hs)
anova(m.H,m.Hs) # model with interaction is better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.H2 = lmer(f0.z ~ cond * gf + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.H2)
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# range
names(d6.predLH)
m.range = lmer(range ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.range)
m.rangeS = lmer(range ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.rangeS)
anova(m.range,m.rangeS)

# slope
m.slope = lmer(slope ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.slope)
m.slopeS = lmer(slope ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.slopeS)
anova(m.slope,m.slopeS)

# temporal location of L
m.locL = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locL)
m.locLS = lmer(Time ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locLS) #condition is not significant
anova(m.locL,m.locLS) # model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.locL2 = lmer(Time ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "L",], REML=F)
summary(m.locL2) # condition is not significant
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")

# temporal location of H
m.locH = lmer(Time ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.locH)
m.locHS = lmer(Time ~ cond + gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH[d6.predLH$labelPoint == "H",], REML=F)
summary(m.locHS) 
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
summary(m.distS) # condition is not significant
anova(m.dist,m.distS) # model with interaction is not better
# relevel gf
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "subject")
m.dist2 = lmer(distance ~ cond * gf + wordDur + (1|label) + (1+cond|talker), data=d6.predLH, REML=F)
summary(m.dist2) # condition is not significant
# return level to predicate
d6.predLH$gf <- relevel(d6.predLH$gf, ref = "predicate")
