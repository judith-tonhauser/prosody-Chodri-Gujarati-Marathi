# Prosody of Chodri, Gujarati, Marati
# contours/dephrasing 
# Praat script: contours.praat

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.r")

# load packages
library(tidyverse)
library(lmerTest)

theme_set(theme_bw())

# read in data
c = read.csv("../data/Chodri-contours.csv")
g = read.csv("../data/Gujarati-contours.csv")
m = read.csv("../data/Marathi-contours.csv")

nrow(c) #176 = 11 talkers x 16 utterances (utterances by one talker already excluded)
nrow(g) #192 = 12 talkers x 16 utterances
nrow(m) #176

# preprocessing ----

### Chodri

# # fix mislabeled contours (have pause after subject but LH annotation on subject)
# c$Contour <- as.character(c$Contour)
# c[c$File == "CHO-P12-target15.wav",]$Contour <- "L-H% LH 0 LH H-L%" #instead of LH LH 0 LH H-L%
# c[c$File == "CHO-P7-target11.wav",]$Contour <- "L-H% LH LH L-H%" #instead of LH LH LH L-H%
# c[c$File == "CHO-P6-target16.wav",]$Contour <- "LH LH 0 LH L-H%" #instead of LHLH LH 0 LH L-H%
# nrow(c) #168
# 
# # fix mislabeled contours (have LH on "kaam", which was not annotated)
# c[c$File == "CHO-P4-target1.wav",]$Contour <- "LH LH LH LH H-L%" #instead of LH LH LH H-L%
# c[c$File == "CHO-P10-target8.wav",]$Contour <- "LH LH LH LH L-H%" #instead of LH LH LH L-H%
# c[c$File == "CHO-P10-target3.wav",]$Contour <- "LH LH LH LH L-H%" #instead of LH LH LH L-H%
# c[c$File == "CHO-P1-target8.wav",]$Contour <- "LH LH LH LH H-L%" #instead of LH LH LH H-L%
# c[c$File == "CHO-P1-target12.wav",]$Contour <- "LH LH LH LH L-H%" #instead of LH LH LH L-H%
# c[c$File == "CHO-P1-target15.wav",]$Contour <- "LH LH LH LH L-L%" #instead of LH LH LH L-L%
# nrow(c) #168
# 
# write.csv(c, "../data/Chodri-contours.csv")

# exclude disfluent utterances
c$File[grepl("disfluent",c$Words)] #6 utterances
disfluent_c <- subset(c,grepl("disfluent",c$Words))
nrow(disfluent_c)
c <- droplevels(subset(c,!grepl("disfluent",c$Words)))
nrow(c) #170

# disfluent: also exclude P11-target13: should have said "ananyaa" but said "laavanya"
disfluent_c <- rbind(disfluent_c,subset(c,c$File == "CHO-P11-target13.wav"))
nrow(disfluent_c) #7
c <- droplevels(subset(c,c$File != "CHO-P11-target13.wav"))
nrow(c) #169

# exclude utterance with LHL-H% on subject
disfluent_c <- rbind(disfluent_c,subset(c,c$File == "CHO-P6-target8.wav"))
nrow(disfluent_c) #8
c <- droplevels(subset(c,c$File != "CHO-P6-target8.wav"))
nrow(c) #168


### Gujarati

# # fix mislabeled contours (have pause after subject but LH annotation on subject)
# g$Contour <- as.character(g$Contour)
# # do all the utterances with pauses have % in the tier 1 annotation?
# pauseGUJ <- read.csv(file="../data/Gujarati-pauses.csv", header=TRUE, sep=",")
# pauseGUJ
# table(pauseGUJ$ContourS) #2 utterances that have pause but not %
# g[g$File == "GUJ-P2-target13.wav",]$Contour <- "L-H% LH LH L-L%" #instead of LH LH LH L-L%
# g[g$File == "GUJ-P2-target3.wav",]$Contour <- "L-H% LH 0 0 L-L%" #instead of LH LH 0 0 L-L%
# nrow(g) #192
# 
# write.csv(g, "../data/Gujarati-contours.csv")

# exclude disfluent utterances
g$File[grepl("disfluent",g$Words)] #6 utterances
disfluent_g <- subset(g,grepl("disfluent",g$Words))
nrow(disfluent_g) #6
g <- droplevels(subset(g,!grepl("disfluent",g$Words)))
nrow(g) #186


### Marathi

# # P8-target4: contour is L-H% LH 0 L-H% instead of LH LH 0 L-H%
# # because there's a pause after the subject
# m$Contour <- as.character(m$Contour)
# m$Contour[m$File == "MAR-P8-target4.wav"] <- "L-H% LH 0 L-H% "
# nrow(m) #176
# 
# write.csv(m, "../data/Marathi-contours.csv")

# exclude disfluent utterances
m$File[grepl("disfluent",m$Words)] #MAR-P6-target5.wav
disfluent_m <- subset(m,grepl("disfluent",m$Words))
nrow(disfluent_m)
m <- droplevels(subset(m,!grepl("disfluent",m$Words)))
nrow(m) #175 (one disfluent utterance excluded)

# exclude P10-target14 because one continuous rise from beginning of subject to end of predicate
disfluent_m <- rbind(disfluent_m,subset(m,m$File == "MAR-P10-target14.wav"))
nrow(disfluent_m) #2
m <- droplevels(subset(m,m$File != "MAR-P10-target14.wav"))
nrow(m) #174 (one utterance with unusual intonation excluded)

# merge and save excluded utterances, for exclusion in other analysis files
excluded <- bind_rows(disfluent_c,disfluent_g,disfluent_m)
nrow(excluded) #16
write.csv(excluded, "../data/excluded.csv")

### merge the data from the three languages
summary(c)
summary(m)
summary(g)

d <- bind_rows(c,m,g)
length(unique(d$File)) #528 = 168+174+186

# add columns 

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

# sentence
# codes the sentences that was uttered, identifies the sentence with the trial numbers
d$sentence <- d$utt
d$sentence <- ifelse(d$utt == "target1" | d$utt == "target8", "sentence18",
                     ifelse(d$utt == "target2" | d$utt == "target5", "sentence25",
                            ifelse(d$utt == "target3" | d$utt == "target6", "sentence36",
                                   ifelse(d$utt == "target4" | d$utt == "target13", "sentence413",
                                          ifelse(d$utt == "target7" | d$utt == "target10", "sentence710",
                                                 ifelse(d$utt == "target9" | d$utt == "target16", "sentence916",
                                                        ifelse(d$utt == "target11" | d$utt == "target14", "sentence1114", "sentence1215")))))))
table(d$sentence)


# sentences with kaam: sentence916,sentence1215,sentence36,sentence18 (4 of 8) in Chodri & Gujarati
d$kaamSentence <- ifelse(d$language != "Marathi" & (d$sentence == "sentence916" | 
                           d$sentence == "sentence1215"  | 
                           d$sentence == "sentence36" | 
                           d$sentence == "sentence18"), "yes", "no")

table(d$kaamSentence)

# what are the utterances
table(d$Words,d$language)

# Chodri utterances have 4 words (ananyaa aaraam karti hase) or 5 words (ananyaa kheDvaanu kaam karti hase)
# Gujarati utterances have 4 words (ananyaa aaraam kare che) or 5 words (ananyaa kheDvaanu kaam kare che)
# Marathi utterances have 4 words (ananyaa aaraam karte aahe) because there is no "kaam"

# trim white space at beginning and end of Contour
d$Contour <- trimws(d$Contour)

# what are the contours
table(d$Contour,d$language)

# how many annotations, by whether "kaam" is realized?
table(d[d$kaamSentence == "no",]$Contour) #4 annotations
table(d[d$kaamSentence == "yes",]$Contour) #5 annotations
# (6 utterances had only 4 annotations, fixed above after inspecting the files)

table(d[d$kaamSentence == "yes" & d$language == "Chodri",]$Contour,d[d$kaamSentence == "yes" & d$language == "Chodri",]$File)


# subject tones
# the first string is the subject tone
# merging LH (no pause after subject) and L-H% (pause after subject)
table(d$Contour,d$language) # reveals that the subject AP always has a rise

# simplify subject tone for further analysis
d$ContourS <- gsub("L-H\\% ","LH ", d$Contour)
table(d$ContourS,d$language)

# predicate tone
# the second string is the predicate tone
d$predTone <- d$ContourS
d$predTone <- ifelse(substr(d$ContourS,1,4) == "LH 0", "0", "LH")
table(d$predTone) #35 dephrased predicates

# tone on kaam
dk <- d %>%
  filter(kaamSentence == "yes") %>%
  droplevels()
nrow(dk) #176 = 184 (4 sentences x 23 talkers (Chodri + Gujarati) x 2 conditions) - some excluded sentences
dk$kaamTone <- dk$ContourS
dk$kaamTone <- gsub("0 L-H\\%|LH L-H\\%|0 L-L\\%|LH L-L\\%|0 H-L\\%|LH H-L\\%","", dk$kaamTone)
dk$kaamTone <- trimws(dk$kaamTone)
final <- 2
dk$kaamTone <- substr(dk$kaamTone, nchar(dk$kaamTone)-final+1,nchar(dk$kaamTone))
table(dk$kaamTone)

# tone on aux
d$auxTone <- d$ContourS
#13: extract from 7 to 9
final <- 4   # extract characters from x to y (which is where final tone begins)
d$auxTone <- substr(d$auxTone, nchar(d$auxTone) - final - 2, nchar(d$auxTone)-final)
table(d$auxTone)

# final tone
d$finalTone <- d$ContourS
final <- 4   # extract the last four characters (i.e., from nchar-3 to nchar)
d$finalTone <- substr(d$finalTone, nchar(d$finalTone) - final + 1, nchar(d$finalTone))
table(d$finalTone)

# pause after subject: if <p> marked on relevant tier, or subject AP ends with %
table(d$Words,d$language)
d$pause <- ifelse(grepl("ananyaa <p>",d$Words,fixed=TRUE), "yes",
                  ifelse(grepl("laavanya <p>",d$Words,fixed=TRUE), "yes", "no"))
table(d$Words,d$language,d$pause)
table(d$pause) #43 pauses after subject AP

# check: how many contours start with L-H% (i.e., have a pause after subject?)
d$pause2 <- substr(d$Contour,1,4)
table(d$pause2) #43

# save the preprocessed data
write.csv(d, "../data/contours-merged-and-cleaned.csv")
nrow(d) #528

# check number of utterances per language
length(unique(d[d$language == "Chodri",]$File)) #168
length(unique(d[d$language == "Gujarati",]$File)) #186
length(unique(d[d$language == "Marathi",]$File)) #174

# load clean data ----
d = read.csv("../data/contours-merged-and-cleaned.csv")
nrow(d) #528

# create files to be used in other analyses ----

# utterances with utterance-internal pauses 

table(d$Words)
d.pause <- droplevels(subset(d,grepl("<p>",d$Words),drop=FALSE))
#View(d.pause)

pauseCHO <- droplevels(subset(d.pause,d.pause$language == "Chodri"))
nrow(pauseCHO) #16
write.csv(pauseCHO, "../data/Chodri-pauses.csv")

pauseGUJ <- droplevels(subset(d.pause,d.pause$language == "Gujarati"))
nrow(pauseGUJ) #18
write.csv(pauseGUJ, "../data/Gujarati-pauses.csv")

pauseMAR <- droplevels(subset(d.pause,d.pause$language == "Marathi"))
nrow(pauseMAR) #10
write.csv(pauseMAR, "../data/Marathi-pauses.csv")

# utterances with LH on predicate
table(d$predTone)
d.predTone <- droplevels(subset(d,d$predTone == "LH"))
nrow(d.predTone)

predToneCHO <- droplevels(subset(d.predTone,d.predTone$language == "Chodri"))
nrow(predToneCHO) #167
write.csv(predToneCHO, "../data/Chodri-pred-LH.csv")

predToneGUJ <- droplevels(subset(d.predTone,d.predTone$language == "Gujarati"))
nrow(predToneGUJ) #173
write.csv(predToneGUJ, "../data/Gujarati-pred-LH.csv")

predToneMAR <- droplevels(subset(d.predTone,d.predTone$language == "Marathi"))
nrow(predToneMAR) #153
write.csv(predToneMAR, "../data/Marathi-pred-LH.csv")

# by-focus condition: data and models ----

# predicate tone by focus condition
table(d$predTone,d$condition,d$language) # predicate dephrasing more likely in subjFoc than predFoc

str(d$condition)
d$condition <- as.factor(d$condition)
str(d$predTone)
d$predTone <- as.factor(d$predTone)

d$
d$condition <- relevel(d$condition, ref = "subjFoc")

# Marathi
m.m <- glmer(predTone ~ condition + (1|sentence) + (1+condition|talker),family=binomial(link='logit'),data=d[d$language == "Marathi",])
summary(m.m) # overfitted, for talker, cond slope correlation = 1

m.m2 <- glmer(predTone ~ condition + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d[d$language == "Marathi",])
ranef(m.m2)
summary(m.m2) # overfitted, remove sentence intercept

# Marathi model reported in the paper
m.m3 <- glmer(predTone ~ condition + (1|talker),family=binomial(link='logit'),data=d[d$language == "Marathi",])
ranef(m.m3)
summary(m.m3) 
# conditionpredFoc    3.994      1.103   3.621 0.000294

# Gujarati
m.g1 <- glmer(predTone ~ condition + (1|sentence) + (1|talker),family=binomial(link='logit'),data=d[d$language == "Gujarati",])
summary(m.g1)
# does not converge

# Gujarai model reported in the paper
m.g2 <- glmer(predTone ~ condition + (1|talker),family=binomial(link='logit'),data=d[d$language == "Gujarati",])
ranef(m.g2)
summary(m.g2) 
# conditionpredFoc   2.6355     1.0519   2.505   0.0122

# kaam tone by focus condition
table(dk$kaamTone,dk$condition,dk$language) 
#Chodri: kaam not more likely to be dephrased in predFoc than in subjFoc

str(dk$kaamTone)
dk$kaamTone <- as.factor(dk$kaamTone)

m.c <- glmer(kaamTone ~ condition + (1|talker),family=binomial(link='logit'),data=dk[dk$language == "Chodri",])
ranef(m.c)
summary(m.c)
# conditionsubjFoc   0.5543     0.6327   0.876    0.381

# non-final aux tone by focus condition
table(d$auxTone,d$condition,d$language) 

str(d$auxTone)
d$auxTone <- as.factor(d$auxTone)

m.m <- glmer(auxTone ~ condition + (1|talker),family=binomial(link='logit'),data=d[d$language == "Marathi",])
ranef(m.m)
summary(m.m)

# final tone by focus condition
table(d$finalTone,d$condition,d$language)

# pause after subject by focus condition
table(d$pause,d$condition,d$language) 

