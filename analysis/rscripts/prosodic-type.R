# Prosody of Chodri, Gujarati, Marathi
# prosodic type analysis
# Praat script: extract-labels-tier4.praat

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source("helpers.r")

# load packages
library(tidyverse)
library(lmerTest)

theme_set(theme_bw())

# load data 
c = read.csv("../data/Chodri-LH-syllables.csv")
g = read.csv("../data/Gujarati-LH-syllables.csv")
m = read.csv("../data/Marathi-LH-syllables.csv")

#View(c)

length(unique(c$File)) #171
length(unique(g$File)) #186
length(unique(m$File)) #175

# bind the data
d <- bind_rows(c,g,m)
length(unique(d$File)) #532

# exclude disfluent utterances
excluded <- read.csv("../data/excluded.csv")
d <- droplevels(subset(d,!d$File %in% excluded$File))
nrow(excluded) #16
length(unique(d$File)) #528
table(d$File %in% excluded$File) # only FALSE, so all good


# trim spaces from the labels
d$Syllable <- trimws(d$Syllable)
d$Word <- trimws(d$Word)

### add columns 

# language
d$language <- ifelse(grepl("CHO-",as.character(d$File)),"Chodri",
                     ifelse(grepl("MAR-",as.character(d$File)),"Marathi",
                            ifelse(grepl("GUJ-",as.character(d$File)),"Gujarati",
                                   "OTHER")))
table(d$language)

# check that we have the appropriate number of files
length(unique(d[d$language == "Chodri",]$File)) #168
length(unique(d[d$language == "Gujarati",]$File)) #186
length(unique(d[d$language == "Marathi",]$File)) #174

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

# grammatical function
table(d$Word)
table(d$Syllable)

d$gf <- ifelse(d$Word == "laavanya" | d$Word == "ananyaa" | 
                                   d$Syllable == "laa" | d$Syllable == "van" |
                                   d$Syllable == "ya" | d$Syllable == "a" |
                                   d$Syllable == "nan" | d$Syllable == "yaa", "subject",
                                 ifelse(d$Word == "aaraam" | d$Word == "vyaayaam" |
                                          d$Word == "nindaNi" | d$Word == "naangarNi" |
                                          d$Syllable == "aa" | d$Syllable == "raam" |
                                          d$Syllable == "vyaa" | d$Syllable == "yaam" |
                                          d$Syllable == "nin" | d$Syllable == "da" | 
                                          d$Syllable == "Ni" | d$Syllable == "naan" | d$Syllable == "gar" |
                                        d$Word == "aaraam" | d$Word == "nindvaanu" |
                                          d$Word == "vyaayaam" | d$Word == "kheDvaanu" |
                                          d$Syllable == "aa" | d$Syllable == "raam" |
                                          d$Syllable == "nind" | d$Syllable == "vaa" |
                                          d$Syllable == "nu" | d$Syllable == "vyaa" | 
                                          d$Syllable == "yaam" | d$Syllable == "kheD" |
                                        d$Word == "aaraam" | d$Word == "nedvaanu" |
                                          d$Word == "kasrat" | d$Word == "kheDvaanu" |
                                          d$Syllable == "aa" | d$Syllable == "raam" |
                                          d$Syllable == "ned" | d$Syllable == "vaa" |
                                          d$Syllable == "nu" | d$Syllable == "kas" | d$Syllable == "rat" |
                                          d$Syllable == "kheD" | d$Syllable == "vaa" | d$Syllable == "nu","predicate", "other"))
                 
table(d$gf) 
# other predicate   subject 
# 5      1016      1056 

d[d$gf == "other" & d$Tone == "H" & d$language == "Chodri",]$File

tmp <- c("CHO-P1-target12.wav","CHO-P1-target15.wav","CHO-P1-target8.wav","CHO-P10-target8.wav","CHO-P4-target1.wav")
nrow(d) #2077

# exclude the predicates of these 5 Chodri utterances
head(d)
d <- droplevels(subset(d,!(d$File %in% tmp & d$gf == "predicate")))
nrow(d) #2072 (5 rows removed)

# # there are 5 H that are associated with auxiliary "ka" and "kar" rather than with predicate
# d$File[d$gf == "other" & d$Tone == "H"]
# 
# # fix
# # 5 files where auxiliary was wrongly extracted as Word and Syllable for H
# #d$File[d$gf == "other" & d$Tone == "H"]
# 
# d$Word[d$File == "MAR-P1-target14.wav" & d$Tone == "H"] <- "vyaayaam"
# d$Syllable[d$File == "MAR-P1-target14.wav" & d$Tone == "H"] <- "yaam"
# 
# d$Word[d$File == "MAR-P10-target8.wav" & d$Tone == "H"] <- "naangarNi"
# d$Syllable[d$File == "MAR-P10-target8.wav" & d$Tone == "H"] <- "Ni"
# 
# d$Word[d$File == "MAR-P3-target12.wav" & d$Tone == "H"] <- "naangarNi"
# d$Syllable[d$File == "MAR-P3-target12.wav" & d$Tone == "H"] <- "Ni"
# 
# d$Word[d$File == "MAR-P4-target3.wav" & d$Tone == "H"] <- "nindaNi"
# d$Syllable[d$File == "MAR-P4-target3.wav" & d$Tone == "H"] <- "Ni"
# 
# d$Word[d$File == "MAR-P5-target2.wav" & d$Tone == "H"] <- "vyaayaam"
# d$Syllable[d$File == "MAR-P5-target2.wav" & d$Tone == "H"] <- "yaam"
# 
# d$Word <- droplevels(d$Word)
# d$Syllable <- droplevels(d$Syllable)
# 
# # redo grammatical function (subject, verb)
# d$gf <- ifelse(d$Word == "laavanya" | d$Word == "ananyaa" | 
#                   d$Syllable == "laa" | d$Syllable == "van" |
#                   d$Syllable == "ya" | d$Syllable == "a" |
#                   d$Syllable == "nan" | d$Syllable == "yaa", "subject", 
#                 ifelse(d$Word == "aaraam" | d$Word == "vyaayaam" |
#                          d$Word == "nindaNi" | d$Word == "naangarNi" |
#                          d$Syllable == "aa" | d$Syllable == "raam" |
#                          d$Syllable == "vyaa" | d$Syllable == "yaam" |
#                          d$Syllable == "nin" | d$Syllable == "da" | d$Syllable == "Ni" |
#                          d$Syllable == "naan" | d$Syllable == "gar","predicate", "other"))
# table(d$gf) # now there are no "other"

# only use the subjects and predicates that have a L and H on them
names(d)

# total number of utterances
length(unique(d$File)) #528

# check that all 528 subjects have a L and a H on it
table(d[d$Word == "ananyaa",]$Tone) #264
table(d[d$Word == "laavanya",]$Tone) #264

# how many predicates with LH?
table(d[d$gf == "predicate",]$Tone)
# H   L 
# 489 522

# 493 total utterances with LH predicates
c.pred.LH <- read.csv(file="../data/Chodri-pred-LH.csv", header=TRUE, sep=",")
nrow(c.pred.LH) #167
g.pred.LH <- read.csv(file="../data/Gujarati-pred-LH.csv", header=TRUE, sep=",")
nrow(g.pred.LH) #173
m.pred.LH <- read.csv(file="../data/Marathi-pred-LH.csv", header=TRUE, sep=",")
nrow(m.pred.LH) #153

d <- droplevels(subset(d,!(d$gf == "predicate" & !(d$File %in% c.pred.LH$File | d$File %in% g.pred.LH$File | d$File %in% m.pred.LH$File))))

# how many predicates with LH?
table(d[d$gf == "predicate",]$Tone)
# H   L 
# 488 488

# check that we have the right amount of files
length(unique(d[d$language == "Chodri",]$File)) #168 (5 predicate removed, but subject remains)
length(unique(d[d$language == "Gujarati",]$File)) #186
length(unique(d[d$language == "Marathi",]$File)) #174

# how many predicates with LH? (528 utterances, but not all have LH on predicate)
# 488 utterances with LH on predicate (not kaam)
table(d[d$gf == "predicate",]$Tone)
#H   L 
#488 488 

# how many subject APs Ananyaa per language?
length(unique(d[d$Word == "ananyaa",]$File)) #264
length(unique(d[d$language == "Chodri" & d$Word == "ananyaa",]$File)) #84
length(unique(d[d$language == "Gujarati" & d$Word == "ananyaa",]$File)) #93
length(unique(d[d$language == "Marathi" & d$Word == "ananyaa",]$File)) #87

# how many predicate APs per language?
length(unique(d[d$gf == "predicate",]$File)) #48
length(unique(d[d$language == "Chodri" & d$gf == "predicate",]$File)) #162
length(unique(d[d$language == "Gujarati" & d$gf == "predicate",]$File)) #173
length(unique(d[d$language == "Marathi" & d$gf == "predicate",]$File)) #153

# save the preprocessed data
write.csv(d, "../data/prosodic-type-cleaned.csv")
nrow(d) #2037

# load clean data ----
d = read.csv("../data/prosodic-type-cleaned.csv")
nrow(d) #2037

summary(d)
table(d$Word)

# alignment of L on Ananyaa ----

ananyaa <- droplevels(subset(d, d$Word == "ananyaa" & d$Tone == "L"))
head(ananyaa)

table(ananyaa$Syllable,ananyaa$language)
#Chodri Gujarati Marathi
#a       65       63      73
#nan     19       30      14

# stats for L of Anaanya
chodri <- data.frame(c(65,19),c(42,42))
chodri
chisq <- chisq.test(chodri)
chisq
# X-squared = 12.458, df = 1, p-value = 0.0004163

gujarati <- data.frame(c(63,30),c(46.5,46.5))
gujarati
chisq <- chisq.test(gujarati)
chisq
# X-squared = 5.3346, df = 1, p-value = 0.02091

marathi <- data.frame(c(73,14),c(43.5,43.5))
marathi
chisq <- chisq.test(marathi)
chisq
# X-squared = 21.098, df = 1, p-value = 4.363e-06

# sanity check for Laavanya (where we expect the L to be in first syllable under either analysis)

laavanya <- droplevels(subset(d, d$Word == "laavanya" & d$Tone == "L"))
head(laavanya)

table(laavanya$Syllable,laavanya$language)
#        Chodri Gujarati Marathi
# laa     75       90      83
# van      7        3       4

# alignment of H ----

# get the tri-syllabic words with L in first syllable
table(d$Word)

tri <- droplevels(subset(d, d$Word == "laavanya" | d$Word == "ananyaa"
                         | d$Word == "kheDvaanu" | d$Word == "naangarNi"
                         | d$Word == "nedvaanu" | d$Word == "nindaNi"
                         | d$Word == "nindvaanu"))
head(tri)
nrow(tri) #1524 rows of syllables from 7 tri-syllabic words
#View(tri)

# first syllables of the 7 tri-syllabic words
first.syl <- c("a","kheD","laa", "naan", "ned", "nin", "nind")

# subset of tri that only has first syllables with L
tmp <- droplevels(subset(tri, tri$Tone == "L" & tri$Syllable %in% first.syl))
head(tmp)
#View(tmp)
nrow(tmp) #680 first syllables with L
length(unique(tmp$File)) #483 utterances/Files

# remove from tri the Files that are not in tmp
tri <- droplevels(subset(tri, tri$File %in% tmp$File))
nrow(tri) #1440
#View(tri)

# is the H realized on the last syllable of the 7 words?

triH <- droplevels(subset(tri, tri$Tone == "H"))
nrow(triH) #720

mid.syl <- c("van","nan","vaa","gar","da")
last.syl <- c("ya","yaa","nu", "Ni")
triH$last <- ifelse(triH$Syllable %in% last.syl, "last", 
                    ifelse(triH$Syllable %in% mid.syl, "mid", "first"))

table(triH$last,triH$language)
#         Chodri Gujarati Marathi
#first      0        3       0
#last     177      197     218
#mid       46       54      25

# stats for H
chodri <- data.frame(c(46,177),c(111.5,111.5))
chodri
chisq <- chisq.test(chodri)
chisq
# X-squared = 40.835, df = 1, p-value = 1.657e-10

gujarati <- data.frame(c(57,197),c(127,127))
gujarati
chisq <- chisq.test(gujarati)
chisq
# X-squared = 40.569, df = 1, p-value = 1.898e-10

marathi <- data.frame(c(25,218),c(121.5,121.5))
marathi
chisq <- chisq.test(marathi)
chisq
#X-squared = 89.118, df = 1, p-value < 2.2e-16
