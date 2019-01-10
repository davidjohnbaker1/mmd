#======================================================================================================
# Analysis For Chapter 3 of Dissertation (Individual Differences)
# Original Analysis used in ICMPC paper 
# David John Baker 
#--------------------------------------------------
# install.packages(c("tidyverse","GGally","data.table","psych","lavaan","semPlot","MVN"))
#--------------------------------------------------
library(tidyverse)
library(GGally)
library(data.table)
library(psych)
library(lavaan)
library(semPlot)
library(MVN)
#--------------------------------------------------
semdata <- fread("analyses/individual-differences-chapter/data/AnalysisData-Deletion.csv")

semdata.tib <- read_csv("analyses/individual-differences-chapter/data/AnalysisData-Deletion.csv")

names(semdata)

# Do it like paper OR do it with individual items? 
# GENERAL  =~ X2 + X6 + X7 + X9 + X11 + X18 +  X19 + X20 + X21 +  X22 + X25 + X26 + X27 + X28 + X29 + X30 + X32 + X33
semdata
2 + 38
adderVector <- c(2,6,7,9,11,18,19,20,21,22,25,26,27,28,29,30,32,33)

# THIS IS CORRECT GENERAL FACTOR, SWAP IN OTHER PAPER 
adderVectorCorrect <- c(1,2,4,6,13, 17,19,20,21,25,26,29,30,31,32,36)
length(adderVector)
forMeasurementModel <- adderVectorCorrect + 38
forMeasurementModel
names(semdata)

# Fix Name Problems
setnames(semdata,c("V39","V40","V41","V42","V43","V44","V45","V46",
                    "V47","V48","V49","V50","V51","V52","V53",
                    "V54","V55","V56","V57","V58","V59","V60",
                    "V61","V62","V63","V64","V65","V66","V67","V68",
                    "V69","V70","V71","V72","V73","V74","V75","V76"),
         c("Freetime",
           "Writing",
           "MusicalStyles",
           "SearchInternet",
           "SpendMoney",
           "Addiction",
           "KeepTrack",
           "LiveEvents",
           "ListenAttentively",
           "Singer",
           "HearFirstTime",
           "HardSpot",
           "ComparePerf",
           "SameSong",
           "HearBeat",
           "HearTune",
           "SelfTonal",
           "IDgenre",
           "NeverComplimented",
           "NotConsiderSelf",
           "RegularPractice",
           "PeakInterest",
           "MusicTheory",
           "Formal",
           "NoInstruments",
           "JoinIN",
           "SingByMemory",
           "HitRightNoteSingAlong",
           "SinginHarmony",
           "DontSingPubic",
           "SingBack23",
           "HearOnceSingBack",
           "ChooseMusic",
           "PiecesEmotion",
           "ExciteMotivate",
           "IdentifySpecial",
           "TalkEmotionslPiece",
           "EvokesMemories"))


#--------------------------------------------------
# Make Z Scores 
#--------------------------------------------------
semdata[, zAdjustedNumberSeries := scale(AdjustedNumberSeries)]
semdata[, zRavensAvg := scale(RavensAvg)]

semdata[, zMeanOspanPartialScore := scale(MeanOspanPartialScore)]
semdata[, zMeanSspanPartialScore := scale(MeanSspanPartialScore)]
semdata[, zTonePartial := scale(TonePartial)]

semdata[, zBeatPerception := scale(BeatPerception)]
semdata[, zMelodicMemory := scale(MelodicMemory)]

semdata[, zFreetime := scale(Freetime)]
semdata[, zWriting := scale(Writing)] 
semdata[, zMusicalStyles := scale(MusicalStyles)] 
semdata[, zSearchInternet := scale(SearchInternet)] 
semdata[, zSpendMoney := scale(SpendMoney)] 
semdata[, zAddiction := scale(Addiction)] 
semdata[, zKeepTrack := scale(KeepTrack)] 
semdata[, zLiveEvents := scale(LiveEvents)] 
semdata[, zListenAttentively := scale(ListenAttentively)] 
semdata[, zSinger := scale(Singer)] 
semdata[, zHearFirstTime := scale(HearFirstTime)] 
semdata[, zHardSpot := scale(HardSpot)] 
semdata[, zComparePerf := scale(ComparePerf)] 
semdata[, zSameSong := scale(SameSong)] 
semdata[, zHearBeat := scale(HearBeat)] 
semdata[, zHearTune := scale(HearTune)] 
semdata[, zSelfTonal := scale(SelfTonal)] 
semdata[, zIDgenre := scale(IDgenre)] 
semdata[, zNeverComplimented := scale(NeverComplimented)] 
semdata[, zNotConsiderSelf := scale(NotConsiderSelf)] 
semdata[, zRegularPractice := scale(RegularPractice)] 
semdata[, zPeakInterest := scale(PeakInterest)] 
semdata[, zMusicTheory := scale(MusicTheory)] 
semdata[, zFormal := scale(Formal)] 
semdata[, zNoInstruments := scale(NoInstruments)] 
semdata[, zJoinIN := scale(JoinIN)] 
semdata[, zSingByMemory := scale(SingByMemory)] 
semdata[, zHitRightNoteSingAlong := scale(HitRightNoteSingAlong)] 
semdata[, zSinginHarmony := scale(SinginHarmony)] 
semdata[, zDontSingPubic := scale(DontSingPubic)] 
semdata[, zSingBack23 := scale(SingBack23)] 
semdata[, zHearOnceSingBack := scale(HearOnceSingBack)] 
semdata[, zChooseMusic := scale(ChooseMusic)] 
semdata[, zPiecesEmotion := scale(PiecesEmotion)] 
semdata[, zExciteMotivate := scale(ExciteMotivate)] 
semdata[, zIdentifySpecial := scale(IdentifySpecial)] 
semdata[, zTalkEmotionslPiece := scale(TalkEmotionslPiece)] 
semdata[, zEvokesMemories := scale(EvokesMemories)] 
#--------------------------------------------------
      
# Check for Normality
#--------------------------------------------------
ofInterest <- semdata[, .(Freetime , Writing , SearchInternet , Addiction , ComparePerf , SelfTonal , NeverComplimented , NotConsiderSelf , RegularPractice , NoInstruments , JoinIN , SinginHarmony , DontSingPubic , SingBack23 , HearOnceSingBack , IdentifySpecial)]

descriptiveOfInterest <- describe(ofInterest)

descriptiveOfInterest.dt <- data.table(describe(ofInterest))
descriptiveOfInterest.dt[order(skew)]
#--------------------------------------------------
# Create Measurment Model 
str(semdata)
## Define Model 
measurement.model <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zMeanOspanPartialScore + zMeanSspanPartialScore + zTonePartial
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
gf ~~ wmc
'

## lavaan
measurment.model.fit <- cfa(measurement.model, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(measurment.model.fit, whatLabels = "std" , edge.label.cex = 2, layout = "tree2", rotation = 2)
summary(measurment.model.fit, standardized = TRUE)
# Remove some self report measures 

parameterEstimates(measurment.model.fit, standardized = TRUE, ci = FALSE)

# Measurement Model 
semPaths(measurment.model.fit,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

fitMeasures(measurment.model.fit, c("df","chisq","pvalue","rmsea","cfi","tli","tli"))

#--------------------------------------------------
# Double Models - One with General Predict Beat and Melodic
#               - Two with Cognitive Predict Beat and Melodic 
#--------------------------------------------------
# Set Up Models for Improving Fits 
#--------------------------------------------------



model.A <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.A <- sem(model.A, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.A, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.A)
fitMeasures(model.fit.A)

# Model 1
semPaths(model.fit.A,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.A, standardized = TRUE)
fitMeasures(model.fit.A, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))
#--------------------------------------------------

# Remove 35, 15, 38, 34, 27, 16, 37
# ExciteMotivate, HearBeat, EvokesMemories, PiecesEmotion, SingByMemory, HearTune, TalkEmotionsPiece
model.B <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.B <- sem(model.B, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.B, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.B)
fitMeasures(model.fit.B)

# SEM2
semPaths(model.fit.B,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.B, standardized = TRUE)
fitMeasures(model.fit.B, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.B, type = "cor")$cor

anova(model.fit.A, model.fit.B)

#--------------------------------------------------
model.C <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.C <- sem(model.C, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.C, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.C)
fitMeasures(model.fit.C)

# SEM 3
semPaths(model.fit.C,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.C, standardized = TRUE)
fitMeasures(model.fit.C, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.C, type = "cor")$cor

anova(model.fit.B, model.fit.C)

#--------------------------------------------------

model.D <- '
wmc =~ TonePartial + MeanOspanPartialScore + MeanSspanPartialScore
BeatPerception ~ wmc
MelodicMemory ~ wmc
BeatPerception ~ gf
MelodicMemory ~ gf
## Covariances
MelodicMemory ~~ BeatPerception
gf ~~ wmc
'

## Fits
model.fit.D <- sem(model.D, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.D, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.D)
fitMeasures(model.fit.D)

# SEM4
semPaths(model.fit.D,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.D, standardized = TRUE)
fitMeasures(model.fit.D, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.D, type = "cor")$cor

anova(model.fit.C, model.fit.D)
#--------------------------------------------------
# Model 5

model.E <- '
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
## Covariances
zMelodicMemory ~~ zBeatPerception
'
## Fits
model.fit.E <- sem(model.E, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.E, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.E)
fitMeasures(model.fit.E)

# SEM 5
semPaths(model.fit.E,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.E, standardized = TRUE)
fitMeasures(model.fit.E, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.E, type = "cor")$cor

anova(model.fit.B, model.fit.E)

# Model 5.b

model.E.b <- '
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
## Covariances
zMelodicMemory ~~ zBeatPerception
'
## Fits
model.fit.E.b <- sem(model.E.b, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.E.b, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.E.b)
fitMeasures(model.fit.E.b)

# SEM5
semPaths(model.fit.E.b,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.E.b, standardized = TRUE)
fitMeasures(model.fit.E.b, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.E.b, type = "cor")$cor

anova(model.fit.B, model.fit.E.b)

ggpairs(semdata[, .(MeanOspanPartialScore, MeanSspanPartialScore, TonePartial, AdjustedNumberSeries, RavensAvg, BeatPerception, MelodicMemory, GENERAL)], title = "SEM Descriptives")

describe(semdata[, .(MeanOspanPartialScore, MeanSspanPartialScore, TonePartial, 
                     AdjustedNumberSeries, RavensAvg, BeatPerception, MelodicMemory, GENERAL)])


