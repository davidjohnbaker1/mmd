#--------------------------------------------------
# Analysis for Experimental Chapter 
#--------------------------------------------------
library(tidyverse)
library(lme4)
library(data.table)
library(psych)
library(GGally)
#--------------------------------------------------
# Data Import

dictationData <- read_csv("experiment/data/Master-Data-Dictation.csv")
ResponseData <- read_csv("experiment/data/Reponse-Data.csv")

dictationData %>%
  left_join(ResponseData) -> dictation_data

#--------------------------------------------------
# Drop Participants

dictation_data %>%
  filter(subjectNo != 1020) %>% # NO WMC Score
  filter(subjectNo != 1032) %>% # No WMC Score
  filter(subjectNo != 1041) %>% # Incomplete
  filter(subjectNo != 1042) -> dictation_data 

mean(dictationData$age, na.rm = TRUE)

#--------------------------------------------------
# Clean Gender

dictation_data$gender <- str_to_lower(dictation_data$gender)
dictation_data$gender <- str_replace(dictation_data$gender,"^.*f.*$","Female")
dictation_data$gender <- str_replace(dictation_data$gender,"^m.*$","Male")
dictation_data$gender <- str_replace(dictation_data$gender,"woman","Female")
table(dictation_data$gender)

#--------------------------------------------------
# Clean More Variables

# AP 
# I'm lazy, don't judge me
dictationData <- dictation_data

dictationData$absolutePitch <- str_to_lower(dictationData$absolutePitch)
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "no","NO")
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "relative","NO")
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "yes","YES")
table(dictationData$absolutePitch)

# Ceeks since last

dictationData$auralWeeksTaken <- str_replace(dictationData$auralWeeksTaken,
                                             "[Ww]eeks","")
dictationData

dictationData[dictationData$subjectNo == 1027,]$auralWeeksTaken <- "30"
dictationData[dictationData$subjectNo == 1015,]$auralWeeksTaken <- "70"
dictationData[dictationData$subjectNo == 1012,]$auralWeeksTaken <- "60"
dictationData$auralWeeksTaken <- str_trim(dictationData$auralWeeksTaken,side = "both")
dictationData$auralWeeksTaken <- as.numeric(dictationData$auralWeeksTaken)

#--------------------------------------------------
#  Initial Analyses done in data.table 
#--------------------------------------------------

dictationData <- data.table(dictationData)

pairs.panels(dictationData[, .(baisScore,rotPartial, symSspanPartialScore,nsTotalScore,ravensScoreA, ravensScoreB, 
                               ravensScoreC,GENERAL,MUSICAL, SINGING,familyIncome, highestFather, highestMother,melody34p,
                               melody112p, melody9p, melody95p)])



musical.variables <- dictationData[, .(baisScore, 
                                       GENERAL, MUSICAL, SINGING, 
                                       melody34p,melody112p, melody9p,melody95p, gender, age)]
ggpairs(musical.variables,axisLabels = "none", title = "Individual Musical Descriptives")


#--------------------------------------------------
# Clean up Cognitive variables 

#--------------------------------------------------
# Check for Proper Correlations (Unsworth, 2009)
# Need sig positive correaltion between SymSpan, Ospan, TSpan -- All measuring WMC 

cognitive <- dictationData[, .(rotPartial, symSspanPartialScore, nsTotalScore, ravensScoreA, ravensScoreB, ravensScoreC)]
pairs.panels(cognitive, lm = TRUE, stars = TRUE)

# Make ravens one thing 
dictationData[, .(nsTotalScore, nsAttempted) ]
str(dictationData)

dictationData$nsAdjusted <- dictationData$nsTotalScore / dictationData$nsAttempted
dictationData$ravens <- (dictationData$ravensScoreA + dictationData$ravensScoreB + dictationData$ravensScoreC)/3

cognitive <- dictationData[, .(rotPartial, symSspanPartialScore, nsAdjusted, ravens)]
pairs.panels(cognitive, lm = TRUE, stars = TRUE)

# And each task should negatively correlate with own processing task , MORE HERE
negatives <- master[, .(rotPartial, symSspanPartialScore, symErrorTotal)]

pairs.panels(negatives, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Above Correlations Suggest Creating new WMC composite

dictationData[, wmc := ((scale(rotPartial)+scale(symSspanPartialScore))/2)]
#--------------------------------------------------
# Check that Gf are both measuring the same 
# Create New Gf Variables

gfscores <- dictationData[, .(wmc, ravens, nsAdjusted)]

pairs.panels(gfscores, lm = TRUE, stars = TRUE)

#--------------------------------------------------
# Above Correlations Suggest Creating new GF composite

dictationData[, gf := (scale(nsAdjusted) + scale(ravens)/2)]

#--------------------------------------------------
# Look at composites and their original scores
#--------------------------------------------------

dictationData[, .(familyIncome, highestFather, highestFather, symSspanPartialScore, rotPartial, gender)]


# Fix here !
cog.variables <- dictationData[, .(wmc, highestFather, 
                                   highestFather, symSspanPartialScore, 
                                   rotPartial, gender)]



# Melt Data for Exploration and Analysis 

dictationData <- dictationData[DROP == "NO"]

dictation.long <- melt(dictationData, 
                       id.vars = "subjectNo", measure.vars = c("melody95p","melody34p","melody9p","melody112p"))

# Get Covariates 

covariates <- dictationData[,.(subjectNo, gf, wmc, 
                               GENERAL, MUSICAL, SINGING, gender,baisScore,
                               auralWeeksTaken)]

melody.long <- dictation.long[covariates, on = "subjectNo"]
setnames(melody.long,
         c("variable","value"),
         c("melody","score"))

melody.long[, Tonalness := "FIX"]
melody.long[, NoteDensity := "FIX"]


melody.long[melody == "melody95p"]$Tonalness <- "LOW"
melody.long[melody == "melody95p"]$NoteDensity <- "HIGH"

melody.long[melody == "melody34p"]$Tonalness <- "HIGH"
melody.long[melody == "melody34p"]$NoteDensity <- "LOW"

melody.long[melody == "melody9p"]$Tonalness <- "LOW"
melody.long[melody == "melody9p"]$NoteDensity <- "LOW"

melody.long[melody == "melody112p"]$Tonalness <- "HIGH"
melody.long[melody == "melody112p"]$NoteDensity <- "HIGH"

fwrite(x = melody.long, file = "experiment/data/melodic_data.csv")

