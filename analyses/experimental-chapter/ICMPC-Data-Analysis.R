#======================================================================================================
# ICMPC Data Analysis 
#--------------------------------------------------
library(ggplot2)
library(data.table)
library(psych)
library(stringr)
library(GGally)
#--------------------------------------------------
# Import Data
# Before running first import, need to manually remove line in master 
dictationData <- fread("data/aggregated_data/Master-Data-Dictation.csv")
ResponseData <- fread("data/aggregated_data/Reponse-Data.csv")
dictationData <- dictationData[ResponseData, on = "subjectNo"]
#--------------------------------------------------
# Drops 
#--------------------------------------------------
# THESE TWO FOR NOT COMPLETELING COGNITIVE TASK 
dictationData[, DROP := "NO"]

# Drop 1032 for no WMC Score
dictationData[subjectNo==1020]$DROP <- "YES"
dictationData[subjectNo==1032]$DROP <- "YES"
# 1041, 1042
dictationData$DROP[42] <- "YES"
dictationData$DROP[43] <- "YES"
nrow(dictationData)
dictationData$completecases <- complete.cases(dictationData)
dictationData[completecases == FALSE]

describe(dictationData[DROP == "NO"]$age)

table(dictationData$gender)

dictationData$gender <- str_to_lower(dictationData$gender)
dictationData$gender <- str_replace(dictationData$gender,"^.*f.*$","Female")
dictationData$gender <- str_replace(dictationData$gender,"^m.*$","Male")
dictationData$gender <- str_replace(dictationData$gender,"woman","Female")
table(dictationData$gender)
#--------------------------------------------------
# Clean more variables
names(dictationData)
# AP 
# weeks since last
# auralWeeksTaken 
dictationData$absolutePitch <- str_to_lower(dictationData$absolutePitch)
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "no","NO")
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "relative","NO")
dictationData$absolutePitch <- str_replace(dictationData$absolutePitch, 
                                           "yes","YES")


dictationData$auralWeeksTaken <- str_replace(dictationData$auralWeeksTaken,
                                            "[Ww]eeks","")
dictationData[subjectNo=="1031"]$auralWeeksTaken <- mean(dictationData$auralWeeksTaken, na.rm = TRUE)

dictationData[subjectNo == 1027]$auralWeeksTaken <- "30"
dictationData[subjectNo == 1015]$auralWeeksTaken <- "70"
dictationData[subjectNo == 1012]$auralWeeksTaken <- "60"
dictationData$auralWeeksTaken <- str_trim(dictationData$auralWeeksTaken,side = "both")
dictationData[, auralWeeksTaken := as.numeric(auralWeeksTaken)]


dictationData <- dictationData[DROP == "NO"]
names(dictationData)

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

ggpairs(cog.variables, title = "Individual Non-Musical Descriptives",axisLabels = "none")


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


# Create Melody Differences Plots

ggplot(melody.long, aes(melody,score)) + 
  geom_boxplot() + geom_jitter(width = 0.2) + 
  labs(title = "Differences in Melody Difficulty", x = "Melody", y = "Error Percentage")

ggplot(melody.long, aes(score , fill = melody)) + geom_density() + facet_wrap(~melody) +
  labs(title = "Density Plots of Dictation Scores", x = "Score", y = "Density")
  
example <- ggplot(melody.long, aes(x = melody, y = score, size = gf) ) +
  geom_point() +
  theme_minimal() + stat_summary(fun.y = "mean", color = "red",
                                 size = 3, geom = "point") +
  xlab("Means of Melody Scores") + ylab("Number of Errors") + labs(title ="Repeated Measures ANOVA")
print(example)


# Mixed Effects Modeling 

# First Null Model
# Just Effect of Melody -- RM ANOVA
# Fit effect of wmc -- Add Covariates
# Fit effect of gf
# Musical Training
# More specific Measures
library(lmerTest)
# RM ANOVA 
# Random Intercept of Melody (RMANVOA)
model.0 <- lmer(score ~ (1|subjectNo) , data = melody.long)

anova(model.0)
summary(model.0)

model.1 <- lmer(score ~ melody + (1|subjectNo), data = melody.long)
anova(model.1)
summary(model.1)

anova(model.0, model.1)
melody.long

model.a <- lmer(score ~ NoteDensity  + (1|subjectNo),data = melody.long )
anova(model.a)

model.b <- lmer(score ~ Tonalness  + (1|subjectNo),data = melody.long )
anova(model.b)

model.c <- lmer(score ~ Tonalness*NoteDensity + (1|subjectNo),data = melody.long )
anova(model.c)

model.d <- ezANOVA(data = melody.long,
                          dv = .(score), wid = .(subjectNo),
                          within = c(Tonalness, NoteDensity),
                          type = 3, detailed = TRUE)
apa.ezANOVA.table(model.d, 
                  table.number = 1)

#--------------------------------------------------
# More?
model.e <- ezANOVA(data = melody.long,
                   dv = .(score), wid = .(subjectNo),
                   within = .(Tonalness, NoteDensity),
                   type = 3, detailed = TRUE)

apa.ezANOVA.table(model.d, 
                  table.number = 1,
                  filename="figures/MelodyDifferences.doc")


print(model.d)
citation(package = "ez")

line1 <- ggplot(melody.long, aes(melody, score, colour = subjectNo, group = subjectNo))
line1 + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, 
                                                                  geom = "line") + labs(x = "Melody", y = "Score")

# Random effect of slopes and intercepts
# ( countinuousPredictor | randomEffectGroup)
View(melody.long)
# Random slope of WMC within Subject Number 
model.2 <- lmer(score ~ melody + (wmc | subjectNo), data = melody.long)
anova(model.2)
summary(model.2)

model.3 <- lmer(score ~ melody + (gf | subjectNo), data = melody.long)
anova(model.3)
summary(model.3)

model.4 <- lmer(score ~ melody + (MUSICAL | subjectNo), data = melody.long)
anova(model.4)
summary(model.4)

model.5 <- lmer(score ~ melody +  gender + (1 | subjectNo), data = melody.long)
anova(model.5)
summary(model.5)

model.6 <- lmer(score ~ melody  +  baisScore + (1 | subjectNo), data = melody.long)
anova(model.6)
summary(model.6)


model.7 <- lmer(score ~ melody +  auralWeeksTaken + (1 | subjectNo), data = melody.long)
anova(model.7)
summary(model.7)

# Model Comparisons
anova(model.1, model.2)
anova(model.1, model.3)
anova(model.1, model.4)
anova(model.1, model.7)

