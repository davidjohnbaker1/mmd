#--------------------------------------------------
# Experimental Analyses
#--------------------------------------------------
library(tidyverse)
library(lme4)
library(viridis)
library(lmerTest)
#--------------------------------------------------

melody.long <- read.csv("experiment/data/melodic_data.csv")

ggplot(melody.long, aes(melody,score)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "Differences in Melody Difficulty", x = "Melody", y = "Error Percentage") +
  theme_minimal() -> melody_differences

melody_differences

ggsave(filename = "document/img/melody_differences.png", melody_differences)

ggplot(melody.long, aes(score , fill = melody)) + 
  geom_density() + 
  facet_wrap(~melody) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(title = "Density Plots of Dictation Scores", x = "Score", y = "Density") -> melody_difficulty

melody_difficulty 

ggsave(filename = "document/img/melody_difficulty.png", melody_difficulty)

#--------------------------------------------------
# Mixed Effects Modeling 

# First Null Model
# Just Effect of Melody -- RM ANOVA
# Fit effect of wmc -- Add Covariates
# Fit effect of gf
# Musical Training
# More specific Measures
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

