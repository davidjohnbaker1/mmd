#======================================================================================================
# Analyze and Clean Survey Data
#--------------------------------------------------
library(tidyverse)
library(irr)
#--------------------------------------------------

dictation_survey <- read_csv("aural_survey/Dictation_Survey_Responses.csv")
View(dictation_survey)

unique(dictation_survey$subject)
length(unique(dictation_survey$subject))
#--------------------------------------------------
# Removal of Subjects

dictation_survey <- dictation_survey %>%
  filter(subject != "73650") # PhD in Neuroscience 

#--------------------------------------------------
# Create Melody Number
head(dictation_survey)

get_melody_number <- function(x){
  x %>%
    str_remove_all(pattern = "[:alpha:]")
}

dictation_survey <- dictation_survey %>%
  mutate(melody_number = as.numeric(get_melody_number(stimulus)),
         subject = as.character(subject)) 

make_semester_category <- function(x){
  
  x[x == 0] <- "First Semester"
  x[x == 1] <- "Second Semester"
  x[x == 2] <- "Third Semester"
  x[x == 3] <- "Fourth Semester"
  x[x == 4] <- "Advanced Undergraduate"
  x[x == 5] <- "Graduate Studies"
  x
}

#--------------------------------------------------
# Assign Semster as Factor 

dictation_survey <-
  dictation_survey %>%
  mutate(What_Semester = make_semester_category(What_Semester),
         What_Semester = factor(What_Semester, levels = c("First Semester",
                                                          "Second Semester",
                                                          "Third Semester",
                                                          "Fourth Semester", 
                                                          "Advanced Undergradaute",
                                                          "Graduate Studies"))) 
#======================================================================================================
# Variable Checks
#--------------------------------------------------
# Figure 1
# * Plot X axis of Index of Melody ~ Semester, Average Difficulty, Adherence to Grammatrial Syntax
# Things to Begin to Plot
# List of All Melodies Use in Chapter
# Basic Demographics of Survey Participants
#--------------------------------------------------
# Calculate IRR for Each Participant for
#--------------------------------------------------
# Difficulty for Average 2nd Year 
#--------------------------------------------------

dictation_survey <- dictation_survey %>%
  mutate(melody_rank = dense_rank(melody_number)) 

sem <-function(x) {sd(x)/sqrt(length(x))}

#--------------------------------------------------
# Second Year Plots

dictation_survey %>%
  select(melody_rank, Difficulty_2nd_Year, subject) %>%
  group_by(melody_rank) %>%
  mutate(difficulty_mean = mean(Difficulty_2nd_Year), 
         difficulty_sem = sem(Difficulty_2nd_Year)) -> dictation_agreement_plot

ggplot(dictation_agreement_plot, aes(x = melody_rank, y = difficulty_mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin=difficulty_mean-difficulty_sem,
                    ymax = difficulty_mean+difficulty_sem),
                width = .1) +
  labs(title = "Average Difficulty for 2nd Year of Melodies Across Sample",
       subtitle = "Bars indicate Standard Error of the Mean",
       x = "Melodies in Sample in Rank Order",
       y = "Averaged Difficulty") +
  theme_classic()

#--------------------------------------------------
# Grammar Plots 

dictation_survey %>%
  select(melody_rank, Grammar, subject) %>%
  group_by(melody_rank) %>%
  mutate(grammar_mean = mean(Grammar), 
         grammar_sem = sem(Grammar)) -> dictation_agreement_plot_grammar

ggplot(dictation_agreement_plot_grammar, aes(x = melody_rank, y = grammar_mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin=grammar_mean-grammar_sem,
                    ymax = grammar_mean+grammar_sem),
                width = .1) +
  labs(title = "Average Grammar Across Sample",
       subtitle = "Bars indicate Standard Error of the Mean",
       x = "Melodies in Sample in Rank Order",
       y = "Averaged Grammar") +
  theme_classic()



#--------------------------------------------------
# Calculate IRR + ICC 
#--------------------------------------------------
# Use two way model because both melodies and raters are selected as random effects 
# Consitency 
dictation_survey %>%
  select(subject, melody_number, Difficulty_2nd_Year) %>%
  spread(subject, Difficulty_2nd_Year) %>%
  select(-melody_number) %>%
  icc(model = "twoway",type = "consistency")
  
# ICC .763 

# Cicchetti (1994)[16] gives the following often quoted guidelines for interpretation for kappa or ICC inter-rater agreement measures:
#   
#   Less than 0.40—poor.
# Between 0.40 and 0.59—fair.
# Between 0.60 and 0.74—good.
# Between 0.75 and 1.00—excellent.
# A different guideline is given by Koo and Li (2016)[17]:
#   
#   below 0.50: poor
# between 0.50 and 0.75: moderate
# between 0.75 and 0.90: good
# above 0.90: excellent
#--------------------------------------------------
# Calculate ICC for Class Category 
#--------------------------------------------------

#--------------------------------------------------
# Calculate All ICC 
#--------------------------------------------------
# Semester -- Use Ranked Data 

dictation_survey %>%
  select(subject, melody_number, What_Semester) %>%
  spread(subject, What_Semester) %>%
  select(-melody_number) %>%
  kappam.fleiss()

dictation_survey %>%
  select(Difficulty_2nd_Year, Grammar, melody_number) %>%
  group_by(melody_number) %>%
  mutate(melody_cor = cor(Difficulty_2nd_Year, Grammar)) %>%
  arrange(melody_cor)

#--------------------------------------------------
# Better to do rank order or melody_number??

library(lme4)
rank_model <- lmer(Difficulty_2nd_Year ~ melody_rank + (1|subject) + (1|stimulus), data = dictation_survey)
index_model<- lmer(Difficulty_2nd_Year ~ melody_number + (1|subject) + (1|stimulus), data = dictation_survey)

summary(rank_model)
summary(index_model)
anova(rank_model, index_model)
#--------------------------------------------------
# Figure 2
# Plot basic Correlations between grammatical coherence and difficulty

dictation_survey %>%
  group_by(stimulus) %>%
  mutate(dif_gram_cor = cor(Difficulty_2nd_Year, Grammar)) %>%
  select(stimulus, dif_gram_cor, melody_rank) %>%
  unique() %>%
  arrange(dif_gram_cor) %>%
  ggplot(aes(x = reorder(stimulus, melody_rank), y = dif_gram_cor)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Correlations Between Difficulty and Melodic Common Practice",
       x = "Melody",
       y = "Pearson r")

dictation_survey %>%
  group_by(stimulus) %>%
  mutate(dif_gram_cor = cor(Difficulty_2nd_Year, Grammar)) %>%
  select(stimulus, dif_gram_cor, melody_rank) %>%
  unique() %>%
  arrange(dif_gram_cor) %>%
  ggplot(aes(x = reorder(stimulus, dif_gram_cor), y = dif_gram_cor)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Correlations Between Difficulty and Melodic Common Practice",
       x = "Melody",
       y = "Pearson r")


#--------------------------------------------------
# Introduce Fantastic
# Merge on Dataset

#--------------------------------------------------
# Figure 3
# Difficulty level for average year undergraduate as DV
# Series of Linear Models
# Number of Notes
# FANTASTIC Features....

#--------------------------------------------------
# Extended Discussion on linear modeling
# Collinearity

#--------------------------------------------------
# Figure 4
# PCA of Features used BEFORE in complexity measures
# Predict difficulty from PCA

#--------------------------------------------------
# Also explore with Cum Inf Content
#


