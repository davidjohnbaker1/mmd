#======================================================================================================
# Analyze and Clean Survey Data
#--------------------------------------------------
library(tidyverse)
library(irr)
library(viridis)
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
# Make Demographic Plots

# Age and Educational Status 

dictation_survey %>%
  group_by(subject) %>%
  select(subject, age) %>%
  unique() %>%
  ungroup() %>%
  mutate(mean_age = mean(age), sd_age = sd(age))

library(stringr)

fix_masters <- function(x){
  x <- ifelse(str_detect(string = x, pattern = "Mas") == TRUE, "Masters", x) 
  x <- ifelse(str_detect(string = x, pattern = "mas") == TRUE, "Masters", x) 
  x
  }

fix_doc <- function(x){
  x <- ifelse(str_detect(string = x, pattern = "[Cc]omp") == TRUE, "Completed Doctorate", x) 
}

fix_doc_candidate <- function(x){
  x <- ifelse(str_detect(string = x, pattern = "[Cc]an") == TRUE, "Doctoral Student", x) 
  x <- ifelse(str_detect(string = x, pattern = "[Ss]tu") == TRUE, "Doctoral Student", x) 
  x <- ifelse(str_detect(string = x, pattern = "ABD") == TRUE, "Doctoral Student", x) 
  }

dictation_survey$education_status <- fix_masters(dictation_survey$education_status)
dictation_survey$education_status <- fix_doc(dictation_survey$education_status)
dictation_survey$education_status <- fix_doc_candidate(dictation_survey$education_status)

dictation_survey <- dictation_survey %>%
  mutate(`Educational Status` = education_status)

dictation_survey %>%
  group_by(subject) %>%
  select(subject, age, education_status) %>%
  unique() %>%
  ungroup() %>%
  ggplot(aes(x = age)) +
  geom_histogram(bins = 10, aes(fill = education_status)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) +
  labs(title = "Age and Educational Distribution of Sample",
       x = "Age",
       y = "Frequency Count", fill = "Educational Status") +
  theme_minimal() + 
  scale_fill_viridis(discrete = TRUE) -> age_ed_survey_distribution

age_ed_survey_distribution

# Save Plot 
ggsave(age_ed_survey_distribution, 
       filename = "document/img/age_ed_survey_distribution.png")

# Years Teaching Aural 
dictation_survey$years_teaching_aural

clean_years <- function(x){
  x <- str_remove_all(string = x, pattern = "[Yy].*$")
  x <- str_replace_all(string = x, pattern = ",",replacement = "\\.")
  x <- as.numeric(x)
  x
}

dictation_survey$years_teaching_aural <- clean_years(dictation_survey$years_teaching_aural)


dictation_survey %>%
  group_by(subject) %>%
  select(subject, `Educational Status`, years_teaching_aural) %>%
  unique() %>%
  ungroup(subject) %>%
  mutate(avg_years = mean(years_teaching_aural), sd_years = sd(years_teaching_aural))

range(dictation_survey$years_teaching_aural)

# Preferred System 

dictation_survey %>%
  select(subject, `Educational Status`, years_teaching_aural, syllable_system, last_school) %>%
  unique() %>%
  print(n = 40)

# 38 report moveable Do, 2 fixed 

# Instrument


dictation_survey %>%
  select(subject, `Educational Status`, years_teaching_aural, instrument, last_school) %>%
  unique() %>%
  print(n = 40)

# Get Help

# Opinions

# Contact Information 

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
  scale_x_continuous(limits = c(1,20), breaks = seq(1,20,1)) +
  geom_errorbar(aes(ymin=difficulty_mean-difficulty_sem,
                    ymax = difficulty_mean+difficulty_sem),
                width = .1) +
  labs(title = "Average Difficulty for 2nd Year of Melodies Across Sample",
       subtitle = "Standard Error of Mean",
       x = "Melodies in Sample in Rank Order",
       y = "Average Difficulty") +
  theme_minimal() -> difficulty_plot

difficulty_plot

ggsave(plot = difficulty_plot, filename = "document/img/difficulty_plot.png")

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
  scale_x_continuous(limits = c(1,20), breaks = seq(1,20,1)) +
  labs(title = "Average Grammar Across Sample",
       subtitle = "Bars indicate Standard Error of the Mean",
       x = "Melodies in Sample in Rank Order",
       y = "Averaged Grammar") +
  theme_minimal() -> grammar_plot

grammar_plot

ggsave(plot = grammar_plot, filename = "document/img/grammar_plot.png")

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
  
# ICC .799

dictation_survey %>%
  select(subject, melody_number,Grammar) %>%
  spread(subject, Grammar) %>%
  select(-melody_number) %>%
  icc(model = "twoway",type = "consistency")

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
  scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,.5)) +
  labs(title = "Correlations Between Difficulty and Melodic Common Practice",
       x = "Melody",
       y = "Pearson Correlation Coefficient") + 
  theme_minimal() -> grammar_difficulty_correlation_plot

grammar_difficulty_correlation_plot

ggsave(filename = "document/img/grammar_difficulty_correlation_plot.png",
       plot = grammar_difficulty_correlation_plot)

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


