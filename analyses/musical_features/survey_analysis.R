#======================================================================================================
# Analyze and Clean Survey Data
#--------------------------------------------------
library(tidyverse)
#--------------------------------------------------

dictation_survey <- read_csv("aural_survey/Dictation_Survey_Responses.csv")
View(dictation_survey)

#--------------------------------------------------
# Create Melody Number
head(dictation_survey)

get_melody_number <- function(x){
  x %>%
    str_remove_all(pattern = "[:alpha:]")
}

dictation_survey$melody_number <- as.numeric(get_melody_number(dictation_survey$stimulus))
dictation_survey$subject <- as.character(dictation_survey$subject)

make_semester_category <- function(x){
  
  x[x == 0] <- "First Semester"
  x[x == 1] <- "Second Semester"
  x[x == 2] <- "Third Semester"
  x[x == 3] <- "Fourth Semester"
  x[x == 4] <- "Advanced Undergraduate"
  x[x == 5] <- "Graduate Studies"
  x
}

dictation_survey$What_Semester <- make_semester_category(dictation_survey$What_Semester)

#======================================================================================================
# Things to Begin to Plot
# List of All Melodies Use in Chapter
# Basic Demographics of Survey Participants
#--------------------------------------------------
# Calculate IRR for Each Participant for

#--------------------------------------------------
# 


#--------------------------------------------------
# Difficulty for Average 2nd Year 
dictation_survey %>%
  filter(melody_number < 500) %>%
  ggplot(aes(x = melody_number, y = Difficulty_2nd_Year, color = subject)) +
  geom_point() + geom_smooth(method = "lm") + 
  labs(title = "Difficulty of Melody Across Sample",
       x = "Melody Number",
       y = "Difficulty Rating") + 
  scale_x_continuous(limits = c(0, 629)) + 
  scale_y_continuous(limits = c(0,100))

#--------------------------------------------------
# Grammatical 
dictation_survey %>%
  ggplot(aes(x = melody_number, y = Grammar, color = subject)) +
  geom_point() + geom_smooth(method = "lm") + 
  labs(title = "How Grammatical",
       x = "Melody Number",
       y = "Grammar Rating") + 
  scale_x_continuous(limits = c(0, 629)) + 
  scale_y_continuous(limits = c(0,100))


# http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
# ICC
#--------------------------------------------------
# Calculate All ICC 
#--------------------------------------------------
# Semester -- Use Ranked Data 




# Semester -- ranked data
# Average Difficulty for second year -- ICC
# Adherence to grammatical syntax of common pratice -- ICC


#--------------------------------------------------
# Figure 1
# * Plot X axis of Index of Melody ~ Semester, Average Difficulty, Adherence to Grammatrial Syntax

#--------------------------------------------------
# Figure 2
# Plot basic Correlations between grammatical coherence and difficulty

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


