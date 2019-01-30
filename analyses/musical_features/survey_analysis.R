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

dictation_survey$melody_number <- get_melody_number(dictation_survey$stimulus)

#======================================================================================================
# Things to Begin to Plot
# List of All Melodies Use in Chapter
# Basic Demographics of Survey Participants
#--------------------------------------------------
# Calculate IRR for Each Participant for 

# Semester
# Average Difficulty for second year
# Adherence to grammatical syntax of common pratice


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


