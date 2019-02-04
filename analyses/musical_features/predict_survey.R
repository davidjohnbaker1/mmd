#======================================================================================================
# Merging With Fantastic
#--------------------------------------------------
# Point of this script is to
# 1. Take Survey Data and Create DV measure 
# 2. Merge FANTASTIC measures to Personal data
# 3. Build models to predict behiavioral measures
#--------------------------------------------------
library(tidyverse)
library(GGally)
library(psych)
m
#--------------------------------------------------
# Data Import
dictation_survey <- read_csv("aural_survey/Dictation_Survey_Responses.csv")
fantastic_computations <- read_csv("corpus/symbolic/CurrentBerkowitz.csv")

#--------------------------------------------------
# Change Fantastic Name for Merge 
fantastic_computations %>%
  rename(stimulus = file.id) -> fantastic_computations

dictation_survey %>%
  select(stimulus, Difficulty_2nd_Year, Grammar) %>%
  group_by(stimulus) %>%
  mutate(mean_diff = mean(Difficulty_2nd_Year),
         mean_gram = mean(Grammar)) %>% 
  select(stimulus, starts_with("mean")) %>%
  unique() -> target

#--------------------------------------------------
# Merge Data 
# Variables Wanted: All Fantastic, 
names(fantastic_computations)

remove_transpose <- function(x){
  x %>%
    str_remove_all(pattern = "t$")
}
dictation_survey$stimulus <- remove_transpose(dictation_survey$stimulus)

target %>% 
  left_join(fantastic_computations) -> melody_data 
  
melody_data %>%
  select(stimulus,mean_diff, mean_gram, p.range,p.entropy,len, note.dens,tonalness) %>%
  pairs.panels()

