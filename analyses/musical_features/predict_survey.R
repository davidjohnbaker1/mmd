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
library(corrr)
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

#--------------------------------------------------
# Plot Melody Against Various Features


# Good Ones
ggplot(melody_data, aes(x = p.entropy, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Pitch Entropy", x = "Pitch Entropy", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = tonalness, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Tonalness", x = "Tonalness", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = step.cont.loc.var, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Stepwise Contour: Local Variation", x = "Stepwise Contour", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = len, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Melody Length", x = "Melody Length", y = "Mean Difficulty") +
  theme_classic() 

# Bad Ones

ggplot(melody_data, aes(x = tonal.spike, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Tonal Spike", x = "Tonal Spike", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = step.cont.glob.dir	, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Stepwise Contour: Global Direction??", x = "Stepwise Contour: Global Direction??", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = mean.entropy, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Mean Pitch Entropy", x = "Mean Pitch Entropy", y = "Mean Difficulty") +
  theme_classic()

ggplot(melody_data, aes(x = d.range, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Durational Range", x = "Durational Range", y = "Mean Difficulty") +
  theme_classic()
#--------------------------------------------------


melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  arrange(-mean_diff) %>%

#--------------------------------------------------
# FILE FOF DISSERATION
melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  kableExtra::kable(digits = 2) -> difficulty_feature_data 

write_rds(difficulty_feature_data,path = "analyses/musical_features/difficulty_feature_data_feb6.rds")
#--------------------------------------------------  

melody_data %>%
  select(stimulus,mean_diff, mean_gram, p.range,p.entropy,len, note.dens,tonalness) %>%
  pairs.panels()

model_dumb <- lm(mean_diff ~ p.entropy + len, data = melody_data)
summary(model_dumb)
