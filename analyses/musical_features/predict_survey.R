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
# Swap Out Transposed Melodies Names 

# Berkowitz334
# Berkowitz382
# Berkowitz417
# Berkowitz607 

accomodate_rename <- function(x){
  x[x=="Berkowitz334"] <- "Berkowitz334t"
  x[x=="Berkowitz382"] <- "Berkowitz382t"
  x[x=="Berkowitz417"] <- "Berkowitz417t"
  x[x=="Berkowitz607"] <- "Berkowitz607t"
  x
}

fantastic_computations$file.id <- accomodate_rename(fantastic_computations$file.id)

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

#remove_transpose <- function(x){
#  x %>%
#    str_remove_all(pattern = "t$")
#}

# dictation_survey$stimulus <- remove_transpose(dictation_survey$stimulus)


target %>% 
  left_join(fantastic_computations) -> melody_data 
#======================================================================================================
# Export Data For Correlation Table 

#--------------------------------------------------

melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  arrange(-mean_diff) 
  
#--------------------------------------------------
# FILE FOF DISSERATION

fix_gt <- function(x){
  x <- x[x=="mean_diff"] <- "Mean Difficulty"
  x <- x[x=="mean_gram"] <- "Mean Grammar"
}

melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
# mutate(strength = abs(mean_diff) + abs(mean_gram)) %>%
  gather(mean_diff, mean_gram, -rowname) %>%
  rename(Feature = rowname, GroundTruth = mean_diff, corr = mean_gram) %>%
  mutate(GT = fix_gt(GroundTruth)) %>%
  filter(Feature != "mean_gram") %>%
  filter(Feature != "mean_diff") %>%
  ggplot(aes(x = reorder(Feature, corr), y = corr, group = GroundTruth)) +
  coord_flip() +
  geom_bar(stat = "identity", aes(fill = GroundTruth)) +
  labs(title = "Correlations Between FANTASTIC Features and Expert Ratings",
       x = "FANTASTIC Features",
       y = "r",
       color = "Ground Truth") +
  theme_minimal() -> fantastic_expert_plot

fantastic_expert_plot 

ggsave(filename = "document/img/FantasticExpertPlot.png", plot = fantastic_expert_plot)

melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  arrange(-mean_diff, mean_gram) %>% 
  filter(rowname != "mean_diff" & rowname != "mean_gram") %>%
  head(n = 5) -> feature_head

melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  arrange(-mean_diff, mean_gram) %>% 
  filter(rowname != "mean_diff" & rowname != "mean_gram") %>%
  tail(n = 5) -> feature_tail

rbind(feature_head, feature_tail) %>%
  mutate(Feature = rowname, Difficulty = mean_diff, Grammar = mean_gram) %>%
  select(Feature, Difficulty, Grammar) %>%
  knitr::kable() -> strong_features

write_rds(strong_features, path = "document/img/strongfeatures.rds")

# %>%
#   rename(`FANTASTIC Feature` = rowname, `Averaged Difficulty` = mean_diff, `Average Grammar` = mean_gram) %>%
#   kableExtra::kable(digits = 2) -> difficulty_feature_data 
# 
# write_rds(difficulty_feature_data,path = "analyses/musical_features/difficulty_feature_data_feb6.rds")
#======================================================================================================
# Show Collinearity of Feature Items 

fantastic_computations %>%
  select(-stimulus, h.contour, starts_with("int")) %>%
  select(i.abs.std, i.abs.mean, step.cont.loc.var, i.entropy, p.entropy, 
         d.median, d.eq.trans, mean.Yules.K, tonalness, mean.Simpsons.D, mode) %>%
  mutate(mode = as.factor(mode)) %>%
  ggpairs(title = "Feature Correlations") -> fantastic_collin

ggsave(filename = "document/img/FANTASTIC_collin.png", plot = fantastic_collin)

#--------------------------------------------------
# Plot Melody Against Various Features
View(melody_data)

# Good Ones
ggplot(melody_data, aes(x = p.entropy, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Pitch Entropy", x = "Pitch Entropy", y = "Mean Difficulty") +
  theme_minimal()

ggplot(melody_data, aes(x = tonalness, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Tonalness", x = "Tonalness", y = "Mean Difficulty") +
  theme_minimal()

ggplot(melody_data, aes(x = step.cont.loc.var, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Stepwise Contour: Local Variation", x = "Stepwise Contour", y = "Mean Difficulty") +
  theme_minimal()

ggplot(melody_data, aes(x = len, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Melody Length", x = "Melody Length", y = "Mean Difficulty") +
  theme_minimal() 

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
  select(stimulus,mean_diff, mean_gram, p.range,p.entropy,len, note.dens,tonalness) %>%
  pairs.panels()

model_dumb <- lm(mean_diff ~ p.entropy + len, data = melody_data)
summary(model_dumb)
