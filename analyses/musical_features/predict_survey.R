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
library(cowplot)
options(scipen = 999)
#--------------------------------------------------
# Data Import
dictation_survey <- read_csv("aural_survey/Dictation_Survey_Responses.csv")
fantastic_computations <- read_csv("corpus/symbolic/Melosol_Features.csv")
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

melody_data %>%
  ungroup(stimulus) %>%
  select(mean_diff:step.cont.loc.var) %>%
  correlate() %>%
  shave() %>%
  select(rowname, mean_diff, mean_gram) %>%
  # mutate(strength = abs(mean_diff) + abs(mean_gram)) %>%
  gather(mean_diff, mean_gram, -rowname) %>%
  rename(Feature = rowname, GroundTruth = mean_diff, corr = mean_gram) %>%
  filter(Feature != "mean_gram") %>%
  filter(Feature != "mean_diff") -> fantplotdata

fix_gt <- function(x){
  x[x=="mean_diff"] <- "Mean Difficulty"
  x[x=="mean_gram"] <- "Mean Grammar"
  x
  }

fantplotdata$GroundTruth <- fix_gt(fantplotdata$GroundTruth)

fantplotdata %>%
  ggplot(aes(x = reorder(Feature, corr), y = corr, group = GroundTruth)) +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat = "identity", aes(fill = GroundTruth)) +
  labs(title = "Correlations Between FANTASTIC Features and Expert Ratings",
       x = "FANTASTIC Features",
       y = "r",
       color = "Ground Truth") +
  theme_minimal() -> fantastic_expert_plot

fantastic_expert_plot 

# ggsave(filename = "document/img/FantasticExpertPlot.png", plot = fantastic_expert_plot)

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
  knitr::kable(digits = 2) -> strong_features

strong_features

# write_rds(strong_features, path = "document/img/strongfeatures.rds")

#======================================================================================================
# Show Collinearity of Feature Items 

fantastic_computations %>%
  select(-stimulus, h.contour, starts_with("int")) %>%
  select(i.abs.std, i.abs.mean, step.cont.loc.var, i.entropy, p.entropy, 
         d.median, d.eq.trans, mean.Yules.K, tonalness, mean.Simpsons.D, mode) %>%
  mutate(mode = as.factor(mode)) %>%
  ggpairs(title = "Feature Correlations") -> fantastic_collin

fantastic_collin

# ggsave(filename = "document/img/FANTASTIC_collin.png", plot = fantastic_collin)

#--------------------------------------------------
# Plot Melody Against Various Features
View(melody_data)

# Good Ones
ggplot(melody_data, aes(x = p.entropy, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Pitch Entropy", x = "Pitch Entropy", y = "Mean Difficulty") +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(c(0,100)) +
  stat_cor(method = "pearson") +
  theme_minimal() -> cow_pentropy

ggplot(melody_data, aes(x = tonalness, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Tonalness", x = "Tonalness", y = "") +
  ylim(c(0,100)) +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(method = "pearson") +
  theme_minimal() -> cow_tonalness

ggplot(melody_data, aes(x = step.cont.loc.var, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Stepwise Contour\nLocal Variation", x = "Stepwise Contour", y = "") +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(method = "pearson") +
  ylim(c(0,100)) +
  theme_minimal() -> cow_stepcontlocalvar

ggplot(melody_data, aes(x = len, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Melody Length", x = "Melody Length", y = "") +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(method = "pearson") +
  ylim(c(0,100)) +
  theme_minimal() -> cow_len

# Bad Ones

ggplot(melody_data, aes(x = tonal.spike, y = mean_diff)) +
  geom_point() + theme_minimal() +
  labs(title = "Tonal Spike", x = "Tonal Spike", y = "Mean Difficulty") +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(c(0,100)) +
  stat_cor(method = "pearson") +
  theme_minimal() -> cow_tonalspike

ggplot(melody_data, aes(x = step.cont.glob.dir	, y = mean_diff)) +
  geom_point() + theme_minimal() +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(method = "pearson") +
  ylim(c(0,100)) +
  labs(title = "Stepwise Contour\nGlobal Direction", x = "Stepwise Contour: Global Direction", y = "") +
  theme_minimal() -> cow_stpcontglobdir

ggplot(melody_data, aes(x = mean.entropy, y = mean_diff)) +
  geom_point() + theme_minimal() +
  geom_smooth(method = 'lm', se = FALSE) +
  stat_cor(method = "pearson") +
  ylim(c(0,100)) +
  labs(title = "Mean Pitch Entropy", x = "Mean Pitch Entropy", y = "") +
  theme_minimal() -> cow_meanentropy

ggplot(melody_data, aes(x = d.range, y = mean_diff)) +
  geom_point() + theme_minimal() +
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(c(0,100)) +
  stat_cor(method = "pearson") +
  labs(title = "Durational Range", x = "Durational Range", y = "") +
  theme_minimal() -> cow_drange

#--------------------------------------------------


plot_grid(cow_pentropy, cow_tonalness, cow_stepcontlocalvar, cow_len, 
          cow_tonalspike, cow_stpcontglobdir, cow_meanentropy, cow_drange, nrow = 2, ncol = 4) -> univariate_features

univariate_features

# ggsave(filename = "document/img/univariate_cow.png")

#--------------------------------------------------
# Regression

model_feat1 <- lm(mean_diff ~ p.entropy + len + tonalness + step.cont.loc.var, data = melody_data)
model_feat2 <-lm(mean_diff ~ p.entropy + len, data = melody_data)
model_feat3 <- lm(mean_diff ~ p.entropy, data = melody_data)

summary(model_feat1)
summary(model_feat2)
summary(model_feat3)

#--------------------------------------------------  

melody_data %>%
  select(stimulus,mean_diff, mean_gram, p.range,p.entropy,len, note.dens,tonalness) %>%
  pairs.panels()

model_dumb <- lm(mean_diff ~ p.entropy + len + tonalness + step.cont.loc.var, data = melody_data)
summary(model_dumb)

