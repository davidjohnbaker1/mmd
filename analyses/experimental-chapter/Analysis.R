#--------------------------------------------------
# Experimental Analyses
#--------------------------------------------------
# In This Script 
# Experimental Data Clean
# Differences in Melody Plot
# Differences in Density Melody Plot 
# Mixed Effects Analysis 
# Create Final Reporting 
#--------------------------------------------------
library(tidyverse)  # Manipulate Data 
library(lme4)       # Original
library(viridis)    # Color Pallete
library(lmerTest)   # P value add on for lme4
library(MuMIn)      # Get R2
library(sjPlot)     # Make Coef Plots
library(cowplot)    # Arrange Grid
#--------------------------------------------------
# Experimental Data Clean

melody.long <- read_csv("experiment/data/melodic_data.csv")
melody_fantastic <- read_csv("corpus/symbolic/Melosol_Features.csv")

melody_fantastic %>%
  select(file.id, tonalness, note.dens, i.entropy) %>%
  filter(file.id == "Berkowitz112" | file.id == "Berkowitz34" | file.id == "Berkowitz95" | file.id == "Berkowitz9") -> melody_fantastic

melody_fantastic$file.id <- str_replace_all(string = melody_fantastic$file.id,pattern = "Berkowitz", replacement = "Melody ")

melody_fantastic %>%
  rename(melody = file.id) -> melody_fantastic

melody.long$melody <- recode(melody.long$melody, "melody112p" = "Melody 112", "melody34p" = "Melody 34", "melody95p" = "Melody 95", "melody9p" = "Melody 9")

melody.long %>%
  left_join(melody_fantastic) -> melody.long

melody.long

melody.long$Tonalness <- recode(melody.long$Tonalness,"HIGH" = "High Tonal Category", "LOW" = "Low Tonal Category")
melody.long$NoteDensity <- recode(melody.long$NoteDensity,"HIGH" = "High Density Category", "LOW" = "Low Density Category")


melody.long %>%
  filter(subjectNo != 1041) %>%
  filter(subjectNo != 1042) %>%
  filter(subjectNo != 1032) %>%
  filter(subjectNo != 1020) -> melody.long

#--------------------------------------------------
# Melody Differences

ggplot(melody.long, aes(melody,score)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) + 
  labs(title = "Differences in Melody Difficulty", x = "Melody", y = "Error Percentage") +
  theme_minimal() -> melody_differences

melody_differences

#ggsave(filename = "document/img/melody_differences.png", melody_differences)

ggplot(melody.long, aes(score , fill = melody)) + 
  geom_density() + 
  facet_wrap(~melody) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(title = "Density Plots of Dictation Scores", x = "Score", y = "Density") -> melody_difficulty

melody_difficulty 

#ggsave(filename = "document/img/melody_difficulty.png", melody_difficulty)

#--------------------------------------------------
# Mixed Effects Modeling 

# (1|subject) == Random Intercept AKA random effect of Subject AKA things that are noise if new experiment 
# (1|item) == Random Effect of Item AKA Things that could be swapped out next experiment 
# Residual is ERROR 

# If you think people will perform different only different items, you need random slope!
# This is assumption of model 
# notate this with:
# (1+modulatingvariableofinterest|randomeffect)
# Effect of Random Effect might be different between itms or people 
# Take into account variablity between people on confounding measures 
# 1+ --> Expect Different Levels of Baseline SCORE 
# (something| ) --> Differing Levels of Response per that Item 
# Coefs of Random Slopes Should be all about same direction showing that it all effects similarliy!!!!
# aka
# (1+modulatingvariableofinterest|subject)
# (1+modulatingvariableofinterest|item)
#======================================================================================================
options(scipen = 999)
#--------------------------------------------------
# Null Model

melody.long %>%
  select(-GENERAL, -MUSICAL, -SINGING, -gender, -baisScore, -auralWeeksTaken)

# Predict Score, Random Intercept of Subject
# People are just random effects 
null_model <- lmer(score ~ (1|subjectNo) , data = melody.long) 

# Predict Based on General Fluid Intelligence 
# score ~ gf | fixed effect of general fluid bc not assumed to change over lifetime
gf_model <- lmer(score ~ gf + (1|subjectNo), data = melody.long)

summary(gf_model)

# Predict Based on WMC 
# score ~ wmc | fixed effect wmc bc not assumed to change over lifetime 
wmc_model <- lmer(score ~ wmc + (1 | subjectNo), data = melody.long)

summary(wmc_model)

# Predicrt with only cognitive 
# score ~ gf + wmc | Cognitives are Fixed effects for reasons above
cognitive_model <- lmer(score ~ wmc + gf + (1|subjectNo), data = melody.long)

summary(cognitive_model)

anova(gf_model, wmc_model)
# Yes WMC better than GF, but not like WAY Better

# Test All Models 
anova(null_model, gf_model, wmc_model, cognitive_model)
# Only Carry Forward WMC , statistical and theoretical reasons later

#--------------------------------------------------
# Now Look at Item Level Features and Ways to think about "Melody"

# score ~ melody as fixed effect (dummy code)
# Fixed Effect of Melody bc it'll always be played that way, dummy coded
# Also random effect of melody on subject, people will perform differently w different melodies 

melody_model <- lmer(score ~ melody + (1+melody|subjectNo), data = melody.long)

summary(melody_model)

anova(wmc_model, melody_model)
# Better than just WMC, nice!

# score ~ melody as tonal*density category 
# What if now based on Experimental conditoins of High low melody ?
# Still conceptualized as fixed effect of Category Features
# Random Slopes of Category Features having different effects on Subjects 

feature_category_model <- lmer(score ~ Tonalness*NoteDensity  + (1+Tonalness*NoteDensity|subjectNo) , data = melody.long)

summary(feature_category_model)

# score ~ melody as 4 continous features 
# Not categorical, use FANTASTIC Measures as fixed
feature_cont_model <- lmer(score ~ tonalness*note.dens + (1+tonalness*note.dens|subjectNo) , data = melody.long)

summary(feature_cont_model)
# Results still consistent 

# What if just i.entropy bc before on Survey 
# Based on Univariate Survey 
feature_ientropy_model <- lmer(score ~ i.entropy + (1+i.entropy|subjectNo), data = melody.long)

summary(feature_ientropy_model)


library(MuMIn)

anova(wmc_model, melody_model, feature_category_model, feature_cont_model, feature_ientropy_model)
# i.entroy for the win? But outside of that, feature continuous? 


#--------------------------------------------------
# score ~ best muscial (4 continous) + cognitive 
# This is model theoretically want to arrive at 
# both features are continuous predictors as fixed effects bc # with FANTASTIC
# Also know WMC, allow that to be random slope interacting with subject 
total_model_experimental <- lmer(score ~ tonalness*note.dens + wmc + (1+wmc|subjectNo) , data = melody.long)

summary(total_model_experimental)

# For Fun, just strong interval entropy FANTASTIC with wmc (strong predictors)
total_model_ientropy <- lmer(score ~ i.entropy + wmc + (1+wmc|subjectNo), data = melody.long)

summary(total_model_ientropy)

total_model_experimental <- lmer(score ~ tonalness*note.dens + wmc + (1+wmc|subjectNo) , data = melody.long)
summary(total_model_experimental_test)

#======================================================================================================
# Final Reporting 
#--------------------------------------------------
# Model Comparision 
anova(null_model,wmc_model,gf_model, cognitive_model, melody_model, feature_category_model, feature_cont_model, 
      feature_ientropy_model,total_model_experimental, total_model_ientropy) -> anova_model_table 

saveRDS(object = anova_model_table, file = "document/img/anova_model_table.RDS")

r.squaredGLMM(null_model) # Marginal and Conditional R2 Values 
r.squaredGLMM(wmc_model)
r.squaredGLMM(melody_model)
r.squaredGLMM(feature_category_model)
r.squaredGLMM(feature_cont_model)
r.squaredGLMM(feature_ientropy_model)
r.squaredGLMM(total_model_ientropy)
r.squaredGLMM(total_model_experimental)


#======================================================================================================
# Model Reporting 
#--------------------------------------------------
# Makes Plot for All Models
# Binds into one Table with R2 and Formula Fitted 
#--------------------------------------------------
# Plot needs-- Name of Model with R2, Formula it was fit with
#--------------------------------------------------

#--------------------------------------------------
# Model 1 - Null
null_model %>%
  plot_model(sort.est = TRUE, type = "re") +
  theme_minimal() +
  labs(title = "Model 1 (Null)", subtitle = "score ~ (1|subjectNo)") -> null_plot

null_plot

#--------------------------------------------------
# Model 2 - WMC Model 

wmc_model %>%
  plot_model(sort.est = TRUE) +
  theme_minimal() +
  labs(title = "Model 2", subtitle = "score ~ wmc + (1 | subjectNo)") -> wmc_plot 

wmc_plot

#--------------------------------------------------
# Model 3 - Melody Model 
melody_model <- lmer(score ~ melody + (1+melody|subjectNo), data = melody.long)

melody_model %>%
  plot_model(sort.est = TRUE, show.intercept = TRUE) +
  theme_minimal() +
  labs(title = "Model 3", subtitle = "score ~ melody + (1+melody|subjectNo)") -> melody_plot

melody_plot

#--------------------------------------------------
# Model 4 -- Feature Category 
feature_category_model %>%
  plot_model(sort.est = TRUE, show.intercept = TRUE) +
  theme_minimal() +
  labs(title = "Model 4", 
       subtitle = "score ~ Tonalness*NoteDensity  + (1+Tonalness*NoteDensity|subjectNo)") -> feat_cateogory_plot

feat_cateogory_plot # Recode 

#--------------------------------------------------
# Model 5 -- Feature Cont

feature_cont_model %>%
  plot_model(sort.est = TRUE) +
  theme_minimal() +
  labs(title = "Model 5", subtitle = "score ~ tonalness*note.dens +\n (1+tonalness*note.dens|subjectNo)") -> feat_cont_plot

feat_cont_plot

#--------------------------------------------------
# Model 6 --- Feature Entropy 
feature_ientropy_model %>%
  plot_model(sort.est = TRUE) +
  theme_minimal() +
  labs(title = "Model 6", subtitle = "score ~ i.entropy + (1|subjectNo)") -> ientropy_plot

ientropy_plot

#--------------------------------------------------
# Model 7 -- Total Model Entropy 
 total_model_ientropy %>%
  plot_model(sort.est = TRUE) +
  theme_minimal() +
  labs(title = "Model 7", subtitle = "score ~ i.entropy + wmc + (1+wmc|subjectNo)") -> entropy_experiemntal_plot

entropy_experiemntal_plot

#--------------------------------------------------
# Model 8 
total_model_experimental %>%
  plot_model(sort.est = TRUE) +
  theme_minimal() +
  labs(title = "Model 8", subtitle = "score ~ tonalness*note.dens\n + wmc + (1+wmc|subjectNo)") -> total_model_experimental_plot

total_model_experimental_plot

plot_grid(null_plot, wmc_plot, melody_plot, feat_cateogory_plot, feat_cont_plot, 
          ientropy_plot, entropy_experiemntal_plot, total_model_experimental_plot,nrow = 4, ncol = 2) -> me_grid


me_grid

#ggsave(filename = "document/img/me_grid.png", plot = me_grid)

#--------------------------------------------------
# forumua
# estimates with SI and CI 
# Test Statistics 
# p value?
# SD Random Effects and correlations 
#--------------------------------------------------
tab_model(null_model, wmc_model,title = "Null Model and WMC Model")

tab_model(melody_model, feature_category_model, title = "Melody and Feature Category")

tab_model(feature_cont_model, title = "Feature Continuous Model")

tab_model(total_model_ientropy, total_model_experimental, title = "Combined Models")



