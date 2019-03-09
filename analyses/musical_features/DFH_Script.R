#--------------------------------------------------
# DFH Script 
#--------------------------------------------------
library(tidyverse)
library(viridis)
berkowitz666 <- read.delim("corpus/symbolic/idyom/berk666.dat")
# Trained on Dataset of 622 krn files 

# CL-USER> (idyom:idyom 666 '(cpitch) '(cpint cpintfref)
#           :output-path  "~/Desktop/projects/mmd/corpus/symbolic/idyom"
#           :overwrite t
#           :separator #\tab
#           :detail 3)



View(berkowitz666)

#======================================================================================================
#--------------------------------------------------
# Get IC for first five notes

berkowitz666 %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(note.id <= 5) %>%
  group_by(melody.name) %>%
  mutate(cumIC = cumsum(information.content)) %>%
  ungroup() %>%
  filter(note.id == 5) %>%
  arrange(cumIC) %>%
  mutate(zCumIC = scale(cumIC), quintile = as.factor(ntile(cumIC, 5))) -> fived

fived %>%
  ggplot(aes(x = reorder(melody.name, cumIC), y = cumIC, color = quintile)) +
  geom_bar(stat = 'identity') + 
  coord_flip(ylim = c(12,30)) +
  labs(title = "Cumulative Information Content of First Five Notes of Berkowitz Corpus",
       subtitle = "Trained on N = 622 Melodies",
       x = "Melody", 
       y = "Cumulative Information Content") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_color_viridis(discrete = TRUE) 

#--------------------------------------------------
# Do Random Sampling from Top, Middle, Highest Bins 
#--------------------------------------------------
set.seed(666)

fived %>%
  filter(quintile == 1) %>%
  sample_n(size = 5) -> sample_1 

# 38, 282, 49, 262, 34

fived %>%
  filter(quintile == 3) %>%
  sample_n(size = 5) -> sample_2

fived %>%
  filter(quintile == 5) %>%
  sample_n(size = 5) -> sample_3

anova_data <- rbind(sample_1, sample_2, sample_3)

anova_data$quintile <- as.double(anova_data$quintile)

anova_model_1 <- lm(cumIC ~ quintile, data = anova_data)

summary(anova_model_1)

plot(anova_model_1)


#--------------------------------------------------