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
  ggplot(aes(x = reorder(melody.name, cumIC), y = cumIC, color = quintile, fill = quintile)) +
  geom_bar(stat = 'identity') + 
  coord_flip(ylim = c(12,30)) +
  labs(title = "Cumulative Information Content of First Five Notes of Berkowitz Corpus",
       subtitle = "Trained on N = 622 Melodies",
       x = "Melody", 
       y = "Cumulative Information Content") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) -> five_incipit_distribution

five_incipit_distribution

# ggsave(filename = "document/img/five_incipit_distiribution.png", five_incipit_distribution)

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
# Get Humdrum Data for Plots 

# NEEDS TO BE JUST START OF MELODIES ! 

bi_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/bi_grams.tsv",header = FALSE, sep = "\t")
tri_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/tri_grams.tsv",header = FALSE, sep = "\t")
quint_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/quint_grams.tsv",header = FALSE, sep = "\t")

bi_grams$grams <- "2-grams"
tri_grams$grams <- "3-grams"
quint_grams$grams <- "5-grams"

grams <- as.tibble(rbind(bi_grams, tri_grams, quint_grams))

grams %>%
  rename(FreqCount = V1, gram = V2) -> grams

grams %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~grams) +
  labs(title = "Frequency Distributions of Bi, Tri, and Quint Grams in Berkowitz Corpus",
       subtitle = "Based on Solfege Patterns", 
       x = "Ordered Solfege m-grams",
       y = "Frequency Count") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) -> bitriquint_gg

bitriquint_gg

# ggsave(filename = "document/img/bitriquint.png",plot = bitriquint_gg)


grams %>%
  filter(grams=="3-grams") %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  geom_bar(stat = 'identity') +
  labs(title = "Frequency Distributions of Trigrams in Berkowitz Corpus",
       subtitle = "Based on Solfege Patterns", 
       x = "Ordered Solfege m-grams",
       y = "Frequency Count") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) -> tri_gg

tri_gg

ggsave(filename = "document/img/trigrams.png",plot = tri_gg)


#--------------------------------------------------
# COUNTING HURON GRAMS FOR LATER

grams %>%
  filter(grams == "2-grams") %>%
  arrange(-FreqCount) %>%
  head(n = 10) -> top_2_grams

grams %>%
  filter(grams == "3-grams") %>%
  arrange(-FreqCount) %>%
  head(n = 10) -> top_3_grams

grams %>%
  filter(grams == "5-grams") %>%
  arrange(-FreqCount) %>%
  head(n = 10)-> top_5_grams

#--------------------------------------------------

grams %>%
  filter(grams == "2-grams") %>%
  arrange(FreqCount) %>%
  head(n = 10) -> bottom_2_grams


grams %>%
  filter(grams == "3-grams") %>%
  arrange(FreqCount) %>%
  head(n = 10) -> bottom_3_grams


grams %>%
  filter(grams == "5-grams") %>%
  arrange(FreqCount) %>%
  head(n = 10) -> bottom_5_grams

rbind(top_2_grams, top_3_grams, top_5_grams, bottom_2_grams, bottom_3_grams, bottom_5_grams) %>%
  select(grams, gram, FreqCount) %>%
  kableExtra::kable(col.names = c( "Type", "m-gram","Frequency Count")) -> gram_table

gram_table

write_rds(gram_table, path = "document/figures/gramtable.rds")

rbind(top_3_grams,bottom_3_grams) %>%
  select(grams, gram, FreqCount) %>%
  kableExtra::kable(col.names = c( "Type", "m-gram","Frequency Count")) -> gram3_table

gram3_table

write_rds(gram3_table, path = "document/figures/gramtable3.rds")


#--------------------------------------------------

berkowitz666 %>%
  filter(melody.name == "Berkowitz2") %>%
  select(melody.id, melody.name,cpitch, information.content, probability, entropy) %>%
  head(n = 20)

# Get melodies from Experiment 

dictation_survey <- read_csv("aural_survey/Dictation_Survey_Responses.csv")
library(stringr)

dictation_survey %>%
  select(stimulus) %>%
  unique() -> experimental_melodies

experimental_melodies$stimulus <- str_remove_all(string = experimental_melodies$stimulus, pattern = "t$")

experimental_melodies <- experimental_melodies %>%
  rename(melody.name = stimulus)


experimental_melodies %>%
  left_join(berkowitz666) -> new666

new666 %>%
  group_by(melody.name) %>%
  mutate(cumIC = cumsum(information.content), mean_idyom_ic = mean(information.content)) %>%
  select(melody.name, note.id, information.content, cumIC) %>%
  mutate(mean_idyom_ic = mean(information.content)) -> experiment_idyom_data

experiment_idyom_data %>%
  rename(stimulus = melody.name) -> experiment_idyom_data

experiment_idyom_data %>%
  mutate(last_note_index = last(note.id)) %>%
  filter(note.id == last_note_index) %>%
  select(stimulus, cumIC, mean_idyom_ic) -> idyom_ic 


#--------------------------------------------------
# Get Old Data  Data Import
fantastic_computations <- read_csv("corpus/symbolic/Melosol_Features.csv")
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

target$stimulus <- str_remove_all(string = target$stimulus, pattern = "t$")


target %>% 
  left_join(fantastic_computations) -> melody_data 

View(melody_data)
#--------------------------------------------------
# Model Comparision Chart 
# Humdrum Data

humdrum_infot_data <- read.csv("corpus/symbolic/krn/berkowitz_meta/incipits/experimental_krns/humdruminfot/for_r.csv",header = FALSE)
humdrum_infot_data <- as_tibble(humdrum_infot_data)

humdrum_infot_data %>%
  rename(stimulus = V1, individual_ic = V2) -> humdrum_infot_data

fix_humdrum_text <- function(x){
  x <- str_replace_all(string = x, pattern = "berk", replacement = "Berkowitz")
  str_replace_all(string = x, pattern = "shan\\.txt", replacement = "")
}

humdrum_infot_data$stimulus <- fix_humdrum_text(humdrum_infot_data$stimulus)

humdrum_infot_data %>%
  group_by(stimulus) %>%
  mutate(infot_cumIC = cumsum(individual_ic),
         infot_mean = mean(individual_ic)) %>%
  mutate(note_id = row_number()) %>%
  mutate(final_note = last(note_id)) %>%
  filter(note_id == final_note) %>%
  select(stimulus, infot_cumIC, infot_mean) -> humdrum_ic



melody_data %>%
  left_join(idyom_ic) %>%
  left_join(humdrum_ic) -> model_data 
  
model_data$melody_rank <- dense_rank(as.numeric(gsub(pattern = "Berkowitz",replacement = "",x = model_data$stimulus)))

model_data %>%
  ungroup(stimulus) %>%
  select(mean_diff, cumIC, mean.entropy, infot_cumIC, i.entropy, infot_mean, mean_idyom_ic) %>% 
  cor(use = "pairwise.complete.obs")

model_data %>%
  select(stimulus, melody_rank, mean_diff, 
         cumIC, mean.entropy, infot_cumIC,
         infot_mean,mean_idyom_ic,i.entropy) %>% 
  ungroup(stimulus) %>%
  mutate(zDiff = scale(mean_diff), zIDYOM = scale(cumIC), zFantastic = scale(i.entropy), 
         zHumdrum = scale(infot_cumIC), zMeanHumdrum = scale(infot_mean),
         zMeanIdyom = scale(mean_idyom_ic)) %>%
  ggplot(aes(x = melody_rank, y = mean_diff)) + 
  geom_point(aes(y = zIDYOM, col = "Cumulative IDyOM, r = .83")) + 
  geom_point(aes(y = zIDYOM, col = "Mean IDyOM, r = .79")) + 
  geom_point(aes(y = zFantastic, col = "Mean FANTASTIC Interval Entropy, r = .85")) + 
  geom_point(aes(y = zHumdrum, col = "Cumulative Humdrum infot, r = .70")) +
  geom_point(aes(y = zMeanHumdrum, col = "Mean Humdrum infot, r =.89")) +
  geom_point(aes(y = zDiff, col = "Ratings -- Expert Ground Truth")) +
  scale_x_continuous(breaks = seq(1,20,1), limits = c(1, 20)) +
  labs(title = "Model Comparison:\nInformation Content Measures",
       x = "Melody Rank",
       y = "Standardized Information Content Measure",
       color = "Model") +
  scale_color_viridis(discrete = T) +
  theme_minimal() -> model_comparison_gg

model_comparison_gg

# ggsave(filename = "document/img/model_comparsion_gg.png", model_comparison_gg)  

#--------------------------------------------------
# Get Solfa Data 

new666 %>%
  filter(melody.name == "Berkowitz3") %>%
  ggplot(aes(x = note.id)) +
  geom_point(aes(y = mpitch, col = "MPITCH MEREDITH PITCH")) +
  geom_point(aes(y = cpitch, col = "CPITCH DIATONIC SET")) +
  geom_point(aes(y = cpitch.order.ltm.cpintfref, col = "CHROMATIC INTERVAL FROM TONIC")) +
  labs(title = "Helping David Understand")

#--------------------------------------------------
# Find Sol Fa Mi aka 7, 5, 4


