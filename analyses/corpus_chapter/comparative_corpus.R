#--------------------------------------------------
# Comparison of Corpora
#--------------------------------------------------
library(tidyverse)
library(cowplot)
library(viridis)

#--------------------------------------------------
# Import Feature Data
melosol_features <- read_csv("corpus/symbolic/Melosol_Features.csv")
densmore_features <- read_csv("corpus/symbolic/Densmore_Features.csv")
essen_features <- read_csv("corpus/symbolic/feature_data/essenFeatures.csv")
#--------------------------------------------------
# Import Dist Data
melosol_krum <- read_csv("corpus/symbolic/krn/melosol_deg.csv")
euro_krum <- read_csv("corpus/symbolic/krn/essen/euro_degs.csv")
asia_krum <- read_csv("corpus/symbolic/krn/essen/asia_deg.csv")

melosol_krum$dataset <- "MeloSol"
euro_krum$dataset <- "Euro"
asia_krum$dataset <- "Asia"

krum_data <- rbind(melosol_krum, euro_krum, asia_krum)

melosol_features$Country <- "Western"
densmore_features$Country <- "North America"

#--------------------------------------------------
# Add Dataset Identifier Tags
melosol_features$dataset <- "MeloSol"
densmore_features$dataset <- "Densmore"
essen_features$dataset <- "Essen"

#--------------------------------------------------
# Create Origin
melosol_features$origin <- "Western Classical"
densmore_features$origin <- "Native American"

# Subset Asia From Essen
# ashsham1.krn  india01.krn   java01.krn    nippon01.krn  turkiye1.krn
# han/    natmin/ shanxi/ xinhua/
essen_features$origin <- "NULL"
#--------------------------------------------------
# Drop Countries where not Enough
# arabic (1), mexico (4), brasil (1), canada (4), usa (10)

essen_features %>%
  filter(Country != "arabic") %>%
  filter(Country != "mexico") %>%
  filter(Country != "brasil") %>%
  filter(Country != "canada") %>%
  filter(Country != "usa") -> essen_features

#--------------------------------------------------
# Asia Origin
essen_features[essen_features$Country=="han",]$origin <- "Asian"
essen_features[essen_features$Country=="natmn",]$origin <- "Asian"
essen_features[essen_features$Country=="shanx",]$origin <- "Asian"
essen_features[essen_features$Country=="china",]$origin <- "Asian"
essen_features[essen_features$Country=="han",]$origin <- "Asian"
essen_features[essen_features$Country=="hanb",]$origin <- "Asian"
essen_features[essen_features$Country=="ashsham",]$origin <- "Asian"

#--------------------------------------------------
# Eurpean Origin 
essen_features[essen_features$origin != "Asian",]$origin <- "Europe"

names(densmore_features)
#--------------------------------------------------
# Rename for Merge
essen_features %>%
  rename(melody_id = folksong) -> essen_features

densmore_features %>%
  rename(melody_id = file.id) -> densmore_features

melosol_features %>%
  rename(melody_id = file.id) -> melosol_features

essen_features %>%
  select(-Country.ID, -Region) -> essen_features

rbind(densmore_features, melosol_features, essen_features) -> melody_features

melody_features %>%
  rename(Origin = origin) -> melody_features

#--------------------------------------------------
# Create Descriptives

melody_features %>%
  group_by(dataset, Origin) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = dataset, y = n, fill = Origin)) +
  geom_bar(stat = 'identity') +
  theme_minimal() + 
  labs(title = "Breakdown of Corpora", x = "Corpus", y = "") +
  coord_flip() +
  scale_fill_viridis(discrete = TRUE) -> corpora_size_comparison

corpora_size_comparison

melody_features %>%
  select(Origin, len, dataset) %>%
  ggplot(aes(x = len, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Length Density", x = "Length", y = "") -> corpora_len_comparision

corpora_len_comparision

melody_features %>%
  select(Origin, p.range, dataset) %>%
  ggplot(aes(x = p.range, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Range Density", x = "Range", y = "") -> corpora_range_comparison

corpora_range_comparison

melody_features %>%
  select(Origin, note.dens, dataset) %>%
  ggplot(aes(x = note.dens, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Note Density", x = "Density") -> corpora_ndensity_comparison

corpora_ndensity_comparison

plot_grid(corpora_range_comparison, 
          corpora_len_comparision, 
          corpora_ndensity_comparison, 
          corpora_size_comparison) -> comparative_descriptive_panel

comparative_descriptive_panel

# ggsave(filename = "document/img/comparative_descritive_panel.png", plot = comparative_descriptive_panel)

# Make Huron Panel Grid 

melody_features %>%
  ggplot(aes(x = i.entropy, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Interval Entropy Density", x = "Entropy", y = "") -> corpora_ientropy_comparision

corpora_ientropy_comparision

melody_features %>%
  ggplot(aes(x = tonalness, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonalness Density", x = "Tonalness", y = "") -> corpora_tonalness_comparision

melody_features %>%
  ggplot(aes(x = tonal.clarity, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonal Clarity Density", x = "Tonal Clarity", y = "") -> corpora_tonalclarity_comparision

melody_features %>%
  ggplot(aes(x = tonal.spike, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Tonal Spike Density", x = "Tonalspike", y = "") -> corpora_tonalspike_comparision

corpora_tonalclarity_comparision
corpora_tonalness_comparision
corpora_tonalspike_comparision

melody_features %>%
  ggplot(aes(x = d.entropy, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Durational Entropy Density", x = "Length", y = "") -> corpora_dentropy_comparision

corpora_dentropy_comparision

melody_features %>%
  ggplot(aes(x = step.cont.glob.var, color = Origin)) +
  scale_color_viridis(discrete = TRUE) +
  geom_density() + 
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title =  "Countour Variation (SCGV)", x = "", y = "") -> corpora_scgv_comparision

corpora_scgv_comparision

plot_grid(ncol =  2, corpora_ientropy_comparision, corpora_dentropy_comparision,
          corpora_tonalclarity_comparision,
          corpora_tonalness_comparision,
          corpora_tonalspike_comparision,
          corpora_scgv_comparision) -> corpora_emergent 

# ggsave(filename = "document/img/corpora_emergent.png", corpora_emergent)

#--------------------------------------------------
# Redo Huron 

melody_features %>%
  select(h.contour, Origin) %>%
  group_by(Origin, h.contour) %>%
  summarise(n = n()) %>%
  group_by(Origin) %>% 
  mutate(perc=100*n/sum(n)) %>%
  ggplot(aes(x = h.contour, y = perc, fill = Origin)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Origin) +
  theme_minimal() +
  labs(title = "Huron Contour Class", 
       x = "Contour Class",
       y = "") +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() -> corpora_contour_distribution 

corpora_contour_distribution

# ggsave(filename = "document/img/huron_recreation.png", corpora_contour_distribution)
#--------------------------------------------------
# Tone Plots 
cat(unique(krum_data$scaledegree),sep = ",")
krum_data

krum_data %>%
  filter(deg != "3 6") %>%
  ggplot(aes(x = deg, y = percent, fill = dataset)) +
  geom_bar(stat = 'identity', position = 'dodge', aes(fill = dataset)) + 
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Chromatic Aggregate", x = "Scale Degree", y = "Percent") -> krum_plot_all

krum_plot_all

krum_data %>%
  filter(deg != "3 6") %>%
  filter(deg != "1-") %>%
  filter(deg != "2-") %>%
  filter(deg != "3-") %>%
  filter(deg != "4-") %>%
  filter(deg != "5-") %>%
  filter(deg != "6-") %>%
  filter(deg != "7-") %>%
  filter(deg != "1+") %>%
  filter(deg != "2+") %>%
  filter(deg != "3+") %>%
  filter(deg != "4+") %>%
  filter(deg != "5+") %>%
  filter(deg != "6+") %>%
  filter(deg != "7+") %>%
  ggplot(aes(x = deg, y = percent, fill = dataset)) +
  geom_bar(stat = 'identity', position = 'dodge', aes(fill = dataset)) + 
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Diatonic Set", x = "Scale Degree", y = "Percent") -> krum_plot_sub

krum_plot_sub

#ggsave("document/img/krum-plot-all.png", krum_plot_all)

plot_grid(krum_plot_sub, krum_plot_all, nrow = 2) -> krum_panel

#ggsave("document/img/krum_panel.png", krum_panel)

#--------------------------------------------------
# Plots this Scritp Makes

comparative_descriptive_panel
corpora_emergent
corpora_contour_distribution
krum_panel
