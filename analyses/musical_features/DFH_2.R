#--------------------------------------------------
# DFH 2
#--------------------------------------------------
# In this script 
# PLOT -- Informaiton Contente of Five Grams from Three Sections
# STATIC Plots of IC Model with Schubert
# STATIC PLots of Five Grams
# GIFs of Five Grams 
#--------------------------------------------------
library(ggpubr)
library(tidyverse)
library(gganimate)
library(viridis)
#--------------------------------------------------
# Read in Data
berkowitz666 <- read.delim("corpus/symbolic/idyom/berk666.dat")
midi_convert <- read_csv("software/midi_convert.csv")
deg_table <- read_csv("software/deg_table.csv")
#--------------------------------------------------

berkowitz666 <- berkowitz666 %>%
  left_join(midi_convert)

# Trained on Dataset of 622 krn files 

berkowitz666$berkNumber <- as.numeric(gsub(pattern = "Berkowitz",
                                             replacement = "",
                                             x = berkowitz666$melody.name))


number_tiles <- 3

berkowitz666 %>%
  select(melody.id, note.id, melody.name, berkNumber, information.content, entropy) %>%
  filter(note.id <= 5) %>%
  mutate(quintile = ntile(berkNumber, number_tiles)) %>%
  group_by(melody.name) %>%
  mutate(mean_melody = mean(information.content)) %>%
  select(melody.name, quintile, mean_melody) %>%
  group_by(quintile) %>%
  mutate(mean_quint = mean(mean_melody)) %>%
  select(quintile, mean_quint) %>%
  unique() -> mean_quint_ratings

mean_quint_ratings

berkowitz666 %>%
  select(melody.id, note.id, melody.name, berkNumber, information.content, entropy) %>%
  filter(note.id <= 5) %>%
  mutate(quintile = ntile(berkNumber, number_tiles)) %>%
  mutate(quintile = quintile) %>%
  left_join(mean_quint_ratings) %>%
  group_by(melody.id) %>%
  mutate(mean_melody_ic = mean(information.content)) %>%
  mutate(quintile = as.factor(quintile)) %>%
  ggplot(aes(mean_melody_ic, color = quintile, fill = quintile)) +
  geom_density(alpha = .75) +
  scale_x_continuous(breaks = seq(2,6.5,.5), limits = c(2,6.5)) +
  geom_vline(aes(xintercept=mean_quint, color=quintile), linetype="dashed") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Average Information Content of First Five Notes of Melodies",
      y = "Density",
      x = "Information Content Distribution",
      color = "3-tile") -> tri_distribution

tri_distribution

ggsave(filename = "document/img/tri_distribution.png", tri_distribution)

berkowitz666 %>%
  select(melody.id, note.id, melody.name, berkNumber, information.content, entropy) %>%
  filter(note.id <= 5) %>%
  mutate(quintile = ntile(berkNumber, 5)) %>%
  group_by(quintile) %>%
  mutate(meanIC = mean(information.content)) %>%
  select(quintile, meanIC) -> model_data_quintile 

model_quintile <- aov(meanIC ~ as.factor(quintile), data = model_data_quintile)
summary(model_quintile)
TukeyHSD(model_quintile)
#======================================================================================================
# Make Scale Degree Calcualtions
# Use Key Sig and Mode 
# Cpitch - referent mod 12 

create_referent <- function(keysig, modal){
  x <- ifelse(test = keysig >= 0, 
              yes = ((keysig * 7) + modal ) %% 12, 
              no = ((keysig * -5) + modal) %% 12  )
  x
  }

berkowitz666$referent <- create_referent(keysig = berkowitz666$keysig, modal = berkowitz666$mode)

berkowitz666 %>%
  select(melody.name, keysig, mode, referent) %>%
  filter(melody.name == "Berkowitz1" | melody.name == "Berkowitz4" | melody.name == "Berkowitz39" | 
           melody.name == "Berkowitz43" | melody.name == "Berkowitz92") 

# Cpint - Referent 
berkowitz666$scale_degree <- (berkowitz666$cpitch - berkowitz666$referent) %% 12

berkowitz666 %>%
  left_join(deg_table) -> berkowitz666

#--------------------------------------------------
# Find most common patterns!!! 
library(ngram)


berkowitz666 %>%
  select(melody.name, note.id, scale_degree, information.content) %>%
  filter(scale_degree == 0) %>%
  group_by(melody.name) %>%
  mutate(sd_do_mean = mean(information.content)) %>%
  ungroup(melody.name) %>%
  mutate(newmean = mean(sd_do_mean))

berkowitz666 %>%
  select(melody.name, note.id, scale_degree, information.content) %>%
  filter(scale_degree == 7) %>%
  group_by(melody.name) %>%
  mutate(sd_sol_mean = mean(information.content)) %>%
  ungroup(melody.name) %>%
  mutate(newmean = mean(sd_sol_mean))

berkowitz666 %>%
  select(melody.name, note.id, scale_degree, information.content) %>%
  filter(scale_degree == 9) %>%
  group_by(melody.name) %>%
  mutate(sd_la_mean = mean(information.content)) %>%
  ungroup(melody.name) %>%
  mutate(newmean = mean(sd_la_mean))

# First find context -n 3 of all the melodies, do grouping ideally

# Count frequency of all 3-grams

# Get cum IC of all 3 grams 

# Show Higher frequency n-grams are lower IC than lower frequency n-grams 


#======================================================================================================
#--------------------------------------------------
# Make Static Plots
#--------------------------------------------------

make_static_ngram <- function(dataset,melody_name, note_index, wmc_limit){
  
  # Cut Up Dataset 
  cut_data <- dataset %>%
    filter(melody.name == melody_name) %>%
    filter(note.id <= note_index) %>%
    select(cpitch, note.id, information.content, ansi) %>%
    mutate(cumIC = cumsum(information.content))

  updated_name <- str_replace(string = melody_name, pattern = "z",replacement = "z ")
  
  ggplot(cut_data, aes(x=note.id, y=cumIC, label = ansi)) + 
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(1,30)) +
    geom_segment(x = 0, xend = 9, y = wmc_limit , yend = wmc_limit, linetype = 2) +
    geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) +
    labs(title = updated_name, y = "", x = "") +
    theme_minimal() +
    geom_label() -> p
  
  p
    
}


  
wmc_threshold <- 15
  
# Make Plots
low_1 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz1", note_index = 5, wmc_limit = wmc_threshold)
low_2 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz41", note_index = 5, wmc_limit = wmc_threshold)
low_3 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz6", note_index = 5, wmc_limit = wmc_threshold)

mid_1 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz366", note_index = 5, wmc_limit = wmc_threshold)
mid_2 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz364", note_index = 5, wmc_limit = wmc_threshold)
mid_3 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz360", note_index = 5, wmc_limit = wmc_threshold)

high_1 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz412", note_index = 5, wmc_limit = wmc_threshold)
high_2 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz591", note_index = 5, wmc_limit = wmc_threshold)
high_3 <- make_static_ngram(dataset = berkowitz666, melody_name = "Berkowitz380", note_index = 5, wmc_limit = wmc_threshold)


core_image <- ggarrange(low_1, low_2, low_3, mid_1, mid_2, mid_3, high_1, high_2, high_3, ncol = 3, nrow = 3)

annotate_figure(core_image, top = "Cumulative Information Content Over Melodies",
                left = "Cumulative Information Content as Calculated by IDyOM",
                bottom = "Position of Notes") -> cum_grid_plot

cum_grid_plot

ggsave(filename = "document/img/cum_grid_plot.png",plot = cum_grid_plot)

#======================================================================================================
# MP Calculations
#--------------------------------------------------
# 17 WMC


make_active_ngram <- function(dataset, melody_name, note_index, wmc_limit){
  
  # Cut Up Dataset 
  cut_data <- dataset %>%
    filter(melody.name == melody_name) %>%
    filter(note.id <= note_index) %>%
    select(cpitch, note.id, information.content, ansi) %>%
    mutate(cumIC = cumsum(information.content))
  
  cut_data
  
  # Fix Name
   updated_name <- str_replace(string = melody_name, pattern = "z",replacement = "z ")
    
  # Make Animation
   cut_data %>%
     ggplot(aes(note.id, cumIC, label = ansi)) +
     geom_line() + 
     geom_segment(aes(xend = wmc_limit, yend = cumIC), linetype = 2, colour = 'grey') +
     scale_x_continuous(limits = c(1,5)) +
     scale_y_continuous(limits = c(1,30)) +
     geom_point(size = 2) + 
     geom_segment(x = 1, xend = 5, y = wmc_limit , yend = wmc_limit, linetype = 2) +  
     transition_reveal(note.id) + 
     coord_cartesian(clip = 'off') + 
     labs(title = updated_name, y = 'Cumulative Information Content',
          x = "") +
     geom_label() +
     theme_minimal() -> q
   
   q
     
}

low_act_1 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz1", note_index = 5, wmc_limit = wmc_threshold)
low_act_2 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz41", note_index = 5, wmc_limit = wmc_threshold)
low_act_3 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz6", note_index = 5, wmc_limit = wmc_threshold)

mid_act_1 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz366", note_index = 5, wmc_limit = wmc_threshold)
mid_act_2 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz364", note_index = 5, wmc_limit = wmc_threshold)
mid_act_3 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz360", note_index = 5, wmc_limit = wmc_threshold)

high_act_1 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz412", note_index = 5, wmc_limit = wmc_threshold)
high_act_2 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz591", note_index = 5, wmc_limit = wmc_threshold)
high_act_3 <- make_active_ngram(dataset = berkowitz666, melody_name = "Berkowitz380", note_index = 5, wmc_limit = wmc_threshold)


anim_save(filename = "document/img/animations/low_act_1.gif", low_act_1)
anim_save(filename = "document/img/animations/low_act_2.gif", low_act_2)
anim_save(filename = "document/img/animations/low_act_3.gif", low_act_3)

anim_save(filename = "document/img/animations/mid_act_1.gif", mid_act_1)
anim_save(filename = "document/img/animations/mid_act_2.gif", mid_act_2)
anim_save(filename = "document/img/animations/mid_act_3.gif", mid_act_3)

anim_save(filename = "document/img/animations/high_act_1.gif", high_act_1)
anim_save(filename = "document/img/animations/high_act_2.gif", high_act_2)
anim_save(filename = "document/img/animations/high_act_3.gif", high_act_3)

