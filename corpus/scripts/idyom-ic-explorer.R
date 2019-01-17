#======================================================================================================
# IDyOM Scratch
#--------------------------------------------------
library(tidyverse)

tenBerk <- read.delim("symbolic/idyom/tenBerk.dat")
hundoBerk <- read.delim("symbolic/idyom/108Berk.dat")
fivehundoBerk <- read.delim("symbolic/idyom/almost500.dat")

table(fivehundoBerk$melody.id)
View(tenBerk)

#======================================================================================================
# Get Melody 9
# Plot Cumulitive Information Content of Melody from ten corpus
# Plot Cumulitive Information Content of Melody from hundo corpus

names(tenBerk)

#--------------------------------------------------
# Get IC for first five notes

fivehundoBerk %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(note.id <= 5) %>%
  group_by(melody.name) %>%
  mutate(cumIC = cumsum(information.content)) %>%
  ungroup() %>%
  filter(note.id == 5) %>%
  arrange(cumIC) %>%
  mutate(zCumIC = scale(cumIC)) -> fived

fivehundoBerk %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(note.id <= 3) %>%
  group_by(melody.name) %>%
  mutate(cumIC = cumsum(information.content)) %>%
  ungroup() %>%
  filter(note.id == 3) %>%
  arrange(cumIC) 

ggplot(fived, aes(x = zCumIC)) + geom_histogram()

  
hundoBerk %>%
  select(melody.name, note.id, information.content) %>%
  arrange(information.content) %>%
  head()

hundoBerk %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(melody.name == "Berkowitz94") %>% 
  mutate(cumIC = cumsum(information.content)) %>%
  ggplot(aes(x = note.id, y = cumIC)) + geom_line() +
  geom_point() + labs(title = "Hundo Berk Cumulitive IC for Melody")
