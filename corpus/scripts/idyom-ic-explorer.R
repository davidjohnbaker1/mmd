#======================================================================================================
# IDyOM Scratch
#--------------------------------------------------
library(tidyverse)

tenBerk <- read.delim("symbolic/idyom/tenBerk.dat")
hundoBerk <- read.delim("symbolic/idyom/108Berk.dat")

View(tenBerk)

#======================================================================================================
# Get Melody 9
# Plot Cumulitive Information Content of Melody from ten corpus
# Plot Cumulitive Information Content of Melody from hundo corpus

names(tenBerk)

tenBerk %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(melody.name == "Berkowitz7") %>% 
  mutate(cumIC = cumsum(information.content)) %>%
  ggplot(aes(x = note.id, y = cumIC)) + geom_line() +
  scale_y_continuous(limits = c(0,30)) +
  geom_point() + labs(title = "Ten Berk Cumulitive IC for Melody")

hundoBerk %>%
  select(melody.name, note.id, information.content, entropy, dataset.id) %>%
  filter(melody.name == "Berkowitz94") %>% 
  mutate(cumIC = cumsum(information.content)) %>%
  ggplot(aes(x = note.id, y = cumIC)) + geom_line() +
  geom_point() + labs(title = "Hundo Berk Cumulitive IC for Melody")
