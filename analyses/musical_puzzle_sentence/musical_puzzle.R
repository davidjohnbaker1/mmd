#--------------------------------------------------
# Musical Puzzle Sentence
# A whole script for one sentence
#--------------------------------------------------
library(tidyverse)
library(viridis)
#--------------------------------------------------
# Update Features 
setwd("corpus/scripts/fantasticSoftware/")
source("Fantastic.R")
setwd("../../../document/img/musicalpuzzle/MP2/")

list.files()

puzzleFeatures <- compute.features(melody.filenames = list.files(pattern=".csv"), 
                                      dir = ".",
                                      use.segmentation = FALSE, 
                                      write.out = TRUE)

View(puzzleFeatures)

#--------------------------------------------------
# Puzzle Feature Chart

gather(puzzleFeatures, "cat", "dog", -file.id) -> puzzleFeaturesLong

puzzleFeaturesLong %>%
  ggplot(aes(x = cat, y = dog, group = file.id)) +
  geom_bar(stat = 'identity', position = 'dodge')
  