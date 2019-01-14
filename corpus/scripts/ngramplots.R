#======================================================================================================
# Make N-Gram Distribution Grid
# Need data exported via make-mint.sh script
# Eventually will move this to solfa, not mint 
#--------------------------------------------------
library(tidyverse)

twogram <- read_tsv("dataviz/twograms.tsv",col_names = c("Frequency","n-gram"))
threegram <- read_tsv("dataviz/threegrams.tsv",col_names = c("Frequency","n-gram"))
fourgram <- read_tsv("dataviz/fourgrams.tsv",col_names = c("Frequency","n-gram"))
fivegram <- read_tsv("dataviz/fourgrams.tsv",col_names = c("Frequency","n-gram"))

twogram
threegram
fourgram
fivegram

ggplot(twogram, aes(x = reorder(`n-gram`,-Frequency), y = Frequency)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "2-gram Distribution in Corpus", subtitle = "First 1,000 2-grams", x = "2-gram", y = "Frequency") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
#.....

ggplot(fivegram, aes(x = reorder(`n-gram`,-Frequency), y = Frequency)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "5-gram Distribution in Corpus", subtitle = "First 1,000 5-grams", x = "5-gram", y = "Frequency") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

ggplot(fivegram[fivegram$Frequency > 100,], aes(x = reorder(`n-gram`,-Frequency), y = Frequency)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "5-gram Distribution in Corpus", subtitle = "First 1,000 5-grams", x = "5-grams", y = "Frequency") 

qqnorm(fivegram$Frequency)
