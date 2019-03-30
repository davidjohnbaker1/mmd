#--------------------------------------------------
# Corpus Analysis
#--------------------------------------------------
library(tidyverse)

meta <- read_csv("corpus/symbolic/krn/melosol/berk_melo_meta.csv") 

meta %>%
  ggplot(aes(x = Key)) +
  geom_bar(aes(fill = `!!! Encoder`))
