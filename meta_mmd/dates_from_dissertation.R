#--------------------------------------------------
# Up to date?
#--------------------------------------------------
library(tidyverse)

years <- read.delim("meta_mmd/years.txt",header = FALSE)

years %>%
  ggplot(aes(x = V1)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(breaks = c(seq(1850,2020,10)), limits = c(1850,2019)) + 
  labs(x = "Year", y = "Density", title = "Citations by Year" , subtitle = "Modeling Melodic Dictation") +
  theme_minimal()
