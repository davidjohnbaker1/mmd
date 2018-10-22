#======================================================================================================
# Make Cumultive Plot
#--------------------------------------------------
library(ggplot2)
library(tidyverse)

schubert <- read_csv("figures/schubertF.csv")
schubert
schubert$CumIC <- cumsum(schubert$IC)

ggplot(schubert, aes(x=Position, y=CumIC, label = Pitch)) + geom_line() +
  geom_point() +
  geom_segment(x = 0, xend = 9, y = 17 , yend = 17, linetype = 2) +
  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) +
  labs(title = "Cumulative Information Content of Melody", x = "Position of Notes",
       y = "Information Content") + geom_label() -> p

p + annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention")
