#======================================================================================================
# Make Cumultive Plot
#--------------------------------------------------
library(ggplot2)
library(tidyverse)
library(gganimate)


schubert <- read_csv("figures/schubertF.csv")
schubert
schubert$CumIC <- cumsum(schubert$IC)

ggplot(schubert, aes(x=Position, y=CumIC, label = Pitch)) + 
  geom_line() +
  geom_point() +
  geom_segment(x = 0, xend = 9, y = 17 , yend = 17, linetype = 2) +
  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) +
  labs(title = "Cumulative Information Content of Melody", x = "Position of Notes",
       y = "Information Content") + 
  geom_label() -> p

p + annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention")

#--------------------------------------------------
# 17 WMC

anim_17 <- 
  schubert %>%
  ggplot(aes(Position, CumIC, label = Pitch)) + 
  geom_line() + 
  geom_segment(aes(xend = 10, yend = CumIC), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_segment(x = 1, xend = 10, y = 17 , yend = 17, linetype = 2) + # Dashed Line based on position
  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) + # Bold Line for Boundary
  transition_reveal(Position) + 
  coord_cartesian(clip = 'off') + 
  labs(title = '"High" Working Memory Capacity', y = 'Cumulative Information Content') + 
  annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") +
  geom_label() +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 1, 5.5, 5.5))

# 11 WMC

anim_11 <- 
  schubert %>%
  ggplot(aes(Position, CumIC, label = Pitch)) + 
  geom_line() + 
  geom_segment(aes(xend = 10, yend = CumIC), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_segment(x = 1, xend = 10, y = 11 , yend = 11, linetype = 2) + # Dashed Line based on position
  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 11) + # Bold Line for Boundary
  transition_reveal(Position) + 
  coord_cartesian(clip = 'off') + 
  labs(title = '"Low" Working Memory Capacity', y = 'Cumulative Information Content') + 
  annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") +
  geom_label() +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 1, 5.5, 5.5))


anim_save("figures/animations/Schubert-Draft.gif", anim)

anim_save("figures/animations/Schubert-Draft-17.gif", anim_17)
anim_save("figures/animations/Schubert-Draft-11.gif", anim_11)
