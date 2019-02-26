#--------------------------------------------------
# Learn gganimate 
#--------------------------------------------------
# NOT MY CODE -- TAKEN FROM GGANIMATE SLIDES
#--------------------------------------------------

library(gganimate)
library(gifski)
library(tidyverse)

# We'll start with a static plot
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(p)

anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim + ggtitle('Now showing {closest_state}',
              subtitle = 'Frame {frame} of {nframes}')


View(airquality)

airq <- airquality
airq$Month <- format(ISOdate(2004,1:12,1),"%B")[airq$Month]



airq %>%
  filter(Month == "August" | Month == "May") %>%
  ggplot(aes(Day, Temp, group = Month)) + 
  geom_line() + 
  geom_segment(aes(xend = 31, yend = Temp), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 31.1, label = Month), hjust = 0) + 
  transition_reveal(Day) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
