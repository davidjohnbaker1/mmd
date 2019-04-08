#======================================================================================================
# Make Cumultive Plot
#--------------------------------------------------
library(ggplot2)
library(tidyverse)
library(gganimate)


schubert <- read_csv("analyses/comp_model//schubertF.csv")
schubert
schubert$CumIC <- cumsum(schubert$IC)

schubert$CumIC_expected <- schubert$CumIC * .70
schubert$CumIC_unexpected <- schubert$CumIC * 1.60


ggplot(schubert, aes(x=Position, y=CumIC, label = Pitch)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(limits = c(1,8), breaks = seq(1,8,1)) +
  geom_segment(x = 0, xend = 9, y = 17 , yend = 17, linetype = 2) +
  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) +
  labs(title = "Cumulative Information Content of Melody", x = "Position of Notes",
       y = "Information Content") + 
  geom_label() -> p

p + annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") -> schubert_new

ggsave(filename = "document/img/SchubertPlotNew.png", schubert_new)
#======================================================================================================
# Create Panel for LTM/Prior Knowledge 

bi_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/bi_grams.tsv",header = FALSE, sep = "\t")
tri_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/tri_grams.tsv",header = FALSE, sep = "\t")
quint_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/quint_grams.tsv",header = FALSE, sep = "\t")

bi_grams$grams <- "2-grams"
tri_grams$grams <- "3-grams"
quint_grams$grams <- "5-grams"

grams <- as.tibble(rbind(bi_grams, tri_grams, quint_grams))



# NEEDS TO BE JUST START OF MELODIES ! 

uni_grams <-read.delim("corpus/symbolic/krn/berkowitz_meta/uni_grams.tsv",header = FALSE, sep = "\t")
bi_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/bi_grams.tsv",header = FALSE, sep = "\t")
tri_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/tri_grams.tsv",header = FALSE, sep = "\t")
quint_grams <- read.delim("corpus/symbolic/krn/berkowitz_meta/quint_grams.tsv",header = FALSE, sep = "\t")

uni_grams$grams <- "1-grams"
bi_grams$grams <- "2-grams"
tri_grams$grams <- "3-grams"
quint_grams$grams <- "5-grams"

grams <- as.tibble(rbind(uni_grams, bi_grams, tri_grams, quint_grams))

grams %>%
  rename(FreqCount = V1, gram = V2) -> grams

# Bi with lower threshold
# Tri with Less 
# Quint with even less 

# 1 Grams

threshold <- 7.5
implicit <- threshold + 1
explicit <- threshold - 1
depth <- 5000

# make me!
grams %>%
  filter(grams == "1-grams") %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  theme_minimal() +
  geom_histogram(stat = 'identity', position = position_dodge(width=0.5)) +
  geom_vline(xintercept = threshold) + 
  coord_flip() + 
  annotate(geom = "text", x = implicit, y = depth, label = "Implicit") +
  annotate(geom = "text", x = explicit, y = depth, label = "Explicit") +
  labs(x = "Musical Pattern", y = "Frequency Count", title = "1-grams") -> pk_1

pk_1

# 2 Grams

threshold <- 29.5
implicit <- threshold + 1
explicit <- threshold - 1
depth <- 950

grams %>%
  filter(grams == "2-grams") %>%
  filter(FreqCount > 200) %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  theme_minimal() +
  geom_histogram(stat = 'identity', position = position_dodge(width=0.5)) +
  geom_vline(xintercept = threshold) + 
  coord_flip() + 
  annotate(geom = "text", x = implicit, y = depth, label = "Implicit") +
  annotate(geom = "text", x = explicit, y = depth, label = "Explicit") +
  labs(x = "Musical Pattern", y = "Frequency Count", title = "2-grams") -> pk_2

# 3 Grams
  
threshold <- 25
implicit <- threshold + 1
explicit <- threshold - 1
depth <- 450

grams %>%
  filter(grams == "3-grams") %>%
  filter(FreqCount > 135) %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  theme_minimal() +
  geom_histogram(stat = 'identity', position = position_dodge(width=0.5)) +
  geom_vline(xintercept = threshold) + 
  coord_flip() + 
  annotate(geom = "text", x = implicit, y = depth, label = "Implicit") +
  annotate(geom = "text", x = explicit, y = depth, label = "Explicit") +
  labs(x = "Musical Pattern", y = "Frequency Count", title = "3-grams") -> pk_3


# 5 Grams 

threshold <- 5.5
implicit <- threshold + 1
explicit <- threshold - 1
depth <- 85

grams %>%
  filter(grams == "5-grams") %>%
  filter(FreqCount > 35) %>%
  ggplot(aes(x = reorder(gram, -FreqCount), y = FreqCount)) +
  theme_minimal() +
  geom_histogram(stat = 'identity', position = position_dodge(width=0.5)) +
  geom_vline(xintercept = threshold) + 
  coord_flip() + 
  annotate(geom = "text", x = implicit, y = depth, label = "Implicit") +
  annotate(geom = "text", x = explicit, y = depth, label = "Explicit") +
  labs(x = "Musical Pattern", y = "Frequency Count", title = "5-grams") -> pk_5

plot_grid(pk_1, pk_2, pk_3, pk_5) -> gram_panel

ggsave(filename = "document/img/pk_grampanel.png")

#======================================================================================================
# Make Many plots! 
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
  geom_segment(x = 3, xend = 3, y = 0, yend = 11) + # Bold Line for Boundary
  transition_reveal(Position) + 
  coord_cartesian(clip = 'off') + 
  labs(title = '"Low" Working Memory Capacity', y = 'Cumulative Information Content') + 
  annotate(geom = "text", x = 1.5, y = 12, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") +
  geom_label() +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 1, 5.5, 5.5))

#======================================================================================================
# Diff Calculations
#--------------------------------------------------
# 17 WMC - Expected 

anim_17_ex <- 
  schubert %>%
  ggplot(aes(Position, CumIC_expected, label = Pitch)) + 
  geom_line() + 
  geom_segment(aes(xend = 10, yend = CumIC), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_segment(x = 1, xend = 10, y = 17 , yend = 17, linetype = 2) + # Dashed Line based on position
#  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) + # Bold Line for Boundary
  transition_reveal(Position) + 
  coord_cartesian(clip = 'off') + 
  labs(title = '"High" Working Memory Capacity',
       y = 'Cumulative Information Content',
       subtitle = "Listener Better at Expecting Notes") + 
  annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") +
  geom_label() +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 1, 5.5, 5.5))

anim_17_ue <- 
  schubert %>%
  ggplot(aes(Position, CumIC_unexpected, label = Pitch)) + 
  geom_line() + 
  geom_segment(aes(xend = 10, yend = CumIC), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_segment(x = 1, xend = 10, y = 17 , yend = 17, linetype = 2) + # Dashed Line based on position
#  geom_segment(x = 5.5, xend = 5.5, y = 0, yend = 17) + # Bold Line for Boundary
  transition_reveal(Position) + 
  coord_cartesian(clip = 'off') + 
  labs(title = '"High" Working Memory Capacity', 
       y = 'Cumulative Information Content',
       subtitle = "Listener Worse At Expecting Notes") + 
  annotate(geom = "text", x = 1.5, y = 18.6, label = "WMC Limit") +
  annotate(geom = "text", x = 7, y = 2.5, label = "Segmentation Boundary \n for Selective Attention") +
  geom_label() +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 1, 5.5, 5.5))



# anim_save("figures/animations/Schubert-Draft.gif", anim)

anim_save("figures/animations/Schubert-Draft-17.gif", anim_17)
anim_save("figures/animations/Schubert-Draft-11.gif", anim_11)
anim_save("figures/animations/Schubert-Draft-17_ex.gif", anim_17_ex)
anim_save("figures/animations/Schubert-Draft-17_ue.gif", anim_17_ue)

#--------------------------------------------------
modz <- lm(Sepal.Width ~ Sepal.Length, iris)

modz$residuals
modz$effects
