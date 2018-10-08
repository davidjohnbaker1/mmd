#======================================================================================================
# Playing with Computational Model
#--------------------------------------------------
library(tidyverse)
library(tokenizers)
library(data.table)
# Import Prior Knowledge
# * CSV with all melodies that are known

prior_knowledge <- read_csv("computationalmodel/playdata/prior_knowledge.csv")
target_melody <- read_csv("computationalmodel/playdata/twinkle.csv")

#--------------------------------------------------
prior_knowledge$Token

# Looks at all n-grams possible, tables them

prior_string <- stringr::str_flatten(prior_knowledge$Token,collapse = " ")

# Ideally here remove duplicates

# max_string <- count_words(prior_string)
max_string <- 4 # PARAMETER HERE

interval_strings$ngrams <- tokenize_ngrams(prior_string,n = max_string, n_min = 2) # need to change this so it's each melody and there is not a boundary issue

bigger <- data.frame(interval_strings$ngrams)

bigger <- data.table(bigger)
names(bigger)
setnames(bigger, old = "c..do4.do4....do4.do4.sol4....do4.do4.sol4.sol4....do4.sol4...","ngrams")
bigger$ngrams <- as.character(bigger$ngrams)

# Each n-gram should be own cell, count it up


# N-grams that appear above X parameter are "Explicitly Known"
# N-grams that apper below X pameter are "Not Known"

prior_knowledge$Token

tokenize_ngrams(song, n = 4, n_min = 2)

# Create cumsum

target_melody$cumIC <- cumsum(target_melody$noteIC)
target_melody

ggplot(target_melody, aes(x=c(1:7), y=cumIC)) + geom_line() +
  geom_point() + geom_hline(yintercept = 1) +
  labs(title = "Working Memory Bin", x = "Notes but make them real time",
       y = "Cumultive Informatoin Content from IDyOM")

# Set Thresholds

wmc_threshold <- 1
interval_explicit <- "WHAT"

chunk1 <- target_melody[target_melody$cumIC <= wmc_threshold,]

chunk1

# Extractive Listening

extractive_listening <- function(x){
  lastNote <- x[x$cumIC <= wmc_threshold,]
  lastNote
}

extractive_listening(target_melody)

filter(target_melody,)
