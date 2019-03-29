#--------------------------------------------------
# Get n-gram distributions
#--------------------------------------------------
library(ggplot2)
library(ngram)
library(tidyverse)
library(tm)
#--------------------------------------------------
# Read in mint file 

minty <- readLines("mnt/611.mnt")

minty$startingNote <- minty[1]


str(minty)
cat(minty)

list.files("mnt/")

list.files("symbolic/krn/") %>%
  map_chr(~readLines(.))

tbl <-
  list.files(pattern = "*.mnt") %>% 
  map_df(~read.table(.))

str(minty)
minty$ONE <- minty$X.G.

ggplot(minty, aes(ONE)) + geom_bar()


multiread(path = "symbolic/krn/*",extension = "*.krn")
ngram(minty$ONE,n = 2)

x <- "A B A C A B B"

ng <- ngram (x , n = 2)
summary(ng)
ng


# Think about the goals that this has.

# Eventually want to be able to make the claim that by learning the 
# intervals in order of their statistical probability and information content
# That htings would be a lot easier to then learn bc of their mental load
# and also things because of statistical learning.
#======================================================================================================
# First level has melody with name as one field, mint string as other
#--------------------------------------------------
# Function to break the corpus into n-gram
#--------------------------------------------------
# Graph that n-gram 