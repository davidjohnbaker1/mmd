#--------------------------------------------------
# Get n-gram distributions
#--------------------------------------------------
library(ggplot2)

minty <- read.delim("symbolic/krn/629.mnt", header = TRUE, stringsAsFactors = FALSE)
minty

str(minty)
minty$ONE <- minty$X.G.

ggplot(minty, aes(ONE)) + geom_bar()

install.packages("ngram")
library(ngram)

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


