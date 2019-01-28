#======================================================================================================
# Get files for Aural Skills Survey
#--------------------------------------------------
setwd("corpus/symbolic/xml/berkowitz")
files <- list.files()
files
set.seed(42)
stimuli <- sample(x = files, size = 30,replace = FALSE)
cat(stimuli,sep = "\n")
cat(stimuli,sep = " ")

stimuli_short <- sample(x = stimuli, size = 15, replace = FALSE)
stimuli_short

cat(stimuli_short,sep = " ")
