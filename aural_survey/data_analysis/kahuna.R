#--------------------------------------------------
# Upadate Data
#--------------------------------------------------
# Source Software Needed
source("aural_survey/data_analysis/clean_jsPsych.R")
source("aural_survey/data_analysis/scoringFunctions.R")
library(tidyverse)

#--------------------------------------------------
# Move to Where Data is Stored
setwd("aural_survey/downloaded_data/")
list.files()
score.directory()
create.dataset()
# Remove Junk Files
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("../..")
