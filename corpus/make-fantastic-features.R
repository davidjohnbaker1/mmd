#--------------------------------------------------
# Update Features 
setwd("symbolic/featureData/")
source("Fantastic.R")
setwd("../csv/")

list.files()

berkowitzFeatures <- compute.features(melody.filenames = list.files(pattern=".csv"), 
                                     dir = ".",
                                     use.segmentation = FALSE, 
                                     write.out = TRUE)
library(data.table)
setwd("../")
fwrite(berkowitzFeatures,"CurrentBerkowitz.csv")
