#--------------------------------------------------
# Update Features 
setwd("scripts/fantasticSoftware/")
source("Fantastic.R")
setwd("../../symbolic/csv/")

list.files()

berkowitzFeatures <- compute.features(melody.filenames = list.files(pattern=".csv"), 
                                     dir = ".",
                                     use.segmentation = FALSE, 
                                     write.out = TRUE)
library(data.table)
setwd("../")
fwrite(berkowitzFeatures,"Melosol_Features.csv")
#--------------------------------------------------
#--------------------------------------------------
# Update Features 
setwd("scripts/fantasticSoftware/")
source("Fantastic.R")
setwd("../../symbolic/krn/densmore/csv/")

list.files()

densmoreFeatures <- compute.features(melody.filenames = list.files(pattern=".csv"), 
                                      dir = ".",
                                      use.segmentation = FALSE, 
                                      write.out = TRUE)
library(data.table)
setwd("../../../")
fwrite(densmoreFeatures,"Densmore_Features.csv")
setwd("..")
