#--------------------------------------------------
# Dissertation Progress Graphs
library(tidyverse)
library(lubridate)
berkowitz_progress <- read_csv("corpus/symbolic/Berkowitz Progress - Sheet1.csv")
head(berkowitz_progress)

# Percent Done
table(berkowitz_progress$Finished)[2] / sum(table(berkowitz_progress$Finished))


# Percent Done, No Literature
(table(berkowitz_progress$Finished)[2]-3) / (629)

# Day Rate For Feb 15th Completion

helpmelodycount <- 300
corpusduedate <- ymd("2019-02-15")
today <- today(tzone = "")
handinduedate <- ymd("2019-04-15")
defensedudedate <- ymd("2019-05-02")
corpusdaysuntil <- corpusduedate - today
handindayuntil <- handinduedate - today
defensedayuntil <- defensedudedate - today
corpusrate <- table(berkowitz_progress$Finished)[1]/ as.numeric(corpusdaysuntil)
corpusratehelp <- (table(berkowitz_progress$Finished)[1]-helpmelodycount)/ as.numeric(corpusdaysuntil)

# Days Until Due Dates
print(paste("You have",corpusduedate - today,"days until your corpus is done"))
print(paste("At your current rate, you need to encode",round(corpusrate,2),"melodies per day to finish on time"))
print(paste("If Rory encodes",helpmelodycount,"melodies, then you only need to work at a rate of",round(corpusratehelp,2),"melodies per day to finish on time"))

print(paste("You have",handindayuntil,"until your hand in."))
print(paste("You have",defensedayuntil,"until your defense."))

#======================================================================================================
# Tweet

library(lubridate)
why_to_write <- function(){
  today <- today(tzone = "")
  defense <- ymd("2019-05-02")
  until <- defense - today
  print(paste("You have",until,"days until your PhD defense."))
}

why_to_write()
