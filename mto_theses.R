#--------------------------------------------------
# Dissertations
#--------------------------------------------------
library(tidyverse)

#======================================================================================================

dissertations <-read_tsv("meta_mmd/mto_3.tsv",col_names = FALSE)



dissertations %>%
  rename(autor = X1, Title = X2, Institution = X3, Date = X4) -> dissertations
  
dissertations$year <- as.numeric(str_extract_all(string = dissertations$Date, pattern = "[0-9][0-9][0-9][0-9]",simplify = TRUE))
dissertations$month <- str_remove_all(string = dissertations$Date, pattern = "[0-9][0-9][0-9][0-9]")
dissertations$month <- str_trim(dissertations$month, side = "both")

dissertations$Institution <- ifelse(test = str_detect(string = dissertations$Institution,pattern = "Eastman"),
                                     yes = "Eastman School of Music", no = dissertations$Institution)

dissertations$Institution <- ifelse(test = str_detect(string = dissertations$Institution,pattern = "CUNY Graduate Center"),
                                    yes = "City University of New York", no = dissertations$Institution)

dissertations %>%
  group_by(Institution) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  filter(n > 2) %>%
  ggplot(aes(x = reorder(Institution,-n), y = n)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of PhD by Institution Listed on MTO",
       x =  "Institution",
       y = "Number of Dissertations Listed")
   
dissertations %>%
  group_by(Institution) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  mutate(bigboys = ifelse(n < 10, "Less than Ten Dissertations", Institution)) -> big_boy

dissertations %>%
  left_join(big_boy) -> dissertations

dissertations %>%
  filter(year > 1990) %>%
  ggplot(aes(x = year, fill = as.factor(bigboys))) +
  scale_x_continuous(breaks = seq(1990,2020,1)) +
  scale_y_continuous(breaks = seq(0,40,1)) +
  geom_bar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Dissertations Listed on MTO after 1990", x = "Year", y = "Number of Dissertations")

library(viridis)


