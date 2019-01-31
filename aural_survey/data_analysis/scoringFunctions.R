#======================================================================================================
# Scoring Functions

get_demographics <- function(x){
  x %>% 
    filter(trial_type == "survey-text") %>%
    select(responses) 
}

clean_demographics <- function(x){
  x %>%
    str_remove_all(pattern = "Q0") %>%
    str_remove_all(pattern = "Q1") %>%
    str_remove_all(pattern = "\\{") %>%
    str_remove_all(pattern = "\\}") %>%
    str_remove_all(pattern = "\\:") %>%
    str_remove_all(pattern = "\"")
}

get_melody_responses <- function(x){
  x %>%
    select(trial_type, stimulus, button_pressed, response, rt, subject) %>%
    filter(trial_type ==  "image-button-response" | trial_type == "image-slider-response") %>%
    fill(stimulus) %>%
    mutate(ratings = coalesce(button_pressed, response)) %>%
    mutate(question_no = rep(1:5,20)) %>%
    select(stimulus, question_no, ratings, rt, subject)
}

clean_stimuli_name <- function(x){
  x %>%
    str_remove_all("img/") %>%
    str_remove_all("\\.png") %>%
    str_remove_all("x") 
}

calcualte_speed_quality <- function(x){
  x %>%
    select(rt) %>%
    summarise(avg_response_time = mean(rt))
}
#======================================================================================================