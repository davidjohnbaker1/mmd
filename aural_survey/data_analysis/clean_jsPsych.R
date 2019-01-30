#--------------------------------------------------
# Clean Aural Skills Data
#--------------------------------------------------

score.directory <- function(fns=list.files(pattern = "*.csv")){

  for(i in seq(along=fns)){
  
  tmp <- read_csv(fns[i])
  print(paste("Now Working on File",fns[i]))

    #--------------------------------------------------
     # Get Demographic Data 
     #--------------------------------------------------
     demo.dt <- get_demographics(tmp)
     demographic_data <- lapply(demo.dt, clean_demographics)
     
     age <- demographic_data$responses[[1]]
     education_status <- demographic_data$responses[[2]]
     years_teaching_aural <- demographic_data$responses[[3]]
     syllable_system <- demographic_data$responses[[4]]
     instrument <- demographic_data$responses[[5]]
     last_degree <- demographic_data$responses[[6]]
     last_school <- demographic_data$responses[[7]]
     get_help <- demographic_data$responses[[8]]
     opinions <- demographic_data$responses[[9]]
     contact_info <- demographic_data$responses[[10]]
     subject <- tmp$subject[[1]]
     speed_quality <- calcualte_speed_quality(tmp)
     
     
     demo_data <- data.frame(age, education_status, years_teaching_aural, syllable_system, instrument,
                             last_degree, last_school, get_help, opinions, contact_info, subject, speed_quality)
     
     #--------------------------------------------------
     # Run It 
     #--------------------------------------------------
     melody_responses <- get_melody_responses(tmp)
     melody_responses$stimulus <- clean_stimuli_name(melody_responses$stimulus)
     
     melody_responses %>%
       select(stimulus, question_no, ratings, subject) %>%
       spread(question_no, ratings, subject) %>%
       rename(What_Semester = `1`,
              Times_Played = `2`,
              Difficulty_2nd_Year = `3`,
              Grammar = `4`,
              Familiar = `5`) -> clean_melody_responses
     
     
     #--------------------------------------------------
     # Write table of one participant 
     aural_data <- demo_data %>% left_join(clean_melody_responses)
     
     write.table(aural_data, 
                 paste0(substr(fns[i], 1, nchar(fns[i])-4),"_data.csv"), 
                 sep = ",", 
                    col.names = TRUE, 
                    row.names = FALSE)
  }
}

create.dataset <- function(){
  filenames <- list.files(pattern = "data")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../Dictation_Survey_Responses.csv")
}
        
        