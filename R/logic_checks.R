##### Data Integrity Check #####
vac_values <- c("Yes, fully vaccinated", "Yes, partially vaccinated")

logic_check <- rbind(
  fmv_df %>% 
    filter(SQ_1 == "Male" & SQ_12 == "Housewife (housekeeping)") %>% 
    mutate(issue="Male and yet Housewife", Question="SQ_1", Related_question="SQ_12") %>% 
    select(Question, value=SQ_1, Related_question, R_value=SQ_12, KEY, issue),
  
  fmv_df %>% 
    filter(SQ_2 < 65 & SQ_12 == "Retired") %>% 
    mutate(issue="Retired but age less than 65", Question="SQ_2", Related_question="SQ_12") %>% 
    select(Question, value=SQ_2, Related_question, R_value=SQ_12, KEY, issue),
  
  fmv_df %>% 
    filter(SQ_8 == "Years" & SQ_9 > SQ_2) %>% 
    mutate(issue="Total years living in the area is more than the person's age", Question="SQ_9", 
           Related_question="SQ_8 - SQ_2", R_value = paste0(SQ_8, " - ", SQ_2)) %>% 
    select(Question, value=SQ_9, Related_question, R_value, KEY, issue),
  
  fmv_df %>% 
    filter((SQ_13 == "Father" & SQ_1 != "Male") | (SQ_13 == "Mother" & SQ_1 != "Female")) %>% 
    mutate(issue="Respondent gender and relationship to child does not match", Question="SQ_13", 
           Related_question="SQ_1", R_value = SQ_1) %>% 
    select(Question, value=SQ_13, Related_question, R_value, KEY, issue),
  
  fmv_df %>% 
    filter(SQ_13 == "Grandparent" & SQ_2 < 30) %>% 
    mutate(issue="Respondent's age is too young to be a grandparent, plz double-check", Question="SQ_13", 
           Related_question="SQ_2", R_value = SQ_2) %>% 
    select(Question, value=SQ_13, Related_question, R_value, KEY, issue),
  
  fmv_children %>% 
    filter(vaccinated == 1 & SQ_14_vaccination_status %notin% vac_values | 
             vaccinated == 0 & SQ_14_vaccination_status %in% vac_values) %>% 
    mutate(issue="Vaccination status and vaccinated does not match", Question="vaccinated", Related_question="SQ_14_vaccination_status") %>% 
    select(Question, value=vaccinated, Related_question, R_value=SQ_14_vaccination_status, KEY, issue),
  
  fmv_df %>% 
    filter(SQ_14_1 > SQ_14_alive) %>% 
    mutate(issue="Children under age 2 more than number of children alive", Question="SQ_14_1", 
           Related_question="SQ_14_alive", R_value = SQ_14_alive) %>% 
    select(Question, value=SQ_14_1, Related_question, R_value, KEY, issue),
  
  fmv_children %>% 
    filter(not_vaccinated == 1 & SQ_14_vaccination_status %notin% "No" | 
             not_vaccinated == 0 & SQ_14_vaccination_status %in% "No") %>% 
    mutate(issue="Vaccinated status and not_vaccinated does not match", Question="not_vaccinated", Related_question="SQ_14_vaccination_status") %>% 
    select(Question, value=not_vaccinated, Related_question, R_value=SQ_14_vaccination_status, KEY, issue),
  
  fmv_df %>% 
    filter(SQ_14 != (SQ_14_alive+SQ_14_died)) %>% 
    mutate(issue="Total children does not match sum of alive/dead children", Question="SQ_14", 
           Related_question="SQ_14_alive - SQ_14_died", R_value = paste(SQ_14_alive, " - ", SQ_14_died)) %>% 
    select(Question, value=SQ_14, Related_question, R_value, KEY, issue)
)

# 
fmv_df_filtered %>% 
  left_join(fmv_children_filtered %>% 
              group_by(PARENT_KEY) %>% 
              summarize(total = length(PARENT_KEY)), by=c("KEY"="PARENT_KEY")) %>% 
  filter(SQ_14_1 != total)


fmv_df_filtered %>% 
  filter(str_length(SQ_5) != 10 | str_length(SQ_7) != 10)

fmv_children_filtered %>% janitor::get_dupes("KEY")
