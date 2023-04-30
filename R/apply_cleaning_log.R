# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
#Filter empty rows
correction_log_filtered <- correction_log %>% 
  filter(!(is.na(KEY) & is.na(question) & is.na(old_value)))

#identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    question %notin% c(names(fmv_df), names(fmv_children)) ~ "question_df",
    KEY %notin% c(fmv_df$KEY, fmv_children$KEY) ~ "KEY"))

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(correction_log_filtered[, c("KEY", "question")])

correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>% 
  arrange(KEY, question)

correction_log_filtered <- correction_log_filtered %>% 
  filter(is.na(issue) & duplicates == FALSE)

# apply the correction-log -------------------------------------------
fmv_df_copy <- fmv_df
fmv_df <- apply_log(data = fmv_df, log = filter(correction_log_filtered, question %in% names(fmv_df)), 
                                             data_KEY = "KEY",
                                             log_columns = c(question = "question",
                                                             old_value = "old_value",
                                                             new_value = "new_value",
                                                             KEY = "KEY"
                                                             ))
fmv_children_copy <- fmv_children
fmv_children <- apply_log(data = fmv_children, log = filter(correction_log_filtered, question %in% names(fmv_children)), 
                    data_KEY = "KEY",
                    log_columns = c(question = "question",
                                    old_value = "old_value",
                                    new_value = "new_value",
                                    KEY = "KEY"
                    ))

# Verify correction log -------------------------------------------
message("Verifying Correction log, please wait!")
correction_log_discrep <- rbind(
  compare_dt(df1 = fmv_df_copy, df2 = fmv_df, unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
    mutate(Sheet="data"),
  compare_dt(df1 = fmv_children_copy, df2 = fmv_children, unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>% 
    mutate(Sheet="Children")
)

# Removing extra spaces from new_value before joining 
correction_log_discrep <- correction_log_discrep %>%
  anti_join(correction_log_filtered, 
            by=c("KEY", "question", "new_value"))

# remove extra objects -------------------------------------------
rm(fmv_df_copy, correction_log_filtered)

