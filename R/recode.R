# split 'starttime' and 'endtime' columns into separate 'date' and 'time' columns; calculate time difference -------------------------------------------
remove_98_99 <- function(x) {
  x = case_when(
    x %in% c(98,99) ~ as.numeric(NA),
    TRUE ~ x
  )}

# Join QA Status -----------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% 
  select(qa_status=`QA status`, KEY) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status)) %>% unique()

fmv_df <- fmv_df %>% left_join(qa_log_sub, by="KEY")

# FMV Data -----------------------------------------------------------------------------------------
mismatch_rows <- which(fmv_df$Surveyor_ID %in% c("Male", "Female"))
# Swap ID and gender
ids <- fmv_df$Surveyor_Gender[mismatch_rows]
fmv_df$Surveyor_Gender[mismatch_rows] <- fmv_df$Surveyor_ID[mismatch_rows]
fmv_df$Surveyor_ID[mismatch_rows] <- ids
# Exception
fmv_df$Surveyor_Gender[fmv_df$Surveyor_ID == "FR3001"] <- "Female"

# Fixing date
fmv_df <- fmv_df %>% 
  mutate(SubmissionDate = openxlsx::convertToDateTime(SubmissionDate),
         age_groups = case_when(
           SQ_2 < 18 ~ "Lower than 18",
           SQ_2 >= 18 & SQ_2 <= 24 ~ "18 - 24 Years",
           SQ_2 >= 25 & SQ_2 <= 29 ~ "25 – 29 Years",
           SQ_2 >= 30 & SQ_2 <= 44 ~ "30 – 44 Years",
           SQ_2 >= 45 & SQ_2 <= 59 ~ "45 – 59 Years",
           SQ_2 >= 60 ~ "60+"), .after = SQ_2)

# Remove extra objects -----------------------------------------------------------------------------
rm(mismatch_rows, ids)
