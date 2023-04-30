##### Data Processing Script #####
# update children sheet in the end to have only main sheet's keys
# Install/load required packages -----------------------------------------------------------------
if(!require(devtools)) install.packages("devtools")
if(!require(atRfunctions)) install_github("atrcodebase/atRfunctions")
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(writexl)) install.packages("writexl")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Read data ----------------------------------------------------------------------------------------
# file.edit("R/read_data.R")
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
fmv_df <- read_excel("input/raw_data/FMV Survey with Clients & Caregivers.xlsx", sheet="data", guess_max = 100000, na = convert_to_na)
fmv_children <- read_excel("input/raw_data/FMV Survey with Clients & Caregivers.xlsx", sheet="children", guess_max = 100000, na = convert_to_na)
fmv_tool_relevancy <- read_excel("input/tool_relevancy_rules/FMV_relevancy_rules.xlsx")


# read qa-log, correction log, and translation log -------------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT0qhygRpvJcvyYrBF8U2dNIimIu9iLvUpIafOcnM3bqk61pHOzi5CAVQHATE1fYTF-uHYxUHugBT-_/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=197981367&single=true&output=csv"), col_types = "c")
correction_log <- readr::read_csv(paste0(url, "gid=262784705&single=true&output=csv"), col_types = "c")
pilot_qa_log <- readr::read_csv(paste0(url, "gid=1295500851&single=true&output=csv"), col_types = "c")
rejection_log <- readr::read_csv(paste0(url, "gid=1532076517&single=true&output=csv"), col_types = "c")
# # Fixing separator in Correction log
correction_log <- correction_log %>%
  mutate(new_value = str_squish(new_value), KEY=str_squish(KEY))

# Remove rejection log
fmv_children <- fmv_children %>% filter(KEY %notin% rejection_log$KEY)

# apply correction log -----------------------------------------------------------------------------
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}


# Update Select_multiple series columns ------------------------------------------------------------
multi_vars=c("SP_3", "SP_5", "SP_6", "SP_18")
fmv_df <- update_series_cols(fmv_df, multi_vars=multi_vars, question_separator="_") 
fmv_children <- update_series_cols(fmv_children, multi_vars="SQ_14_reason", question_separator="_") 


# Relevancy check ----------------------------------------------------------------------------------
fmv_df_joined <- left_join(fmv_df,
                           fmv_children %>%
                             group_by(PARENT_KEY) %>%
                             summarize(total_vaccinated=sum(vaccinated),
                                       total_not_vaccinated=sum(not_vaccinated)) %>% ungroup(), by=c("KEY"="PARENT_KEY"))
fmv_children_joined <- left_join(fmv_children,
                                 fmv_df %>% select(consent, KEY), by=c("PARENT_KEY"="KEY"))

fmv_tool_relevancy_dt <- fmv_tool_relevancy %>% filter(sheet == "data")
fmv_tool_relevancy_ch <- fmv_tool_relevancy %>% filter(sheet == "children")

# Check Relevancy Rules
fmv_issues <- rbind(
  check_relevancy_rules(fmv_df_joined, fmv_tool_relevancy_dt),
  check_relevancy_rules(fmv_children_joined, fmv_tool_relevancy_ch)
) %>% filter(question != "respondent_name")

# Check Select_Multiple Questions
fmv_SM_issues <- check_select_multiple(fmv_df, multi_vars, separator="_")


# attach value labels ------------------------------------------------------------------------------
fmv_tool <- "input/tools/FMV_+Survey+with+Clients+&+Caregivers.xlsx"
fmv_df <- labeler(data = fmv_df, tool = fmv_tool, survey_label = "label",
                             choice_lable = "label", multi_response_sep = ";")
fmv_children <- labeler(data = fmv_children, tool = fmv_tool, survey_label = "label",
                  choice_lable = "label", multi_response_sep = ";")


# recode variables ---------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R") 


# remove extra columns -----------------------------------------------------------------------------
extra_cols <- c("Deviceid", "Subscriberid", "Simid", "Devicephonenum", "Username", "SQ_8_Label",
                "audio_audit_introduction", "TA", "Surveyor_Name", "respondent_name", 
                #"SQ_5",	"SQ_7", #double-check if these need to be removed
                "audio_audit_child_birth", "SET-OF-children", "formdef_version")
fmv_df <- fmv_df %>% select(-all_of(extra_cols))
fmv_children <- fmv_children %>% select(-"SQ_14_name")

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(qa_status=`QA status`, KEY)
## Filter
QA_backlog_keys <- left_join(
  fmv_df %>% filter(review_status == "APPROVED") %>% select(SubmissionDate, KEY), qa_log_sub, by = "KEY") %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>% filter(qa_status %notin% c("Approved", "Rejected"))
QA_backlog <- QA_backlog_keys %>% 
  group_by(SubmissionDate) %>% 
  count(qa_status, name = "freq") %>% 
  mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% 
  arrange(SubmissionDate)
# Print
print(knitr::kable(QA_backlog, format = "simple"))


# remove Rejected keys -----------------------------------------------------------------------------
count(qa_log, `QA status`)
fmv_df <- fmv_df %>% filter(qa_status %notin% "Rejected" & review_status %notin% "REJECTED")
# Remove pilot data
fmv_df <- fmv_df %>% filter(KEY %notin% pilot_qa_log$KEY)

# Filter repeating groups --------------------------------------------------------------------------
main_sub <- fmv_df %>% 
  select(SubmissionDate, Province, District, Region, Type, Location_Type, 
         HF_Type_based_on_sample, HF_Name_based_on_Sample, SQ_1, qa_status, KEY)
# Filter
fmv_children <- fmv_children %>%
  filter(PARENT_KEY %in% fmv_df$KEY)
fmv_children <- fmv_children %>% 
  left_join(main_sub, by = c("PARENT_KEY" = "KEY")) %>% 
  relocate(SubmissionDate:SQ_1, .before = i)

# Filter Approved Data -----------------------------------------------------------------------------
fmv_df_filtered <- fmv_df %>% 
  filter(qa_status %in% "Approved")
fmv_children_filtered <- fmv_children %>% 
  filter(PARENT_KEY %in% fmv_df_filtered$KEY)


# Logical Checks -----------------------------------------------------------------------------------
file.edit("R/logic_checks.R")


# generate data with missing translations ----------------------------------------------------------
excluded_cols <- c("SQ_14_name", "SQ_5", "SQ_7")
missing_translation_log <- rbind(
  missing_translation(data = fmv_df, KEY = "KEY", excluded_cols) %>% mutate(sheet="Data"),
  missing_translation(data = fmv_children, KEY = "KEY", excluded_cols) %>% mutate(sheet="Children"))


# Renaming Datasets --------------------------------------------------------------------------------
var_map <- read_excel("input/Column Mapping.xlsx")
data_vars <- var_map %>% filter(sheet == "data" & xml %in% names(fmv_df_filtered))
children_vars <- var_map %>% filter(sheet == "children" & xml %in% names(fmv_children_filtered))

#Rename
fmv_df_renamed <- fmv_df_filtered %>% 
  rename_at(vars(data_vars$xml), function(x) {x = data_vars$english[data_vars$xml == x]})
fmv_children_renamed <- fmv_children_filtered %>% 
  rename_at(vars(children_vars$xml), function(x) {x = children_vars$english[children_vars$xml == x]})


# Export -------------------------------------------------------------------------------------------
## Dataset
cleaned_data <- list(
  data=fmv_df,
  children=fmv_children
)
cleaned_filtered <- list(
  data=fmv_df_filtered,
  children=fmv_children_filtered
)
renamed_data <- list(
  data=fmv_df_renamed,
  children=fmv_children_renamed
)
## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)
#Relevancy
relevancy_issues <- list(
  Relevancy_issues=fmv_issues,
  Select_multiple_issues=fmv_SM_issues)

## create the output path
check_path("output/cleaned_data")
## export cleaned datasets
writexl::write_xlsx(cleaned_data, "output/cleaned_data/FMV_Survey_with_Clients_Caregivers_cleaned.xlsx", format_headers = F) # Cleaned
writexl::write_xlsx(cleaned_filtered, "output/cleaned_data/FMV_Survey_with_Clients_Caregivers_cleaned_approved.xlsx", format_headers = F) # Cleaned & Approved
writexl::write_xlsx(renamed_data, "output/cleaned_data/FMV_Survey_with_Clients_Caregivers_cleaned_approved_English.xlsx", format_headers = F) # Cleaned & Approved

## keep a copy of correct & translation log, export log issues, export missing translation, etc.
writexl::write_xlsx(correction_log, "output/correction_log.xlsx", format_headers = F) # correction log
writexl::write_xlsx(correction_log_issues, "output/Correction_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(qa_backlog_list, "output/QA_Backlog_by_Date.xlsx", format_headers = F)
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/Tool_relevancy_issues.xlsx")
writexl::write_xlsx(logic_check, "output/logical_issues_FMV.xlsx")

