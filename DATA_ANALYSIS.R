### Data Analysis
# This script was not used in final analysis, only for some ad-hoc requests
# Load required packages -----------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(writexl)) install.packages("writexl")
if(!require(glue)) install.packages("glue")
source("R/functions/Analysis_double_disagg.R") # custom function for anlaysis
`%notin%` <- Negate(`%in%`)

# Read data ------------------------------------------------------------------------------
convert_to_na <- c("NA", "N/A", "-", " ", 9999, 7777, 8888)
data_path <- "output/cleaned_data/Finalized Cleaned dataset/FMV_Survey_with_Clients_Caregivers_cleaned_approved.xlsx" # data path
fmv_df <- read_excel(data_path, sheet="data", guess_max = 100000, na = convert_to_na)
fmv_children <- read_excel(data_path, sheet="children", guess_max = 100000, na = convert_to_na)


# Read analysis plans --------------------------------------------------------------------
fmv_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_FMV.xlsx")

fmv_ap_dt <- fmv_ap %>% filter(sheet == "data")
fmv_ap_ch <- fmv_ap %>% filter(sheet == "children")

# Check question names in AP with question/column names in data
## custom function
check_ap_questions_with_data_columns <- function(ap, dt) {
  unmatched_questions <- ap$variable[ap$variable %notin% names(dt)]
  
  if (length(unmatched_questions) == 0) {
    print("All questions in DAP match with questions/columns in data")
  } else {
    print("----Below questions in DAP do not match with questions/column in data")
    print(unmatched_questions)
  }
}

## check
check_ap_questions_with_data_columns(ap = fmv_ap_dt, dt = fmv_df)
check_ap_questions_with_data_columns(ap = fmv_ap_ch, dt = fmv_children)

# Analysis -------------------------------------------------------------------------------
## Data
fmv_df_analysis <- analysis_func(df = fmv_df, ap = fmv_ap_dt, multi_response_sep = ";") %>% 
  arrange(Disaggregation) %>% 
  filter(Denominator != 0)

## Children
fmv_children_analysis <- analysis_func(df = fmv_children, ap = fmv_ap_ch, multi_response_sep = ";") %>% 
  arrange(Disaggregation) %>% 
  filter(Denominator != 0)

# Merge
fmv_analysis <- rbind(
  fmv_df_analysis,
  fmv_children_analysis
)

# export results -------------------------------------------------------------------------
if (!file.exists(glue::glue("output/analysis"))) {
  dir.create(glue::glue("output/analysis"), showWarnings = TRUE, recursive = TRUE)
  cat("created 'output/analysis' folder")
} else {
  cat("The 'output/analysis' folder already exists")
}

writexl::write_xlsx(fmv_analysis, glue::glue("output/analysis/UNICEF_FMV_Analysis.xlsx"), format_headers = F) 

