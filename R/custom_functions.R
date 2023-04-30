compare_dt <- function (df1, df2, unique_id_df1, unique_id_df2, compare_all = TRUE) 
{
  if (compare_all == FALSE) {
    df1 <- df1[, colnames(df1) %in% colnames(df2)]
    df2 <- df2[, colnames(df2) %in% colnames(df1)]
  }
  if ("KEY" %in% colnames(df1) && unique_id_df1 != "KEY") {
    df1 <- df1 %>% rename(key = KEY)
  }
  df1 <- df1 %>% select(KEY = all_of(unique_id_df1), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_1") %>% mutate(value_1 = str_squish(value_1))
  if ("KEY" %in% colnames(df2) && unique_id_df2 != "KEY") {
    df2 <- df2 %>% rename(key = KEY)
  }
  df2 <- df2 %>% select(KEY = all_of(unique_id_df2), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_2") %>% mutate(value_2 = str_squish(value_2))
  df_both <- full_join(df1, df2, by = c("KEY", "name"))
  diff <- df_both %>% filter((value_1 != value_2) | (is.na(value_1) & 
                                                       !is.na(value_2)) | (!is.na(value_1) & is.na(value_2))) %>% 
    rename(question = name, old_value = value_1, new_value = value_2) %>% 
    mutate(question = ifelse(question == "key", "KEY", question))
  if (nrow(diff) == 0) {
    paste0("No difference in df1 and df2")
    return(diff)
  }
  else {
    return(diff)
  }
}

apply_log <- function (data, log, data_KEY = "KEY", log_columns = c(question = "question", 
                                                       old_value = "old_value", new_value = "new_value", KEY = "KEY")) 
{
  tryCatch(
    # Specifying expression
    expr = {					
      for (rowi in 1:nrow(log)) {
        var_i <- log[[log_columns[["question"]]]][rowi]
        old_i <- log[[log_columns[["old_value"]]]][rowi]
        new_i <- log[[log_columns[["new_value"]]]][rowi]
        uuid_i <- log[[log_columns[["KEY"]]]][rowi]
        if (var_i %in% colnames(data)) {
          var_type <- class(data[[var_i]])
          if (var_type %in% "character") {
            data[data[[data_KEY]] %in% uuid_i, var_i] <- as.character(new_i)
          }
          else {
            data[data[[data_KEY]] %in% uuid_i, var_i] <- as.numeric(new_i)
          }
        }
      }
      return(data)
    },
    # Specifying error message
    error = function(e){		
      stop(paste("uuid:", uuid_i, "Old value: ", old_i,
                 "changed to", new_i, "for", var_i), call. = FALSE)
    }
  )
}

check_path <- function(path){
  if (!file.exists(path)) {
    dir.create(path, showWarnings = TRUE, recursive = TRUE)
    cat("Created '", path, "' folder")
  } else {
    cat("The '",path,"' folder already exists")
  }
}

# Relevancy Function -------------------------------------------------------------------------------
check_relevancy_rules <- function(data, tool_relevancy){
  
  # #test
  # data=fmv_children_joined
  # tool_relevancy=fmv_tool_relevancy_ch
  # #

  # initiate Log
  relevancy_log <- data.frame()
  
  # Loop through relevancy rules
  questions <- tool_relevancy$name
  for(row_i in 1:length(questions)){
    question <- questions[row_i]
    relevant_question <- str_split(tool_relevancy$relevant_question[row_i], " - ")[[1]]
    check_reverse <- tool_relevancy$check_reverse[row_i]
    relevancy_sub <- tool_relevancy[tool_relevancy$name == question,]
    
    # Skip if question is missing
    if(question %notin% names(data)){
      print(paste0("Column missing in dataset: ", question))
      next
    } else if(any(!relevant_question %in% names(data))){
      print(paste0("Relevant column missing in dataset: ", relevant_question[!relevant_question %in% names(data)]))
    }
    
    # Conditional string
    conditional_string <- relevancy_sub$Rcondition
    conditional_str_negated <- relevancy_sub$Rcondition %>% paste0("!(", ., ")") #Negate
    
    # Flag issues
    if(check_reverse){
      flagged_rows <- c(
        # Rows where Question has a value but relevant question does not apply 
        which(!is.na(data[[question]]) & eval(parse(text=conditional_str_negated))),
        # Rows where Relevant Question applies but the actual question is null
        which(is.na(data[[question]]) & eval(parse(text=conditional_string)))
      )
    } else {
      # Rows where Question has a value but relevant question does not apply 
      flagged_rows <- which(!is.na(data[[question]]) & eval(parse(text=conditional_str_negated)))
    }
  
    #test
    # data[which(is.na(data[[question]]) & eval(parse(text=conditional_string))),] %>% View
    # data[eval(parse(text=conditional_str_negated)),] %>% View
    
    # Log if rows are flagged
    len_flagged <- length(flagged_rows)
    if(len_flagged > 0){
      # Get the values of relevant questions
      relevant_values <- data[flagged_rows, c("KEY",relevant_question)] %>%
        pivot_longer(-KEY, names_to = "cols", values_to = "value", values_transform=as.character) %>% 
        group_by(KEY) %>% 
        summarize(total = paste0(value, collapse = " - ")) %>% ungroup() %>% pull(total)
      
      log <- data.frame(KEY=data$KEY[flagged_rows],
                        question=rep(question, len_flagged),
                        value=data[[question]][flagged_rows],
                        relevancy_rule=rep(relevancy_sub$relevance_rule[1], len_flagged),
                        relevant_question=rep(paste0(relevant_question, collapse = " - "), len_flagged),
                        relev_value=relevant_values)
      # Rbind 
      relevancy_log <- rbind(relevancy_log, log)
    }
  }
  return(relevancy_log)
}

update_series_cols <- function(data, multi_vars, question_separator){
  for(question in multi_vars){
    print(paste0("Updating: ", question))
    # Get all series columns
    series_cols <- names(select(data, starts_with(paste0(question, question_separator))))
    series_cols <- series_cols[!grepl("_other", series_cols, ignore.case = T)] # exclude the Other questions
    
    # Make all series cols numeric
    data <- data %>% mutate(across(all_of(series_cols), as.numeric))
    # Get rows with non-NA values
    rows <- which(!is.na(data[[question]]))
    na_rows <- which(is.na(data[[question]]))
    
    # Loop each series column
    for(column in series_cols){
      # Add word boundary for str_detect (differentiate 1 from 13)
      response <- paste0("\\b", gsub(paste0(question, question_separator), "", column),"\\b")
      # Assign 1 if value exists in main question, else 0
      data[rows, column] <- ifelse(str_detect(data[[question]][rows], response), 1, 0)
      # Make the rest of the rows na
      data[na_rows, column] <- NA_integer_
    }
  }
  return(data)
}

check_select_multiple <- function(data, multi_vars, separator, KEY="KEY"){
  
  # data=fmv_df
  # multi_vars
  # separator="_"
  # KEY="KEY"
  
  series_log <- data.frame(KEY=NA,question=NA,value=NA,series_columns=NA,
                           series_values=NA,Remarks=NA)
  
  for(question in multi_vars){
    print(paste0("Checking: ", question))
    # Get all series columns
    series_cols <- names(select(data, starts_with(paste0(question, separator))))
    
    data_sub <- data %>% 
      select(all_of(question), all_of(series_cols), all_of(KEY)) %>% 
      filter(!is.na(get(question)))
    
    for(i in 1:nrow(data_sub)){
      #question value
      val <- str_split(data_sub[[question]][i], " |-")[[1]]
      #make related series column name
      series_columns <- paste0(question,separator, val) 
      other_columns <- names(data_sub)[names(data_sub) %notin% c(series_columns, question, "KEY")]
      if(!all(series_columns %in% names(data_sub))){
        log <- c(data_sub$KEY[i], 
                 question, 
                 data_sub[[question]][i], 
                 paste0(series_columns, collapse = " - "),
                 "", 
                 Remarks="Series column not in data")
        series_log <- rbind(series_log, log)
      } else if(any(data_sub[i,series_columns] %in% c(NA, 0))){
        log <- c(data_sub$KEY[i], 
                 question, 
                 data_sub[[question]][i], 
                 paste0(series_columns, collapse = " - "),
                 paste0(data_sub[i,series_columns], collapse = " - "),
                 Remarks = "Inonsistent series columns")
        series_log <- rbind(series_log, log)
      } else if(any(data_sub[i, other_columns] %in% 1)){
        
        other_cols <- other_columns[which(data_sub[i, other_columns] %in% 1)]
        log <- c(data_sub$KEY[i], 
                 question, 
                 data_sub[[question]][i], 
                 paste0(other_cols, collapse = " - "),
                 paste0(data_sub[i,other_cols], collapse = " - "),
                 Remarks = "At least one response is not in the tool choices")
        series_log <- rbind(series_log, log)
      }
    }
  }
  if(nrow(series_log) == 1){
    print("No mismatches found!")
    return(series_log)
  } else {
    return(series_log[-1,])
  }
}

reorder_choices_v2 <- function(data, questions, default_value){
  # Remove 1,2,3 ranks
  data <- data %>% 
    mutate(across(all_of(questions), function(x) x = str_replace_all(x, paste0(default_value, " [123]"), default_value)))
  data_reshaped <- data %>% 
    select(KEY=`_uuid`, all_of(questions)) %>% 
    pivot_longer(-KEY, names_to = "cols", values_to = "values")
  
  # Reorder the values
  data_reshaped <- data_reshaped %>% 
    group_by(KEY) %>% 
    mutate(values = c(values[values %notin% default_value], values[values %in% default_value])) %>% 
    # # Re-add the Ranks
    # mutate(values = case_when(
    #   values %in% default_value ~ paste0(values, " ", row_number()),
    #   TRUE ~ values
    # )) %>%
    ungroup() %>% 
    pivot_wider(id_cols = KEY, names_from = cols, values_from = values)
  
  relocate_col <- names(data)[which(names(data) %in% questions[1])-1]
  # Join initial data with reordered values
  data <- data %>% 
    select(-all_of(questions)) %>%
    left_join(data_reshaped, by=c('_uuid'="KEY")) %>% 
    relocate(all_of(questions), .after = all_of(relocate_col))
  gc()
  return(data)
}



#Logs all the data points that are not translated
missing_translation <- function(data, KEY, excluded_cols){
  
  question <- c(); old_value <- c(); uuid <- c()
  data_cols <- names(data)[names(data) %notin% excluded_cols]
  
  #checking each column
  for(col_name in data_cols){
    
    # remove UTF characters to avoid false flagging
    cell_values <- as.character(str_remove_all(data[[col_name]], "\\–|\\’|é"))
    # Filter UTF strings
    logical_filter <- Encoding(cell_values) %in% "UTF-8"
    cell_val <- data[[col_name]][logical_filter]
    keys <- data[[KEY]][logical_filter]
    
    # log
    question <- c(question, rep(col_name, length(cell_val)))
    old_value <- c(old_value, cell_val)
    uuid <- c(uuid, keys)
  }
  
  if(length(question)+length(old_value)+length(uuid) == 0){
    print("No untranslated data found!")
    log <- data.frame()
  } else{
    log <- data.frame(question, old_value, new_value=NA, uuid, Remarks=NA) %>% unique()
  }
}

labeler <-function (data, tool, survey_label = "label::English", choice_lable = "label::English", 
                    multi_response_sep = ";") 
{
  survey_questions <- read_excel(tool, "survey")
  survey_choices <- read_excel(tool, "choices")
  if ("value" %in% names(survey_choices)) {
    names(survey_choices)[names(survey_choices) == "value"] <- "name"
  }
  if ("list name" %in% names(survey_choices)) {
    names(survey_choices)[names(survey_choices) == "list name"] <- "list_name"
  }
  survey_choices$name <- gsub("\\.0", "", survey_choices$name)
  survey_questions <- survey_questions[grepl("\\bselect_", 
                                             survey_questions$type), ]
  survey_questions$select_type <- survey_questions$type %>% 
    str_replace_all(" .*", "")
  survey_questions$type <- survey_questions$type %>% str_replace_all("select_one ", 
                                                                     "") %>% str_replace_all("select_multiple ", "")
  survey_questions <- survey_questions %>% select(type, name, 
                                                  select_type, all_of(survey_label))
  survey_choices$name <- survey_choices$name %>% as.character
  survey_choices <- survey_choices[!is.na(survey_choices$list_name), 
  ]
  for (var in names(data)) {
    if (var %in% survey_questions$name) {
      survey_choices_i <- survey_choices[survey_choices$list_name %in% 
                                           survey_questions$type[survey_questions$name %in% 
                                                                   var], ]
      add_underscore <- function() {
        index <- gregexpr("[0-9]", survey_choices_i[[choice_lable]])
        regmatches(survey_choices_i[[choice_lable]], 
                   index) <<- lapply(regmatches(survey_choices_i[[choice_lable]], 
                                                index), function(x) paste0("_", x, "_"))
      }
      add_underscore()
      if (survey_questions$select_type[survey_questions$name %in% 
                                       var] == "select_one") {
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>% str_replace_all(paste0("\\b", 
                                                                survey_choices_i$name[choice_i], "\\b"), 
                                                         survey_choices_i[[choice_lable]][choice_i])
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", 
                                                       "")
      }
      else if (survey_questions$select_type[survey_questions$name %in% 
                                            var] == "select_multiple") {
        data[[var]] <- data[[var]] %>% str_replace_all("  ", 
                                                       " ") %>% str_replace_all(" ", paste0(multi_response_sep))
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>% str_replace_all(paste0("\\b", 
                                                                survey_choices_i$name[choice_i], "\\b"), 
                                                         survey_choices_i[[choice_lable]][choice_i])
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", 
                                                       "")
      }
    }
  }
  return(data)
}
