library(dplyr)
library(rlang)

# Extract column names of multi_select question
extract_column_names <- function(data, prefix) {
  column_names <- names(data)
  matching_columns <- column_names[startsWith(column_names, paste0(prefix, "."))]
  return(matching_columns)
}

# Helper function to apply grouping
apply_grouping <- function(data, disaggregation_var) {
  if (!is.null(disaggregation_var) && !is.na(disaggregation_var)) {
    data <- data %>% group_by(group_var_value = !!sym(disaggregation_var))
  }
  return(data)
}

# Function to calculate select_one percentage
summarize_select_one <- function(data, select_one_var, disaggregation_var = NULL) {
  data_clean <- data %>%
    filter(!is.na(!!sym(select_one_var)))
  
  result <- data_clean %>%
    {
      if (!is.null(disaggregation_var) && !is.na(disaggregation_var)) {
        group_by(., group_var_value = !!sym(disaggregation_var), analysis_var_value = !!sym(select_one_var))
      } else {
        group_by(., analysis_var_value = !!sym(select_one_var))
      }
    } %>%
    summarise(
      n = n(), 
      .groups = 'drop'
    )
  
  # Calculate total per group (for percentage)
  if (!is.null(disaggregation_var) && !is.na(disaggregation_var)) {
    result <- result %>%
      group_by(group_var_value) %>%
      mutate(n_total = sum(n)) %>%
      ungroup()
  } else {
    result <- result %>%
      mutate(n_total = sum(n))  # Overall total if no disaggregation
  }
  
  # Calculate percentage and other stats
  result <- result %>%
    mutate(
      group_var = disaggregation_var,
      analysis_var = select_one_var,
      stat = round(n / n_total, 3), 
      analysis_type = "percentage"
    )
  
  col_list <- c("group_var", "group_var_value", "analysis_var", "analysis_var_value", "stat", "n", "n_total", "analysis_type")
  result <- result %>% select(any_of(col_list))
  
  return(result)
}

# Function for select_multiple questions
summarize_select_multiple <- function(data, prefix, disaggregation_var = NULL) {
  
  columns <- extract_column_names(data, prefix)
  
  result_list <- list()
  
  for (col in columns) {
    
    suffix  <- sub(".*\\.", "", col)
    
    data_clean <- data %>%
      filter(!is.na(!!sym(col)))
    
    result <- data_clean %>%
      apply_grouping(disaggregation_var) %>%
      summarise(
        n = sum(!!sym(col) == 1, na.rm = TRUE),
        n_total = n(),
        stat = round(n / n_total, 3),
        .groups = 'drop'
      ) %>%
      mutate(
        group_var = disaggregation_var,
        analysis_var = prefix,
        analysis_var_value = suffix,
        analysis_type = "percentage"
      )
    
    result_list[[col]] <- result
  }
  
  final_result <- bind_rows(result_list)
  
  col_list <- c("group_var", "group_var_value", "analysis_var", "analysis_var_value", "stat", "n", "n_total", "analysis_type")
  final_result <- final_result %>% select(any_of(col_list))
  
  return(final_result)
}

# Function for numeric variables (mean calculation)
summarize_numeric <- function(data, numeric_var, disaggregation_var = NULL) {
  data_filtered <- data %>%
    filter(!is.na(!!sym(numeric_var)))
  
  result <- data_filtered %>%
    apply_grouping(disaggregation_var) %>%
    summarise(
      group_var = disaggregation_var,
      analysis_var = numeric_var,
      stat = round(mean(!!sym(numeric_var), na.rm = TRUE), 2),
      n = n(),
      analysis_type = "mean",
      .groups = 'drop'  
    ) %>%
    select(any_of(c("group_var", "group_var_value", "analysis_var", "stat", "n", "analysis_type")))
  
  return(result)
}

# Main analysis function
analyse_data <- function(data, param_data, disaggregations) {
  results <- list() 
  
  # Loop through each variable in the parameter data
  for (i in 1:nrow(param_data)) {
    variable <- param_data$variable[i]
    cat("proccessing: ", variable, "\n")
    type <- param_data$type[i]
    
    # Loop through each disaggregation level
    for (disaggregation in disaggregations) {
      if (is.na(disaggregation) || is.null(disaggregation)) {
        if (type == "select_one") {
          result <- summarize_select_one(data, variable, disaggregation)
        } else if (type == "select_multiple") {
          result <- summarize_select_multiple(data, variable, disaggregation)
        } else if (type == "integer" || type == "decimal") {
          result <- summarize_numeric(data, variable, disaggregation)
        }
      }
      
      if (type == "select_one") {
        result <- summarize_select_one(data, variable, disaggregation)
      } else if (type == "select_multiple") {
        result <- summarize_select_multiple(data, variable, disaggregation)
      } else if (type == "integer" || type == "decimal") {
        result <- summarize_numeric(data, variable, disaggregation)
      }
      
      # Store result in the list, ensuring unique names for each combination
      result <- result %>%
        mutate(variable = variable, disaggregation = disaggregation)
      results[[paste(variable, disaggregation, sep = "_")]] <- result
    }
  }
  
  # Combine all results into a single dataframe
  final_result <- bind_rows(results)
  col_list <- c("group_var", "group_var_value", "analysis_var", "analysis_var_value", "stat", "n", "n_total", "analysis_type")
  final_result <- final_result %>% select(any_of(col_list))
  cat("proccessing finished.    ", "\n")
  return(final_result)
}