# The aim of this analysis is to provide a minimal summary of the data by generating
# mean for numeric variables and proportion for nominal variables.

library(dplyr)
library(openxlsx)

source("./R/core_analysis_functions.R")

# loading clean data set and list of desired variables for analysis
main_data <- read.xlsx("./input/Shelter_MARKET_ASSESSMENT_clean_data.xlsx")
param_data <- read.xlsx("./input/variables.xlsx") 

# NA in the disaggregations vector is for national/overall analysis
disaggregations <- c(NA, "region", "province", "district")
 
# Summarize the data based on the parameters
result <- analyse_data(main_data, param_data, disaggregations)

write.xlsx(result, "./output/analysis_result.xlsx")
