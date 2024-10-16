# The aim of this analysis is to provide minimal summary mean for numeric variables
# and percentage for nominal variables.

library(dplyr)
library(openxlsx)

source("./R/core_analysis_functions.R")

main_data <- read.xlsx("./input/Shelter_MARKET_ASSESSMENT_clean_data.xlsx")
param_data <- read.xlsx("./input/variables.xlsx") 

# NA in the disaggregations vector is for national/overall wise analysis
disaggregations <- c(NA, "region", "province", "district")
 
# Summarize the data based on parameters
result <- analyse_data(main_data, param_data, disaggregations)

write.xlsx(result, "./output/analysis_result.xlsx")
