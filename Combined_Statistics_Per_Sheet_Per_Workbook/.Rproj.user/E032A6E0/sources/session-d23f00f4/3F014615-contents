library(tidyverse)
library(readxl)
library(writexl)

# Function to perform data wrangling and calculate statistics
calculate_stats <- function(data) {
  data <- data %>%
    mutate(vendor = str_extract(Analyzer, "^[^-]+")) %>%
    mutate(vendor = gsub("\\s+$", "", vendor)) %>%
    select(vendor, AM_L.L, AM_U.L, AF_L.L, AF_U.L, CM_L.L, CM_U.L, CF_L.L, CF_U.L, Gender_male, Gender_female, Age_adult, Age_child)  # Select relevant columns
  
  # Calculate summary statistics for each vendor, gender, and age group
  summary_data <- data %>%
    group_by(vendor) %>%
    summarise(
      Number = n(),
      mean_AM_L.L = mean(AM_L.L),
      mean_AM_U.L = mean(AM_U.L),
      mean_AF_L.L = mean(AF_L.L),
      mean_AF_U.L = mean(AF_U.L),
      mean_CM_L.L = mean(CM_L.L),
      mean_CM_U.L = mean(CM_U.L),
      mean_CF_L.L = mean(CF_L.L),
      mean_CF_U.L = mean(CF_U.L),
      sd_AM_L.L = sd(AM_L.L),
      sd_AM_U.L = sd(AM_U.L),
      sd_AF_L.L = sd(AF_L.L),
      sd_AF_U.L = sd(AF_U.L),
      sd_CM_L.L = sd(CM_L.L),
      sd_CM_U.L = sd(CM_U.L),
      sd_CF_L.L = sd(CF_L.L),
      sd_CF_U.L = sd(CF_U.L),
      .groups = "drop"
    )
  
  # Calculate intra-analyzer variability for L.L and U.L separately
  intra_analyzer_variability <- data %>%
    group_by(vendor) %>%
    summarise(
      Intra_CV_L.L = (sd(AM_L.L) / mean(AM_L.L)) * 100,
      Intra_CV_U.L = (sd(AM_U.L) / mean(AM_U.L)) * 100,
      .groups = "drop"
    )
  
  # Calculate inter-analyzer variability for L.L and U.L separately
  inter_analyzer_variability <- summary_data %>%
    summarise(
      Inter_Variability_AM_L.L = sd(mean_AM_L.L),
      Inter_Variability_AM_U.L = sd(mean_AM_U.L),
      Inter_Variability_AF_L.L = sd(mean_AF_L.L),
      Inter_Variability_AF_U.L = sd(mean_AF_U.L),
      Inter_Variability_CM_L.L = sd(mean_CM_L.L),
      Inter_Variability_CM_U.L = sd(mean_CM_U.L),
      Inter_Variability_CF_L.L = sd(mean_CF_L.L),
      Inter_Variability_CF_U.L = sd(mean_CF_U.L),
      Inter_CV_AM_L.L = (sd(mean_AM_L.L) / mean(mean_AM_L.L)) * 100,
      Inter_CV_AM_U.L = (sd(mean_AM_U.L) / mean(mean_AM_U.L)) * 100,
      Inter_CV_AF_L.L = (sd(mean_AF_L.L) / mean(mean_AF_L.L)) * 100,
      Inter_CV_AF_U.L = (sd(mean_AF_U.L) / mean(mean_AF_U.L)) * 100,
      Inter_CV_CM_L.L = (sd(mean_CM_L.L) / mean(mean_CM_L.L)) * 100,
      Inter_CV_CM_U.L = (sd(mean_CM_U.L) / mean(mean_CM_U.L)) * 100,
      Inter_CV_CF_L.L = (sd(mean_CF_L.L) / mean(mean_CF_L.L)) * 100,
      Inter_CV_CF_U.L = (sd(mean_CF_U.L) / mean(mean_CF_U.L)) * 100,
      .groups = "drop"
    )
  
  # Combine summary statistics and variability measures
  result <- bind_rows(
    summary_data,
    intra_analyzer_variability,
    inter_analyzer_variability %>% mutate(vendor = "Inter-analyzer variability")
  )
  
  return(result)
}

# Upload the Excel file
file_path <- file.choose()
excel_data <- readxl::excel_sheets(file_path)

# Create an empty list to store the results for each sheet
results_list <- list()

# Loop through each sheet in the Excel file
for (sheet in excel_data) {
  # Read the data from the current sheet
  data <- read_excel(file_path, sheet = sheet)
  
  # Calculate the descriptive statistics
  result <- calculate_stats(data)
  
  # Store the result in the results_list
  results_list[[sheet]] <- result
}

# Combine the results from all sheets into a single data frame
combined_table <- bind_rows(results_list, .id = "Sheet")

# View the combined table
View(combined_table)

# Save the combined table to an Excel file
write_xlsx(combined_table, "combined_table.xlsx")
