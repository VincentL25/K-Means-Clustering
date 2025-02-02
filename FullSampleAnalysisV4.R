# Load required libraries
library(tidyverse)
library(readxl)
library(effectsize)

# Read the data
data <- read_excel("Full Dataset.xlsx")

# Convert binary variables to factors
data <- data %>%
  mutate(
    included = factor(`Included (1 = yes)`, levels = c(0, 1), labels = c("Excluded", "Included")),
    specialty = factor(`Specialty (1 = Primary Care)`, levels = c(0, 1), labels = c("Specialty Care", "Primary Care")),
    gender = factor(Gender)
  )

# Perform demographic analyses
## Sex distribution
sex_table <- table(data$included, data$gender)
sex_test <- chisq.test(sex_table)
sex_effect <- cramers_v(sex_table)

## Years since graduation
ysg_test <- t.test(data$`Years Since Graduation` ~ data$included)

## Specialty distribution
specialty_table <- table(data$included, data$specialty)
specialty_test <- chisq.test(specialty_table)
specialty_effect <- cramers_v(specialty_table)

# Define metrics
metrics <- c("Appointments per Day", "Time On Unscheduled Days", "Pajama Time", 
             "Time Outside of 7 AM to 7 PM", "Time in In Basket per Day", 
             "Aggregate Messages Received per day", "Turnaround Time")

# Create summary dataframes
# 1. Demographic summary
demographic_summary <- data.frame(
  Variable = c("Sex Distribution", "Years Since Graduation", "Specialty Distribution"),
  Test_Used = c("Chi-square", "t-test", "Chi-square"),
  Included_Count = c(
    sum(!is.na(data$gender[data$included == "Included"])),
    sum(!is.na(data$`Years Since Graduation`[data$included == "Included"])),
    sum(!is.na(data$specialty[data$included == "Included"]))
  ),
  Excluded_Count = c(
    sum(!is.na(data$gender[data$included == "Excluded"])),
    sum(!is.na(data$`Years Since Graduation`[data$included == "Excluded"])),
    sum(!is.na(data$specialty[data$included == "Excluded"]))
  ),
  P_Value = c(
    sex_test$p.value,
    ysg_test$p.value,
    specialty_test$p.value
  )
)

# 2. EHR metrics summary
ehr_metrics_summary <- do.call(rbind, lapply(metrics, function(m) {
  included_data <- data[[m]][data$included == "Included"]
  excluded_data <- data[[m]][data$included == "Excluded"]
  test_result <- t.test(included_data, excluded_data)
  
  data.frame(
    Metric = m,
    Included_Mean = mean(included_data, na.rm = TRUE),
    Included_SD = sd(included_data, na.rm = TRUE),
    Excluded_Mean = mean(excluded_data, na.rm = TRUE),
    Excluded_SD = sd(excluded_data, na.rm = TRUE),
    P_Value = test_result$p.value,
    Mean_Difference = diff(test_result$estimate)
  )
}))

# 3. Pre-clustering summary (full population analysis)
preclustering_summary <- do.call(rbind, lapply(metrics, function(m) {
  # By sex
  sex_test <- t.test(data[[m]] ~ data$gender)
  # By specialty
  spec_test <- t.test(data[[m]] ~ data$specialty)
  
  data.frame(
    Metric = m,
    Sex_P_Value = sex_test$p.value,
    Sex_Mean_Diff = diff(sex_test$estimate),
    Specialty_P_Value = spec_test$p.value,
    Specialty_Mean_Diff = diff(spec_test$estimate)
  )
}))

# 4. Missing data summary
missing_summary <- data.frame(
  Variable = names(data),
  Missing_Count = sapply(data, function(x) sum(is.na(x))),
  Missing_Percentage = sapply(data, function(x) mean(is.na(x)) * 100)
)

# Write CSV files
write.csv(demographic_summary, "demographic_comparisons.csv", row.names = FALSE)
write.csv(ehr_metrics_summary, "ehr_metrics_comparisons.csv", row.names = FALSE)
write.csv(preclustering_summary, "preclustering_analysis.csv", row.names = FALSE)
write.csv(missing_summary, "missing_data_analysis.csv", row.names = FALSE)

# Print confirmation and summary
print("Files have been written successfully. Summary of results:")
print("\nDemographic Summary:")
print(demographic_summary)
print("\nEHR Metrics Summary (first few rows):")
print(head(ehr_metrics_summary))
print("\nPre-clustering Summary (first few rows):")
print(head(preclustering_summary))
print("\nMissing Data Summary (first few rows):")
print(head(missing_summary))
