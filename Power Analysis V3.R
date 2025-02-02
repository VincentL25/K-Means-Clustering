if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr")
}
library(pwr)

#############################
# Helper Functions
#############################

# Function to calculate Cohen's d effect size using pooled SD
calculate_cohens_d <- function(m1, m2, sd1, sd2, n1, n2) {
  # Pooled standard deviation
  sd_pooled <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
  # Cohen's d
  d <- abs(m1 - m2) / sd_pooled
  return(d)
}

# Function to calculate achieved power
calculate_power <- function(n1, n2, d, sig_level = 0.05) {
  pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = sig_level)$power
}

# Function to calculate minimum detectable effect size
min_effect_size <- function(n1, n2, power = 0.80, sig_level = 0.05) {
  pwr.t2n.test(n1 = n1, n2 = n2, power = power, sig.level = sig_level)$d
}

#############################
# Between-Cluster Comparisons
#############################

# Define metrics data
between_cluster_metrics <- list(
  "Time on Unscheduled Days" = list(
    cluster1 = c(mean = 84, sd = 43, n = 81),
    cluster2 = c(mean = 32, sd = 21, n = 242)
  ),
  "Pajama Time" = list(
    cluster1 = c(mean = 86, sd = 53, n = 81),
    cluster2 = c(mean = 27, sd = 23, n = 242)
  ),
  "Time Outside 7AM-7PM" = list(
    cluster1 = c(mean = 44, sd = 25, n = 81),
    cluster2 = c(mean = 16, sd = 14, n = 242)
  ),
  "Time in In Basket" = list(
    cluster1 = c(mean = 26, sd = 12, n = 81),
    cluster2 = c(mean = 10, sd = 7, n = 242)
  ),
  "Messages Received" = list(
    cluster1 = c(mean = 37, sd = 19, n = 81),
    cluster2 = c(mean = 16, sd = 10, n = 242)
  ),
  "Turnaround Time" = list(
    cluster1 = c(mean = 5.9, sd = 8.2, n = 81),
    cluster2 = c(mean = 6.6, sd = 12.1, n = 242)
  )
)

# Adjusted alpha level for multiple comparisons
adjusted_alpha <- 0.05 / length(between_cluster_metrics)

# Calculate effect sizes and power for between-cluster comparisons
between_cluster_results <- data.frame(
  Metric = character(),
  Cohens_d = numeric(),
  Power_adjusted = numeric(),
  Power_unadjusted = numeric(),
  stringsAsFactors = FALSE
)

for (metric_name in names(between_cluster_metrics)) {
  metric <- between_cluster_metrics[[metric_name]]
  
  d <- calculate_cohens_d(
    m1 = metric$cluster1["mean"],
    m2 = metric$cluster2["mean"],
    sd1 = metric$cluster1["sd"],
    sd2 = metric$cluster2["sd"],
    n1 = metric$cluster1["n"],
    n2 = metric$cluster2["n"]
  )
  
  power_adjusted <- calculate_power(
    n1 = metric$cluster1["n"],
    n2 = metric$cluster2["n"],
    d = d,
    sig_level = adjusted_alpha
  )
  
  power_unadjusted <- calculate_power(
    n1 = metric$cluster1["n"],
    n2 = metric$cluster2["n"],
    d = d,
    sig_level = 0.05
  )
  
  between_cluster_results <- rbind(
    between_cluster_results,
    data.frame(
      Metric = metric_name,
      Cohens_d = round(d, 3),
      Power_adjusted = round(power_adjusted, 3),
      Power_unadjusted = round(power_unadjusted, 3),
      stringsAsFactors = FALSE
    )
  )
}

#############################
# Within-Cluster Comparisons
#############################

# Cluster 1 Sex Comparisons (37 male vs 44 female)
cluster1_sex_power <- pwr.t2n.test(
  n1 = 37,
  n2 = 44,
  d = 0.7,  # Medium-to-large effect
  sig.level = 0.05,
  power = NULL
)

# Cluster 1 Specialty Comparisons (53 primary vs 28 specialty)
cluster1_specialty_power <- pwr.t2n.test(
  n1 = 53,
  n2 = 28,
  d = 0.8,  # Large effect
  sig.level = 0.05,
  power = NULL
)

# Cluster 2 Sex Comparisons (115 male vs 127 female)
cluster2_sex_power <- pwr.t2n.test(
  n1 = 115,
  n2 = 127,
  d = 0.4,  # Small-to-medium effect
  sig.level = 0.05,
  power = NULL
)

# Cluster 2 Specialty Comparisons (59 primary vs 183 specialty)
cluster2_specialty_power <- pwr.t2n.test(
  n1 = 59,
  n2 = 183,
  d = 0.5,  # Medium effect
  sig.level = 0.05,
  power = NULL
)

#############################
# Minimum Detectable Effect Sizes
#############################

min_effects <- data.frame(
  Comparison = c(
    "Cluster 1 - Sex",
    "Cluster 1 - Specialty",
    "Cluster 2 - Sex",
    "Cluster 2 - Specialty"
  ),
  n1 = c(37, 53, 115, 59),
  n2 = c(44, 28, 127, 183),
  stringsAsFactors = FALSE
)

min_effects$min_d <- mapply(
  min_effect_size,
  min_effects$n1,
  min_effects$n2
)

#############################
# Print Results
#############################

# Between-cluster results
cat("\nBETWEEN-CLUSTER COMPARISONS (alpha = 0.05/6 for adjusted power):\n")
print(between_cluster_results)

# Within-cluster results
cat("\nWITHIN-CLUSTER COMPARISONS:\n")
cat("\nCluster 1:")
cat("\nSex comparisons (n1=37, n2=44):")
cat("\nPower to detect d=0.7:", round(cluster1_sex_power$power, 3))
cat("\nSpecialty comparisons (n1=53, n2=28):")
cat("\nPower to detect d=0.8:", round(cluster1_specialty_power$power, 3))

cat("\n\nCluster 2:")
cat("\nSex comparisons (n1=115, n2=127):")
cat("\nPower to detect d=0.4:", round(cluster2_sex_power$power, 3))
cat("\nSpecialty comparisons (n1=59, n2=183):")
cat("\nPower to detect d=0.5:", round(cluster2_specialty_power$power, 3))

# Minimum detectable effect sizes
cat("\n\nMINIMUM DETECTABLE EFFECT SIZES (80% power):\n")
print(round(min_effects[, -1], 3)) # Exclude the non-numeric column

