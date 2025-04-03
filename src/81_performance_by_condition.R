#################################################################
# PERFORMANCE BY CONDITION ANALYSIS
#################################################################
# This script analyzes model performance metrics (correlation, MAE, F1-scores)
# grouped by the three main prompt-text language conditions:
# 1. French Prompts for French Text (FR→FR)
# 2. English Prompts for French Text (EN→FR)
# 3. English Prompts for English Text (EN→EN)

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization

#################################################################
# LOAD DATASETS WITH PERFORMANCE METRICS
#################################################################
# Load correlation and error metrics
cor_results <- readRDS("data/clean/cor_results.rds")

# Load F1 scores for 7-category and 3-category sentiment analysis
f1_scores_7 <- readRDS("results/analysis/f1_scores_7.rds")
f1_scores_3 <- readRDS("results/analysis/f1_scores_3.rds")

#################################################################
# DATA PREPARATION
#################################################################
# Function to classify models by condition
classify_condition <- function(model_name) {
  case_when(
    grepl("_fr_fr$", model_name) ~ "FR→FR",  # French prompt for French text
    grepl("_en_fr$", model_name) ~ "EN→FR",  # English prompt for French text
    grepl("_en_en$", model_name) ~ "EN→EN",  # English prompt for English text
    grepl("^lsd_fr", model_name) ~ "Dictionary (FR)",  # French dictionary
    grepl("^lsd_en", model_name) ~ "Dictionary (EN)",  # English dictionary
    TRUE ~ "Other"
  )
}

# Function to prepare metrics dataframe
prepare_metrics <- function(data, metric_name, metric_column) {
  data %>%
    mutate(
      condition = classify_condition(model),
      metric = metric_name,
      value = .data[[metric_column]]
    ) %>%
    select(model, condition, metric, value)
}

# Prepare correlation data
corr_data <- prepare_metrics(cor_results, "Correlation", "correlation")

# Prepare MAE data
mae_data <- prepare_metrics(cor_results, "MAE", "mae")

# Prepare F1 scores data (7-category)
f1_7_data <- prepare_metrics(f1_scores_7, "F1 Score (7-cat)", "weighted_f1")

# Prepare F1 scores data (3-category)
f1_3_data <- prepare_metrics(f1_scores_3, "F1 Score (3-cat)", "weighted_f1")

# Combine all metrics into a single dataframe
all_metrics <- bind_rows(corr_data, mae_data, f1_7_data, f1_3_data)

# Filter out dictionary models and other non-standard conditions for focus on the three main conditions
main_conditions <- c("FR→FR", "EN→FR", "EN→EN")
filtered_metrics <- all_metrics %>%
  filter(condition %in% main_conditions)

# Extract model identifier without condition suffix
filtered_metrics <- filtered_metrics %>%
  mutate(
    base_model = str_replace(model, "_(fr|en)_(fr|en)$", "")
  )

#################################################################
# CALCULATE SUMMARY STATISTICS BY CONDITION
#################################################################
# Calculate average performance by condition for each metric
condition_summary <- filtered_metrics %>%
  group_by(metric, condition) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(metric, desc(mean_value))

# Display summary by condition
print("Performance Summary by Condition:")
print(condition_summary)

#################################################################
# MODEL-SPECIFIC CONDITION COMPARISON
#################################################################
# For each model, compare performance across conditions
model_condition_comparison <- filtered_metrics %>%
  # Pivot to wide format to compare conditions side by side
  pivot_wider(
    id_cols = c(base_model, metric),
    names_from = condition,
    values_from = value
  ) %>%
  # Calculate differences between conditions
  mutate(
    FR_FR_vs_EN_FR = `FR→FR` - `EN→FR`,
    FR_FR_vs_EN_EN = `FR→FR` - `EN→EN`,
    EN_FR_vs_EN_EN = `EN→FR` - `EN→EN`
  ) %>%
  # Sort by metric and then by model name
  arrange(metric, base_model)

# Display model-specific condition comparisons
print("Model-Specific Condition Comparison:")
print(model_condition_comparison)

#################################################################
# CONDITION PERFORMANCE RANKING
#################################################################
# Rank conditions by performance for each metric
condition_ranking <- filtered_metrics %>%
  group_by(metric, base_model) %>%
  # Rank conditions for each model (handling MAE differently as lower is better)
  mutate(
    rank = if_else(
      metric == "MAE",
      rank(value, ties.method = "min"),  # For MAE, lower is better
      rank(-value, ties.method = "min")  # For others, higher is better
    )
  ) %>%
  # Count how often each condition achieves each rank
  ungroup() %>%
  count(metric, condition, rank) %>%
  # Pivot to see rank distribution by condition
  pivot_wider(
    id_cols = c(metric, condition),
    names_from = rank,
    values_from = n,
    names_prefix = "rank_",
    values_fill = 0
  ) %>%
  # Calculate a weighted score (rank 1 is best)
  mutate(
    models_ranked = rank_1 + rank_2 + rank_3,
    weighted_score = (3 * rank_1 + 2 * rank_2 + 1 * rank_3) / models_ranked
  ) %>%
  # Sort by metric and then by weighted score (descending)
  arrange(metric, desc(weighted_score))

# Display condition ranking
print("Condition Performance Ranking:")
print(condition_ranking)

#################################################################
# STATISTICAL SIGNIFICANCE TESTING
#################################################################
# Paired t-tests to assess if differences between conditions are statistically significant
# Prepare a results dataframe for t-tests
t_test_results <- data.frame(
  metric = character(),
  comparison = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  significant = logical(),
  stringsAsFactors = FALSE
)

# Function to run paired t-test between two conditions
run_paired_ttest <- function(data, metric_filter, cond1, cond2) {
  # Filter data for the specific metric
  test_data <- data %>%
    filter(metric == metric_filter) %>%
    select(base_model, condition, value) %>%
    pivot_wider(
      id_cols = base_model,
      names_from = condition,
      values_from = value
    )
  
  # Only include models that have data for both conditions
  test_data <- test_data %>%
    filter(!is.na(.data[[cond1]]) & !is.na(.data[[cond2]]))
  
  # Skip if insufficient data
  if(nrow(test_data) < 3) {
    return(data.frame(
      metric = metric_filter,
      comparison = paste(cond1, "vs", cond2),
      t_statistic = NA,
      p_value = NA,
      significant = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Run paired t-test
  test_result <- t.test(test_data[[cond1]], test_data[[cond2]], paired = TRUE)
  
  # Return results
  data.frame(
    metric = metric_filter,
    comparison = paste(cond1, "vs", cond2),
    t_statistic = test_result$statistic,
    p_value = test_result$p.value,
    significant = test_result$p.value < 0.05,
    stringsAsFactors = FALSE
  )
}

# Metrics to test
metrics_to_test <- unique(filtered_metrics$metric)

# Run t-tests for all metric and condition pairs
for(m in metrics_to_test) {
  # FR→FR vs EN→FR
  test1 <- run_paired_ttest(filtered_metrics, m, "FR→FR", "EN→FR")
  t_test_results <- rbind(t_test_results, test1)
  
  # FR→FR vs EN→EN
  test2 <- run_paired_ttest(filtered_metrics, m, "FR→FR", "EN→EN")
  t_test_results <- rbind(t_test_results, test2)
  
  # EN→FR vs EN→EN
  test3 <- run_paired_ttest(filtered_metrics, m, "EN→FR", "EN→EN")
  t_test_results <- rbind(t_test_results, test3)
}

# Display t-test results
print("Statistical Significance of Condition Differences:")
print(t_test_results)

#################################################################
# SAVE ANALYSIS RESULTS
#################################################################
# Combine all results into a list
condition_analysis_results <- list(
  summary = condition_summary,
  model_comparison = model_condition_comparison,
  ranking = condition_ranking,
  significance_tests = t_test_results
)

# Save the results
saveRDS(condition_analysis_results, "results/analysis/condition_analysis_results.rds")

# Print final conclusion
cat("\n=== CONCLUSION ===\n")
cat("Analysis of model performance across different prompt-text language conditions is complete.\n")
cat("Results indicate which approach (FR→FR, EN→FR, or EN→EN) yields the best performance\n")
cat("for sentiment analysis tasks based on correlation, MAE, and F1 scores.\n")