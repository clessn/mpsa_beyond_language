#################################################################
# RESULTS SUMMARY
#################################################################
# This script compiles and summarizes all performance metrics from the various
# analysis methods. It calculates aggregate statistics, identifies top performing
# models, and generates comprehensive summaries by model type and condition.

library(tidyverse)

#----------------------------------------------------------------
# DATA LOADING
#----------------------------------------------------------------
# Load the result files from previous analyses
cor_results <- readRDS("data/clean/cor_results.rds")
f1_scores_7 <- readRDS("results/analysis/f1_scores_7.rds")
f1_scores_detailed_7 <- readRDS("results/analysis/f1_scores_detailed_7.rds")
f1_scores_3 <- readRDS("results/analysis/f1_scores_3.rds")
f1_scores_detailed_3 <- readRDS("results/analysis/f1_scores_detailed_3.rds")

#----------------------------------------------------------------
# MODEL NAME STANDARDIZATION
#----------------------------------------------------------------
# Function to clean and standardize model names across datasets
clean_model_name <- function(model_name) {
  model_name %>%
    # Remove any binary indicators
    str_remove("_bin$") %>%
    # Format the model conditions consistently
    str_replace("_en_fr$", " (EN→FR)") %>%
    str_replace("_fr_fr$", " (FR→FR)") %>%
    str_replace("_en_en$", " (EN→EN)") %>%
    str_replace("^lsd_", "Dictionary (") %>%
    str_replace("^Dictionary \\(fr", "Dictionary (FR") %>%
    str_replace("^Dictionary \\(en", "Dictionary (EN") %>%
    # Fix dictionary parentheses if needed
    sapply(function(x) {
      if(str_detect(x, "^Dictionary \\(") && !str_detect(x, "\\)$")) {
        paste0(x, ")")
      } else {
        x
      }
    })
}

#----------------------------------------------------------------
# CORRELATION RESULTS PROCESSING
#----------------------------------------------------------------
# Process and format correlation results
correlation_summary <- cor_results %>%
  mutate(
    clean_model = clean_model_name(model),
    # Determine prompt condition
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "French Prompt → French Text",
      str_detect(model, "_en_fr$") ~ "English Prompt → French Text",
      str_detect(model, "_en_en$") ~ "English Prompt → English Text",
      str_detect(model, "^lsd_fr") ~ "Dictionary (French)",
      str_detect(model, "^lsd_en") ~ "Dictionary (English)",
      TRUE ~ "Other"
    ),
    # Extract base model name without condition
    base_model = str_remove(clean_model, " \\(.*\\)$"),
    # Format significance for reporting
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # Format correlation for reporting
    corr_formatted = sprintf("%.3f%s", correlation, significance)
  ) %>%
  arrange(desc(abs(correlation)))

#----------------------------------------------------------------
# F1 SCORE PROCESSING (7 CATEGORIES)
#----------------------------------------------------------------
# Process and format 7-category F1 score results
f1_7_summary <- f1_scores_7 %>%
  mutate(
    clean_model = clean_model_name(model),
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "French Prompt → French Text",
      str_detect(model, "_en_fr$") ~ "English Prompt → French Text",
      str_detect(model, "_en_en$") ~ "English Prompt → English Text",
      str_detect(model, "^lsd_fr") ~ "Dictionary (French)",
      str_detect(model, "^lsd_en") ~ "Dictionary (English)",
      TRUE ~ "Other"
    ),
    base_model = str_remove(clean_model, " \\(.*\\)$"),
    # Format F1 score for reporting
    f1_formatted = sprintf("%.3f", weighted_f1)
  ) %>%
  arrange(desc(weighted_f1))

#----------------------------------------------------------------
# F1 SCORE PROCESSING (3 CATEGORIES)
#----------------------------------------------------------------
# Process and format 3-category F1 score results
f1_3_summary <- f1_scores_3 %>%
  mutate(
    clean_model = clean_model_name(model),
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "French Prompt → French Text",
      str_detect(model, "_en_fr$") ~ "English Prompt → French Text",
      str_detect(model, "_en_en$") ~ "English Prompt → English Text",
      str_detect(model, "^lsd_fr") ~ "Dictionary (French)",
      str_detect(model, "^lsd_en") ~ "Dictionary (English)",
      TRUE ~ "Other"
    ),
    base_model = str_remove(clean_model, " \\(.*\\)$"),
    # Format F1 score for reporting
    f1_formatted = sprintf("%.3f", weighted_f1)
  ) %>%
  arrange(desc(weighted_f1))

#----------------------------------------------------------------
# COMBINED PERFORMANCE METRICS
#----------------------------------------------------------------
# Create a unified dataframe with all performance metrics
combined_ranking <- bind_rows(
  correlation_summary %>% 
    select(metric = "correlation", value = correlation, clean_model, condition, base_model) %>%
    mutate(metric_type = "Correlation"),
  
  f1_7_summary %>% 
    select(metric = "weighted_f1", value = weighted_f1, clean_model, condition, base_model) %>%
    mutate(metric_type = "F1 Score (7-cat)"),
  
  f1_3_summary %>% 
    select(metric = "weighted_f1", value = weighted_f1, clean_model, condition, base_model) %>%
    mutate(metric_type = "F1 Score (3-cat)")
)

#----------------------------------------------------------------
# MODEL-CONDITION ANALYSIS
#----------------------------------------------------------------
# Analyze performance grouped by model and prompt condition
# Include all models except QwQ and Deepseek R1 Basic
model_condition_summary <- combined_ranking %>%
  # Filter out QwQ and Deepseek R1 Basic models
  filter(!str_detect(base_model, "QwQ|Deepseek R1 Basic")) %>%
  group_by(base_model, condition, metric_type) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(metric_type, desc(mean_value))

#----------------------------------------------------------------
# TOP PERFORMERS IDENTIFICATION
#----------------------------------------------------------------
# Find top performing models for each metric type
# Filter out QwQ and Deepseek R1 Basic models
top_performers <- combined_ranking %>%
  filter(!str_detect(base_model, "QwQ|Deepseek R1 Basic")) %>%
  group_by(metric_type) %>%
  slice_max(order_by = abs(value), n = 5) %>%
  ungroup() %>%
  select(metric_type, clean_model, value, condition) %>%
  arrange(metric_type, desc(abs(value)))

#----------------------------------------------------------------
# CONDITION SUMMARY ANALYSIS
#----------------------------------------------------------------
# Analyze performance by prompt and text language condition
# Include all models except QwQ and Deepseek R1 Basic
condition_summary <- combined_ranking %>%
  # Filter out QwQ and Deepseek R1 Basic models
  filter(!str_detect(base_model, "QwQ|Deepseek R1 Basic")) %>%
  group_by(condition, metric_type) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(metric_type, desc(mean_value))

#----------------------------------------------------------------
# DETAILED F1 SCORE ANALYSIS
#----------------------------------------------------------------
# Detailed analysis of F1 scores by sentiment category
detailed_f1_7_summary <- f1_scores_detailed_7 %>%
  mutate(clean_model = clean_model_name(model)) %>%
  arrange(desc(weighted_f1))

detailed_f1_3_summary <- f1_scores_detailed_3 %>%
  mutate(clean_model = clean_model_name(model)) %>%
  arrange(desc(weighted_f1))

#----------------------------------------------------------------
# RESULTS REPORTING
#----------------------------------------------------------------
# Print summary reports to console

# Print top performers
cat("\n=== TOP PERFORMERS BY METRIC ===\n")
print(top_performers)

# Print condition summary
cat("\n=== PERFORMANCE BY CONDITION ===\n")
print(condition_summary)

# Print detailed metrics for top models
cat("\n=== DETAILED METRICS FOR TOP MODELS ===\n")

# Top correlation models
cat("\nTop Correlation Models:\n")
correlation_summary %>% 
  slice_head(n = 5) %>%
  select(clean_model, correlation, p_value, mae, rmse) %>%
  print()

# Top F1 (7-cat) models
cat("\nTop F1 Score (7-cat) Models:\n")
f1_7_summary %>% 
  slice_head(n = 5) %>%
  select(clean_model, weighted_f1) %>%
  print()

# Top F1 (3-cat) models
cat("\nTop F1 Score (3-cat) Models:\n")
f1_3_summary %>% 
  slice_head(n = 5) %>%
  select(clean_model, weighted_f1) %>%
  print()

#----------------------------------------------------------------
# SAVE RESULTS
#----------------------------------------------------------------
# Save the processed summary data for later use in reports
results_output <- list(
  correlation = correlation_summary,
  f1_7 = f1_7_summary,
  f1_3 = f1_3_summary,
  detailed_f1_7 = detailed_f1_7_summary,
  detailed_f1_3 = detailed_f1_3_summary,
  top_performers = top_performers,
  condition_summary = condition_summary,
  model_condition_summary = model_condition_summary
)

saveRDS(results_output, "results/analysis/results_summary.rds")