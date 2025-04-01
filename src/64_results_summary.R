# Results Summary Script
# This script extracts and summarizes the performance metrics from all analysis methods

library(tidyverse)

# Load the result files
cor_results <- readRDS("data/clean/cor_results.rds")
f1_scores_7 <- readRDS("results/fscores/f1_scores_7.rds")
f1_scores_detailed_7 <- readRDS("results/fscores/f1_scores_detailed_7.rds")
f1_scores_3 <- readRDS("results/fscores/f1_scores_3.rds")
f1_scores_detailed_3 <- readRDS("results/fscores/f1_scores_detailed_3.rds")

# Clean up model names across datasets to ensure consistency
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
    # If it ends with a closing paren, leave it, otherwise add it
    {if(!str_detect(., "\\)$") && str_detect(., "Dictionary \\(")) paste0(., ")") else .}
}

# Process correlation results
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

# Process F1 score results (7 categories)
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

# Process F1 score results (3 categories)
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

# Create a combined ranking summary
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

# Group and summarize by model and condition
model_condition_summary <- combined_ranking %>%
  group_by(base_model, condition, metric_type) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(metric_type, desc(mean_value))

# Identify top 3 performers by metric type
top_performers <- combined_ranking %>%
  group_by(metric_type) %>%
  slice_max(order_by = abs(value), n = 5) %>%
  ungroup() %>%
  select(metric_type, clean_model, value, condition) %>%
  arrange(metric_type, desc(abs(value)))

# Summarize performance by condition (prompt language and text language)
condition_summary <- combined_ranking %>%
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

# Detailed F1 scores by sentiment category (7-cat)
detailed_f1_7_summary <- f1_scores_detailed_7 %>%
  mutate(clean_model = clean_model_name(model)) %>%
  arrange(desc(weighted_f1))

# Detailed F1 scores by sentiment category (3-cat)
detailed_f1_3_summary <- f1_scores_detailed_3 %>%
  mutate(clean_model = clean_model_name(model)) %>%
  arrange(desc(weighted_f1))

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

# Save the processed data for easy access
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
