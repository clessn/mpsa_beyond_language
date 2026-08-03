#################################################################
# CUSTOM RESULTS SUMMARY
#################################################################
# This script compiles and summarizes specific performance metrics from the
# various analysis methods, excluding QwQ, Deepseek R1, English LSD and French LSD.
# It calculates average correlation, average F1 score and average MAE for FRFR, ENFR, ENEN.

library(tidyverse)

#----------------------------------------------------------------
# DATA LOADING
#----------------------------------------------------------------
# Load the result files from previous analyses
cor_results <- readRDS("data/clean/cor_results.rds")
f1_scores_7 <- readRDS("results/analysis/f1_scores_7.rds")
f1_scores_3 <- readRDS("results/analysis/f1_scores_3.rds")

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
    str_replace("^Dictionary \\(en", "Dictionary (EN)") %>%
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
# DATA PREPARATION
#----------------------------------------------------------------
# Process correlation results
correlation_summary <- cor_results %>%
  mutate(
    clean_model = clean_model_name(model),
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "FR→FR",
      str_detect(model, "_en_fr$") ~ "EN→FR",
      str_detect(model, "_en_en$") ~ "EN→EN",
      str_detect(model, "^lsd_fr") ~ "Dictionary (FR)",
      str_detect(model, "^lsd_en") ~ "Dictionary (EN)",
      TRUE ~ "Other"
    )
  )

# Process F1 score results (7 categories)
f1_7_summary <- f1_scores_7 %>%
  mutate(
    clean_model = clean_model_name(model),
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "FR→FR",
      str_detect(model, "_en_fr$") ~ "EN→FR",
      str_detect(model, "_en_en$") ~ "EN→EN",
      str_detect(model, "^lsd_fr") ~ "Dictionary (FR)",
      str_detect(model, "^lsd_en") ~ "Dictionary (EN)",
      TRUE ~ "Other"
    )
  )

# Process F1 score results (3 categories)
f1_3_summary <- f1_scores_3 %>%
  mutate(
    clean_model = clean_model_name(model),
    condition = case_when(
      str_detect(model, "_fr_fr$") ~ "FR→FR",
      str_detect(model, "_en_fr$") ~ "EN→FR",
      str_detect(model, "_en_en$") ~ "EN→EN",
      str_detect(model, "^lsd_fr") ~ "Dictionary (FR)",
      str_detect(model, "^lsd_en") ~ "Dictionary (EN)",
      TRUE ~ "Other"
    )
  )

#----------------------------------------------------------------
# FILTERED ANALYSIS
#----------------------------------------------------------------
# Function to filter out excluded models
filter_excluded_models <- function(df) {
  df %>%
    filter(
      # Exclude DeepSeek V3.2 (dual reasoning/non-reasoning mode, prone to
      # inconsistent output formatting, same rationale as the previous
      # batch's exclusion of QwQ-32B and DeepSeek R1 Basic)
      !str_detect(clean_model, "deepseekv32"),
      # Exclude LSD dictionaries
      !str_detect(clean_model, "Dictionary")
    )
}

# Apply filtering to all datasets
correlation_filtered <- filter_excluded_models(correlation_summary)
f1_7_filtered <- filter_excluded_models(f1_7_summary)
f1_3_filtered <- filter_excluded_models(f1_3_summary)

#----------------------------------------------------------------
# SUMMARY BY CONDITION
#----------------------------------------------------------------
# Calculate average correlation by condition
cor_by_condition <- correlation_filtered %>%
  group_by(condition) %>%
  summarize(
    avg_correlation = mean(correlation, na.rm = TRUE),
    avg_mae = mean(mae, na.rm = TRUE),
    models_count = n(),
    .groups = "drop"
  ) %>%
  arrange(condition)

# Calculate average F1 score (7-cat) by condition
f1_7_by_condition <- f1_7_filtered %>%
  group_by(condition) %>%
  summarize(
    avg_f1_score = mean(weighted_f1, na.rm = TRUE),
    models_count = n(),
    .groups = "drop"
  ) %>%
  arrange(condition)

# Calculate average F1 score (3-cat) by condition
f1_3_by_condition <- f1_3_filtered %>%
  group_by(condition) %>%
  summarize(
    avg_f1_score = mean(weighted_f1, na.rm = TRUE),
    models_count = n(),
    .groups = "drop"
  ) %>%
  arrange(condition)

#----------------------------------------------------------------
# PRINT RESULTS
#----------------------------------------------------------------
cat("\n=== AVERAGE METRICS BY CONDITION (EXCLUDING deepseekv32 AND DICTIONARIES) ===\n\n")

cat("CORRELATION AND MAE BY CONDITION:\n")
print(cor_by_condition)

cat("\nF1 SCORE (7 CATEGORIES) BY CONDITION:\n")
print(f1_7_by_condition)

cat("\nF1 SCORE (3 CATEGORIES) BY CONDITION:\n")
print(f1_3_by_condition)

#----------------------------------------------------------------
# DETAILED ANALYSIS
#----------------------------------------------------------------
# Show all models included in the filtered analysis
cat("\n=== MODELS INCLUDED IN ANALYSIS ===\n")
unique_models <- correlation_filtered %>%
  select(clean_model) %>%
  distinct() %>%
  arrange(clean_model)
print(unique_models)