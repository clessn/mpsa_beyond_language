#################################################################
# F1 SCORES CATEGORY AVERAGES ANALYSIS
#################################################################
# This script calculates average F1 scores by sentiment category for the 7-category
# classification, excluding DeepSeek V3.2 and LSD models to focus on
# production-ready general-purpose LLMs.

library(tidyverse)

#----------------------------------------------------------------
# DATA LOADING
#----------------------------------------------------------------
detailed_f1_7 <- readRDS("results/analysis/f1_scores_detailed_7.rds")
detailed_f1_3 <- readRDS("results/analysis/f1_scores_detailed_3.rds")

print("All models in 7-category dataset:")
print(unique(detailed_f1_7$model))

#----------------------------------------------------------------
# DATA FILTERING
#----------------------------------------------------------------
# Filter out excluded models based on model names
detailed_f1_7_filtered <- detailed_f1_7 %>%
  filter(
    # Exclude DeepSeek V3.2 (reasoning-mode model)
    !str_detect(model, "deepseekv32"),
    # Exclude LSD dictionaries
    !str_detect(model, "^lsd_")
  )

detailed_f1_3_filtered <- detailed_f1_3 %>%
  filter(
    # Exclude DeepSeek V3.2 (reasoning-mode model)
    !str_detect(model, "deepseekv32"),
    # Exclude LSD dictionaries
    !str_detect(model, "^lsd_")
  )

cat("\nModels included in filtered analysis:\n")
print(unique(detailed_f1_7_filtered$model))
cat("Number of model-condition combinations (7-cat):", nrow(detailed_f1_7_filtered), "\n")
cat("Number of model-condition combinations (3-cat):", nrow(detailed_f1_3_filtered), "\n")

#----------------------------------------------------------------
# CATEGORY AVERAGES CALCULATION
#----------------------------------------------------------------
# Calculate averages for each category (7-category filtered)
category_averages_7_filtered <- detailed_f1_7_filtered %>%
  summarise(
    very_negative_avg = mean(very_negative, na.rm = TRUE),
    negative_avg = mean(negative, na.rm = TRUE),
    somewhat_negative_avg = mean(somewhat_negative, na.rm = TRUE),
    neutral_avg = mean(neutral, na.rm = TRUE),
    somewhat_positive_avg = mean(somewhat_positive, na.rm = TRUE),
    positive_avg = mean(positive, na.rm = TRUE),
    very_positive_avg = mean(very_positive, na.rm = TRUE)
  )

# Calculate averages for each category (3-category filtered)
category_averages_3_filtered <- detailed_f1_3_filtered %>%
  summarise(
    negative_avg = mean(negative, na.rm = TRUE),
    neutral_avg = mean(neutral, na.rm = TRUE),
    positive_avg = mean(positive, na.rm = TRUE)
  )

#----------------------------------------------------------------
# RESULTS REPORTING
#----------------------------------------------------------------
cat("\n=== 7-CATEGORY CLASSIFICATION ===\n")
cat("Filtered Average F1 scores by category (excluding QwQ, DeepSeek R1, and LSD):\n")
cat(sprintf("Very Negative: %.3f\n", category_averages_7_filtered$very_negative_avg))
cat(sprintf("Negative: %.3f\n", category_averages_7_filtered$negative_avg))
cat(sprintf("Somewhat Negative: %.3f\n", category_averages_7_filtered$somewhat_negative_avg))
cat(sprintf("Neutral: %.3f\n", category_averages_7_filtered$neutral_avg))
cat(sprintf("Somewhat Positive: %.3f\n", category_averages_7_filtered$somewhat_positive_avg))
cat(sprintf("Positive: %.3f\n", category_averages_7_filtered$positive_avg))
cat(sprintf("Very Positive: %.3f\n", category_averages_7_filtered$very_positive_avg))

# Find which categories performed best and worst (7-category filtered)
categories_7_filtered <- c("very_negative" = category_averages_7_filtered$very_negative_avg,
                "negative" = category_averages_7_filtered$negative_avg,
                "somewhat_negative" = category_averages_7_filtered$somewhat_negative_avg,
                "neutral" = category_averages_7_filtered$neutral_avg,
                "somewhat_positive" = category_averages_7_filtered$somewhat_positive_avg,
                "positive" = category_averages_7_filtered$positive_avg,
                "very_positive" = category_averages_7_filtered$very_positive_avg)

cat("\nRanked from best to worst performance (7-category):\n")
ranked_7_filtered <- sort(categories_7_filtered, decreasing = TRUE)
for(i in 1:length(ranked_7_filtered)) {
  cat(sprintf("%d. %s: %.3f\n", i, names(ranked_7_filtered)[i], ranked_7_filtered[i]))
}

cat("\n=== 3-CATEGORY CLASSIFICATION ===\n")
cat("Filtered Average F1 scores by category (excluding QwQ, DeepSeek R1, and LSD):\n")
cat(sprintf("Negative: %.3f\n", category_averages_3_filtered$negative_avg))
cat(sprintf("Neutral: %.3f\n", category_averages_3_filtered$neutral_avg))
cat(sprintf("Positive: %.3f\n", category_averages_3_filtered$positive_avg))

# Find which categories performed best and worst (3-category filtered)
categories_3_filtered <- c("negative" = category_averages_3_filtered$negative_avg,
                "neutral" = category_averages_3_filtered$neutral_avg,
                "positive" = category_averages_3_filtered$positive_avg)

cat("\nRanked from best to worst performance (3-category):\n")
ranked_3_filtered <- sort(categories_3_filtered, decreasing = TRUE)
for(i in 1:length(ranked_3_filtered)) {
  cat(sprintf("%d. %s: %.3f\n", i, names(ranked_3_filtered)[i], ranked_3_filtered[i]))
}

#----------------------------------------------------------------
# SAVE RESULTS
#----------------------------------------------------------------
# Save the filtered category averages for later use
results_list <- list(
  category_averages_7 = category_averages_7_filtered,
  category_averages_3 = category_averages_3_filtered
)
saveRDS(results_list, "results/analysis/f1_category_averages_filtered.rds")