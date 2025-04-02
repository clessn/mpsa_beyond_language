###############################################################################
# CORPUS ANALYSIS - GEMINI SENTIMENT RESULTS
# 
# This script analyzes the results of the Gemini sentiment analysis run on the
# full corpus of French news articles. It generates summary statistics, visualizations,
# and comparative analysis between different prompt language conditions.
#
# Author: Ral Zarek
# Date: March 2025
###############################################################################

#==============================================================================
# 1. SETUP AND DEPENDENCIES
#==============================================================================

# Load necessary libraries for data manipulation and visualization
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping
library(ggplot2)    # Visualization
library(scales)     # Scale functions for visualizations
library(lubridate)  # Date handling

# Settings for visualizations
theme_set(theme_minimal())

#==============================================================================
# 2. DATA LOADING
#==============================================================================

# Load the sentiment results from both French and English prompts
df <- readRDS("data/clean/news_df_sentiment_gemini.rds")

# Check if the necessary sentiment columns exist
if (!all(c("gemini_fr_fr", "gemini_en_fr") %in% names(df))) {
  missing_cols <- setdiff(c("gemini_fr_fr", "gemini_en_fr"), names(df))
  stop("Missing required sentiment columns: ", paste(missing_cols, collapse=", "), 
       ". Please run both sentiment analysis scripts first.")
}

#==============================================================================
# 3. SUMMARY STATISTICS
#==============================================================================

# Overall statistics for French prompt
fr_stats <- df %>%
  summarize(
    Mean = mean(gemini_fr_fr, na.rm = TRUE),
    Median = median(gemini_fr_fr, na.rm = TRUE),
    SD = sd(gemini_fr_fr, na.rm = TRUE),
    Min = min(gemini_fr_fr, na.rm = TRUE),
    Max = max(gemini_fr_fr, na.rm = TRUE),
    NA_Count = sum(is.na(gemini_fr_fr)),
    NA_Pct = sum(is.na(gemini_fr_fr)) / n() * 100
  )

# Overall statistics for English prompt
en_stats <- df %>%
  summarize(
    Mean = mean(gemini_en_fr, na.rm = TRUE),
    Median = median(gemini_en_fr, na.rm = TRUE),
    SD = sd(gemini_en_fr, na.rm = TRUE),
    Min = min(gemini_en_fr, na.rm = TRUE),
    Max = max(gemini_en_fr, na.rm = TRUE),
    NA_Count = sum(is.na(gemini_en_fr)),
    NA_Pct = sum(is.na(gemini_en_fr)) / n() * 100
  )

# Create a table for easy comparison
summary_stats <- bind_rows(
  mutate(fr_stats, Prompt = "French"),
  mutate(en_stats, Prompt = "English")
) %>%
  select(Prompt, everything())

# Print the summary statistics
print(summary_stats)

#==============================================================================
# 4. CORRELATION ANALYSIS
#==============================================================================

# Calculate correlation between French and English prompt results
correlation <- cor(df$gemini_fr_fr, df$gemini_en_fr, use = "pairwise.complete.obs")
cat("Correlation between French and English prompt results:", correlation, "\n")

#==============================================================================
# 5. DISTRIBUTION VISUALIZATION
#==============================================================================

# Create histograms of sentiment distributions
p1 <- ggplot(df, aes(x = gemini_fr_fr)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Sentiment Scores (French Prompt)",
       x = "Sentiment Score", y = "Count") +
  theme_minimal()

p2 <- ggplot(df, aes(x = gemini_en_fr)) +
  geom_histogram(bins = 30, fill = "darkred", alpha = 0.7) +
  labs(title = "Distribution of Sentiment Scores (English Prompt)",
       x = "Sentiment Score", y = "Count") +
  theme_minimal()

# Display the histograms
print(p1)
print(p2)

# Visualization for comparing distributions
df_long <- df %>%
  select(gemini_fr_fr, gemini_en_fr) %>%
  pivot_longer(cols = c(gemini_fr_fr, gemini_en_fr),
               names_to = "prompt",
               values_to = "sentiment") %>%
  mutate(prompt = case_when(
    prompt == "gemini_fr_fr" ~ "French Prompt",
    prompt == "gemini_en_fr" ~ "English Prompt",
    TRUE ~ prompt
  ))

# Create overlapping density plot
p3 <- ggplot(df_long, aes(x = sentiment, fill = prompt)) +
  geom_density(alpha = 0.5) +
  labs(title = "Comparison of Sentiment Distributions by Prompt Language",
       x = "Sentiment Score", y = "Density", fill = "Prompt Language") +
  theme_minimal() +
  scale_fill_manual(values = c("French Prompt" = "steelblue", "English Prompt" = "darkred"))

# Display the comparison plot
print(p3)

#==============================================================================
# 6. TIME SERIES ANALYSIS
#==============================================================================

# Ensure date column is properly formatted
if ("date" %in% names(df)) {
  df <- df %>%
    mutate(date = as.Date(date))
  
  # Create monthly averages
  monthly_sentiment <- df %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(
      fr_sentiment = mean(gemini_fr_fr, na.rm = TRUE),
      en_sentiment = mean(gemini_en_fr, na.rm = TRUE),
      n_articles = n()
    ) %>%
    ungroup()
  
  # Plot sentiment over time
  monthly_long <- monthly_sentiment %>%
    pivot_longer(cols = c(fr_sentiment, en_sentiment),
                 names_to = "prompt",
                 values_to = "sentiment") %>%
    mutate(prompt = case_when(
      prompt == "fr_sentiment" ~ "French Prompt",
      prompt == "en_sentiment" ~ "English Prompt",
      TRUE ~ prompt
    ))
  
  p4 <- ggplot(monthly_long, aes(x = month, y = sentiment, color = prompt)) +
    geom_line() +
    geom_point() +
    labs(title = "Monthly Average Sentiment Over Time",
         x = "Date", y = "Average Sentiment", color = "Prompt Language") +
    theme_minimal() +
    scale_color_manual(values = c("French Prompt" = "steelblue", "English Prompt" = "darkred"))
  
  # Display the time series plot
  print(p4)
}

#==============================================================================
# 7. CONCORDANCE AND DISCORDANCE ANALYSIS
#==============================================================================

# Calculate the absolute difference between prompt conditions
df <- df %>%
  mutate(sentiment_diff = abs(gemini_fr_fr - gemini_en_fr))

# Summary of differences
diff_summary <- df %>%
  summarize(
    mean_diff = mean(sentiment_diff, na.rm = TRUE),
    median_diff = median(sentiment_diff, na.rm = TRUE),
    max_diff = max(sentiment_diff, na.rm = TRUE),
    min_diff = min(sentiment_diff, na.rm = TRUE)
  )

print("Summary of absolute differences between French and English prompts:")
print(diff_summary)

# Find articles with the largest sentiment differences
large_diff_articles <- df %>%
  filter(!is.na(sentiment_diff)) %>%
  arrange(desc(sentiment_diff)) %>%
  select(doc_id, title, date, gemini_fr_fr, gemini_en_fr, sentiment_diff) %>%
  head(10)

print("Top 10 articles with largest differences between prompt conditions:")
print(large_diff_articles)

# Create categories for directional differences
df <- df %>%
  mutate(diff_category = case_when(
    is.na(gemini_fr_fr) | is.na(gemini_en_fr) ~ "Missing Data",
    gemini_fr_fr > gemini_en_fr + 0.2 ~ "French More Positive",
    gemini_en_fr > gemini_fr_fr + 0.2 ~ "English More Positive",
    abs(gemini_fr_fr - gemini_en_fr) <= 0.2 ~ "Similar",
    TRUE ~ "Other"
  ))

# Summary of difference categories
diff_categories <- df %>%
  count(diff_category) %>%
  mutate(percentage = n / sum(n) * 100)

print("Distribution of sentiment difference categories:")
print(diff_categories)

# Visualization of category distribution
p5 <- ggplot(diff_categories, aes(x = reorder(diff_category, -n), y = n, fill = diff_category)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
  labs(title = "Distribution of Sentiment Difference Categories",
       x = NULL, y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

#==============================================================================
# 8. SAVE RESULTS
#==============================================================================

# Create a list with all analysis results
analysis_results <- list(
  summary_stats = summary_stats,
  correlation = correlation,
  diff_summary = diff_summary,
  large_diff_articles = large_diff_articles,
  diff_categories = diff_categories
)

# Save the analysis results
saveRDS(analysis_results, "data/clean/gemini_corpus_analysis.rds")

# Save plots
ggsave("results/graphs/gemini_fr_distribution.png", p1, width = 8, height = 6)
ggsave("results/graphs/gemini_en_distribution.png", p2, width = 8, height = 6)
ggsave("results/graphs/gemini_distribution_comparison.png", p3, width = 8, height = 6)
if (exists("p4")) {
  ggsave("results/graphs/gemini_time_series.png", p4, width = 10, height = 6)
}
ggsave("results/graphs/gemini_diff_categories.png", p5, width = 8, height = 6)

cat("Analysis complete. Results saved to data/clean/gemini_corpus_analysis.rds\n")
cat("Plots saved to results/graphs/\n")
