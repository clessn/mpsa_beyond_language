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

# Create publication-quality histograms with consistent social science styling
create_pub_theme <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "#636363", margin = margin(b = 10)),
      plot.caption = element_text(size = 9, color = "#636363", hjust = 0, margin = margin(t = 10)),
      axis.title.x = element_text(size = 11, margin = margin(t = 10)),
      axis.title.y = element_text(size = 11, margin = margin(r = 10)),
      axis.text = element_text(size = 10, color = "#252525"),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Create histogram for French prompt
p1 <- ggplot(df, aes(x = gemini_fr_fr)) +
  # Add reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  # Create histogram with better aesthetics
  geom_histogram(bins = 30, 
                 fill = "#1F78B4", 
                 color = "white", 
                 alpha = 0.8,
                 boundary = 0) +
  # Add density curve overlay
  geom_density(aes(y = ..count.. * 0.35), 
               color = "#08519C", 
               linewidth = 1, 
               alpha = 0) +
  # Professional labeling
  labs(title = "Distribution of Sentiment Scores",
       subtitle = "Gemini 2.0 Flash with French Prompt",
       x = "Sentiment Score", 
       y = "Number of Articles",
       caption = "Negative scores indicate negative sentiment; positive scores indicate positive sentiment") +
  # Consistent scale
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  # Apply publication theme
  create_pub_theme()

# Create histogram for English prompt with matching design
p2 <- ggplot(df, aes(x = gemini_en_fr)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  geom_histogram(bins = 30, 
                 fill = "#D62728", 
                 color = "white", 
                 alpha = 0.8,
                 boundary = 0) +
  geom_density(aes(y = ..count.. * 0.35), 
               color = "#A50F15", 
               linewidth = 1, 
               alpha = 0) +
  labs(title = "Distribution of Sentiment Scores",
       subtitle = "Gemini 2.0 Flash with English Prompt",
       x = "Sentiment Score", 
       y = "Number of Articles",
       caption = "Negative scores indicate negative sentiment; positive scores indicate positive sentiment") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  create_pub_theme()

# Display the histograms
print(p1)
print(p2)

# Load LSD English sentiment for comparison if available
df_lsd_en <- tryCatch({
  readRDS("data/tmp/news_df_tone_index.rds") %>%
    select(doc_id, en_tone_index) %>%
    rename(lsd_en = en_tone_index)
}, error = function(e) {
  message("LSD English sentiment data not found, proceeding without it.")
  data.frame(doc_id = character(0), lsd_en = numeric(0))
})

# Join LSD English data with Gemini data if available
if (nrow(df_lsd_en) > 0) {
  df <- df %>% left_join(df_lsd_en, by = "doc_id")
  
  # Prepare data for comparing distributions with all three methods
  df_long <- df %>%
    select(gemini_fr_fr, gemini_en_fr, lsd_en) %>%
    pivot_longer(cols = c(gemini_fr_fr, gemini_en_fr, lsd_en),
                 names_to = "prompt",
                 values_to = "sentiment") %>%
    mutate(prompt = case_when(
      prompt == "gemini_fr_fr" ~ "Gemini: French Prompt",
      prompt == "gemini_en_fr" ~ "Gemini: English Prompt",
      prompt == "lsd_en" ~ "Dictionary (LSDEN)",
      TRUE ~ prompt
    ))
} else {
  # If LSD data not available, just use Gemini data
  df_long <- df %>%
    select(gemini_fr_fr, gemini_en_fr) %>%
    pivot_longer(cols = c(gemini_fr_fr, gemini_en_fr),
                 names_to = "prompt",
                 values_to = "sentiment") %>%
    mutate(prompt = case_when(
      prompt == "gemini_fr_fr" ~ "Gemini: French Prompt",
      prompt == "gemini_en_fr" ~ "Gemini: English Prompt",
      TRUE ~ prompt
    ))
}

# Create enhanced overlapping density plot with publication styling
p3 <- ggplot(df_long, aes(x = sentiment, fill = prompt, color = prompt)) +
  # Add white background explicitly
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "white", alpha = 1) +
  # Add reference line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  # Create density plot with transparent fill and solid outline
  geom_density(alpha = 0.3, linewidth = 1) +
  # Professional labeling
  labs(title = "Comparison of Sentiment Distributions by Method",
       subtitle = "Sentiment analysis distributions on French news corpus",
       x = "Sentiment Score", 
       y = "Density", 
       fill = "Analysis Method",
       color = "Analysis Method",
       caption = "Comparing Gemini 2.0 Flash with different prompting approaches and dictionary-based (LSDEN) methods") +
  # Consistent scale
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  # Professional color palette - adding green for dictionary
  scale_fill_manual(values = c(
    "Gemini: French Prompt" = "#1F78B4", 
    "Gemini: English Prompt" = "#D62728",
    "Dictionary (LSDEN)" = "#2CA02C"
  )) +
  scale_color_manual(values = c(
    "Gemini: French Prompt" = "#08519C", 
    "Gemini: English Prompt" = "#A50F15",
    "Dictionary (LSDEN)" = "#006D2C"
  )) +
  # Apply publication theme
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "#636363", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "#636363", hjust = 0, margin = margin(t = 10)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "#252525"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Ensure legend shows both color and fill
  guides(color = "none")

# Display the comparison plot
print(p3)

#==============================================================================
# 6. TIME SERIES ANALYSIS
#==============================================================================

# Ensure date column is properly formatted
if ("date" %in% names(df)) {
  df <- df %>%
    mutate(date = as.Date(date))
  
  # Load LSD sentiment for comparison
  df_lsd <- tryCatch({
    readRDS("data/tmp/news_df_tone_index.rds") %>%
      select(doc_id, fr_tone_index) %>%
      rename(lsden = fr_tone_index)
  }, error = function(e) {
    message("LSD sentiment data not found, proceeding without it.")
    data.frame(doc_id = character(0), lsden = numeric(0))
  })
  
  # Join LSD data with Gemini data
  df <- df %>%
    left_join(df_lsd, by = "doc_id")
  
  # Create monthly averages
  monthly_sentiment <- df %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(
      fr_sentiment = mean(gemini_fr_fr, na.rm = TRUE),
      en_sentiment = mean(gemini_en_fr, na.rm = TRUE),
      lsden_sentiment = mean(lsden, na.rm = TRUE),
      n_articles = n()
    ) %>%
    ungroup()
  
  # Plot sentiment over time - create data in long format for plotting
  monthly_long <- monthly_sentiment %>%
    pivot_longer(cols = c(fr_sentiment, en_sentiment, lsden_sentiment),
                 names_to = "prompt",
                 values_to = "sentiment") %>%
    mutate(prompt = case_when(
      prompt == "fr_sentiment" ~ "Gemini: French Prompt",
      prompt == "en_sentiment" ~ "Gemini: English Prompt",
      prompt == "lsden_sentiment" ~ "Dictionary (LSDEN)",
      TRUE ~ prompt
    ))
  
  # Create enhanced time series plot with publication-quality styling
  p4 <- ggplot(monthly_long, aes(x = month, y = sentiment, color = prompt, group = prompt)) +
    # Add raw data points with lower opacity in background
    geom_point(aes(shape = prompt), alpha = 0.25, size = 2.5) +
    
    # Add connecting lines between points with very low opacity
    geom_line(alpha = 0.15, linewidth = 0.5) +
    
    # Add smoothed lines with higher opacity in foreground
    geom_smooth(method = "loess", span = 0.35, se = FALSE, linewidth = 1.5) +
    
    # Add horizontal reference line at neutral sentiment
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.7) +
    
    # Professional styling with descriptive labels
    labs(title = "Sentiment Analysis Comparison Over Time",
         subtitle = "Monthly averages with smoothed trend lines (1991-2025)",
         x = "", y = "Sentiment Score",
         color = "Analysis Method",
         shape = "Analysis Method",
         caption = "Data source: French news corpus on open source software (N=2,683 articles)\nNegative values indicate negative sentiment; positive values indicate positive sentiment\nSmoothing: LOESS method with span=0.35") +
    
    # Apply publication theme with white background
    theme_minimal() +
    theme(
      # Background elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Grid elements
      panel.grid.major = element_line(color = "gray95", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      # Text elements
      plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 13, color = "#636363", margin = margin(b = 15)),
      plot.caption = element_text(size = 9, color = "#636363", hjust = 0, margin = margin(t = 15)),
      axis.title.y = element_text(size = 13, margin = margin(r = 15), face = "bold"),
      axis.text = element_text(size = 11, color = "#252525"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      
      # Legend elements
      legend.position = "bottom",
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.margin = margin(t = 10),
      
      # Overall plot margins
      plot.margin = margin(30, 30, 30, 30)
    ) +
    
    # Date breaks
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    
    # Y-axis formatting with consistent limits
    scale_y_continuous(
      limits = c(-1, 1), 
      breaks = seq(-1, 1, by = 0.25)
    ) +
    
    # Define a professional color palette with deeper, more distinct colors
    scale_color_manual(values = c(
      "Gemini: French Prompt" = "#0072B2",    # Blue
      "Gemini: English Prompt" = "#D55E00",   # Orange-red
      "Dictionary (LSDEN)" = "#009E73"        # Green
    )) +
    
    # Add distinct point shapes for better distinguishability
    scale_shape_manual(values = c(
      "Gemini: French Prompt" = 16,           # Circle
      "Gemini: English Prompt" = 17,          # Triangle
      "Dictionary (LSDEN)" = 15               # Square
    ))
  
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

# Visualization of category distribution with publication styling
p5 <- ggplot(diff_categories, aes(x = reorder(diff_category, -n), y = n, fill = diff_category)) +
  # Add white background
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "white", alpha = 1) +
  # Draw bars with cleaner borders
  geom_col(color = "white", linewidth = 0.2) +
  # Add percentage labels above bars
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            vjust = -0.8,
            size = 4.5,
            fontface = "bold") +
  # Add count labels inside bars
  geom_text(aes(label = n, y = n/2),
            color = "white",
            size = 4,
            fontface = "bold") +
  # Professional labeling
  labs(title = "Distribution of Sentiment Difference Categories",
       subtitle = "Comparison between French and English prompt conditions",
       x = NULL, 
       y = "Number of Articles",
       caption = "Categories represent the directional difference between sentiment scores from\nFrench and English prompts. 'Similar' indicates differences less than or equal to 0.2.") +
  # Professional, color-blind friendly palette
  scale_fill_manual(values = c(
    "Similar" = "#0072B2",             # Blue
    "English More Positive" = "#D55E00", # Orange-red
    "French More Positive" = "#009E73",  # Green
    "Missing Data" = "#CCCCCC"           # Gray
  )) +
  # Apply publication theme
  create_pub_theme() +
  # Hide legend
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 18, face = "bold")
  )

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
  ggsave("results/graphs/gemini_time_series.png", p4, width = 12, height = 8, dpi = 300)
}
ggsave("results/graphs/gemini_diff_categories.png", p5, width = 8, height = 6)

cat("Analysis complete. Results saved to data/clean/gemini_corpus_analysis.rds\n")
cat("Plots saved to results/graphs/\n")
