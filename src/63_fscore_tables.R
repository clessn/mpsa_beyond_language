#################################################################
# F-SCORE TABLE GENERATION
#################################################################
# This script creates formatted tables of F1 scores for sentiment analysis models
# in both LaTeX and Markdown formats. It generates tables for 7-category and
# 3-category sentiment classifications, as well as a combined comparison table.

# Load required libraries
library(dplyr)  # For data manipulation

#################################################################
# DATA LOADING
#################################################################
# Read in the detailed F1 score data
df_7 <- readRDS("results/fscores/f1_scores_detailed_7.rds")  # 7-category results
df_3 <- readRDS("results/fscores/f1_scores_detailed_3.rds")  # 3-category results

# Round numeric columns to 3 decimal places for readability
df_7_rounded <- df_7 %>%
  mutate(across(where(is.numeric), ~round(., 3)))

df_3_rounded <- df_3 %>%
  mutate(across(where(is.numeric), ~round(., 3)))

#################################################################
# 7-CATEGORY LATEX TABLE GENERATION
#################################################################
# Create LaTeX table for 7-category classification results
latex_table_7 <- "\\begin{tabular}{lcccccccc}\n"
latex_table_7 <- paste0(latex_table_7, "\\hline\n")
latex_table_7 <- paste0(latex_table_7, "\\textbf{Model} & \\textbf{very\\_neg} & \\textbf{negative} & \\textbf{somewhat\\_neg} & \\textbf{neutral} & \\textbf{somewhat\\_pos} & \\textbf{positive} & \\textbf{very\\_pos} & \\textbf{weighted} \\\\\n")
latex_table_7 <- paste0(latex_table_7, "\\hline\n")

# Add each model's results as a row in the table
for(i in 1:nrow(df_7_rounded)) {
  row_7 <- df_7_rounded[i,]
  # Replace underscores with hyphens for LaTeX and escape special characters
  model_name <- gsub("_", "-", row_7$model)
  # Also escape any other potential special LaTeX characters
  model_name <- gsub("&", "\\\\&", model_name)
  model_name <- gsub("%", "\\\\%", model_name)
  model_name <- gsub("#", "\\\\#", model_name)
  
  # Build the table row with all F1 scores
  latex_table_7 <- paste0(latex_table_7, model_name, " & ", 
                        row_7$very_negative, " & ", 
                        row_7$negative, " & ", 
                        row_7$somewhat_negative, " & ", 
                        row_7$neutral, " & ", 
                        row_7$somewhat_positive, " & ", 
                        row_7$positive, " & ", 
                        row_7$very_positive, " & ", 
                        row_7$weighted_f1, " \\\\\n")
}

# Complete the LaTeX table
latex_table_7 <- paste0(latex_table_7, "\\hline\n")
latex_table_7 <- paste0(latex_table_7, "\\end{tabular}")

# Write LaTeX table to file
writeLines(latex_table_7, "results/tables/f1_scores_7cat_table.tex")

#################################################################
# 7-CATEGORY MARKDOWN TABLE GENERATION
#################################################################
# Create a Markdown version of the 7-category table
markdown_table_7 <- "# F1 Score Results by Sentiment Category (7-category)\n\n"
markdown_table_7 <- paste0(markdown_table_7, "| Model | very_negative | negative | somewhat_negative | neutral | somewhat_positive | positive | very_positive | weighted_f1 |\n")
markdown_table_7 <- paste0(markdown_table_7, "|-------|--------------|----------|-------------------|---------|-------------------|----------|---------------|------------|\n")

# Add each model's results as a row in the table
for(i in 1:nrow(df_7_rounded)) {
  row_7 <- df_7_rounded[i,]
  markdown_table_7 <- paste0(markdown_table_7, "| ", row_7$model, " | ", 
                        row_7$very_negative, " | ", 
                        row_7$negative, " | ", 
                        row_7$somewhat_negative, " | ", 
                        row_7$neutral, " | ", 
                        row_7$somewhat_positive, " | ", 
                        row_7$positive, " | ", 
                        row_7$very_positive, " | ", 
                        row_7$weighted_f1, " |\n")
}

# Write markdown table to file
writeLines(markdown_table_7, "results/tables/f1_scores_7cat_table.md")

#################################################################
# 3-CATEGORY MARKDOWN TABLE GENERATION
#################################################################
# Create a Markdown table for 3-category classification results
markdown_table_3 <- "# Grouped Sentiment F1 Score Results (3-category)\n\n"
markdown_table_3 <- paste0(markdown_table_3, "| Model | negative | neutral | positive | weighted_f1 |\n")
markdown_table_3 <- paste0(markdown_table_3, "|-------|----------|---------|----------|------------|\n")

# Add each model's results as a row in the table
for(i in 1:nrow(df_3_rounded)) {
  row_3 <- df_3_rounded[i,]
  markdown_table_3 <- paste0(markdown_table_3, "| ", row_3$model, " | ", 
                        row_3$negative, " | ", 
                        row_3$neutral, " | ", 
                        row_3$positive, " | ", 
                        row_3$weighted_f1, " |\n")
}

# Write markdown table to file
writeLines(markdown_table_3, "results/tables/f1_scores_3cat_table.md")

#################################################################
# COMBINED COMPARISON TABLE GENERATION
#################################################################
# First, rename columns in df_3 to avoid column name conflicts
df_3_renamed <- df_3_rounded %>%
  rename(
    negative_3cat = negative,
    neutral_3cat = neutral,
    positive_3cat = positive,
    weighted_f1_3cat = weighted_f1
  )

# Now, join the datasets by model
merged_df <- df_7_rounded %>%
  left_join(df_3_renamed, by = "model") %>%
  # Sort by the weighted_f1 of the 3-category model to match the graph
  arrange(desc(weighted_f1_3cat))

# Create combined markdown table with both 7-category and 3-category results
markdown_table_combined <- "# Combined F1 Score Results by Sentiment Category\n\n"

# Single row header with clear column labels
markdown_table_combined <- paste0(markdown_table_combined, 
  "| Model | very_negative | negative | somewhat_negative | neutral | somewhat_positive | positive | very_positive | negative (3-cat) | neutral (3-cat) | positive (3-cat) | weighted_f1 (7-cat) | weighted_f1 (3-cat) |\n")
markdown_table_combined <- paste0(markdown_table_combined, 
  "|-------|--------------|----------|-------------------|---------|-------------------|----------|---------------|-----------------|----------------|-----------------|-------------------|-------------------|\n")

# Add rows with all metrics for each model
for(i in 1:nrow(merged_df)) {
  row <- merged_df[i,]
  markdown_table_combined <- paste0(markdown_table_combined, "| ", row$model, " | ", 
                        row$very_negative, " | ", 
                        row$negative, " | ", 
                        row$somewhat_negative, " | ", 
                        row$neutral, " | ", 
                        row$somewhat_positive, " | ", 
                        row$positive, " | ", 
                        row$very_positive, " | ", 
                        row$negative_3cat, " | ", 
                        row$neutral_3cat, " | ", 
                        row$positive_3cat, " | ", 
                        row$weighted_f1, " | ", 
                        row$weighted_f1_3cat, " |\n")
}

# Write combined markdown table to file
writeLines(markdown_table_combined, "results/tables/f1_scores_combined_table.md")

#################################################################
# COMPLETION CONFIRMATION
#################################################################
# Print confirmation of created tables
cat("Tables have been created:\n")
cat("1. 7-category LaTeX table: results/tables/f1_scores_7cat_table.tex\n")
cat("2. 7-category markdown table: results/tables/f1_scores_7cat_table.md\n")
cat("3. 3-category table: results/tables/f1_scores_3cat_table.md\n")
cat("4. Combined table: results/tables/f1_scores_combined_table.md\n")