library(dplyr)

# Read in the data
df_7 <- readRDS("results/fscores/f1_scores_detailed_7.rds")
df_3 <- readRDS("results/fscores/f1_scores_detailed_3.rds")

# Round numeric columns to 3 decimal places for readability
df_7_rounded <- df_7 %>%
  mutate(across(where(is.numeric), ~round(., 3)))

df_3_rounded <- df_3 %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# 1. TABLE FOR 7-CATEGORY CLASSIFICATION
# Create markdown table
markdown_table_7 <- "# F1 Score Results by Sentiment Category (7-category)\n\n"
markdown_table_7 <- paste0(markdown_table_7, "| Model | very_negative | negative | somewhat_negative | neutral | somewhat_positive | positive | very_positive | weighted_f1 |\n")
markdown_table_7 <- paste0(markdown_table_7, "|-------|--------------|----------|-------------------|---------|-------------------|----------|---------------|------------|\n")

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

# 2. TABLE FOR 3-CATEGORY CLASSIFICATION
# Create markdown table
markdown_table_3 <- "# Grouped Sentiment F1 Score Results (3-category)\n\n"
markdown_table_3 <- paste0(markdown_table_3, "| Model | negative | neutral | positive | weighted_f1 |\n")
markdown_table_3 <- paste0(markdown_table_3, "|-------|----------|---------|----------|------------|\n")

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

# 3. COMBINED TABLE
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

# Create markdown table - CORRECTED FORMAT
markdown_table_combined <- "# Combined F1 Score Results by Sentiment Category\n\n"

# Single row header with clear column labels
markdown_table_combined <- paste0(markdown_table_combined, 
  "| Model | very_negative | negative | somewhat_negative | neutral | somewhat_positive | positive | very_positive | negative (3-cat) | neutral (3-cat) | positive (3-cat) | weighted_f1 (7-cat) | weighted_f1 (3-cat) |\n")
markdown_table_combined <- paste0(markdown_table_combined, 
  "|-------|--------------|----------|-------------------|---------|-------------------|----------|---------------|-----------------|----------------|-----------------|-------------------|-------------------|\n")

# Add rows
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

# Print confirmation
cat("Tables have been created:\n")
cat("1. 7-category table: results/tables/f1_scores_7cat_table.md\n")
cat("2. 3-category table: results/tables/f1_scores_3cat_table.md\n")
cat("3. Combined table: results/tables/f1_scores_combined_table.md\n")
