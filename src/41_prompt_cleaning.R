#################################################################
# LLM SENTIMENT DATA CLEANING AND TRANSFORMATION
#################################################################
# This script processes the raw LLM sentiment analysis results, removing 
# intermediate data, renaming columns for clarity, and transforming numerical
# sentiment scores into categorical values according to a 7-point Likert scale.

# Load required libraries
library(dplyr)      # For data manipulation
library(stringr)    # For string operations

#################################################################
# LOAD RAW SENTIMENT DATA
#################################################################
# Load the dataset containing all LLM sentiment analysis results
df_raw <- readRDS("data/tmp/data_manual_ranking_with_llm_scores.rds")

#################################################################
# DATA CLEANING AND TRANSFORMATION
#################################################################
# Clean the dataset by:
# 1. Removing intermediate run columns and unnecessary models
# 2. Renaming columns for clarity
# 3. Converting sentiment scores to categorical values
# 4. Setting proper factor levels for categorical values
df <- df_raw %>% 
  # Remove intermediate run columns and unnecessary models/columns
  select(-contains("_run"), -contains("distill"), -manual_bin) %>%
  
  # Rename the manual rating column to ground_truth
  rename(
    ground_truth = "manual",
  ) %>%
  
  # Transform numerical sentiment scores to 7-category Likert scale
  mutate(across(
    ends_with(c("_en", "_fr", "_mean", "_truth")) & !matches("sentences_en"),
    ~ case_when(
      . < -0.66 ~ "very_negative",
      . < -0.33 ~ "negative",
      . < 0 ~ "somewhat_negative",
      . == 0 ~ "neutral",
      . <= 0.33 ~ "somewhat_positive",
      . <= 0.66 ~ "positive",
      TRUE ~ "very_positive"
    ),
    .names = "{str_remove(.col, '_mean')}_cat"
  )) %>%
  
  # Convert categorical columns to ordered factors
  mutate(across(
    ends_with("_cat"),
    ~ factor(
      .,
      levels = c("very_negative", "negative", "somewhat_negative", "neutral", 
                 "somewhat_positive", "positive", "very_positive"),
      ordered = TRUE
    )
  )) %>%
  
  # Remove '_mean' suffix from column names
  rename_with(~ gsub("_mean$", "", .), matches("_mean$")) %>%
  
  # Remove binary columns
  select(-ends_with("_bin")) %>%
  
  # Reorder columns logically
  select(doc_id, date, source_media, sentences, sentences_en, starts_with("ground"), starts_with("lsd"), everything())

#################################################################
# SAVE CLEANED DATASET
#################################################################
# Save the cleaned dataset with full information
saveRDS(df, "data/clean/df.rds")

#################################################################
# CREATE F-SCORE DATASET
#################################################################
# Create a simplified version of the dataset for F-score calculation
# This version removes text and metadata, keeping only categorical results
df_fscores <- df %>%
  # Remove language-specific columns and ground truth numerical values
  select(-ends_with("_en"), -ends_with("_fr"), -ends_with("_truth")) %>%
  
  # Remove "_cat" suffix from categorical column names
  rename_with(~ gsub("_cat$", "", .), ends_with("_cat")) %>%
  
  # Remove metadata columns
  select(-c(doc_id, date, source_media, sentences))

# Save the F-score dataset
saveRDS(df_fscores, "data/clean/df_fscores.rds")