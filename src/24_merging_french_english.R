#################################################################
# MERGE FRENCH AND ENGLISH SENTIMENT ANALYSIS RESULTS
#################################################################
# This script combines sentiment analysis results from both the original French
# text (FRLSD) and the English-translated text (LSD) into a unified dataset
# to enable cross-language sentiment comparison.

# Load required libraries
library(dplyr)  # For data manipulation

#################################################################
# LOAD DATASETS
#################################################################
# Load the original cleaned news dataset
df <- readRDS("data/tmp/news_df.rds")

#################################################################
# PREPARE FRENCH SENTIMENT DATA
#################################################################
# Extract French sentiment scores (from FRLSD analysis)
df_frlsd <- readRDS("data/tmp/news_df_sentiment.rds") %>%
  filter(!is.na(tone_index)) %>%         # Remove entries with missing sentiment
  select(doc_id, tone_index) %>%         # Keep only ID and sentiment score
  rename(fr_tone_index = tone_index)     # Rename to indicate language

#################################################################
# PREPARE ENGLISH SENTIMENT DATA
#################################################################
# Extract English sentiment scores (from LSD analysis)
df_lsd <- readRDS("data/tmp/df_lsd_sentiments.rds") %>%
  rename(doc_id = id) %>%                # Standardize ID column name
  filter(!is.na(tone_index)) %>%         # Remove entries with missing sentiment
  select(doc_id, tone_index) %>%         # Keep only ID and sentiment score
  rename(en_tone_index = tone_index)     # Rename to indicate language

#################################################################
# PREPARE TRANSLATED TEXT DATA
#################################################################
# Extract English translated text
df_translated <- readRDS("data/tmp/news_df_translated.rds") %>%
  select(doc_id, translated_text) %>%    # Keep only needed columns
  rename(en_text_body = translated_text) %>%  # Rename for clarity
  mutate(en_text_body = as.character(en_text_body))  # Ensure proper character format

#################################################################
# MERGE ALL DATASETS
#################################################################
# Create unified dataset with both French and English data
df_tone_index <- df %>%
  # Add French sentiment scores
  left_join(df_frlsd, by = "doc_id") %>%
  # Add English sentiment scores
  left_join(df_lsd, by = "doc_id") %>%
  # Add English translated text
  left_join(df_translated, by = "doc_id") %>%
  # Select and reorder final columns
  select(doc_id, date, title, text_body, en_text_body, source_media, fr_tone_index, en_tone_index)

# Note: Missing text bodies for IDs [1] 820 1349 1415 2548

#################################################################
# SAVE FINAL MERGED DATASET
#################################################################
# Save the merged dataset for further analysis
saveRDS(df_tone_index, "data/tmp/news_df_tone_index.rds")