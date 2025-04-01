#################################################################
# ENGLISH SENTIMENT ANALYSIS USING LEXICODER SENTIMENT DICTIONARY
#################################################################
# This script applies the Lexicoder Sentiment Dictionary (LSD) to the 
# prepared English-translated news articles to calculate sentiment scores.
# It processes text at the sentence level and then aggregates to document level.

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(tibble)     # For enhanced data frames
library(quanteda)   # For text analysis

#################################################################
# DATA LOADING AND PREPARATION
#################################################################
# Load the prepared dataset
df_raw <- readRDS("data/tmp/df_lsd_prepped.rds") %>%
  rename(id = doc_id)  # Rename doc_id to id for consistency with LSD conventions

# Create a clean working dataset with concatenated document identifiers
df <- df_raw %>%
  as.data.frame() %>%
  mutate(
    doc_id = paste(id, id_sentence, sep = "_")  # Create unique document IDs
  ) %>%
  select(id, id_sentence, source_media, date, body_prepped)

#################################################################
# SENTIMENT ANALYSIS WITH LEXICODER
#################################################################
# Tokenize the prepared text
corpus <- quanteda::tokens(df$body_prepped)

# Apply the Lexicoder Sentiment Dictionary
# This identifies positive, negative, negated positive, and negated negative terms
dfm <- quanteda::dfm(quanteda::tokens_lookup(corpus, data_dictionary_LSD2015, nested_scope = "dictionary"))

# Convert the document-feature matrix to a data frame
results <- quanteda::convert(dfm, to = "data.frame")

# Combine sentiment results with the original data
df_sentiments <- cbind(df, results) 

#################################################################
# SENTIMENT METRICS CALCULATION
#################################################################
# Calculate sentiment metrics at the sentence level
df_sentiments <- df_sentiments %>%
  mutate(
    total_words = str_count(body_prepped, "\\S+"),  # Count total words per sentence
    # Calculate proportions considering negated terms
    proportion_positive = (positive + neg_negative) / total_words,
    proportion_negative = (negative + neg_positive) / total_words,
    tone_index = proportion_positive - proportion_negative  # Overall sentiment score
  )

#################################################################
# DOCUMENT-LEVEL AGGREGATION
#################################################################
# Aggregate sentiment scores by document
df_aggregated <- df_sentiments %>%
  group_by(id) %>%
  summarise(
    # Sum raw counts across all sentences in a document
    total_positive = sum(positive, na.rm = TRUE),
    total_negative = sum(negative, na.rm = TRUE),
    total_neg_positive = sum(neg_positive, na.rm = TRUE),
    total_neg_negative = sum(neg_negative, na.rm = TRUE),
    total_words = sum(total_words, na.rm = TRUE),
    # Calculate proportions at document level
    proportion_positive = sum(positive) / sum(total_words),
    proportion_negative = sum(negative) / sum(total_words),
    # Calculate normalized tone index
    tone_index = (sum(positive) - sum(negative)) / (sum(positive) + sum(negative)),
    .groups = "drop"
  )

#################################################################
# FINAL DATASET CREATION
#################################################################
# Create document-level dataset with metadata and sentiment scores
df_final <- df_sentiments %>%
  # Select document-level metadata
  select(id, source_media, date) %>%
  # Keep only unique document-level rows
  distinct(id, .keep_all = TRUE) %>%
  # Merge aggregated sentiment results
  left_join(df_aggregated, by = "id")

# Save the final sentiment analysis results
saveRDS(df_final, "data/tmp/df_lsd_sentiments.rds")

# Load the saved data for verification or further analysis
df <- readRDS("data/tmp/df_lsd_sentiments.rds")