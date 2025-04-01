#################################################################
# LEXICODER SENTIMENT DICTIONARY (LSD) PREPARATION
#################################################################
# This script prepares English-translated news articles for sentiment analysis
# using the Lexicoder Sentiment Dictionary (LSD). It splits text into sentences
# and applies various text preparation functions to optimize for dictionary matching.

# Load required libraries
library(dplyr)      # For data manipulation
library(tidytext)   # For text tokenization and processing

#################################################################
# LOAD SUPPORTING CODE AND DATA
#################################################################
# Source helper functions for text preparation
source(file = "src/91_LSDprep_dec2017.R")

# Read the dataset with translated text
df <- readRDS("data/tmp/news_df_translated.rds")

#################################################################
# SENTENCE SEGMENTATION
#################################################################
# Split articles into sentences and create proper sentence identifiers
df_lsd <- df %>%
  # Split into sentences while preserving document structure
  unnest_sentences(
    output = sentence_text,  # Temporary name for the unnested text
    input = translated_text, 
    drop = FALSE            # Keep original columns
  ) %>%
  # Create sentence IDs
  group_by(doc_id) %>%
  mutate(
    sentence_in_doc = row_number(),  # Sentence number WITHIN document
    id_sentence = paste(doc_id, sentence_in_doc, sep = "_")  # Unique ID
  ) %>%
  ungroup()

# Initialize body_prepped column with the original sentence text
df_lsd$body_prepped <- df_lsd$sentence_text

#################################################################
# TEXT PREPROCESSING PIPELINE
#################################################################
# Apply each preparation function from the LSDprep helper file
# These functions handle contractions, punctuation, negations, and other text features
# to improve dictionary-based sentiment analysis accuracy

# Process contractions (e.g., "don't" -> "do not")
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_contr)

# Handle dictionary punctuation
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict_punct)

# Remove punctuation from acronyms
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_acronyms)

# Remove punctuation from abbreviations
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_abbreviations)

# Standardize spacing around punctuation
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_punctspace)

# Process negation terms
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_negation)

# Apply dictionary-specific preparation
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict)

# Mark proper nouns to handle them appropriately
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, mark_proper_nouns)

#################################################################
# FINAL DATASET ORGANIZATION AND STORAGE
#################################################################
# Select and reorder relevant columns for the final dataset
df_lsd <- df_lsd %>%
  select(doc_id, id_sentence, source_media, date, sentence_text, body_prepped, everything())

# Save the prepared dataset for sentiment analysis
saveRDS(df_lsd, "data/tmp/df_lsd_prepped.rds")