#################################################################
# SAMPLE CREATION FOR MANUAL ANNOTATION
#################################################################
# This script creates a random sample of sentences from the news articles 
# for manual annotation. It translates the French sentences to English
# and calculates automated sentiment scores using both French and English
# lexicons for comparison with future manual ratings.

# Load required libraries
library(stringr)    # For string manipulation
library(quanteda)   # For text analysis
library(tidyr)      # For data reshaping
library(tidytext)   # For text mining
library(purrr)      # For functional programming
library(dplyr)      # For data manipulation
library(polyglotr)  # For machine translation

# Source helper functions for text preparation
source(file = "src/91_LSDprep_dec2017.R")

#################################################################
# DATA LOADING
#################################################################
# Load news articles with tone indices
df <- readRDS("data/tmp/news_df_tone_index.rds") 

# Set random seed for reproducibility
set.seed(42069)

#################################################################
# RANDOM SAMPLING
#################################################################
# Define sample size
sample_size <- 200

# Randomly select document IDs
sample_ids <- sample(df$doc_id, sample_size) 

# Select only required columns
df <- df %>%
  select(doc_id, text_body, date, source_media)

#################################################################
# SENTENCE SEGMENTATION
#################################################################
# Split documents into sentences
df_sample <- df %>%
  # Split text into sentences using punctuation as delimiters
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  # Create separate rows for each sentence
  unnest(sentences) %>%
  # Number sentences within each document
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  # Create unique sentence identifiers
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  ungroup()

# Randomly select sentences from the extracted set
sample_sentences <- sample(df_sample$sentence_id, sample_size) 

# Filter to keep only sampled sentences
df_sentences <- df_sample %>%
  filter(sentence_id %in% sample_sentences) %>%
  mutate(sentences_en = NA)

#################################################################
# TRANSLATION TO ENGLISH
#################################################################
# Translate each sentence from French to English using Google Translate
for (i in 1:nrow(df_sentences)) {
  cat(i, "/", nrow(df_sentences), "\n")
  print(paste0("Original: ", df_sentences$sentences[i]))
  
  # Improved URL pattern to catch URLs with spaces
  url_pattern <- "https?:\\s*//\\s*[^\\s]+|www\\.[^\\s]+"
  
  # Also catch URLs inside parentheses
  parentheses_url_pattern <- "\\([^)]*(?:https?:|www\\.)[^)]*\\)"
  
  # Replace URLs with [WEBSITE URL] placeholder
  text <- paste0(df_sentences$sentences[i])
  text <- str_replace_all(text, parentheses_url_pattern, "[WEBSITE URL]")
  text <- str_replace_all(text, url_pattern, "[WEBSITE URL]")
  
  # Translate the text from French to English
  df_sentences$sentences_en[i] <- google_translate(text, "en", "fr")
  print(paste0("Translated: ", df_sentences$sentences_en[i]))
}

#################################################################
# TEXT CLEANING AND PREPROCESSING
#################################################################
# Clean and preprocess both French and English sentences
df_clean_sentences <- df_sentences %>%
  mutate(
    # Clean French sentences
    sentences_clean = sentences %>%
      str_to_lower() %>%           # Convert to lowercase
      str_remove_all("!") %>%      # Remove exclamation marks
      str_remove_all("\\?") %>%    # Remove question marks
      str_remove_all("\\.") %>%    # Remove periods
      str_remove_all(","),         # Remove commas
    
    # Clean English sentences with LSD preparation functions
    sentences_clean_en = sentences_en %>%
      pbapply::pbsapply(LSDprep_contr) %>%                      # Process contractions
      pbapply::pbsapply(LSDprep_dict_punct) %>%                 # Handle punctuation
      pbapply::pbsapply(remove_punctuation_from_acronyms) %>%   # Process acronyms
      pbapply::pbsapply(remove_punctuation_from_abbreviations) %>% # Process abbreviations
      pbapply::pbsapply(LSDprep_punctspace) %>%                 # Standardize spacing
      pbapply::pbsapply(LSDprep_negation) %>%                   # Handle negations
      pbapply::pbsapply(LSDprep_dict) %>%                       # Dictionary preparation
      pbapply::pbsapply(mark_proper_nouns)                      # Mark proper nouns
  )

#################################################################
# CORPUS CREATION AND TOKENIZATION
#################################################################
# Create named corpus objects for both languages
corpus_fr <- corpus(df_clean_sentences$sentences_clean, 
                   docnames = df_clean_sentences$sentence_id)
corpus_en <- corpus(df_clean_sentences$sentences_clean_en, 
                    docnames = df_clean_sentences$sentence_id)

# Tokenize from the corpus objects to maintain the original doc_ids
tokens_fr_with_stopwords <- tokens(corpus_fr)
tokens_en <- tokens(corpus_en)

# Remove French stopwords
tokens_fr <- quanteda::tokens_remove(
  tokens_fr_with_stopwords, 
  stopwords("french")  # Use French stopwords
)

#################################################################
# SENTIMENT ANALYSIS WITH LEXICONS
#################################################################
# Load the French and English sentiment dictionaries
lsdfr <- dictionary(readRDS("data/dict/frlsd_dict.rds"))
lsden <- data_dictionary_LSD2015

# Apply the French lexicon to French text
matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_fr, lsdfr, nested_scope = "dictionary")
)

# Apply the English lexicon to English text
matrice_sentiment_en <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_en, lsden, nested_scope = "dictionary")
)

# Convert the document-feature matrices to dataframes
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "sentence_id")

# Convert English sentiment results and rename columns for clarity
resultats_sentiment_en <- quanteda::convert(matrice_sentiment_en, to = "data.frame", docid_field = "sentence_id") %>%
  rename(en_negative = negative, en_positive = positive, en_neg_positive = neg_positive, en_neg_negative = neg_negative)

# Merge all dataframes
df_combined <- df_clean_sentences %>%
  left_join(resultats_sentiment, by = "sentence_id") %>%
  left_join(resultats_sentiment_en, by = "sentence_id")

#################################################################
# SENTIMENT METRICS CALCULATION
#################################################################
# Calculate sentiment metrics for both languages
df_lsd <- df_combined %>%
  mutate(
    # Count words in each language
    total_words = str_count(sentences_clean, "\\S+"),  
    total_words_en = str_count(sentences_clean_en, "\\S+"),
    
    # Calculate French sentiment proportions
    proportion_positive = (positive) / total_words,  
    proportion_negative = (negative) / total_words, 
    
    # Calculate English sentiment proportions (accounting for negation)
    proportion_positive_en = (en_positive + en_neg_negative) / total_words_en,
    proportion_negative_en = (en_negative + en_neg_positive) / total_words_en,
    
    # Calculate tone indices
    tone_index = proportion_positive - proportion_negative,  
    tone_index_en = proportion_positive_en - proportion_negative_en
  ) %>%
  # Remove intermediate columns to create a cleaner dataset
  select(-c(sentence_number, sentence_id, text_body, sentences_clean, sentences_clean_en, positive, negative, en_positive, en_negative, en_neg_positive, en_neg_negative, total_words, total_words_en, proportion_positive, proportion_negative, proportion_positive_en, proportion_negative_en))

#################################################################
# SAVE SAMPLE FOR ANNOTATION
#################################################################
# Save the processed sample for manual annotation
saveRDS(df_lsd, "data/tmp/data_manual_ranking.rds")