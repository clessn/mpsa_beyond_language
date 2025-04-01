#################################################################
# FRENCH LEXICON-BASED SENTIMENT DETECTION (FRLSD)
#################################################################
# This script implements lexicon-based sentiment analysis for French text.
# It processes news articles at the sentence level using a French sentiment
# dictionary, calculates sentiment metrics, and aggregates results by document.

# Load required libraries
library(stringr)    # String manipulation
library(quanteda)   # Text processing and analysis
library(tidyr)      # Data reshaping
library(tidytext)   # Text mining
library(purrr)      # Functional programming
library(dplyr)      # Data manipulation

#################################################################
# DATA LOADING
#################################################################
# Load the cleaned news dataset
df <- readRDS("data/tmp/news_df.rds")

#################################################################
# SENTENCE LEVEL PROCESSING
#################################################################
# Split articles into sentences and assign unique identifiers
df_prepped <- df %>%
  # Split text into sentences using punctuation as delimiters
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  # Create separate rows for each sentence
  unnest(sentences) %>%
  # Add sentence numbering within each document
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  ungroup() %>%
  # Create unique sentence identifiers
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  # Filter out documents with no valid sentences
  group_by(doc_id) %>%
  filter(n() > 0) %>%
  ungroup()

#################################################################
# TEXT CLEANING
#################################################################
# Clean the sentences for sentiment analysis
clean_sentences <- df_prepped %>%
  mutate(
    sentences_clean = sentences %>%
      str_to_lower() %>%                # Convert to lowercase
      str_remove_all("!") %>%           # Remove exclamation marks
      str_remove_all("\\?") %>%         # Remove question marks
      str_remove_all("\\.") %>%         # Remove periods
      str_remove_all(",")               # Remove commas
  )

#################################################################
# TOKENIZATION AND STOPWORD REMOVAL
#################################################################
# Tokenize the cleaned sentences
corpus_texte <- quanteda::tokens(clean_sentences$sentences_clean)

# Remove French stopwords to improve sentiment analysis accuracy
tokens_clean <- quanteda::tokens_remove(
  corpus_texte, 
  stopwords("french")  # Use French stopwords
)

#################################################################
# SENTIMENT LEXICON APPLICATION
#################################################################
# Load the French sentiment dictionary
frlsd <- dictionary(readRDS("data/dict/frlsd_dict.rds"))

# Apply the sentiment dictionary to create a document-feature matrix
matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_clean, frlsd, nested_scope = "dictionary")
)

# Convert the document-feature matrix to a dataframe
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "id")

# Combine sentiment results with original sentence data
articles_sentiment <- cbind(clean_sentences, resultats_sentiment) %>%
  select(-id)  # Remove redundant ID column

#################################################################
# SENTIMENT METRICS CALCULATION
#################################################################
# Calculate sentiment metrics at the sentence level
articles_sentiment <- articles_sentiment %>%
  mutate(
    total_words = str_count(sentences_clean, "\\S+"),  # Count total words
    proportion_positive = (positive) / total_words,     # Positive word proportion
    proportion_negative = (negative) / total_words,     # Negative word proportion
    tone_index = proportion_positive - proportion_negative  # Overall tone index
  )

#################################################################
# DOCUMENT-LEVEL AGGREGATION
#################################################################
# Aggregate sentiment scores by document
df_aggregated <- articles_sentiment %>%
  group_by(doc_id) %>%
  summarise(
    total_positive = sum(positive, na.rm = TRUE),
    total_negative = sum(negative, na.rm = TRUE),
    total_words = sum(total_words, na.rm = TRUE),
    proportion_positive = sum(positive) / sum(total_words),
    proportion_negative = sum(negative) / sum(total_words),
    tone_index = (sum(positive) - sum(negative)) / (sum(positive) + sum(negative)),
    .groups = "drop"
  )

# Create final document-level dataset with metadata
df_final <- articles_sentiment %>%
  # Select document-level metadata (ensure these are constant within doc_id)
  select(doc_id, source_media, date) %>%
  # Keep only unique document-level rows
  distinct(doc_id, .keep_all = TRUE) %>%
  # Merge aggregated sentiment results
  left_join(df_aggregated, by = "doc_id")

#################################################################
# SAVE RESULTS
#################################################################
# Save the final sentiment analysis results
saveRDS(df_final, "data/tmp/news_df_sentiment.rds")