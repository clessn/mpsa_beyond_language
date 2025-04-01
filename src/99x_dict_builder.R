###############################################################################
# FRENCH LEXICON SENTIMENT DICTIONARY BUILDER
# 
# This script processes raw sentiment lexicon files into structured R dictionaries
# for use with the quanteda package. It extracts positive and negative word lists
# and creates both a dataframe and dictionary object for sentiment analysis.
#
# Author: Ral Zarek
# Date: March 2025
###############################################################################

#==============================================================================
# 1. SETUP AND DEPENDENCIES
#==============================================================================

# Load necessary libraries
library(quanteda)  # For text analysis and dictionary functions

#==============================================================================
# 2. DATA LOADING
#==============================================================================

# Read the raw dictionary file
lexicon_raw <- readLines("data/raw/frlsd.cat", warn = FALSE)

# Identify where the POSITIVE section begins
positive_start <- which(lexicon_raw == "POSITIVE")

# Split into negative and positive words
negative_words <- lexicon_raw[2:(positive_start-1)]
positive_words <- lexicon_raw[(positive_start+1):length(lexicon_raw)]

#==============================================================================
# 3. ENTRY CLEANING AND PROCESSING
#==============================================================================

# Function to clean up each dictionary entry
clean_entry <- function(entry) {
  # Remove tabs and leading/trailing whitespace
  entry <- trimws(gsub("\t", "", entry))
  
  # Extract the word (everything before the parenthesis)
  word <- trimws(gsub("\\s*\\([0-9]+\\).*$", "", entry))
  
  # Extract the weight if present (the number in parentheses)
  weight <- as.numeric(gsub(".*\\(([0-9]+)\\).*", "\\1", entry))
  
  return(data.frame(word = word, weight = weight, stringsAsFactors = FALSE))
}

# Process all entries
negative_df <- do.call(rbind, lapply(negative_words, clean_entry))
positive_df <- do.call(rbind, lapply(positive_words, clean_entry))

#==============================================================================
# 4. DICTIONARY CONSTRUCTION
#==============================================================================

# Add sentiment column
negative_df$sentiment <- "negative"
positive_df$sentiment <- "positive"

# Combine into one dataframe
lexicon_df <- rbind(negative_df, positive_df)

# Extract the word lists by sentiment
positive_words_list <- lexicon_df$word[lexicon_df$sentiment == "positive"]
negative_words_list <- lexicon_df$word[lexicon_df$sentiment == "negative"]

# Create a quanteda dictionary
frlsd_dict <- quanteda::dictionary(
  list(
    positive = positive_words_list,
    negative = negative_words_list
  )
)

#==============================================================================
# 5. OUTPUT AND SUMMARY
#==============================================================================

# Save both the dataframe and the dictionary for different use cases
saveRDS(lexicon_df, "data/dict/lexicon_df.rds")
saveRDS(frlsd_dict, "data/dict/frlsd_dict.rds")

# Print summary
cat("Dictionary created successfully!\n")
cat("Positive words:", length(positive_words_list), "\n")
cat("Negative words:", length(negative_words_list), "\n")
cat("Saved to data/dict/frlsd_dict.rds\n")