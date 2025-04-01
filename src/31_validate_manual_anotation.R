#################################################################
# MANUAL ANNOTATION VALIDATION AND INTEGRATION
#################################################################
# This script processes manually annotated sentiment scores, integrates them
# with the existing automated sentiment scores, and creates categorical 
# versions of the sentiment ratings for evaluation purposes.

# Load required libraries
library(dplyr)      # For data manipulation

#################################################################
# DATA LOADING
#################################################################
# Load the prepared sample dataset with automated sentiment scores
df_lsd <- readRDS("data/tmp/data_manual_ranking.rds")

# Load the manually annotated sentiment scores from CSV file
df_manual <- read.csv("data/tmp/sentence_annotations_20250316_212317.csv") %>%
  # Rename column to match the sample dataset
  rename(sentences = sentence)

#################################################################
# DATA MERGING AND TRANSFORMATION
#################################################################
# Join the manual annotations with the automated sentiment scores
df <- df_lsd %>%
  # Merge datasets on document ID and sentence text
  left_join(df_manual, by = c("doc_id", "sentences")) %>%
  
  # Rename columns for clarity and consistency
  rename(
    lsd_fr = tone_index,           # French lexicon sentiment score
    lsd_en = tone_index_en,        # English lexicon sentiment score
    manual = manual_score          # Human annotated sentiment score
  ) %>%
  
  # Create categorical versions (positive/neutral/negative) of each score type
  mutate(
    # Convert French lexicon scores to categories
    lsd_fr_bin = ifelse(lsd_fr > 0, "positive", 
                       ifelse(lsd_fr < 0, "negative", "neutral")),
    
    # Convert English lexicon scores to categories
    lsd_en_bin = ifelse(lsd_en > 0, "positive", 
                       ifelse(lsd_en < 0, "negative", "neutral")),
    
    # Convert manual scores to categories
    manual_bin = ifelse(manual > 0, "positive", 
                       ifelse(manual < 0, "negative", "neutral"))
  ) %>%
  
  # Select and order final columns
  select(doc_id, date, source_media, sentences, sentences_en, 
         lsd_fr, lsd_fr_bin, lsd_en, lsd_en_bin, manual, manual_bin)

#################################################################
# SAVE FINAL DATASET
#################################################################
# Save the final dataset with all sentiment scores for further analysis
saveRDS(df, "data/tmp/data_manual_ranking.rds")