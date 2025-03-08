library(dplyr)
library(tidytext)

# Source helper functions
source(file = "src/91_LSDprep_dec2017.R")

# Read data
df <- readRDS("data/tmp/news_df_translated.rds")

# Split articles into sentences and create proper sentence IDs
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

# Create body_prepped column with the original sentence text
df_lsd$body_prepped <- df_lsd$sentence_text

# Apply each prep function
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_contr)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict_punct)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_acronyms)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_abbreviations)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_punctspace)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_negation)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict)
df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, mark_proper_nouns)

# Select relevant columns
df_lsd <- df_lsd %>%
  select(doc_id, id_sentence, source_media, date, sentence_text, body_prepped, everything())

# Save the df 
saveRDS(df_lsd, "data/tmp/df_lsd_prepped.rds")
