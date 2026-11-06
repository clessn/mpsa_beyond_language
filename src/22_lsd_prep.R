# Prepares the English translation for the Lexicoder Sentiment Dictionary
# (contractions, negations, punctuation), with the preparation functions
# distributed with the dictionary (src/91_LSDprep_dec2017.R).
#
# In:  data/tmp/news_df_translated.rds
# Out: data/tmp/df_lsd_prepped.rds

library(dplyr)
library(tidytext)

source(file = "src/91_LSDprep_dec2017.R")

df <- readRDS("data/tmp/news_df_translated.rds")

df_lsd <- df %>%
  unnest_sentences(
    output = sentence_text,  # Temporary name for the unnested text
    input = translated_text,
    drop = FALSE            # Keep original columns
  ) %>%
  group_by(doc_id) %>%
  mutate(
    sentence_in_doc = row_number(),  # Sentence number WITHIN document
    id_sentence = paste(doc_id, sentence_in_doc, sep = "_")  # Unique ID
  ) %>%
  ungroup()

df_lsd$body_prepped <- df_lsd$sentence_text

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_contr)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict_punct)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_acronyms)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, remove_punctuation_from_abbreviations)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_punctspace)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_negation)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, LSDprep_dict)

df_lsd$body_prepped <- pbapply::pbsapply(df_lsd$body_prepped, mark_proper_nouns)

df_lsd <- df_lsd %>%
  select(doc_id, id_sentence, source_media, date, sentence_text, body_prepped, everything())

saveRDS(df_lsd, "data/tmp/df_lsd_prepped.rds")
