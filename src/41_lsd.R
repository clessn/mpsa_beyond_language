library(tidyverse)
library(tibble)
library(quanteda)

df_raw <- readRDS("data/tmp/df_lsd_prepped.rds") %>%
  rename(id = doc_id)  # Rename doc_id to id

df <- df_raw %>%
  as.data.frame() %>%
  mutate(
    doc_id = paste(id, id_sentence, sep = "_")  # Create unique document IDs
  ) %>%
  select(id, id_sentence, source_media, date, body_prepped)

corpus <- quanteda::tokens(df$body_prepped)

dfm <- quanteda::dfm(quanteda::tokens_lookup(corpus, data_dictionary_LSD2015, nested_scope = "dictionary"))

results <- quanteda::convert(dfm, to = "data.frame")

df_sentiments <- cbind(df, results) 

df_sentiments <- df_sentiments %>%
  mutate(
    total_words = str_count(body_prepped, "\\S+"),
    proportion_positive = (positive + neg_negative) / total_words,
    proportion_negative = (negative + neg_positive) / total_words,
    tone_index = proportion_positive - proportion_negative
  )

df_aggregated <- df_sentiments %>%
  group_by(id) %>%
  summarise(
    total_positive = sum(positive, na.rm = TRUE),
    total_negative = sum(negative, na.rm = TRUE),
    total_neg_positive = sum(neg_positive, na.rm = TRUE),
    total_neg_negative = sum(neg_negative, na.rm = TRUE),
    total_words = sum(total_words, na.rm = TRUE),
    proportion_positive = sum(positive) / sum(total_words),
    proportion_negative = sum(negative) / sum(total_words),
    tone_index = (sum(positive) - sum(negative)) / (sum(positive) + sum(negative)),
    .groups = "drop"
  )

# Keep only one row per document with metadata
df_final <- df_sentiments %>%
  # Select document-level metadata (ensure these are constant within doc_id)
  select(id, source_media, date) %>%
  # Keep only unique document-level rows
  distinct(id, .keep_all = TRUE) %>%  # <-- Critical fix here
  # Merge aggregated results
  left_join(df_aggregated, by = "id")

saveRDS(df_final, "data/tmp/df_lsd_sentiments.rds")

df <- readRDS("data/tmp/df_lsd_sentiments.rds")
