# Joins the French and English dictionary scores and the translations into
# one corpus table.
#
# In:  data/tmp/news_df.rds, data/tmp/news_df_sentiment.rds,
#      data/tmp/df_lsd_sentiments.rds, data/tmp/news_df_translated.rds
# Out: data/tmp/news_df_tone_index.rds

library(dplyr)

df <- readRDS("data/tmp/news_df.rds")

df_frlsd <- readRDS("data/tmp/news_df_sentiment.rds") %>%
  filter(!is.na(tone_index)) %>%         # Remove entries with missing sentiment
  select(doc_id, tone_index) %>%         # Keep only ID and sentiment score
  rename(fr_tone_index = tone_index)     # Rename to indicate language

df_lsd <- readRDS("data/tmp/df_lsd_sentiments.rds") %>%
  rename(doc_id = id) %>%                # Standardize ID column name
  filter(!is.na(tone_index)) %>%         # Remove entries with missing sentiment
  select(doc_id, tone_index) %>%         # Keep only ID and sentiment score
  rename(en_tone_index = tone_index)     # Rename to indicate language

df_translated <- readRDS("data/tmp/news_df_translated.rds") %>%
  select(doc_id, translated_text) %>%    # Keep only needed columns
  rename(en_text_body = translated_text) %>%  # Rename for clarity
  mutate(en_text_body = as.character(en_text_body))  # Ensure proper character format

df_tone_index <- df %>%
  left_join(df_frlsd, by = "doc_id") %>%
  left_join(df_lsd, by = "doc_id") %>%
  left_join(df_translated, by = "doc_id") %>%
  select(doc_id, date, title, text_body, en_text_body, source_media, fr_tone_index, en_tone_index)

# Note: Missing text bodies for IDs [1] 820 1349 1415 2548

saveRDS(df_tone_index, "data/tmp/news_df_tone_index.rds")
