library(dplyr)

df <- readRDS("data/tmp/news_df.rds")

df_frlsd <- readRDS("data/tmp/news_df_sentiment.rds") %>%
  filter(!is.na(tone_index)) %>%
  select(doc_id, tone_index) %>%
  rename(fr_tone_index = tone_index)

df_lsd <- readRDS("data/tmp/df_lsd_sentiments.rds") %>%
  rename(doc_id = id) %>%
  filter(!is.na(tone_index)) %>%
  select(doc_id, tone_index) %>%
  rename(en_tone_index = tone_index)

df_translated <- readRDS("data/tmp/news_df_translated.rds") %>%
  select(doc_id, translated_text) %>%
  rename(en_text_body = translated_text) %>%
  mutate(en_text_body = as.character(en_text_body))

# merge
df_tone_index <- df %>%
  left_join(df_frlsd, by = "doc_id") %>%
  left_join(df_lsd, by = "doc_id") %>%
  left_join(df_translated, by = "doc_id") %>%
  select(doc_id, date, title, text_body, en_text_body, source_media, fr_tone_index, en_tone_index)

# IDs with missing text body
# [1]  820 1349 1415 2548
saveRDS(df_tone_index, "data/tmp/news_df_tone_index.rds")
