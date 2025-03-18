library(dplyr)

df_lsd <- readRDS("data/tmp/data_manual_ranking.rds")

df_manual <- read.csv("data/tmp/sentence_annotations_20250316_212317.csv") %>%
  rename(sentences = sentence)

df <- df_lsd %>%
  left_join(df_manual, by = c("doc_id", "sentences")) %>%
  rename(
    lsd_fr = tone_index,
    lsd_en = tone_index_en,
    manual = manual_score
  ) %>%
  mutate(
    lsd_fr_bin = ifelse(lsd_fr > 0, "positive", ifelse(lsd_fr < 0, "negative", "neutral")),
    lsd_en_bin = ifelse(lsd_en > 0, "positive", ifelse(lsd_en < 0, "negative", "neutral")),
    manual_bin = ifelse(manual > 0, "positive", ifelse(manual < 0, "negative", "neutral"))
  ) %>%
  select(doc_id, date, source_media, sentences, sentences_en, lsd_fr, lsd_fr_bin, lsd_en, lsd_en_bin, manual, manual_bin)

saveRDS(df, "data/tmp/data_manual_ranking.rds" )
