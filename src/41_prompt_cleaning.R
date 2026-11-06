# Builds the analysis dataset from the model scores: adds the two other
# coders, averages the three ratings into the ground truth, and recodes every
# score into the seven sentiment categories used for the F1 scores.
#
# In:  data/tmp/data_manual_ranking_with_llm_scores.rds, data/clean/annotator_*.rds
# Out: data/clean/df.rds, data/clean/df_fscores.rds

library(dplyr)
library(stringr)

scores <- readRDS("data/tmp/data_manual_ranking_with_llm_scores.rds")

# Sentences are identified by (doc_id, sentences): six articles contribute two
# sentences each, so joining on doc_id alone would duplicate rows.
coder_2 <- readRDS("data/clean/annotator_1.rds") %>%
  select(doc_id, sentences, coder_2 = manual_sentiment_cam)
coder_3 <- readRDS("data/clean/annotator_2.rds") %>%
  select(doc_id, sentences, coder_3 = manual_sentiment_etienne)

with_coders <- scores %>%
  merge(coder_2, by = c("doc_id", "sentences")) %>%
  merge(coder_3, by = c("doc_id", "sentences")) %>%
  # Rounded so that ratings that cancel out (0.1, -0.2, 0.1) give exactly 0
  mutate(ground_truth = round(rowMeans(cbind(manual, coder_2, coder_3), na.rm = TRUE), 10)) %>%
  select(-manual, -coder_2, -coder_3)

stopifnot(nrow(with_coders) == nrow(scores))

to_category <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x < -0.66 ~ "very_negative",
    x < -0.33 ~ "negative",
    x < 0 ~ "somewhat_negative",
    x == 0 ~ "neutral",
    x <= 0.33 ~ "somewhat_positive",
    x <= 0.66 ~ "positive",
    TRUE ~ "very_positive"
  )
}
categories <- c("very_negative", "negative", "somewhat_negative", "neutral",
                "somewhat_positive", "positive", "very_positive")

df <- with_coders %>%
  select(-contains("_run"), -ends_with("_bin")) %>%
  mutate(across(
    ends_with(c("_en", "_fr", "_mean", "_truth")) & !matches("sentences_en"),
    ~ factor(to_category(.x), levels = categories, ordered = TRUE),
    .names = "{str_remove(.col, '_mean')}_cat"
  )) %>%
  rename_with(~ str_remove(.x, "_mean$"), ends_with("_mean")) %>%
  select(doc_id, date, source_media, sentences, sentences_en,
         starts_with("ground"), starts_with("lsd"), everything())

saveRDS(df, "data/clean/df.rds")

df_fscores <- df %>%
  select(ends_with("_cat")) %>%
  rename_with(~ str_remove(.x, "_cat$"))

saveRDS(df_fscores, "data/clean/df_fscores.rds")
