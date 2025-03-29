library(dplyr)

library(stringr)

df_raw <- readRDS("data/tmp/data_manual_ranking_with_llm_scores.rds")

df <- df_raw %>% 
  select(-contains("_run"), -contains("distill"), -manual_bin) %>%
  rename(
    ground_truth = "manual",
  ) %>%
  # Create 7-category Likert transformation for columns ending with *en or *fr
  mutate(across(
    ends_with(c("_en", "_fr", "_mean", "_truth")) & !matches("sentences_en"),
    ~ case_when(
      . < -0.66 ~ "very_negative",
      . < -0.33 ~ "negative",
      . < 0 ~ "somewhat_negative",
      . == 0 ~ "neutral",
      . <= 0.33 ~ "somewhat_positive",
      . <= 0.66 ~ "positive",
      TRUE ~ "very_positive"
    ),
    .names = "{str_remove(.col, '_mean')}_cat"
  )) %>%
  # Convert the new categorical columns to ordered factors
  mutate(across(
    ends_with("_cat"),
    ~ factor(
      .,
      levels = c("very_negative", "negative", "somewhat_negative", "neutral", 
                 "somewhat_positive", "positive", "very_positive"),
      ordered = TRUE
    )
  )) %>%
  rename_with(~ gsub("_mean$", "", .), matches("_mean$")) %>%
  select(-ends_with("_bin")) %>%
  select(doc_id, date, source_media, sentences, sentences_en, starts_with("ground"), starts_with("lsd"), everything())

saveRDS(df, "data/clean/df.rds")

df_fscores <- df %>%
  select(-ends_with("_en"), -ends_with("_fr"), -ends_with("_truth")) %>%
  rename_with(~ gsub("_cat$", "", .), ends_with("_cat")) %>%
  select(-c(doc_id, date, source_media, sentences))

saveRDS(df_fscores, "data/clean/df_fscores.rds")
