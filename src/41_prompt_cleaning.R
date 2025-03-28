library(dplyr)
library(stringr)

df_raw<- readRDS("data/tmp/data_manual_ranking_with_llm_scores.rds")

df <- df_raw %>% 
  select(-contains("_run"), -contains("distill")) %>%
  rename(
    ground_truth = "manual",
    ground_truth_bin = "manual_bin"
  ) %>%
  mutate(across(
    ends_with("_mean"),
    ~ case_when(
      . < 0 ~ 0,
      . == 0 ~ 0.5,
      . > 0 ~ 1
    ),
    .names = "{str_remove(.col, '_mean')}_bin"
  )) %>%
  rename_with(~ gsub("_mean$", "", .), matches("_mean$"))

saveRDS(df, "data/clean/df.rds")


