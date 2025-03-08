library(dplyr)

df <- readRDS("data/tmp/news_df_tone_index.rds")

set.seed(42069)

sample_size <- 200

sample_ids <- sample(df$doc_id, sample_size) 

sample_df <- df %>%
  filter(doc_id %in% sample_ids)
