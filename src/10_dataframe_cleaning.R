# Drops articles with missing fields, parses dates and adds document ids.
#
# In:  data/raw/combined_news_articles.csv
# Out: data/tmp/news_df.rds

news_df <- read.csv("data/raw/combined_news_articles.csv") %>%
  filter(!is.na(source_media)) %>%
  filter(!is.na(publication_date)) %>%
  rename(date = publication_date) %>%
  mutate(date = lubridate::ymd(date), doc_id = row_number())

saveRDS(news_df, "data/tmp/news_df.rds")
