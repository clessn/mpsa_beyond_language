#################################################################
# DATA CLEANING AND PREPARATION
#################################################################
# This script performs initial data cleaning and preparation on the 
# combined news articles dataset. It filters out records with missing
# critical fields, standardizes date formats, and adds document IDs.

# Load and clean the dataset
news_df <- read.csv("data/raw/combined_news_articles.csv") %>%
  # Remove records with missing critical fields
  filter(!is.na(source_media)) %>%
  filter(!is.na(publication_date)) %>%
  # Standardize column names
  rename(date = publication_date) %>%
  # Convert dates to proper format and add document IDs
  mutate(date = lubridate::ymd(date), doc_id = row_number())

# Save the cleaned dataset for further processing
saveRDS(news_df, "data/tmp/news_df.rds")