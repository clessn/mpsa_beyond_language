# French Lexicoder on the full corpus: (positive - negative) / words for each
# article.
#
# In:  data/tmp/news_df.rds, data/dict/frlsd_dict.rds
# Out: data/tmp/news_df_sentiment.rds

library(stringr)
library(quanteda)
library(tidyr)
library(tidytext)
library(purrr)
library(dplyr)

df <- readRDS("data/tmp/news_df.rds")

df_prepped <- df %>%
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  unnest(sentences) %>%
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  ungroup() %>%
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  group_by(doc_id) %>%
  filter(n() > 0) %>%
  ungroup()

clean_sentences <- df_prepped %>%
  mutate(
    sentences_clean = sentences %>%
      str_to_lower() %>%                # Convert to lowercase
      str_remove_all("!") %>%           # Remove exclamation marks
      str_remove_all("\\?") %>%         # Remove question marks
      str_remove_all("\\.") %>%         # Remove periods
      str_remove_all(",")               # Remove commas
  )

corpus_texte <- quanteda::tokens(clean_sentences$sentences_clean)

tokens_clean <- quanteda::tokens_remove(
  corpus_texte,
  stopwords("french")  # Use French stopwords
)

frlsd <- dictionary(readRDS("data/dict/frlsd_dict.rds"))

matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_clean, frlsd, nested_scope = "dictionary")
)

resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "id")

articles_sentiment <- cbind(clean_sentences, resultats_sentiment) %>%
  select(-id)  # Remove redundant ID column

articles_sentiment <- articles_sentiment %>%
  mutate(
    total_words = str_count(sentences_clean, "\\S+"),  # Count total words
    proportion_positive = (positive) / total_words,     # Positive word proportion
    proportion_negative = (negative) / total_words,     # Negative word proportion
    tone_index = proportion_positive - proportion_negative  # Overall tone index
  )

df_aggregated <- articles_sentiment %>%
  group_by(doc_id) %>%
  summarise(
    total_positive = sum(positive, na.rm = TRUE),
    total_negative = sum(negative, na.rm = TRUE),
    total_words = sum(total_words, na.rm = TRUE),
    proportion_positive = sum(positive) / sum(total_words),
    proportion_negative = sum(negative) / sum(total_words),
    tone_index = (sum(positive) - sum(negative)) / (sum(positive) + sum(negative)),
    .groups = "drop"
  )

df_final <- articles_sentiment %>%
  select(doc_id, source_media, date) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  left_join(df_aggregated, by = "doc_id")

saveRDS(df_final, "data/tmp/news_df_sentiment.rds")
