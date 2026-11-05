# Draws the 200 benchmark sentences, translates them with Google Translate
# (polyglotr) and scores them with the French and English Lexicoder.
#
# In:  data/tmp/news_df_tone_index.rds, data/dict/frlsd_dict.rds
# Out: data/tmp/data_manual_ranking.rds

library(stringr)
library(quanteda)
library(tidyr)
library(tidytext)
library(purrr)
library(dplyr)
library(polyglotr)

source(file = "src/91_LSDprep_dec2017.R")

df <- readRDS("data/tmp/news_df_tone_index.rds")

set.seed(42069)

sample_size <- 200

sample_ids <- sample(df$doc_id, sample_size)

df <- df %>%
  select(doc_id, text_body, date, source_media)

df_sample <- df %>%
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  unnest(sentences) %>%
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  ungroup()

sample_sentences <- sample(df_sample$sentence_id, sample_size)

df_sentences <- df_sample %>%
  filter(sentence_id %in% sample_sentences) %>%
  mutate(sentences_en = NA)

for (i in 1:nrow(df_sentences)) {
  cat(i, "/", nrow(df_sentences), "\n")
  print(paste0("Original: ", df_sentences$sentences[i]))

  # URLs, including ones broken by spaces, are masked before translation
  url_pattern <- "https?:\\s*//\\s*[^\\s]+|www\\.[^\\s]+"

  parentheses_url_pattern <- "\\([^)]*(?:https?:|www\\.)[^)]*\\)"

  text <- paste0(df_sentences$sentences[i])
  text <- str_replace_all(text, parentheses_url_pattern, "[WEBSITE URL]")
  text <- str_replace_all(text, url_pattern, "[WEBSITE URL]")

  df_sentences$sentences_en[i] <- google_translate(text, "en", "fr")
  print(paste0("Translated: ", df_sentences$sentences_en[i]))
}

df_clean_sentences <- df_sentences %>%
  mutate(
    sentences_clean = sentences %>%
      str_to_lower() %>%           # Convert to lowercase
      str_remove_all("!") %>%      # Remove exclamation marks
      str_remove_all("\\?") %>%    # Remove question marks
      str_remove_all("\\.") %>%    # Remove periods
      str_remove_all(","),         # Remove commas

    sentences_clean_en = sentences_en %>%
      pbapply::pbsapply(LSDprep_contr) %>%                      # Process contractions
      pbapply::pbsapply(LSDprep_dict_punct) %>%                 # Handle punctuation
      pbapply::pbsapply(remove_punctuation_from_acronyms) %>%   # Process acronyms
      pbapply::pbsapply(remove_punctuation_from_abbreviations) %>% # Process abbreviations
      pbapply::pbsapply(LSDprep_punctspace) %>%                 # Standardize spacing
      pbapply::pbsapply(LSDprep_negation) %>%                   # Handle negations
      pbapply::pbsapply(LSDprep_dict) %>%                       # Dictionary preparation
      pbapply::pbsapply(mark_proper_nouns)                      # Mark proper nouns
  )

corpus_fr <- corpus(df_clean_sentences$sentences_clean,
                   docnames = df_clean_sentences$sentence_id)
corpus_en <- corpus(df_clean_sentences$sentences_clean_en,
                    docnames = df_clean_sentences$sentence_id)

tokens_fr_with_stopwords <- tokens(corpus_fr)
tokens_en <- tokens(corpus_en)

tokens_fr <- quanteda::tokens_remove(
  tokens_fr_with_stopwords,
  stopwords("french")  # Use French stopwords
)

lsdfr <- dictionary(readRDS("data/dict/frlsd_dict.rds"))
lsden <- data_dictionary_LSD2015

matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_fr, lsdfr, nested_scope = "dictionary")
)

matrice_sentiment_en <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_en, lsden, nested_scope = "dictionary")
)

resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "sentence_id")

resultats_sentiment_en <- quanteda::convert(matrice_sentiment_en, to = "data.frame", docid_field = "sentence_id") %>%
  rename(en_negative = negative, en_positive = positive, en_neg_positive = neg_positive, en_neg_negative = neg_negative)

df_combined <- df_clean_sentences %>%
  left_join(resultats_sentiment, by = "sentence_id") %>%
  left_join(resultats_sentiment_en, by = "sentence_id")

df_lsd <- df_combined %>%
  mutate(
    total_words = str_count(sentences_clean, "\\S+"),
    total_words_en = str_count(sentences_clean_en, "\\S+"),

    proportion_positive = (positive) / total_words,
    proportion_negative = (negative) / total_words,

    proportion_positive_en = (en_positive + en_neg_negative) / total_words_en,
    proportion_negative_en = (en_negative + en_neg_positive) / total_words_en,

    tone_index = proportion_positive - proportion_negative,
    tone_index_en = proportion_positive_en - proportion_negative_en
  ) %>%
  select(-c(sentence_number, sentence_id, text_body, sentences_clean, sentences_clean_en, positive, negative, en_positive, en_negative, en_neg_positive, en_neg_negative, total_words, total_words_en, proportion_positive, proportion_negative, proportion_positive_en, proportion_negative_en))

saveRDS(df_lsd, "data/tmp/data_manual_ranking.rds")
