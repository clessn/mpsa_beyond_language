library(stringr)
library(quanteda)
library(tidyr)
library(tidytext)
library(purrr)
library(dplyr)

source(file = "src/91_LSDprep_dec2017.R")

df <- readRDS("data/tmp/news_df_tone_index.rds")

set.seed(42069)

sample_size <- 200

sample_ids <- sample(df$doc_id, sample_size) 

sample_df <- df %>%
  filter(doc_id %in% sample_ids)

df_fr <- sample_df %>%
  select(doc_id, text_body, date, source_media)

df_en <- sample_df %>%
  select(doc_id, en_text_body, date, source_media)

df_sample_fr <- df_fr %>%
  # Split text into sentences
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  # Unnest to get one row per sentence
  unnest(sentences) %>%
  # Group by document ID to number sentences within each document
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  # Create sentence_id that combines doc_id and sentence position
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  ungroup() %>%
  # filter for sentence_id that ends with _1
  filter(str_detect(sentence_id, "_1$"))

df_sample_en <- df_en %>%
  # Split text into sentences
  mutate(sentences = str_split(en_text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  # Unnest to get one row per sentence
  unnest(sentences) %>%
  # Group by document ID to number sentences within each document
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  # Create sentence_id that combines doc_id and sentence position
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  ungroup() %>%
  # filter for sentence_id that ends with _1
  filter(str_detect(sentence_id, "_1$"))

# Merge the two dataframes
df_merged <- df_sample_fr %>%
  left_join(
    df_sample_en %>% rename(en_sentences = sentences),
    by = c("doc_id", "sentence_id", "sentence_number","date", "source_media")
  )

df_clean_sentences <- df_merged %>%
  mutate(
    sentences_clean = sentences %>%
      str_to_lower() %>%
      str_remove_all("!") %>%
      str_remove_all("\\?") %>%
      str_remove_all("\\.") %>%
      str_remove_all(","),    

    sentences_clean_en = en_sentences %>%
      pbapply::pbsapply(LSDprep_contr) %>%
      pbapply::pbsapply(LSDprep_dict_punct) %>%
      pbapply::pbsapply(remove_punctuation_from_acronyms) %>%
      pbapply::pbsapply(remove_punctuation_from_abbreviations) %>%
      pbapply::pbsapply(LSDprep_punctspace) %>%
      pbapply::pbsapply(LSDprep_negation) %>%
      pbapply::pbsapply(LSDprep_dict) %>%
      pbapply::pbsapply(mark_proper_nouns)
  )

# Create named corpus objects
corpus_fr <- corpus(df_clean_sentences$sentences_clean, 
                   docnames = df_clean_sentences$doc_id)
corpus_en <- corpus(df_clean_sentences$sentences_clean_en, 
                    docnames = df_clean_sentences$doc_id)

# Now tokenize from the corpus objects to maintain the original doc_ids
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

# Conversion de la matrice de fréquence des termes en dataframe
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "doc_id") %>%
  mutate(doc_id = as.numeric(doc_id))
resultats_sentiment_en <- quanteda::convert(matrice_sentiment_en, to = "data.frame", docid_field = "doc_id") %>%
  rename(en_negative = negative, en_positive = positive, en_neg_positive = neg_positive, en_neg_negative = neg_negative) %>%
  mutate(doc_id = as.numeric(doc_id))

# Merge all three dataframes
df_combined <- df_clean_sentences %>%
  left_join(resultats_sentiment, by = "doc_id") %>%
  left_join(resultats_sentiment_en, by = "doc_id")

# SECTION 5: CALCUL DES MÉTRIQUES DE SENTIMENT
###############################################################################
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
  select(-c(sentence_number, sentence_id, text_body, en_text_body, sentences_clean, sentences_clean_en, positive, negative, en_positive, en_negative, en_neg_positive, en_neg_negative, total_words, total_words_en, proportion_positive, proportion_negative, proportion_positive_en, proportion_negative_en))

saveRDS(df_lsd, "data/tmp/data_manual_ranking.rds" )
