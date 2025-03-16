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

search_pattern <- paste0(
  # Free/Libre Software terms
  "logiciel[s]?\\s+libres?|",
  "software\\s+libres?|",
  "free\\s+software|",
  "code\\s+source\\s+(libre|ouvert)|",
  
  # Open Source terms
  "open[\\s-]source|",
  "logiciel[s]?\\s+open[\\s-]source|",
  
  # Organizations and people
  "free\\s+software\\s+foundation|",
  "fsf|gnu|",
  "richard\\s+stallman|rms|",
  "linus\\s+torvalds|",
  
  # Related concepts
  "\\bfoss\\b|\\bfloss\\b|",
  "source\\s+ouverte?|",
  "licence[s]?\\s+(gpl|mit|apache|bsd)|",
  "copyleft|",
  "linux|gnu\\/linux|ubuntu|debian|",
  "\\bgit\\b|github|gitlab|",
  
  # French variations with optional spaces and hyphens
  "libres?\\s+de\\s+droit[s]?|",
  "communauté[s]?\\s+open[\\s-]source"
)

sample_ids <- sample(df$doc_id, sample_size) 

df <- df %>%
  select(doc_id, text_body, date, source_media)

df_sample <- df %>%
  mutate(sentences = str_split(text_body, "(?<=\\.|\\?|\\!)\\s+")) %>%
  unnest(sentences) %>%
  group_by(doc_id) %>%
  mutate(sentence_number = row_number()) %>%
  mutate(sentence_id = paste(doc_id, sentence_number, sep = "_")) %>%
  ungroup() %>%
  filter(str_detect(sentences, search_pattern))

sample_sentences <- sample(df_sample$sentence_id, sample_size) 

df_sentences <- df_sample %>%
  filter(sentence_id %in% sample_sentences) %>%
  mutate(sentences_en = NA)

for (i in 1:nrow(df_sentences)) {
  cat(i, "/", nrow(df_sentences), "\n")
  print(paste0("Original: ", df_sentences$sentences[i]))
  df_sentences$sentences_en[i] <- google_translate(df_sentences$sentences[i], "en", "fr")
  print(paste0("Translated: ", df_sentences$sentences_en[i]))
}

df_clean_sentences <- df_sentences %>%
  mutate(
    sentences_clean = sentences %>%
      str_to_lower() %>%
      str_remove_all("!") %>%
      str_remove_all("\\?") %>%
      str_remove_all("\\.") %>%
      str_remove_all(","),    

    sentences_clean_en = sentences_en %>%
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
                   docnames = df_clean_sentences$sentence_id)
corpus_en <- corpus(df_clean_sentences$sentences_clean_en, 
                    docnames = df_clean_sentences$sentence_id)

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
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "sentence_id")

resultats_sentiment_en <- quanteda::convert(matrice_sentiment_en, to = "data.frame", docid_field = "sentence_id") %>%
  rename(en_negative = negative, en_positive = positive, en_neg_positive = neg_positive, en_neg_negative = neg_negative)

# Merge all three dataframes
df_combined <- df_clean_sentences %>%
  left_join(resultats_sentiment, by = "sentence_id") %>%
  left_join(resultats_sentiment_en, by = "sentence_id")

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
  select(-c(sentence_number, sentence_id, text_body, sentences_clean, sentences_clean_en, positive, negative, en_positive, en_negative, en_neg_positive, en_neg_negative, total_words, total_words_en, proportion_positive, proportion_negative, proportion_positive_en, proportion_negative_en))

saveRDS(df_lsd, "data/tmp/data_manual_ranking.rds" )
