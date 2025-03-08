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
      str_to_lower() %>%                # Conversion en minuscules
      str_remove_all("!") %>%           # Suppression des points d'exclamation
      str_remove_all("\\?") %>%
      str_remove_all("\\.") %>%         # Suppression des points
      str_remove_all(",")               # Suppression des virgules
  )

corpus_texte <- quanteda::tokens(clean_sentences$sentences_clean)

# Remove stopwords here - ADDED THIS STEP
tokens_clean <- quanteda::tokens_remove(
  corpus_texte, 
  stopwords("french")  # Use French stopwords
)

frlsd <- dictionary(readRDS("data/dict/frlsd_dict.rds"))

matrice_sentiment <- quanteda::dfm(
  quanteda::tokens_lookup(tokens_clean, frlsd, nested_scope = "dictionary")
)

# Conversion de la matrice de fréquence des termes en dataframe
resultats_sentiment <- quanteda::convert(matrice_sentiment, to = "data.frame", docid_field = "id")

# Combinaison des résultats de sentiment avec les données originales
articles_sentiment <- cbind(clean_sentences, resultats_sentiment) %>%
  select(-id)  # Supprimer la colonne d'identifiant redondante

# SECTION 5: CALCUL DES MÉTRIQUES DE SENTIMENT
###############################################################################
articles_sentiment <- articles_sentiment %>%
  mutate(
    total_words = str_count(sentences_clean, "\\S+"),  
    proportion_positive = (positive) / total_words,  
    proportion_negative = (negative) / total_words, 
    tone_index = proportion_positive - proportion_negative  
  )

# SECTION 6: AGRÉGATION PAR DOCUMENT
###############################################################################
# Aggregate sentiment scores by document (corrected)
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

# Keep only one row per document with metadata
df_final <- articles_sentiment %>%
  # Select document-level metadata (ensure these are constant within doc_id)
  select(doc_id, source_media, date) %>%
  # Keep only unique document-level rows
  distinct(doc_id, .keep_all = TRUE) %>%  # <-- Critical fix here
  # Merge aggregated results
  left_join(df_aggregated, by = "doc_id")

# Save the final dataframe
saveRDS(df_final, "data/tmp/news_df_sentiment.rds")
