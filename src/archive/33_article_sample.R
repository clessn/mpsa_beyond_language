#################################################################
# ARTICLE-LEVEL VALIDATION: DRAW THE SAMPLE TO CODE
#################################################################
# The sentence benchmark favors the LLMs over a dictionary built for
# whole articles. This script draws a random sample of full articles
# for the three coders, in the format that src/99x_manual_notation_shiny.R
# accepts (one row per text, a text column and a translation column),
# with no model or dictionary score attached. Scores are joined back
# by src/34_article_validation.R once the coding is done.

library(dplyr)

set.seed(2026)
N_ARTICLES <- 60
MIN_WORDS  <- 150   # long enough to carry a tone
MAX_WORDS  <- 900   # short enough to code in a few minutes

corpus <- readRDS("data/clean/news_df_sentiment_corpus.rds") %>%
  mutate(n_words = lengths(strsplit(text_body, "[[:space:]]+"))) %>%
  filter(!is.na(gpt56luna_fr_fr), !is.na(gpt56luna_en_fr), !is.na(fr_tone_index),
         !is.na(en_text_body), n_words >= MIN_WORDS, n_words <= MAX_WORDS)

sample_df <- corpus %>%
  slice_sample(n = N_ARTICLES) %>%
  arrange(date) %>%
  transmute(doc_id, date, source_media, title, n_words,
            sentence_text = text_body,          # column the coding app displays
            sentence_translation = en_text_body)

dir.create("data/tmp/article_validation", showWarnings = FALSE)
saveRDS(sample_df, "data/tmp/article_validation/article_sample_for_coding.rds")
write.csv(sample_df %>% select(-sentence_translation),
          "data/tmp/article_validation/article_sample_for_coding.csv", row.names = FALSE)

cat(sprintf("Eligible articles: %d of %d. Sampled: %d (%d to %d words, median %d).\n",
            nrow(corpus), 2683, nrow(sample_df), min(sample_df$n_words), max(sample_df$n_words),
            median(sample_df$n_words)))
cat("Load data/tmp/article_validation/article_sample_for_coding.rds in the coding app,\n",
    "choose sentence_text as the text column, and save one file per coder as\n",
    "data/tmp/article_validation/coder_<k>.rds (columns doc_id and sentiment_score).\n")
