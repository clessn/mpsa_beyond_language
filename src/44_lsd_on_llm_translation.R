#################################################################
# ENGLISH LEXICODER ON THE LLM TRANSLATION OF THE BENCHMARK
#################################################################
# Completes the translator check (src/43) for the dictionary: scores the
# 200 benchmark sentences translated by GPT-5.6 Luna with the English
# Lexicoder, using exactly the preparation and scoring of src/22 and
# src/23, and rescored the Google translation the same way as a check
# that the pipeline reproduces the lsd_en column of data/clean/df.rds.

suppressMessages({library(dplyr); library(stringr); library(quanteda)})
source("src/91_LSDprep_dec2017.R")

df <- readRDS("data/clean/df.rds") %>% select(doc_id, sentences_en, ground_truth, lsd_en, lsd_fr)
df$sentences_en_llm <- readRDS("data/tmp/llm_translation/sentences_en_llm.rds")

lsd_score <- function(text) {
  prepped <- text
  for (f in list(LSDprep_contr, LSDprep_dict_punct, remove_punctuation_from_acronyms,
                 remove_punctuation_from_abbreviations, LSDprep_punctspace, LSDprep_negation,
                 LSDprep_dict, mark_proper_nouns)) prepped <- sapply(prepped, f, USE.NAMES = FALSE)
  d <- convert(dfm(tokens_lookup(tokens(prepped), data_dictionary_LSD2015, nested_scope = "dictionary")), to = "data.frame")
  total <- str_count(prepped, "\\S+")
  ((d$positive + d$neg_negative) - (d$negative + d$neg_positive)) / total
}

df$lsd_en_google_check <- lsd_score(df$sentences_en)
df$lsd_en_llm <- lsd_score(df$sentences_en_llm)

to3 <- function(x) ifelse(x < 0, "negative", ifelse(x == 0, "neutral", "positive"))
f1_3 <- function(pred, truth) {
  lv <- c("negative", "neutral", "positive"); cm <- table(factor(pred, lv), factor(truth, lv))
  f1 <- sapply(seq_along(lv), function(i) { tp <- cm[i, i]; p <- tp / max(sum(cm[i, ]), 1); r <- tp / max(sum(cm[, i]), 1)
    if (p + r == 0) 0 else 2 * p * r / (p + r) })
  sum(f1 * colSums(cm)) / sum(cm)
}
summ <- function(x, label) { ok <- !is.na(x) & !is.na(df$ground_truth)
  data.frame(text = label, n = sum(ok), r = cor(x[ok], df$ground_truth[ok]), mae = mean(abs(x[ok] - df$ground_truth[ok])),
             f1_3 = f1_3(to3(x[ok]), to3(df$ground_truth[ok]))) }
out <- bind_rows(summ(df$lsd_fr, "French Lexicoder, French original"),
                 summ(df$lsd_en, "English Lexicoder, Google translation (df.rds)"),
                 summ(df$lsd_en_google_check, "English Lexicoder, Google translation (recomputed)"),
                 summ(df$lsd_en_llm, "English Lexicoder, GPT-5.6 Luna translation"))
print(out, digits = 3, row.names = FALSE)
cat(sprintf("Pipeline check: recomputed vs stored lsd_en, r = %.4f\n", cor(df$lsd_en, df$lsd_en_google_check, use = "complete.obs")))
saveRDS(list(results = out, scores = df %>% select(doc_id, lsd_en_llm)), "results/analysis/lsd_on_llm_translation.rds")

# ---- Append the dictionary row to the manuscript table written by src/43 ----
tbl_file <- "results/tables/translator_table.md"
lines <- readLines(tbl_file)
lines <- lines[!grepl("^\\| Lexicoder", lines)]
row <- sprintf("| Lexicoder (dictionary) | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |",
               out$r[1], out$r[2], out$r[4], out$f1_3[1], out$f1_3[2], out$f1_3[4])
writeLines(c(lines, row), tbl_file)
cat("Appended the dictionary row to", tbl_file, "\n")
