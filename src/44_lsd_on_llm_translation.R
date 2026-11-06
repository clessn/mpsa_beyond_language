# Dictionary row of Table 1: the English Lexicoder on the GPT-5.6 Luna
# translation, prepared and scored exactly as in src/22 and src/23. Rescoring
# the Google translation the same way checks that the pipeline reproduces the
# stored lsd_en column. Run after src/43, which writes the rest of the table.
#
# In:  data/clean/df.rds, data/tmp/llm_translation/sentences_en_llm.rds
# Out: results/analysis/lsd_on_llm_translation.rds, row added to results/tables/translator_table.md

suppressMessages({
  library(dplyr)
  library(stringr)
  library(quanteda)
})
source("src/91_LSDprep_dec2017.R")
source("src/92_llm_helper_funcs.R")

df <- readRDS("data/clean/df.rds") %>% select(doc_id, sentences_en, ground_truth, lsd_en, lsd_fr)
df$sentences_en_llm <- readRDS("data/tmp/llm_translation/sentences_en_llm.rds")

lsd_score <- function(text) {
  steps <- list(LSDprep_contr, LSDprep_dict_punct, remove_punctuation_from_acronyms,
                remove_punctuation_from_abbreviations, LSDprep_punctspace, LSDprep_negation,
                LSDprep_dict, mark_proper_nouns)
  for (step in steps) text <- sapply(text, step, USE.NAMES = FALSE)
  counts <- convert(dfm(tokens_lookup(tokens(text), data_dictionary_LSD2015, nested_scope = "dictionary")),
                    to = "data.frame")
  ((counts$positive + counts$neg_negative) - (counts$negative + counts$neg_positive)) / str_count(text, "\\S+")
}

df$lsd_en_google_check <- lsd_score(df$sentences_en)
df$lsd_en_llm <- lsd_score(df$sentences_en_llm)
stopifnot(isTRUE(all.equal(df$lsd_en, df$lsd_en_google_check)))

evaluate <- function(x, label) {
  data.frame(text = label, n = sum(!is.na(x)), r = cor(x, df$ground_truth),
             mae = mean(abs(x - df$ground_truth)), f1_3 = polarity_f1(x, df$ground_truth))
}
results <- bind_rows(
  evaluate(df$lsd_fr, "French Lexicoder, French original"),
  evaluate(df$lsd_en, "English Lexicoder, Google translation"),
  evaluate(df$lsd_en_llm, "English Lexicoder, GPT-5.6 Luna translation")
)
print(results, digits = 3, row.names = FALSE)
saveRDS(list(results = results, scores = select(df, doc_id, lsd_en_llm)),
        "results/analysis/lsd_on_llm_translation.rds")

table_file <- "results/tables/translator_table.md"
table_lines <- grep("^\\| Lexicoder", readLines(table_file), value = TRUE, invert = TRUE)
writeLines(c(table_lines, sprintf(
  "| Lexicoder (dictionary) | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |",
  results$r[1], results$r[2], results$r[3], results$f1_3[1], results$f1_3[2], results$f1_3[3]
)), table_file)
