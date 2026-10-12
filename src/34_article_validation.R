#################################################################
# ARTICLE-LEVEL VALIDATION: SCORE THE METHODS AGAINST THE CODERS
#################################################################
# Reads the coders' files written by the coding app, averages them into
# an article-level ground truth, and compares GPT-5.6 Luna (French and
# English prompts), Jev (French and English prompts) and the two
# Lexicoder dictionaries on the same articles, with correlation and MAE.

library(dplyr)

files <- list.files("data/tmp/article_validation", pattern = "^coder_[0-9]+\\.rds$", full.names = TRUE)
if (length(files) < 2) stop("Need at least two coder files in data/tmp/article_validation/")

coded <- lapply(files, function(f) {
  x <- readRDS(f)
  score_col <- intersect(c("sentiment_score", "score", "manual"), names(x))[1]
  x %>% transmute(doc_id, score = as.numeric(.data[[score_col]]))
})
truth <- Reduce(function(a, b) full_join(a, b, by = "doc_id"), coded)
names(truth) <- c("doc_id", paste0("coder_", seq_along(files)))
truth$ground_truth <- rowMeans(truth[, -1], na.rm = TRUE)

cors <- cor(truth[, grep("^coder_", names(truth))], use = "pairwise.complete.obs")
cat(sprintf("Articles coded: %d. Mean pairwise correlation between coders: %.3f\n",
            nrow(truth), mean(cors[upper.tri(cors)])))

scores <- readRDS("data/clean/news_df_sentiment_jev.rds") %>%
  select(doc_id, luna_fr_fr, luna_en_fr, jev_fr_fr, jev_en_fr, lsd_fr, lsd_en)
d <- inner_join(truth, scores, by = "doc_id")

methods <- c(luna_fr_fr = "GPT-5.6 Luna, French prompt", luna_en_fr = "GPT-5.6 Luna, English prompt",
             jev_fr_fr = "Jev, French prompt", jev_en_fr = "Jev, English prompt",
             lsd_fr = "French Lexicoder on original", lsd_en = "English Lexicoder on translation")
out <- lapply(names(methods), function(m) {
  ok <- !is.na(d[[m]]) & !is.na(d$ground_truth)
  data.frame(method = methods[[m]], n = sum(ok),
             r = cor(d[[m]][ok], d$ground_truth[ok]),
             mae = mean(abs(d[[m]][ok] - d$ground_truth[ok])))
}) %>% bind_rows()
print(out, digits = 3, row.names = FALSE)
saveRDS(list(truth = truth, results = out), "results/analysis/article_validation.rds")
cat("Saved: results/analysis/article_validation.rds\n")
