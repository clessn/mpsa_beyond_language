# Weighted F1 of every model and dictionary on the seven sentiment categories
# and on polarity (negative, neutral, positive), overall and by class. Classes
# are weighted by their frequency in the ground truth.
#
# In:  data/clean/df_fscores.rds
# Out: results/analysis/f1_scores_{7,3}.rds, results/analysis/f1_scores_detailed_{7,3}.rds

library(dplyr)

df <- readRDS("data/clean/df_fscores.rds")
models <- setdiff(names(df), "ground_truth")

seven <- c("very_negative", "negative", "somewhat_negative", "neutral",
           "somewhat_positive", "positive", "very_positive")
three <- c("negative", "neutral", "positive")

to_polarity <- function(x) {
  case_when(
    x %in% c("very_negative", "negative", "somewhat_negative") ~ "negative",
    x == "neutral" ~ "neutral",
    x %in% c("somewhat_positive", "positive", "very_positive") ~ "positive"
  )
}

f1_by_class <- function(pred, truth, classes) {
  cm <- table(factor(pred, levels = classes), factor(truth, levels = classes))
  f1 <- sapply(seq_along(classes), function(i) {
    precision <- if (sum(cm[i, ]) > 0) cm[i, i] / sum(cm[i, ]) else 0
    recall <- if (sum(cm[, i]) > 0) cm[i, i] / sum(cm[, i]) else 0
    if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0
  })
  c(setNames(f1, classes), weighted_f1 = sum(f1 * colSums(cm)) / sum(cm))
}

f1_table <- function(recode, classes) {
  truth <- recode(df$ground_truth)
  bind_rows(lapply(models, function(m) {
    data.frame(model = m, t(f1_by_class(recode(df[[m]]), truth, classes)))
  })) %>%
    arrange(desc(weighted_f1))
}

detailed_7 <- f1_table(as.character, seven)
detailed_3 <- f1_table(to_polarity, three)

saveRDS(detailed_7, "results/analysis/f1_scores_detailed_7.rds")
saveRDS(detailed_3, "results/analysis/f1_scores_detailed_3.rds")
saveRDS(select(detailed_7, model, weighted_f1), "results/analysis/f1_scores_7.rds")
saveRDS(select(detailed_3, model, weighted_f1), "results/analysis/f1_scores_3.rds")
