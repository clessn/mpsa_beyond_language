# Agreement between the three coders and the human ceiling for the models:
# pairwise correlations, Krippendorff's alpha (interval), the reliability of
# the three-coder mean, and each coder against the mean of the other two
# (leave-one-out, so that a coder is never compared with a mean it is part of).
#
# In:  data/tmp/data_manual_ranking.rds, data/clean/annotator_*.rds, data/clean/df.rds
# Out: results/analysis/intercoder_reliability.rds

library(dplyr)

coders <- readRDS("data/tmp/data_manual_ranking.rds") %>%
  select(doc_id, sentences, coder_1 = manual) %>%
  inner_join(readRDS("data/clean/annotator_1.rds") %>%
               select(doc_id, sentences, coder_2 = manual_sentiment_cam),
             by = c("doc_id", "sentences")) %>%
  inner_join(readRDS("data/clean/annotator_2.rds") %>%
               select(doc_id, sentences, coder_3 = manual_sentiment_etienne),
             by = c("doc_id", "sentences"))
stopifnot(nrow(coders) == 200)

ratings <- as.matrix(coders[, c("coder_1", "coder_2", "coder_3")])

pairwise <- c(c1_c2 = cor(ratings[, 1], ratings[, 2]),
              c1_c3 = cor(ratings[, 1], ratings[, 3]),
              c2_c3 = cor(ratings[, 2], ratings[, 3]))

leave_one_out <- sapply(1:3, function(k) cor(ratings[, k], rowMeans(ratings[, -k])))
names(leave_one_out) <- paste0("coder_", 1:3)
human_ceiling <- mean(leave_one_out)

alpha_interval <- function(m) {
  values <- as.vector(t(m))
  n <- length(values)
  observed <- sum(apply(m, 1, function(r) sum(outer(r, r, "-")^2) / (length(r) - 1))) / n
  expected <- sum(outer(values, values, "-")^2) / (n * (n - 1))
  1 - observed / expected
}
alpha <- alpha_interval(ratings)
set.seed(20260916)
alpha_boot <- replicate(2000, alpha_interval(ratings[sample(nrow(ratings), replace = TRUE), ]))

# Spearman-Brown reliability of the mean of the three coders
reliability_of_mean <- 3 * mean(pairwise) / (1 + 2 * mean(pairwise))

df <- readRDS("data/clean/df.rds")
models <- grep("_(fr_fr|en_fr|en_en)$|^lsd_(fr|en)$", names(df), value = TRUE)
models_vs_ceiling <- data.frame(
  model = models,
  r = sapply(models, function(m) cor(df[[m]], df$ground_truth, use = "complete.obs"))
) %>%
  mutate(above_ceiling = r > human_ceiling) %>%
  arrange(desc(r))

results <- list(
  n_sentences = nrow(ratings),
  pairwise = pairwise,
  mean_pairwise = mean(pairwise),
  leave_one_out = leave_one_out,
  human_ceiling = human_ceiling,
  mean_sd = mean(apply(ratings, 1, sd)),
  mean_range = mean(apply(ratings, 1, function(x) diff(range(x)))),
  sign_agreement = mean(apply(ratings, 1, function(x) length(unique(sign(x))) == 1)),
  krippendorff_alpha = alpha,
  krippendorff_alpha_ci = unname(quantile(alpha_boot, c(0.025, 0.975))),
  reliability_of_mean = reliability_of_mean,
  models_vs_ceiling = models_vs_ceiling
)
saveRDS(results, "results/analysis/intercoder_reliability.rds")

cat(sprintf("Krippendorff's alpha %.3f [%.3f, %.3f]; mean pairwise r %.3f; human ceiling %.3f\n",
            alpha, results$krippendorff_alpha_ci[1], results$krippendorff_alpha_ci[2],
            mean(pairwise), human_ceiling))
