#################################################################
# INTER-CODER RELIABILITY AND THE HUMAN CEILING
#################################################################
# Three coders rated the 200 validation sentences, but their level of agreement
# was never measured. That agreement is the most consequential unreported number
# in the study, for two reasons.
#
# 1. IT IS THE STUDY'S STRONGEST POSITIVE RESULT. If the best models agree with
#    the human consensus more closely than a human coder does, "can LLMs do this
#    task" is answered in absolute terms, not merely relative to a dictionary.
#
# 2. IT IS A NOISE FLOOR. No model can meaningfully exceed the level at which
#    the humans agree with each other — past that point the metric measures
#    coder disagreement, not model quality. Rankings among the top models are
#    therefore not interpretable, and the prompt-language effect (~0.006) sits
#    far below this floor, which is a stronger way to state the null result than
#    a non-significant test.
#
# THE PART-WHOLE TRAP: comparing a coder to the mean of all three inflates the
# correlation, because that coder contributes a third of the mean. The ceiling
# below is computed leave-one-out — each coder against the mean of the OTHER
# two — which is the fair comparison against a model, since a model contributes
# nothing to the ground truth.
#
# Added: September 2026
#################################################################

library(dplyr)

#################################################################
# ASSEMBLE THE THREE CODERS
#################################################################
# Joined on (doc_id, sentences). Joining on doc_id alone silently multiplies
# rows for the six articles that contributed two sentences each — see the note
# in src/41_prompt_cleaning.R.
base <- readRDS("data/tmp/data_manual_ranking.rds") %>%
  select(doc_id, sentences, coder_1 = manual)

coder_2 <- readRDS("data/clean/annotator_1.rds") %>%
  select(doc_id, sentences, coder_2 = manual_sentiment_cam)

coder_3 <- readRDS("data/clean/annotator_2.rds") %>%
  select(doc_id, sentences, coder_3 = manual_sentiment_etienne)

coders <- base %>%
  inner_join(coder_2, by = c("doc_id", "sentences")) %>%
  inner_join(coder_3, by = c("doc_id", "sentences"))

if (nrow(coders) != nrow(base)) {
  stop(sprintf("Coder join changed the row count: %d in, %d out.",
               nrow(base), nrow(coders)))
}

M <- as.matrix(coders[, c("coder_1", "coder_2", "coder_3")])
cat("Sentences with all three coders:", nrow(M), "\n\n")

#################################################################
# 1. PAIRWISE AGREEMENT
#################################################################
pairs <- list(c(1, 2), c(1, 3), c(2, 3))
pairwise <- sapply(pairs, function(p) cor(M[, p[1]], M[, p[2]], use = "complete.obs"))

cat("=== PAIRWISE AGREEMENT BETWEEN CODERS ===\n")
for (i in seq_along(pairs)) {
  cat(sprintf("  coder %d vs coder %d   r = %.3f\n",
              pairs[[i]][1], pairs[[i]][2], pairwise[i]))
}
cat(sprintf("  mean                  r = %.3f\n\n", mean(pairwise)))

#################################################################
# 2. THE HUMAN CEILING (LEAVE-ONE-OUT)
#################################################################
loo <- sapply(1:3, function(k) {
  others <- rowMeans(M[, -k, drop = FALSE], na.rm = TRUE)
  cor(M[, k], others, use = "complete.obs")
})
human_ceiling <- mean(loo)

cat("=== HUMAN CEILING (each coder vs the mean of the other two) ===\n")
for (k in 1:3) cat(sprintf("  coder %d               r = %.3f\n", k, loo[k]))
cat(sprintf("  HUMAN CEILING         r = %.3f\n\n", human_ceiling))

#################################################################
# 3. DISPERSION
#################################################################
sd_per_sentence    <- apply(M, 1, sd, na.rm = TRUE)
range_per_sentence <- apply(M, 1, function(x) diff(range(x, na.rm = TRUE)))
sign_agreement     <- mean(apply(M, 1, function(x) length(unique(sign(x))) == 1), na.rm = TRUE)

cat("=== DISPERSION ACROSS THE THREE RATINGS ===\n")
cat(sprintf("  mean SD per sentence            %.3f\n", mean(sd_per_sentence, na.rm = TRUE)))
cat(sprintf("  mean range (max - min)          %.3f   on a -1 to 1 scale\n",
            mean(range_per_sentence, na.rm = TRUE)))
cat(sprintf("  all three agree on the SIGN     %.0f%% of sentences\n\n", 100 * sign_agreement))

#################################################################
# 4. MODELS AGAINST THE CEILING
#################################################################
# Read whichever df.rds is current: the 2024-2025 batch before the rerun
# completes, the 2026 batch afterwards. Column names differ between batches, so
# models are discovered rather than hard-coded.
df <- readRDS("data/clean/df.rds")

model_cols <- names(df)[
  grepl("_(fr_fr|en_fr|en_en)$", names(df)) &
  !grepl("_cat$|_bin$", names(df))
]
model_cols <- c(model_cols, intersect(c("lsd_fr", "lsd_en"), names(df)))

vs_ceiling <- data.frame(
  model = model_cols,
  r = sapply(model_cols, function(m) {
    ok <- !is.na(df[[m]]) & !is.na(df$ground_truth)
    if (sum(ok) < 5) return(NA_real_)
    cor(df[[m]][ok], df$ground_truth[ok])
  }),
  stringsAsFactors = FALSE
)
vs_ceiling$above_ceiling <- vs_ceiling$r > human_ceiling
vs_ceiling <- vs_ceiling[order(-vs_ceiling$r), ]
rownames(vs_ceiling) <- NULL

n_above <- sum(vs_ceiling$above_ceiling, na.rm = TRUE)

cat("=== MODELS AGAINST THE HUMAN CEILING ===\n")
cat(sprintf("  %d of %d model-condition combinations reach or exceed r = %.3f\n\n",
            n_above, nrow(vs_ceiling), human_ceiling))
top <- head(vs_ceiling, 12)
for (i in seq_len(nrow(top))) {
  cat(sprintf("  %-24s %.3f  %s\n", top$model[i], top$r[i],
              ifelse(isTRUE(top$above_ceiling[i]), "above human ceiling", "")))
}

#################################################################
# 5. SAVE
#################################################################
results <- list(
  n_sentences      = nrow(M),
  pairwise         = setNames(pairwise, c("c1_c2", "c1_c3", "c2_c3")),
  mean_pairwise    = mean(pairwise),
  leave_one_out    = setNames(loo, paste0("coder_", 1:3)),
  human_ceiling    = human_ceiling,
  mean_sd          = mean(sd_per_sentence, na.rm = TRUE),
  mean_range       = mean(range_per_sentence, na.rm = TRUE),
  sign_agreement   = sign_agreement,
  models_vs_ceiling = vs_ceiling
)

dir.create("results/analysis", showWarnings = FALSE, recursive = TRUE)
saveRDS(results, "results/analysis/intercoder_reliability.rds")

#################################################################
# 6. SENTENCE FOR THE MANUSCRIPT
#################################################################
cat("\n=== READY-TO-ADAPT SENTENCE FOR THE METHODS SECTION ===\n\n")
cat(sprintf(
"  The three coders agreed with one another at r = %.2f on average (pairwise),
  and each coder correlated with the mean of the other two at r = %.2f. This
  last figure is the ceiling any automated method can meaningfully reach: %d of
  the %d model-condition combinations evaluated here meet or exceed it.\n\n",
  mean(pairwise), human_ceiling, n_above, nrow(vs_ceiling)))

cat("Saved: results/analysis/intercoder_reliability.rds\n")
