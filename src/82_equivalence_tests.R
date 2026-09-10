#################################################################
# EQUIVALENCE TESTS FOR THE PROMPT-LANGUAGE COMPARISON
#################################################################
# The paper's central claim is a NULL result: prompt language does not affect
# performance. src/81_performance_by_condition.R supports it with paired t-tests
# that come back non-significant.
#
# That is the wrong test for the claim. A non-significant t-test says "we did
# not detect a difference", which with 12 paired models mostly reflects low
# power. A reviewer will answer, correctly, that absence of evidence is not
# evidence of absence.
#
# The right tool is a TWO ONE-SIDED TESTS procedure (TOST). Instead of asking
# "is the difference non-zero?", it asks "is the difference smaller than a bound
# we declare negligible?". A significant TOST is positive evidence of
# equivalence — an affirmative claim rather than a failure to reject.
#
# Mechanically TOST is two ordinary one-sided t-tests, and it is equivalent to
# checking whether the 90% confidence interval of the difference falls entirely
# inside the bound. Nothing here is more complex than a t-test.
#
# CHOOSING THE BOUND. It must be declared, not fished for. Two defensible
# anchors are reported below:
#
#   - the spread among the study's own human coders, so "negligible" means
#     "smaller than the disagreement between our own coders";
#   - a conventional 0.05 on the correlation scale.
#
# The script also reports the TIGHTEST bound the data supports, which is the
# most informative single number: it lets the paper say "we can rule out any
# difference larger than X" instead of "we found nothing".
#
# Added: September 2026
#################################################################

library(dplyr)
library(tidyr)
library(stringr)

DECLARED_BOUND <- 0.05   # on each metric's own scale; see note above

#################################################################
# PER-MODEL, PER-CONDITION PERFORMANCE
#################################################################
df <- readRDS("data/clean/df.rds")

model_cols <- names(df)[
  grepl("_(fr_fr|en_fr|en_en)$", names(df)) & !grepl("_cat$|_bin$", names(df))
]

# Dictionaries have no prompt language, and reasoning-mode models with unstable
# output formatting are excluded from the condition analysis, consistent with
# src/65_averages_summary.R. Adjust this pattern if the exclusion list changes.
EXCLUDE <- "lsd|qwq|deepseekr1"
model_cols <- model_cols[!grepl(EXCLUDE, model_cols)]

perf <- data.frame(
  col = model_cols,
  base_model = str_remove(model_cols, "_(fr_fr|en_fr|en_en)$"),
  condition  = str_extract(model_cols, "(fr_fr|en_fr|en_en)$"),
  stringsAsFactors = FALSE
)
perf$correlation <- sapply(perf$col, function(m) {
  ok <- !is.na(df[[m]]) & !is.na(df$ground_truth)
  if (sum(ok) < 5) NA_real_ else cor(df[[m]][ok], df$ground_truth[ok])
})
perf$mae <- sapply(perf$col, function(m) {
  ok <- !is.na(df[[m]]) & !is.na(df$ground_truth)
  if (sum(ok) < 5) NA_real_ else mean(abs(df[[m]][ok] - df$ground_truth[ok]))
})

cat(sprintf("Models in the condition analysis: %d, across %d conditions\n\n",
            length(unique(perf$base_model)), length(unique(perf$condition))))

#################################################################
# TOST
#################################################################

#' Two one-sided tests for paired differences
#'
#' @param d Vector of per-model differences between two conditions
#' @param bound Equivalence bound, on the same scale as d
#' @return One-row data frame
tost <- function(d, bound) {
  d <- d[!is.na(d)]
  n <- length(d)
  m <- mean(d); se <- sd(d) / sqrt(n)

  # Two one-sided tests: is d reliably above -bound, and reliably below +bound?
  t_lower <- (m + bound) / se
  t_upper <- (m - bound) / se
  p_lower <- pt(t_lower, df = n - 1, lower.tail = FALSE)  # H0: d <= -bound
  p_upper <- pt(t_upper, df = n - 1, lower.tail = TRUE)   # H0: d >= +bound
  p_tost  <- max(p_lower, p_upper)

  # 90% CI: equivalent decision rule — equivalence holds iff it sits inside
  ci <- m + c(-1, 1) * qt(0.95, df = n - 1) * se

  data.frame(
    n = n, mean_diff = m, ci_low = ci[1], ci_high = ci[2],
    p_tost = p_tost, equivalent = p_tost < 0.05,
    tightest_bound = max(abs(ci)),
    p_nhst = t.test(d)$p.value,
    stringsAsFactors = FALSE
  )
}

COMPARISONS <- list(
  c("fr_fr", "en_fr"),
  c("fr_fr", "en_en"),
  c("en_fr", "en_en")
)

results <- data.frame()
for (metric in c("correlation", "mae")) {
  wide <- perf %>%
    select(base_model, condition, all_of(metric)) %>%
    pivot_wider(names_from = condition, values_from = all_of(metric))

  for (cmp in COMPARISONS) {
    if (!all(cmp %in% names(wide))) next
    d <- wide[[cmp[1]]] - wide[[cmp[2]]]
    row <- tost(d, DECLARED_BOUND)
    row$metric <- metric
    row$comparison <- paste(cmp[1], "vs", cmp[2])
    results <- rbind(results, row)
  }
}

results <- results[, c("metric", "comparison", "n", "mean_diff", "ci_low",
                       "ci_high", "p_nhst", "p_tost", "equivalent",
                       "tightest_bound")]

#################################################################
# REPORT
#################################################################
cat(sprintf("=== EQUIVALENCE TESTS, declared bound = %.3f ===\n\n", DECLARED_BOUND))
cat(sprintf("  %-12s %-18s %9s %19s %8s %8s %6s\n",
            "metric", "comparison", "mean diff", "90% CI", "p NHST", "p TOST", "equiv"))
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("  %-12s %-18s %+9.4f  [%+.3f, %+.3f] %8.3f %8.3f %6s\n",
              r$metric, r$comparison, r$mean_diff, r$ci_low, r$ci_high,
              r$p_nhst, r$p_tost, ifelse(r$equivalent, "YES", "no")))
}

cat("\n=== TIGHTEST BOUND THE DATA SUPPORTS ===\n")
cat("  The largest difference compatible with the data, per comparison.\n")
cat("  This is what the paper can affirmatively rule out.\n\n")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("  %-12s %-18s  rules out differences larger than %.3f\n",
              r$metric, r$comparison, r$tightest_bound))
}

#################################################################
# ANCHOR THE BOUND IN THE STUDY'S OWN HUMAN CODERS
#################################################################
rel_file <- "results/analysis/intercoder_reliability.rds"
if (file.exists(rel_file)) {
  rel <- readRDS(rel_file)
  coder_spread <- diff(range(rel$leave_one_out))
  cat("\n=== A BOUND ANCHORED IN THE DATA ===\n")
  cat(sprintf("  The study's own three coders span r = %.3f to %.3f,\n",
              min(rel$leave_one_out), max(rel$leave_one_out)))
  cat(sprintf("  a spread of %.3f. A prompt-language difference smaller than the\n",
              coder_spread))
  cat("  disagreement among the human coders themselves is not a difference\n")
  cat("  worth acting on, which makes this a defensible declared bound.\n\n")

  worst <- max(results$tightest_bound[results$metric == "correlation"])
  cat(sprintf("  Largest correlation difference the data allows: %.3f\n", worst))
  cat(sprintf("  Spread among the human coders:                  %.3f\n", coder_spread))
  cat(sprintf("  => the prompt-language effect is %s the human coder spread.\n",
              ifelse(worst < coder_spread, "SMALLER than", "of the same order as")))
} else {
  cat("\n  Run src/56_intercoder_reliability.R first to anchor the bound in the\n")
  cat("  study's own coder disagreement.\n")
}

saveRDS(list(results = results, declared_bound = DECLARED_BOUND),
        "results/analysis/equivalence_tests.rds")
cat("\nSaved: results/analysis/equivalence_tests.rds\n")
