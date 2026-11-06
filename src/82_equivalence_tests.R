# Equivalence tests (two one-sided tests, TOST) between conditions, with
# models as the units. A difference is declared negligible when its 90%
# confidence interval lies within ±0.05 on the metric's scale, a bound close
# to the spread among the three coders (each against the mean of the other
# two). Same model sets as src/81: the thirteen models of about 8B parameters
# and more, with Llama 3.2 3B added as a sensitivity check.
#
# In:  data/clean/cor_results.rds, results/analysis/f1_scores_{7,3}.rds
# Out: results/analysis/equivalence_tests.rds

library(dplyr)
library(tidyr)

bound <- 0.05

cor_results <- readRDS("data/clean/cor_results.rds")
f1_7 <- readRDS("results/analysis/f1_scores_7.rds")
f1_3 <- readRDS("results/analysis/f1_scores_3.rds")

performance <- cor_results %>%
  filter(grepl("_(fr_fr|en_fr|en_en)$", model), !grepl("^llama321b_", model)) %>%
  transmute(model,
            base_model = sub("_(fr_fr|en_fr|en_en)$", "", model),
            condition = sub(".*_(fr_fr|en_fr|en_en)$", "\\1", model),
            correlation, mae,
            f1_7 = f1_7$weighted_f1[match(model, f1_7$model)],
            f1_3 = f1_3$weighted_f1[match(model, f1_3$model)])

tost <- function(d) {
  n <- length(d)
  se <- sd(d) / sqrt(n)
  p_lower <- pt((mean(d) + bound) / se, df = n - 1, lower.tail = FALSE)
  p_upper <- pt((mean(d) - bound) / se, df = n - 1)
  ci <- mean(d) + c(-1, 1) * qt(0.95, df = n - 1) * se
  data.frame(n = n, mean_diff = mean(d), ci_low = ci[1], ci_high = ci[2],
             p_nhst = t.test(d)$p.value, p_tost = max(p_lower, p_upper),
             equivalent = max(p_lower, p_upper) < 0.05, tightest_bound = max(abs(ci)))
}

equivalence_tests <- function(data) {
  comparisons <- list(c("fr_fr", "en_fr"), c("fr_fr", "en_en"), c("en_fr", "en_en"))
  bind_rows(lapply(c("correlation", "mae", "f1_7", "f1_3"), function(metric) {
    wide <- data %>%
      select(base_model, condition, value = all_of(metric)) %>%
      pivot_wider(names_from = condition, values_from = value)
    bind_rows(lapply(comparisons, function(cmp) {
      cbind(metric = metric, comparison = paste(cmp[1], "vs", cmp[2]),
            tost(wide[[cmp[1]]] - wide[[cmp[2]]]))
    }))
  }))
}

results <- equivalence_tests(filter(performance, base_model != "llama323b"))
results_with_3b <- equivalence_tests(performance)
print(results, digits = 3)

saveRDS(list(results = results, results_with_3b = results_with_3b, declared_bound = bound),
        "results/analysis/equivalence_tests.rds")
