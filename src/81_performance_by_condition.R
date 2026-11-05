# Mean performance by condition and paired t-tests between conditions, with
# models as the units. The tests cover the thirteen models of about 8B
# parameters and more; Llama 3.2 1B (which scored a different subset of
# sentences in each condition) is always left out, and the tests with Llama
# 3.2 3B included are kept as a sensitivity check.
#
# In:  data/clean/cor_results.rds, results/analysis/f1_scores_{7,3}.rds
# Out: results/analysis/condition_analysis_results.rds

library(dplyr)
library(tidyr)

conditions <- c(fr_fr = "FR→FR", en_fr = "EN→FR", en_en = "EN→EN")
cor_results <- readRDS("data/clean/cor_results.rds")

metrics <- bind_rows(
  `Correlation` = select(cor_results, model, value = correlation),
  `MAE` = select(cor_results, model, value = mae),
  `F1 Score (7-cat)` = select(readRDS("results/analysis/f1_scores_7.rds"), model, value = weighted_f1),
  `F1 Score (3-cat)` = select(readRDS("results/analysis/f1_scores_3.rds"), model, value = weighted_f1),
  .id = "metric"
) %>%
  filter(grepl("_(fr_fr|en_fr|en_en)$", model), !grepl("^llama321b_", model)) %>%
  mutate(base_model = sub("_(fr_fr|en_fr|en_en)$", "", model),
         condition = unname(conditions[sub(".*_(fr_fr|en_fr|en_en)$", "\\1", model)]))

main <- filter(metrics, base_model != "llama323b")

summary_by_condition <- main %>%
  group_by(metric, condition) %>%
  summarise(mean_value = mean(value), median_value = median(value),
            min_value = min(value), max_value = max(value),
            sd_value = sd(value), count = n(), .groups = "drop") %>%
  arrange(metric, desc(mean_value))

paired_tests <- function(data) {
  comparisons <- list(c("FR→FR", "EN→FR"), c("FR→FR", "EN→EN"), c("EN→FR", "EN→EN"))
  bind_rows(lapply(unique(data$metric), function(m) {
    wide <- data %>%
      filter(metric == m) %>%
      select(base_model, condition, value) %>%
      pivot_wider(names_from = condition, values_from = value)
    bind_rows(lapply(comparisons, function(cmp) {
      test <- t.test(wide[[cmp[1]]], wide[[cmp[2]]], paired = TRUE)
      data.frame(metric = m, comparison = paste(cmp[1], "vs", cmp[2]),
                 t_statistic = unname(test$statistic), p_value = test$p.value,
                 significant = test$p.value < 0.05)
    }))
  }))
}

results <- list(
  summary = summary_by_condition,
  significance_tests = paired_tests(main),
  significance_tests_with_3b = paired_tests(metrics)
)
print(results, digits = 3)
saveRDS(results, "results/analysis/condition_analysis_results.rds")
