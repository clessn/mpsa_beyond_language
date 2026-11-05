# F1 by sentiment category, averaged over the model-condition combinations of
# the thirteen models of about 8B parameters and more (reported in the
# Classification Granularity section).
#
# In:  results/analysis/f1_scores_detailed_{7,3}.rds
# Out: results/analysis/f1_category_averages_filtered.rds

library(dplyr)

excluded <- "^(llama321b|llama323b|lsd)_"

category_means <- function(file) {
  readRDS(file) %>%
    filter(!grepl(excluded, model)) %>%
    summarise(across(-c(model, weighted_f1), mean, .names = "{.col}_avg"))
}

averages <- list(
  category_averages_7 = category_means("results/analysis/f1_scores_detailed_7.rds"),
  category_averages_3 = category_means("results/analysis/f1_scores_detailed_3.rds")
)
print(averages, digits = 3)
saveRDS(averages, "results/analysis/f1_category_averages_filtered.rds")
