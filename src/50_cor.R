# Correlation and mean absolute error of every model and dictionary against
# the ground truth.
#
# In:  data/clean/df.rds
# Out: data/clean/cor_results.rds

library(dplyr)

df <- readRDS("data/clean/df.rds")

score_columns <- grep("_(fr_fr|en_fr|en_en)$|^lsd_(fr|en)$", names(df), value = TRUE)

cor_results <- bind_rows(lapply(score_columns, function(col) {
  ok <- !is.na(df[[col]])
  test <- cor.test(df$ground_truth[ok], df[[col]][ok])
  data.frame(model = col,
             correlation = unname(test$estimate),
             p_value = test$p.value,
             mae = mean(abs(df$ground_truth[ok] - df[[col]][ok])),
             n_obs = sum(ok))
})) %>%
  arrange(desc(abs(correlation)))

saveRDS(cor_results, "data/clean/cor_results.rds")
