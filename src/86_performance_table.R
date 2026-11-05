# Supplemental Table S4: correlation with its 95% confidence interval, mean
# absolute error and number of sentences, by model and condition, in the
# order of Figure 2.
#
# In:  data/clean/cor_results.rds
# Out: results/tables/performance_table.tex

library(dplyr)
source("src/94_models_map.R")

conditions <- c(fr_fr = "FR→FR", en_fr = "EN→FR", en_en = "EN→EN", fr = "FR", en = "EN")

df <- readRDS("data/clean/cor_results.rds") %>%
  mutate(weights = case_when(grepl("^lsd_", model) ~ "Dictionary",
                             sapply(model, is_open_source) ~ "Open",
                             TRUE ~ "Closed"),
         name = sapply(model, get_model_display_name, with_condition = FALSE),
         condition = unname(conditions[sub("^.*?_((fr|en)_(fr|en)|fr|en)$", "\\1", model)]),
         # Same interval as cor.test()
         ci_lo = tanh(atanh(correlation) - qnorm(0.975) / sqrt(n_obs - 3)),
         ci_hi = tanh(atanh(correlation) + qnorm(0.975) / sqrt(n_obs - 3)))

model_order <- df %>%
  group_by(name) %>%
  summarise(avg = mean(correlation)) %>%
  arrange(desc(avg)) %>%
  pull(name)

rows <- df %>%
  arrange(factor(name, levels = model_order), factor(condition, levels = conditions)) %>%
  with(sprintf("%s & %s & %s & %.3f & [%.2f, %.2f] & %.3f & %d \\\\",
               name, weights, condition, correlation, ci_lo, ci_hi, mae, n_obs))

writeLines(c(
  "\\begin{tabular}{llcrlrr}",
  "\\hline",
  "\\textbf{Model} & \\textbf{Weights} & \\textbf{Condition} & \\textbf{r} & \\textbf{95\\% CI} & \\textbf{MAE} & \\textbf{n} \\\\",
  "\\hline",
  rows,
  "\\hline",
  "\\end{tabular}"
), "results/tables/performance_table.tex")
