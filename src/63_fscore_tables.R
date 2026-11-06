# Supplemental Tables S6 (F1 on three categories) and S7 (F1 on seven
# categories), by model and condition.
#
# In:  results/analysis/f1_scores_detailed_{3,7}.rds
# Out: results/tables/f1_scores_3cat_table.md, results/tables/f1_scores_7cat_table.tex

library(dplyr)
source("src/94_models_map.R")

format_scores <- function(file) {
  readRDS(file) %>%
    mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 3)),
           label = sapply(model, get_model_display_name))
}

f1_3 <- format_scores("results/analysis/f1_scores_detailed_3.rds")
writeLines(c(
  "| Model | Negative | Neutral | Positive | Weighted F1 |",
  "|-------|----------|---------|----------|------------|",
  paste0("| ", f1_3$label, " | ", f1_3$negative, " | ", f1_3$neutral, " | ",
         f1_3$positive, " | ", f1_3$weighted_f1, " |"),
  ""
), "results/tables/f1_scores_3cat_table.md")

f1_7 <- format_scores("results/analysis/f1_scores_detailed_7.rds")
columns <- c("very_negative", "negative", "somewhat_negative", "neutral",
             "somewhat_positive", "positive", "very_positive", "weighted_f1")
rows <- paste0(f1_7$label, " & ", do.call(paste, c(f1_7[columns], sep = " & ")), " \\\\")
writeLines(c(
  "\\begin{tabular}{lcccccccc}",
  "\\hline",
  paste("\\textbf{Model} & \\textbf{Very neg.} & \\textbf{Negative} & \\textbf{Somewhat neg.}",
        "& \\textbf{Neutral} & \\textbf{Somewhat pos.} & \\textbf{Positive} & \\textbf{Very pos.}",
        "& \\textbf{Weighted F1} \\\\"),
  "\\hline",
  rows,
  "\\hline",
  "\\end{tabular}"
), "results/tables/f1_scores_7cat_table.tex")
