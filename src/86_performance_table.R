#################################################################
# PERFORMANCE TABLE: r WITH 95% CI AND MAE, BY MODEL AND CONDITION
#################################################################
# Appendix table that carries the exact values behind the performance
# figure (src/85_performance_graph.R). Rows in the figure's order.

library(dplyr)
source("src/94_models_map.R")

df <- readRDS("data/clean/cor_results.rds") %>%
  mutate(
    is_open_source = sapply(model, is_open_source),
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is_open_source ~ "Open",
      TRUE ~ "Closed"
    ),
    display_name = sapply(model, get_model_display_name, with_condition = FALSE),
    condition = case_when(
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "FR",
      grepl("^lsd_en", model) ~ "EN"
    ),
    ci_half = 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    ci = sprintf("[%.2f, %.2f]", correlation - ci_half, correlation + ci_half)
  )

order_tbl <- df %>%
  group_by(display_name) %>%
  summarize(avg = mean(correlation), .groups = "drop") %>%
  arrange(desc(avg))

out <- df %>%
  mutate(display_name = factor(display_name, levels = order_tbl$display_name),
         condition = factor(condition, levels = c("FR→FR", "EN→FR", "EN→EN", "FR", "EN"))) %>%
  arrange(display_name, condition) %>%
  transmute(Model = as.character(display_name), Weights = model_type, Condition = as.character(condition),
            r = sprintf("%.3f", correlation), `95% CI` = ci,
            MAE = sprintf("%.3f", mae), n = n_obs)

lines <- c(
  "| Model | Weights | Condition | r | 95% CI | MAE | n |",
  "|:-----------------|:------|:------|-----:|:------------|-----:|---:|",
  apply(out, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
)
writeLines(lines, "results/tables/performance_table.md")

# LaTeX version: a floating table cannot break across pages, so the
# manuscript includes this tabular in a small font (see docs/pub/pub.qmd)
tex <- c(
  "\\begin{tabular}{llcrlrr}",
  "\\hline",
  "\\textbf{Model} & \\textbf{Weights} & \\textbf{Condition} & \\textbf{r} & \\textbf{95\\% CI} & \\textbf{MAE} & \\textbf{n} \\\\",
  "\\hline",
  apply(out, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
  "\\hline",
  "\\end{tabular}"
)
writeLines(tex, "results/tables/performance_table.tex")
cat("Wrote", nrow(out), "rows to results/tables/performance_table.md\n")
