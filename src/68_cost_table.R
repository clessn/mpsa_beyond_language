# Supplemental Table S5: tokens and cost per model, from the per-call token
# log written while scoring the 200 sentences.
#
# In:  results/analysis/token_usage_log.csv (via summarize_costs())
# Out: results/tables/cost_table.md, results/analysis/cost_summary.rds

library(dplyr)
source("src/94_models_map.R")
source("src/95_token_logging.R")

cost_summary <- summarize_costs()
cost_summary$weights <- sapply(cost_summary$model_prefix, function(p) {
  open <- is_open_source(p)
  if (is.na(open)) "unknown" else if (open) "Open" else "Closed"
})

unpriced <- cost_summary$model_prefix[is.na(cost_summary$price_in_per_mtok)]
if (length(unpriced) > 0) warning("No price for: ", paste(unpriced, collapse = ", "))

count <- function(x) ifelse(is.na(x), "--", formatC(x, format = "d", big.mark = ","))
decimal <- function(x, digits) ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))

# Some providers report cached prompt tokens separately from the others
input_tokens <- cost_summary$input_tokens + cost_summary$cached_input_tokens

writeLines(c(
  "| Model | Provider | Weights | API calls per score | Input tokens | Output tokens | USD per 1,000 sentences |",
  "|:------------------|:----------|:------|---------:|---------:|---------:|-----------:|",
  paste0("| ", coalesce(cost_summary$display_name, cost_summary$model_prefix),
         " | ", coalesce(cost_summary$provider, "--"),
         " | ", cost_summary$weights,
         " | ", decimal(cost_summary$calls_per_valid, 2),
         " | ", count(input_tokens),
         " | ", count(cost_summary$output_tokens),
         " | ", decimal(cost_summary$cost_per_1k_sentences, 3), " |"),
  ""
), "results/tables/cost_table.md")
saveRDS(cost_summary, "results/analysis/cost_summary.rds")

cat(sprintf("Total spend on priced models: USD %.2f\n", sum(cost_summary$cost_total, na.rm = TRUE)))
