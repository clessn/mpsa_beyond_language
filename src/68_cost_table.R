#################################################################
# COST AND TOKEN USAGE TABLE
#################################################################
# Turns the per-call token log written during src/40_prompt.R into the
# publication table reporting what each model actually costs to run.
#
# This addresses a limitation the manuscript currently concedes ("no cost or
# runtime data were collected") and directly answers the reviewer question of
# why the study evaluates affordable models rather than the strongest available
# ones: at the scale the paper's framing assumes — millions of sentences — cost
# per sentence is the operative constraint, not peak accuracy.
#
# Run this AFTER src/40_prompt.R has completed.

library(dplyr)     # For data manipulation
library(stringr)   # For string manipulation

source("src/94_models_map.R")      # model_mapping, is_open_source()
source("src/95_token_logging.R")   # MODEL_PRICES, summarize_costs()

#################################################################
# LOAD AND SUMMARIZE THE TOKEN LOG
#################################################################
cost_summary <- summarize_costs()

# Label each model by licence using the shared categorization, so this table
# stays consistent with the open/closed split used everywhere else.
cost_summary$weights <- sapply(cost_summary$model_prefix, function(p) {
  open <- is_open_source(p)
  if (is.na(open)) "unknown" else if (open) "Open" else "Closed"
})

#################################################################
# REPORT MISSING PRICES
#################################################################
# A model with no price still contributes token counts but produces NA cost.
# Surface this loudly: a half-filled price table would otherwise yield a
# publication table with silent gaps.
unpriced <- cost_summary$model_prefix[is.na(cost_summary$price_in_per_mtok)]
if (length(unpriced) > 0) {
  cat("\n!! WARNING:", length(unpriced), "model(s) have no price and will show",
      "as '--' in the table:\n   ", paste(unpriced, collapse = ", "), "\n")
  cat("   Fill in MODEL_PRICES in src/95_token_logging.R, then re-run.\n\n")
}

#################################################################
# BUILD MARKDOWN TABLE
#################################################################
# Matches the pipe-table convention used by src/63_fscore_tables.R so the
# manuscript can pull it in with a Quarto {{< include >}} directive.

fmt_num <- function(x) ifelse(is.na(x), "--", formatC(x, format = "d", big.mark = ","))
fmt_usd <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
fmt_rat <- function(x) ifelse(is.na(x), "--", sprintf("%.2f", x))

markdown_table <- paste0(
  "| Model | Provider | Weights | API calls per usable score | ",
  "Input tokens | Output tokens | USD per 1,000 sentences |\n",
  "|-------|----------|---------|---------------------------|",
  "--------------|---------------|------------------------|\n"
)

for (i in 1:nrow(cost_summary)) {
  row <- cost_summary[i, ]
  markdown_table <- paste0(
    markdown_table, "| ",
    ifelse(is.na(row$display_name), row$model_prefix, row$display_name), " | ",
    ifelse(is.na(row$provider), "--", row$provider), " | ",
    row$weights, " | ",
    fmt_rat(row$calls_per_valid), " | ",
    fmt_num(row$input_tokens), " | ",
    fmt_num(row$output_tokens), " | ",
    fmt_usd(row$cost_per_1k_sentences), " |\n"
  )
}

writeLines(markdown_table, "results/tables/cost_table.md")
saveRDS(cost_summary, "results/analysis/cost_summary.rds")

#################################################################
# CONSOLE SUMMARY
#################################################################
cat("=== TOKEN USAGE AND COST BY MODEL ===\n\n")
print(cost_summary[, c("display_name", "weights", "total_calls", "valid_calls",
                       "calls_per_valid", "input_tokens", "output_tokens",
                       "cost_total", "cost_per_1k_sentences")],
      row.names = FALSE)

total_calls <- sum(cost_summary$total_calls, na.rm = TRUE)
total_spend <- sum(cost_summary$cost_total, na.rm = TRUE)

cat("\nTotal API calls logged:", format(total_calls, big.mark = ","), "\n")
cat("Total spend on priced models: USD", sprintf("%.2f", total_spend), "\n")
if (length(unpriced) > 0) {
  cat("(excludes", length(unpriced), "model(s) with no price set)\n")
}

# Retry overhead is a finding in its own right: a model that needs several
# attempts per sentence — typically a reasoning model truncated by the
# max_tokens cap — costs a multiple of its headline price.
worst <- cost_summary[which.max(cost_summary$calls_per_valid), ]
if (nrow(worst) == 1 && !is.na(worst$calls_per_valid) && worst$calls_per_valid > 1.1) {
  cat("\nHighest retry overhead:", worst$display_name,
      "needed", sprintf("%.2f", worst$calls_per_valid),
      "API calls per usable score.\n")
  cat("Check whether reasoning mode is exhausting the 100-token cap.\n")
}

cat("\nTables have been created:\n")
cat("1. Cost table (markdown): results/tables/cost_table.md\n")
cat("2. Cost summary (RDS):    results/analysis/cost_summary.rds\n")
