#################################################################
# TOKEN USAGE BY LANGUAGE CONDITION
#################################################################
# Measures what each language condition costs in tokens, from the per-call
# token log written during src/40_prompt.R. Prompt language is held to make
# no difference to accuracy (Results, H1); this table shows that it does make
# a difference to cost, and that the size of that difference is a property
# of each model's tokenizer rather than of its size or licence.
#
# Two quantities per model:
#   1. Mean input tokens per call in each condition, and the overhead of the
#      French prompt over the English one on the same French sentences. The
#      sentence is identical across those two conditions, so the overhead is
#      the extra tokens the instructions cost in French — a fixed amount per
#      model.
#   2. Tokens per word for French and for English text, estimated by
#      regressing input tokens on the word count of the 200 sentences
#      (English prompt in both cases, so the instructions are a constant
#      intercept). This isolates how compactly each tokenizer encodes
#      each language.
#
# Run this AFTER src/40_prompt.R has completed.

library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping

source("src/94_models_map.R")      # model_display_name
source("src/95_token_logging.R")   # TOKEN_LOG_PATH, MODEL_PRICES, UNRELIABLE_INPUT_COUNTS

#################################################################
# LOAD THE TOKEN LOG
#################################################################
log_df <- read.csv(TOKEN_LOG_PATH, stringsAsFactors = FALSE)

# Same wave filter as summarize_costs(): per (model, condition, item, run),
# keep the calls within a few minutes of that triple's last call, so a model
# that was cleared and scored again contributes only its final wave.
wave_window_s <- 300
ts <- as.POSIXct(log_df$timestamp)
triple <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
last_of_triple <- tapply(ts, triple, max)
log_df <- log_df[
  as.numeric(difftime(last_of_triple[triple], ts, units = "secs")) <= wave_window_s, ]

# Models with unmeasured input counts (see src/95_token_logging.R) are
# excluded here and flagged in the table caption.
calls <- log_df %>%
  filter(valid_response, !model_prefix %in% UNRELIABLE_INPUT_COUNTS) %>%
  mutate(input_total = coalesce(input_tokens, 0L) + coalesce(cached_input_tokens, 0L))

#################################################################
# 1. INPUT TOKENS PER CALL BY CONDITION
#################################################################
per_condition <- calls %>%
  group_by(model_prefix, condition) %>%
  summarise(mean_input = mean(input_total), mean_output = mean(output_tokens, na.rm = TRUE),
            n_calls = n(), .groups = "drop")

by_model <- per_condition %>%
  select(model_prefix, condition, mean_input) %>%
  pivot_wider(names_from = condition, values_from = mean_input) %>%
  mutate(
    fr_prompt_overhead_tokens = fr_fr - en_fr,            # same sentences, French vs English instructions
    fr_prompt_overhead_pct    = 100 * (fr_fr / en_fr - 1),
    fr_text_overhead_pct      = 100 * (en_fr / en_en - 1)  # same instructions, French vs translated text
  )

#################################################################
# 2. TOKENS PER WORD, BY TEXT LANGUAGE
#################################################################
sentences <- readRDS("data/tmp/data_manual_ranking.rds") %>%
  mutate(item = row_number(),
         fr_words = lengths(strsplit(trimws(sentences), "\\s+")),
         en_words = lengths(strsplit(trimws(sentences_en), "\\s+"))) %>%
  select(item, fr_words, en_words)

# Input tokens are deterministic for a given prompt, so one value per item
per_item <- calls %>%
  filter(condition %in% c("en_fr", "en_en")) %>%
  group_by(model_prefix, condition, item) %>%
  summarise(tokens = median(input_total), .groups = "drop") %>%
  left_join(sentences, by = "item")

tokens_per_word <- per_item %>%
  group_by(model_prefix) %>%
  summarise(
    tokens_per_word_fr = coef(lm(tokens ~ fr_words, data = pick(everything())[condition == "en_fr", ]))[2],
    tokens_per_word_en = coef(lm(tokens ~ en_words, data = pick(everything())[condition == "en_en", ]))[2],
    .groups = "drop"
  )

#################################################################
# 3. COST PER 1,000 SENTENCES, BY CONDITION
#################################################################
# Mean tokens of a valid call in each condition at list prices. Unlike the
# cost table, retries are not included: this isolates the price of the
# language choice from the price of a model's failure rate.
cost_by_condition <- per_condition %>%
  left_join(MODEL_PRICES, by = c("model_prefix" = "prefix")) %>%
  mutate(usd_per_1k = (mean_input * price_in_per_mtok + mean_output * price_out_per_mtok) / 1e6 * 1000) %>%
  select(model_prefix, condition, usd_per_1k) %>%
  pivot_wider(names_from = condition, values_from = usd_per_1k, names_prefix = "usd_")

#################################################################
# ASSEMBLE
#################################################################
tokens_by_language <- by_model %>%
  left_join(tokens_per_word, by = "model_prefix") %>%
  left_join(cost_by_condition, by = "model_prefix") %>%
  mutate(display_name = unname(model_display_name[model_prefix])) %>%
  arrange(fr_prompt_overhead_pct)

saveRDS(tokens_by_language, "results/analysis/tokens_by_language.rds")

#################################################################
# MARKDOWN TABLE
#################################################################
# Same pipe-table convention as src/68_cost_table.R, for a Quarto include.
fmt1 <- function(x) sprintf("%.1f", x)
fmt2 <- function(x) sprintf("%.2f", x)

markdown_table <- paste0(
  "| Model | EN→EN | EN→FR | FR→FR | French-prompt overhead | ",
  "Tokens per French word | Tokens per English word |\n",
  "|-------|------:|------:|------:|-----------------------:|",
  "-----------------------:|------------------------:|\n"
)
for (i in seq_len(nrow(tokens_by_language))) {
  row <- tokens_by_language[i, ]
  markdown_table <- paste0(
    markdown_table, "| ", row$display_name, " | ",
    fmt1(row$en_en), " | ", fmt1(row$en_fr), " | ", fmt1(row$fr_fr), " | ",
    sprintf("+%.0f (+%.1f%%)", row$fr_prompt_overhead_tokens, row$fr_prompt_overhead_pct), " | ",
    fmt2(row$tokens_per_word_fr), " | ", fmt2(row$tokens_per_word_en), " |\n"
  )
}
writeLines(markdown_table, "results/tables/tokens_by_language.md")

#################################################################
# CONSOLE SUMMARY
#################################################################
cat("=== INPUT TOKENS PER CALL BY CONDITION ===\n\n")
print(as.data.frame(tokens_by_language %>%
  select(display_name, en_en, en_fr, fr_fr, fr_prompt_overhead_pct, fr_text_overhead_pct,
         tokens_per_word_fr, tokens_per_word_en)), digits = 3, row.names = FALSE)

cat("\n=== USD PER 1,000 SENTENCES BY CONDITION (valid calls, list prices) ===\n\n")
print(as.data.frame(tokens_by_language %>%
  select(display_name, usd_en_en, usd_en_fr, usd_fr_fr) %>%
  mutate(fr_vs_en_prompt_pct = 100 * (usd_fr_fr / usd_en_fr - 1))), digits = 3, row.names = FALSE)

cat(sprintf("\nFrench-prompt overhead: %.1f%% to %.1f%% more input tokens than the English prompt.\n",
            min(tokens_by_language$fr_prompt_overhead_pct), max(tokens_by_language$fr_prompt_overhead_pct)))
cat(sprintf("French text: %.1f%% to %.1f%% more input tokens than its English translation.\n",
            min(tokens_by_language$fr_text_overhead_pct), max(tokens_by_language$fr_text_overhead_pct)))

out_diff <- calls %>%
  filter(condition %in% c("fr_fr", "en_fr")) %>%
  group_by(model_prefix, condition, item, run) %>%
  summarise(out = last(output_tokens), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = out) %>%
  filter(!is.na(fr_fr), !is.na(en_fr))
tt <- t.test(out_diff$fr_fr, out_diff$en_fr, paired = TRUE)
cat(sprintf("Output tokens, FR prompt minus EN prompt (paired, %d pairs): %.2f tokens, p = %.2f\n",
            nrow(out_diff), tt$estimate, tt$p.value))

cat("\nTables have been created:\n")
cat("1. Token table (markdown): results/tables/tokens_by_language.md\n")
cat("2. Token summary (RDS):    results/analysis/tokens_by_language.rds\n")
