# Supplemental Table S8: input tokens per call in each condition, the extra
# tokens of the French prompt, and tokens per word in French and English.
#
# The French-prompt overhead compares FR→FR with EN→FR, which score the same
# French sentences, so it is the cost of the instructions alone. Tokens per
# word are slopes of input tokens on sentence length under the English
# prompt, where the instructions are a constant intercept.
#
# In:  results/analysis/token_usage_log.csv, data/tmp/data_manual_ranking.rds
# Out: results/tables/tokens_by_language.md, results/analysis/tokens_by_language.rds

library(dplyr)
library(tidyr)
source("src/94_models_map.R")
source("src/95_token_logging.R")

# Keep only the last scoring wave of each (model, condition, item, run), as
# in summarize_costs()
log_df <- read.csv(TOKEN_LOG_PATH, stringsAsFactors = FALSE)
time <- as.POSIXct(log_df$timestamp)
call_id <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
last_call <- tapply(time, call_id, max)
log_df <- log_df[as.numeric(difftime(last_call[call_id], time, units = "secs")) <= 300, ]

calls <- log_df %>%
  filter(valid_response, !model_prefix %in% UNRELIABLE_INPUT_COUNTS) %>%
  mutate(input_total = coalesce(input_tokens, 0L) + coalesce(cached_input_tokens, 0L))

per_condition <- calls %>%
  group_by(model_prefix, condition) %>%
  summarise(mean_input = mean(input_total), mean_output = mean(output_tokens, na.rm = TRUE),
            .groups = "drop")

by_model <- per_condition %>%
  select(model_prefix, condition, mean_input) %>%
  pivot_wider(names_from = condition, values_from = mean_input) %>%
  mutate(fr_prompt_overhead_tokens = fr_fr - en_fr,
         fr_prompt_overhead_pct = 100 * (fr_fr / en_fr - 1),
         fr_text_overhead_pct = 100 * (en_fr / en_en - 1))

words <- readRDS("data/tmp/data_manual_ranking.rds") %>%
  mutate(item = row_number(),
         fr_words = lengths(strsplit(trimws(sentences), "\\s+")),
         en_words = lengths(strsplit(trimws(sentences_en), "\\s+"))) %>%
  select(item, fr_words, en_words)

slope <- function(y, x) unname(coef(lm(y ~ x))[2])
tokens_per_word <- calls %>%
  filter(condition %in% c("en_fr", "en_en")) %>%
  group_by(model_prefix, condition, item) %>%
  summarise(tokens = median(input_total), .groups = "drop") %>%
  left_join(words, by = "item") %>%
  group_by(model_prefix) %>%
  summarise(tokens_per_word_fr = slope(tokens[condition == "en_fr"], fr_words[condition == "en_fr"]),
            tokens_per_word_en = slope(tokens[condition == "en_en"], en_words[condition == "en_en"]))

# Price of a valid call in each condition, without retries
cost_by_condition <- per_condition %>%
  left_join(MODEL_PRICES, by = c("model_prefix" = "prefix")) %>%
  mutate(usd_per_1k = (mean_input * price_in_per_mtok + mean_output * price_out_per_mtok) / 1000) %>%
  select(model_prefix, condition, usd_per_1k) %>%
  pivot_wider(names_from = condition, values_from = usd_per_1k, names_prefix = "usd_")

tokens_by_language <- by_model %>%
  left_join(tokens_per_word, by = "model_prefix") %>%
  left_join(cost_by_condition, by = "model_prefix") %>%
  mutate(display_name = unname(model_display_name[model_prefix])) %>%
  arrange(fr_prompt_overhead_pct)
saveRDS(tokens_by_language, "results/analysis/tokens_by_language.rds")

t <- tokens_by_language
writeLines(c(
  "| Model | EN→EN | EN→FR | FR→FR | French-prompt overhead | Tokens per French word | Tokens per English word |",
  "|:---------------------|------:|------:|------:|---------------:|---------:|---------:|",
  sprintf("| %s | %.1f | %.1f | %.1f | +%.0f (+%.1f%%) | %.2f | %.2f |",
          t$display_name, t$en_en, t$en_fr, t$fr_fr, t$fr_prompt_overhead_tokens,
          t$fr_prompt_overhead_pct, t$tokens_per_word_fr, t$tokens_per_word_en),
  ""
), "results/tables/tokens_by_language.md")

print(as.data.frame(t %>% transmute(display_name, fr_prompt_overhead_pct,
                                    saving_pct = 100 * (1 - usd_en_fr / usd_fr_fr))),
      digits = 3, row.names = FALSE)

output_tokens <- calls %>%
  filter(condition %in% c("fr_fr", "en_fr")) %>%
  group_by(model_prefix, condition, item, run) %>%
  summarise(out = last(output_tokens), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = out) %>%
  filter(!is.na(fr_fr), !is.na(en_fr))
test <- t.test(output_tokens$fr_fr, output_tokens$en_fr, paired = TRUE)
cat(sprintf("Output tokens, French minus English prompt: %.2f (p = %.2f)\n", test$estimate, test$p.value))
