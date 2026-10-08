#################################################################
# JEV SENTIMENT SCORES FOR THE VALIDATION SAMPLE
#################################################################
# Adds Jev (TypeSafe's System One model) to the 200-sentence validation data,
# in the same shape src/40_prompt.R gives the other models: three runs per
# sentence and condition, stored as jev_<condition>_run1..3, and their mean as
# jev_<condition>_mean.
#
# RUN ORDER: src/40_prompt.R -> src/42_jev_prompt.R -> src/41_prompt_cleaning.R
# 40 rebuilds data_manual_ranking_with_llm_scores.rds from its checkpoint, which
# has no Jev columns, so run this again after any rerun of 40. It is cheap: API
# answers are cached in data/tmp/jev_pipeline_calls.rds and replayed.
#
# Same design as the other models, so the comparison holds:
#   - one condition per request (no batching), as in 40 and in the cost table
#   - three runs; the mean is taken over valid runs, NA only if all three fail
#   - every API call, including retries, is appended to the token log, once:
#     replaying from the cache does not log again
#
# What differs, and belongs in the methods section: Jev is not asked to write a
# number. It answers a Score question over the prompt's five anchors and
# returns a probability per anchor; the score is their probability-weighted
# position (see src/98_jev_helpers.R).
#
# Added: September 2026
#################################################################

library(dplyr)
library(tidyr)

source("src/94_models_map.R")
source("src/95_token_logging.R")
source("src/98_jev_helpers.R")

SCORES_PATH <- "data/tmp/data_manual_ranking_with_llm_scores.rds"
CACHE_PATH <- "data/tmp/jev_pipeline_calls.rds"
N_RUNS <- 3
MAX_ATTEMPTS <- 3
CONDITION_TEXT <- c(en_fr = "sentences", fr_fr = "sentences", en_en = "sentences_en")

stopifnot(nzchar(Sys.getenv("TYPESAFE_API_KEY")))

df <- readRDS(SCORES_PATH) %>% select(-starts_with("jev_"))
stopifnot(nrow(df) == 200)

#################################################################
# 1. QUERY JEV (OR REPLAY THE CACHE)
#################################################################

log_calls <- function(calls) {
  for (k in seq_len(nrow(calls))) {
    log_api_call(
      model_prefix = "jev", condition = calls$condition[k], item = calls$item[k],
      run = calls$run[k], attempt = calls$attempt[k],
      usage = list(input_tokens = calls$input_tokens[k], cached_input_tokens = NA,
                   output_tokens = calls$output_tokens[k]),
      valid_response = !is.na(calls$level[k])
    )
  }
}

if (!file.exists(CACHE_PATH)) {
  init_token_log()
  calls <- list()

  for (run in seq_len(N_RUNS)) {
    for (cnd in names(CONDITION_TEXT)) {
      texts <- df[[CONDITION_TEXT[[cnd]]]]
      cat(sprintf("Run %d, %s: %d requests\n", run, cnd, length(texts)))
      batch <- run_jev(texts, cnd)
      batch$run <- run
      batch$attempt <- 1L
      log_calls(batch)
      calls[[length(calls) + 1]] <- batch

      # Retry the items whose call failed after httr2's own retries
      for (attempt in 2:MAX_ATTEMPTS) {
        failed <- batch$item[is.na(batch$level)]
        if (length(failed) == 0) break
        cat(sprintf("  retrying %d failed item(s), attempt %d\n", length(failed), attempt))
        batch <- run_jev(texts[failed], cnd)
        batch$item <- failed[batch$item]
        batch$run <- run
        batch$attempt <- attempt
        log_calls(batch)
        calls[[length(calls) + 1]] <- batch
      }
    }
  }
  calls <- bind_rows(calls)
  saveRDS(calls, CACHE_PATH)
}
calls <- readRDS(CACHE_PATH)

#################################################################
# 2. ONE VALUE PER RUN, THEN THE MEAN OVER VALID RUNS
#################################################################

runs <- calls %>%
  arrange(attempt) %>%
  group_by(item, condition, run) %>%
  # The last attempt is the one that counts: earlier ones failed
  summarise(value = jev_to_scale(last(level)), .groups = "drop")

run_cols <- runs %>%
  pivot_wider(id_cols = item, names_from = c(condition, run),
              values_from = value, names_glue = "jev_{condition}_run{run}")

mean_cols <- runs %>%
  group_by(item, condition) %>%
  # Rounded because 41 bins neutral with `== 0`, and a mean of three
  # two-decimal values can otherwise miss zero by floating-point error
  summarise(mean = if (all(is.na(value))) NA_real_ else round(mean(value, na.rm = TRUE), 4),
            .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = mean,
              names_glue = "jev_{condition}_mean")

jev_cols <- left_join(run_cols, mean_cols, by = "item") %>% arrange(item)

stopifnot(nrow(jev_cols) == nrow(df), identical(jev_cols$item, seq_len(nrow(df))))
df <- bind_cols(df, select(jev_cols, -item))

#################################################################
# 3. SAVE AND REPORT
#################################################################

saveRDS(df, SCORES_PATH)
readr::write_csv(df, sub("\\.rds$", ".csv", SCORES_PATH))

means <- grep("^jev_.*_mean$", names(df), value = TRUE)
cat("\nJev columns added to", SCORES_PATH, "\n")
for (m in means) {
  cat(sprintf("  %-15s scored %d/200, exactly neutral %d\n",
              m, sum(!is.na(df[[m]])), sum(df[[m]] == 0, na.rm = TRUE)))
}
cat(sprintf("  API calls: %d (%d failed), model %s\n",
            nrow(calls), sum(is.na(calls$level)),
            paste(unique(na.omit(calls$model)), collapse = ",")))
