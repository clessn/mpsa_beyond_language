# Token accounting: list prices, a per-call log written while scoring, and
# the cost summary behind Supplemental Table S5. Logging never stops a run.

# List prices in USD per million tokens. OpenRouter prices are those of the
# endpoint each model is pinned to (src/94_models_map.R); the closed-weight
# models use each vendor's list price; Jev bills input tokens only.
MODEL_PRICES <- data.frame(
  prefix = c("llama321b", "llama323b", "llama318b", "gptoss20b", "qwen332b",
             "llama4scout", "gptoss120b", "qwen3235b", "deepseekv32",
             "claudehaiku45", "gemini35", "gpt56luna", "jev",
             "mistralsmall32", "mistrallarge4"),
  display_name = c("Llama 3.2 1B", "Llama 3.2 3B", "Llama 3.1 8B", "GPT-OSS 20B", "Qwen3 32B",
                   "Llama 4 Scout", "GPT-OSS 120B", "Qwen3 235B-A22B", "DeepSeek V3.2",
                   "Claude Haiku 4.5", "Gemini 3.5 Flash", "GPT-5.6 Luna", "Jev 1.13",
                   "Mistral Small 3.2", "Mistral Large 4"),
  provider = c(rep("OpenRouter", 9), "Anthropic", "Google", "OpenAI", "TypeSafe",
               "OpenRouter", "OpenRouter"),
  price_in_per_mtok = c(0.027, 0.050, 0.220, 0.030, 0.080,
                        0.180, 0.030, 0.087, 0.209,
                        1.00, 1.50, 0.20, 0.042,
                        0.090, 0.680),
  price_out_per_mtok = c(0.201, 0.330, 0.220, 0.140, 0.280,
                         0.590, 0.170, 0.350, 0.310,
                         5.00, 9.00, 1.20, 0,
                         0.300, 2.090),
  price_verified_on = c(rep("2026-09-09", 10), "2026-09-16", "2026-09-16", "2026-09-28",
                        "2026-10-09", "2026-10-09")
)

# Llama 3.2 1B's endpoint reported 45 input tokens on every call, whatever the
# prompt, so its input tokens and cost are left missing.
UNRELIABLE_INPUT_COUNTS <- c("llama321b")

TOKEN_LOG_PATH <- "results/analysis/token_usage_log.csv"
TOKEN_LOG_COLUMNS <- c("timestamp", "model_prefix", "model_id", "condition", "item", "run",
                       "attempt", "input_tokens", "cached_input_tokens", "output_tokens",
                       "valid_response")

check_price_coverage <- function() {
  missing <- MODEL_PRICES$prefix[is.na(MODEL_PRICES$price_in_per_mtok)]
  if (length(missing) > 0) message("No price for: ", paste(missing, collapse = ", "))
  invisible(missing)
}

init_token_log <- function(path = TOKEN_LOG_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(path)) writeLines(paste(TOKEN_LOG_COLUMNS, collapse = ","), path)
  invisible(path)
}

# Token counts of the last call made by an ellmer client. "input" counts only
# uncached prompt tokens, so cached tokens are recorded separately.
capture_usage <- function(model_client) {
  empty <- list(input_tokens = NA_integer_, cached_input_tokens = NA_integer_,
                output_tokens = NA_integer_)
  tryCatch({
    usage <- if (is.function(model_client$get_tokens)) model_client$get_tokens() else model_client$tokens()
    if (!is.data.frame(usage) || nrow(usage) == 0) return(empty)
    last <- utils::tail(usage, 1)
    pick <- function(candidates) {
      hit <- intersect(candidates, names(last))
      if (length(hit) == 0) return(NA_integer_)
      value <- suppressWarnings(as.integer(last[[hit[1]]]))
      if (length(value) == 0) NA_integer_ else value[1]
    }
    list(input_tokens = pick(c("prompt_tokens", "input_tokens", "input")),
         cached_input_tokens = pick(c("cached_input", "cache_read_input_tokens")),
         output_tokens = pick(c("completion_tokens", "output_tokens", "output")))
  }, error = function(e) empty)
}

log_api_call <- function(path = TOKEN_LOG_PATH, model_prefix, condition,
                         item, run, attempt, usage, valid_response) {
  tryCatch({
    model_id <- if (exists("model_mapping") && model_prefix %in% names(model_mapping))
      unname(model_mapping[model_prefix]) else NA_character_
    blank_na <- function(x) ifelse(is.na(x), "", x)
    row <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), model_prefix, model_id, condition,
                 item, run, attempt, blank_na(usage$input_tokens),
                 blank_na(usage$cached_input_tokens), blank_na(usage$output_tokens),
                 ifelse(isTRUE(valid_response), "TRUE", "FALSE"), sep = ",")
    cat(row, "\n", file = path, sep = "", append = TRUE)
  }, error = function(e) NULL)
  invisible(NULL)
}

# A model cleared and scored again leaves its earlier calls in the log. For
# each (model, condition, item, run), keep the calls made within five minutes
# of the last one, which keeps the retries of the final wave only.
keep_last_wave <- function(log_df, window_s = 300) {
  time <- as.POSIXct(log_df$timestamp)
  call_id <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
  last_call <- tapply(time, call_id, max)
  log_df[as.numeric(difftime(last_call[call_id], time, units = "secs")) <= window_s, ]
}

# Tokens, calls and cost by model. Failed attempts are billed, so they count.
# Cached prompt tokens are charged at the full input rate, which makes every
# cost an upper bound. Cost per 1,000 sentences divides out the 3 runs x 3
# conditions of the validation run.
summarize_costs <- function(path = TOKEN_LOG_PATH, n_sentences = 200, n_runs = 3, n_conditions = 3) {
  log_df <- keep_last_wave(utils::read.csv(path, stringsAsFactors = FALSE))

  by_model <- stats::aggregate(
    cbind(input_tokens, cached_input_tokens, output_tokens) ~ model_prefix,
    data = log_df, FUN = sum, na.rm = TRUE, na.action = stats::na.pass
  )
  counts <- stats::aggregate(valid_response ~ model_prefix, data = log_df,
                             FUN = function(x) c(total = length(x), valid = sum(x)))
  by_model$total_calls <- counts$valid_response[, "total"]
  by_model$valid_calls <- counts$valid_response[, "valid"]

  out <- merge(by_model, MODEL_PRICES, by.x = "model_prefix", by.y = "prefix", all.x = TRUE)
  unreliable <- out$model_prefix %in% UNRELIABLE_INPUT_COUNTS
  out$input_tokens[unreliable] <- NA_integer_
  out$cached_input_tokens[unreliable] <- NA_integer_

  out$cost_input <- (out$input_tokens + out$cached_input_tokens) / 1e6 * out$price_in_per_mtok
  out$cost_output <- out$output_tokens / 1e6 * out$price_out_per_mtok
  out$cost_total <- out$cost_input + out$cost_output
  out$calls_per_valid <- round(out$total_calls / out$valid_calls, 2)
  out$cost_per_1k_sentences <- out$cost_total / n_sentences / (n_runs * n_conditions) * 1000

  out <- out[order(out$cost_per_1k_sentences, na.last = TRUE), ]
  rownames(out) <- NULL
  out
}
