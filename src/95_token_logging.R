###############################################################################
# TOKEN AND COST INSTRUMENTATION
#
# Records per-call token usage during the LLM evaluation pipeline so that the
# study can report actual cost per 1,000 sentences for each model — the metric
# that matters to the researcher persona the paper adopts (someone annotating
# millions of sentences, not 200).
#
# The previous batch collected no cost or runtime data, which the manuscript
# lists as a limitation. This closes that gap.
#
# DESIGN CONSTRAINT: logging is observational. Every function here fails soft —
# if token accounting breaks, the sentiment run must continue unaffected.
#
# Added: September 2026 (cost instrumentation for the 2026 model refresh)
###############################################################################

#==============================================================================
# 1. PRICE TABLE
#==============================================================================

# API list prices in USD per million tokens (MTok).
#
# Open-weight rows are read from the exact OpenRouter endpoint each model is
# pinned to in src/94_models_map.R — a different endpoint for the same model
# carries a different price, so these belong together. Fetched 2026-09-09 from
# /api/v1/models/{id}/endpoints.
#
# Closed-weight rows are each vendor's own list price: Anthropic's pricing
# page, the Gemini API pricing page (paid tier), and OpenAI's API pricing page
# (standard tier, short context). GPT-5.6 Luna's price was cut on 2026-07-30,
# before the September run, so the post-cut price applies.
# Do not guess — a wrong price silently produces a wrong published figure.
#
# `prefix` matches the keys of `model_mapping` in src/94_models_map.R.
MODEL_PRICES <- data.frame(
  prefix = c(
    "llama321b", "llama323b", "llama318b", "gptoss20b", "qwen332b",
    "llama4scout", "gptoss120b", "qwen3235b", "deepseekv32",
    "claudehaiku45", "gemini35", "gpt56luna"
  ),
  display_name = c(
    "Llama 3.2 1B", "Llama 3.2 3B", "Llama 3.1 8B", "GPT-OSS 20B", "Qwen3 32B",
    "Llama 4 Scout", "GPT-OSS 120B", "Qwen3 235B-A22B", "DeepSeek V3.2",
    "Claude Haiku 4.5", "Gemini 3.5 Flash", "GPT-5.6 Luna"
  ),
  provider = c(
    rep("OpenRouter", 9),
    "Anthropic", "Google", "OpenAI"
  ),
  price_in_per_mtok = c(
    0.027, 0.050, 0.220, 0.030, 0.080,
    0.180, 0.030, 0.087, 0.209,
    1.00, 1.50, 0.20
  ),
  price_out_per_mtok = c(
    0.201, 0.330, 0.220, 0.140, 0.280,
    0.590, 0.170, 0.350, 0.310,
    5.00, 9.00, 1.20
  ),
  price_verified_on = c(
    rep("2026-09-09", 10), "2026-09-16", "2026-09-16"
  ),
  stringsAsFactors = FALSE
)

# Models whose endpoint does not report real token counts. Llama 3.2 1B's
# OpenRouter endpoint returned exactly 45 input tokens on every one of its
# 5,171 calls, whatever the prompt (~300 tokens) — a placeholder, not a
# measurement. Its input tokens and dollar cost are reported as missing rather
# than as a figure known to be wrong by a factor of ~7. Output counts vary
# call to call and are kept.
UNRELIABLE_INPUT_COUNTS <- c("llama321b")

#' Report which models still lack pricing
#'
#' Call this before a run so missing prices are discovered early rather than
#' after 16,200 API calls.
#'
#' @return Invisibly, the character vector of prefixes with no price
check_price_coverage <- function() {
  missing <- MODEL_PRICES$prefix[is.na(MODEL_PRICES$price_in_per_mtok)]
  if (length(missing) == 0) {
    cat("Price table complete for all", nrow(MODEL_PRICES), "models.\n")
  } else {
    cat("NOTE:", length(missing), "of", nrow(MODEL_PRICES),
        "models have no price yet:\n  ", paste(missing, collapse = ", "), "\n")
    cat("  Token counts will still be logged; dollar figures will be NA for these.\n")
    cat("  Fill in MODEL_PRICES in src/95_token_logging.R as accounts are created.\n")
  }
  invisible(missing)
}

#==============================================================================
# 2. LOG FILE
#==============================================================================

# One row per API call (not per sentence). CSV rather than RDS so a crashed or
# killed run keeps everything written so far, and so the file can be appended
# to across separate sessions.
TOKEN_LOG_PATH <- "results/analysis/token_usage_log.csv"

TOKEN_LOG_COLUMNS <- c(
  "timestamp", "model_prefix", "model_id", "condition",
  "item", "run", "attempt",
  "input_tokens", "cached_input_tokens", "output_tokens", "valid_response"
)

#' Create the token log file with a header if it does not already exist
#'
#' @param path Path to the log file
#' @return Invisibly, the path
init_token_log <- function(path = TOKEN_LOG_PATH) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(path)) {
    header <- paste(TOKEN_LOG_COLUMNS, collapse = ",")
    writeLines(header, path)
    cat("Created token log:", path, "\n")
  } else {
    n <- max(0, length(readLines(path, warn = FALSE)) - 1)
    cat("Appending to existing token log:", path, "(", n, "calls recorded )\n")
  }
  invisible(path)
}

#==============================================================================
# 3. USAGE CAPTURE
#==============================================================================

#' Extract token usage for the most recent call from an ellmer client
#'
#' ellmer's `$tokens()` accumulates one row per API call; the last row is the
#' call we just made. Column naming has varied across ellmer versions, so this
#' accepts either the prompt/completion or input/output convention.
#'
#' Returns NA rather than raising: a failure to read the token counter must
#' never abort a sentiment run.
#'
#' @param model_client An initialized ellmer chat client
#' @return A list with `input_tokens` and `output_tokens` (possibly NA)
capture_usage <- function(model_client) {
  empty <- list(input_tokens = NA_integer_, cached_input_tokens = NA_integer_,
                output_tokens = NA_integer_)

  tryCatch({
    # ellmer >= 0.4 exposes $get_tokens(); older versions used $tokens().
    # Column names differ too, hence the candidate lists below. The repo's
    # original code called $tokens(), which errors on 0.4.x and silently
    # produced an empty cost log until this was caught on 2026-09-09.
    usage <- if (is.function(model_client$get_tokens)) {
      model_client$get_tokens()
    } else {
      model_client$tokens()
    }
    if (is.null(usage) || !is.data.frame(usage) || nrow(usage) == 0) {
      return(empty)
    }
    last <- utils::tail(usage, 1)

    # Accept either naming convention; take the first column that is present
    pick <- function(candidates) {
      hit <- candidates[candidates %in% names(last)]
      if (length(hit) == 0) return(NA_integer_)
      value <- suppressWarnings(as.integer(last[[hit[1]]]))
      if (length(value) == 0) NA_integer_ else value[1]
    }

    # `input` counts only the UNCACHED prompt tokens. These prompts share a
    # ~270-token prefix, so providers cache it and `input` collapses to single
    # digits from the second call on — reading it alone understates input by
    # ~97%. Cached tokens are billed (usually at a discount), so they are
    # recorded separately rather than folded in or dropped.
    list(
      input_tokens        = pick(c("prompt_tokens", "input_tokens", "input")),
      cached_input_tokens = pick(c("cached_input", "cache_read_input_tokens")),
      output_tokens       = pick(c("completion_tokens", "output_tokens", "output"))
    )
  }, error = function(e) {
    # Silent by design: a broken token counter is not worth aborting a run over,
    # and a warning per call would flood a 16,200-call log.
    empty
  })
}

#' Append one API call to the token log
#'
#' @param path Log file path
#' @param model_prefix Short model identifier (e.g. "claudehaiku45")
#' @param condition Language condition ("en_fr", "fr_fr", "en_en")
#' @param item Sentence index
#' @param run Run number (1-3)
#' @param attempt Retry attempt number
#' @param usage List returned by `capture_usage()`
#' @param valid_response TRUE if a usable sentiment value was parsed
#' @return Invisibly TRUE on success, FALSE on failure
log_api_call <- function(path = TOKEN_LOG_PATH, model_prefix, condition,
                         item, run, attempt, usage, valid_response) {
  tryCatch({
    # Resolve the full API model id from the shared mapping when available
    model_id <- if (exists("model_mapping") && model_prefix %in% names(model_mapping)) {
      unname(model_mapping[model_prefix])
    } else {
      NA_character_
    }

    row <- paste(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      model_prefix,
      model_id,
      condition,
      item,
      run,
      attempt,
      ifelse(is.na(usage$input_tokens), "", usage$input_tokens),
      ifelse(is.na(usage$cached_input_tokens), "", usage$cached_input_tokens),
      ifelse(is.na(usage$output_tokens), "", usage$output_tokens),
      ifelse(isTRUE(valid_response), "TRUE", "FALSE"),
      sep = ","
    )
    cat(row, "\n", file = path, sep = "", append = TRUE)
    invisible(TRUE)
  }, error = function(e) {
    invisible(FALSE)
  })
}

#==============================================================================
# 4. COST SUMMARY
#==============================================================================

#' Summarize the token log into per-model token counts and costs
#'
#' Produces both the raw accounting (what was spent on this 200-sentence
#' validation run) and the extrapolation the paper actually needs (cost per
#' 1,000 sentences), which is what a researcher facing a large corpus cares
#' about.
#'
#' Note on retries: failed attempts are billed by the provider, so they are
#' included in cost. `valid_calls` vs `total_calls` exposes the retry overhead,
#' which is itself a finding — a model that needs three attempts per sentence
#' costs three times as much as its headline price suggests.
#'
#' @param path Log file path
#' @param n_sentences Number of distinct sentences evaluated (default 200)
#' @param n_runs Runs per sentence per condition (default 3)
#' @param n_conditions Language conditions (default 3)
#' @return A data frame, one row per model
summarize_costs <- function(path = TOKEN_LOG_PATH, n_sentences = 200,
                            n_runs = 3, n_conditions = 3) {
  if (!file.exists(path)) {
    stop("No token log found at ", path,
         ". Run src/40_prompt.R with logging enabled first.")
  }

  log_df <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (nrow(log_df) == 0) {
    stop("Token log at ", path, " is empty.")
  }

  # Keep only the calls of the configuration that produced the published
  # scores. When a model was cleared and scored again (qwen332b moved from
  # DeepInfra to SiliconFlow on 2026-09-10), the superseded wave is still in
  # the log and would inflate its cost. Same rule as src/97_rerun_status.R:
  # per (model, condition, item, run), keep the calls within a few minutes of
  # that triple's last call, which also keeps its retries.
  wave_window_s <- 300
  ts <- as.POSIXct(log_df$timestamp)
  triple <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
  last_of_triple <- tapply(ts, triple, max)
  log_df <- log_df[
    as.numeric(difftime(last_of_triple[triple], ts, units = "secs")) <= wave_window_s, ]

  by_model <- stats::aggregate(
    cbind(input_tokens, cached_input_tokens, output_tokens) ~ model_prefix,
    data = log_df, FUN = sum, na.rm = TRUE, na.action = stats::na.pass
  )

  counts <- stats::aggregate(
    valid_response ~ model_prefix, data = log_df,
    FUN = function(x) c(total = length(x), valid = sum(x, na.rm = TRUE))
  )
  by_model$total_calls <- counts$valid_response[, "total"]
  by_model$valid_calls <- counts$valid_response[, "valid"]

  out <- merge(by_model, MODEL_PRICES, by.x = "model_prefix", by.y = "prefix",
               all.x = TRUE)

  # Blank the input counts the endpoint did not really measure; NA then
  # propagates to every dollar figure for that model.
  bad <- out$model_prefix %in% UNRELIABLE_INPUT_COUNTS
  out$input_tokens[bad] <- NA_integer_
  out$cached_input_tokens[bad] <- NA_integer_

  # Cost of this run, in USD. NA propagates for models with no price.
  # Cached prompt tokens are charged at the FULL input rate here, which makes
  # every figure a conservative upper bound: providers discount cache reads by
  # a factor that varies per provider and is not exposed in the response. The
  # cached_input_tokens column is kept separately so this can be refined.
  out$cost_input  <- (out$input_tokens + out$cached_input_tokens) /
    1e6 * out$price_in_per_mtok
  out$cost_output <- out$output_tokens / 1e6 * out$price_out_per_mtok
  out$cost_total  <- out$cost_input + out$cost_output

  # Retry overhead: how many API calls each usable score actually required
  out$calls_per_valid <- round(out$total_calls / out$valid_calls, 2)

  # The figure the paper reports. This validation run scored each sentence
  # n_runs times in each of n_conditions, so divide out that multiplier to get
  # the cost of scoring 1,000 fresh sentences once, in a single condition.
  scores_per_sentence <- n_runs * n_conditions
  out$cost_per_1k_sentences <- out$cost_total / n_sentences /
    scores_per_sentence * 1000

  out <- out[order(out$cost_per_1k_sentences, na.last = TRUE), ]
  rownames(out) <- NULL
  out
}
