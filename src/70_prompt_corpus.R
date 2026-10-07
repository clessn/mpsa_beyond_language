###############################################################################
# FULL CORPUS SENTIMENT ANALYSIS WITH ONE LLM
#
# Scores every article of the French news corpus with a single model, under
# the two prompt-language conditions the validation study compares on French
# text: French prompt (fr_fr) and English prompt (en_fr). The result feeds the
# corpus-level comparison with the Lexicoder dictionary (src/80_corpus_analysis.R
# and Figure 5 of the manuscript).
#
# The model is chosen by its prefix in src/94_models_map.R, so the script
# follows the lineup rather than hard-coding a vendor:
#   Rscript src/70_prompt_corpus.R                       # default: gpt56luna
#   CORPUS_MODEL=claudehaiku45 Rscript src/70_prompt_corpus.R
#
# The default is GPT-5.6 Luna because it is the best-performing model of the
# 2026 validation run (r = 0.81 in FR->FR, 0.80 in EN->FR) and the cheapest of
# the closed-weight ones on this corpus. The 2025 version of this script was
# Gemini-only (then the best model); its English-prompt twin,
# src/71_en_prompt_corpus.R, is folded into this one.
#
# Client settings are identical to src/40_prompt.R (same model id, 400-token
# output cap, same system prompt), so corpus scores are comparable with the
# validation scores. Every API call is logged to a separate token log.
#
# Author: Ral Zarek
# Date: March 2025; rewritten October 2026
###############################################################################

#==============================================================================
# 1. SETUP AND DEPENDENCIES
#==============================================================================

options(warn = 1)  # Print warnings as they occur

library(ellmer)   # LLM API interactions
library(dplyr)    # Data manipulation
library(stringr)  # Used by clean_sentiment_value()

source("src/92_llm_helper_funcs.R")  # retry_with_backoff(), clean_sentiment_value()
source("src/93_prompts.R")           # fr_prompt_fr_text(), en_prompt_fr_text(), get_system_prompt()
source("src/94_models_map.R")        # model_mapping, model_provider_pin, openrouter_args()
source("src/95_token_logging.R")     # capture_usage(), log_api_call(), init_token_log()

#------------------------------------------------------------------------------
# 1.1 MODEL SELECTION
#------------------------------------------------------------------------------

MODEL_PREFIX <- Sys.getenv("CORPUS_MODEL", "gpt56luna")
if (!MODEL_PREFIX %in% names(model_mapping)) {
  stop(sprintf("Unknown CORPUS_MODEL '%s'. Known prefixes: %s",
               MODEL_PREFIX, paste(names(model_mapping), collapse = ", ")))
}
MODEL_ID <- unname(model_mapping[MODEL_PREFIX])
cat(sprintf("Corpus model: %s (%s)\n", MODEL_PREFIX, MODEL_ID))

# Same cap as src/40_prompt.R. Providers bill tokens generated, not the
# ceiling, so this costs nothing for models that answer in 3-4 tokens.
OUTPUT_MAX_TOKENS <- 400

#------------------------------------------------------------------------------
# 1.2 FILE LAYOUT
#------------------------------------------------------------------------------

# Checkpoints carry the model prefix so a run for one model can never resume
# from another model's scores. The 2025 Gemini checkpoints had generic names,
# and a rerun would have silently skipped every article already scored by the
# old model (see data/tmp/archive_2025/README.md).
CHECKPOINT_PREFIX <- sprintf("data/tmp/corpus_sentiment_%s_", MODEL_PREFIX)
LATEST_CHECKPOINT <- paste0(CHECKPOINT_PREFIX, "latest_checkpoint.rds")

# One output file for the corpus, with one column per model and condition
# (e.g. gpt56luna_fr_fr, gpt56luna_en_fr). Running a second model adds its
# columns alongside the first's.
OUTPUT_PATH <- "data/clean/news_df_sentiment_corpus.rds"

# Separate log from the validation run's: summarize_costs() aggregates the
# default log by model, and these article-length calls would otherwise be
# folded into the per-sentence cost table.
CORPUS_TOKEN_LOG_PATH <- "results/analysis/token_usage_log_corpus.csv"
init_token_log(CORPUS_TOKEN_LOG_PATH)

#------------------------------------------------------------------------------
# 1.3 SINGLE-INSTANCE LOCK
#------------------------------------------------------------------------------
# Two concurrent runs overwrite each other's checkpoints and interleave token
# log lines (this happened to src/40_prompt.R on 2026-09-09). The lock stores
# this process's PID; a stale lock from a killed run is detected and replaced.
LOCK_PATH <- "data/tmp/.corpus.lock"

if (file.exists(LOCK_PATH)) {
  old_pid <- suppressWarnings(as.integer(readLines(LOCK_PATH, warn = FALSE)[1]))
  alive <- !is.na(old_pid) &&
    length(system(sprintf("ps -p %d -o pid=", old_pid), intern = TRUE,
                  ignore.stderr = TRUE)) > 0
  if (alive) {
    stop(sprintf("Another instance of this script is running (PID %d). Refusing to start.",
                 old_pid))
  }
  cat("Removing stale lock from PID", old_pid, "\n")
}
writeLines(as.character(Sys.getpid()), LOCK_PATH)

tryCatch({

#==============================================================================
# 2. DATA LOADING AND CHECKPOINT RESUME
#==============================================================================

df_raw <- readRDS("data/tmp/news_df_tone_index.rds")

# Start from the shared output file when it exists, so columns from earlier
# models are preserved; otherwise from the raw corpus.
df <- if (file.exists(OUTPUT_PATH)) {
  cat("Loading existing corpus scores from", OUTPUT_PATH, "\n")
  readRDS(OUTPUT_PATH)
} else {
  df_raw
}

# Resume this model's run from its latest checkpoint if there is one
if (file.exists(LATEST_CHECKPOINT)) {
  checkpoint_df <- tryCatch(readRDS(LATEST_CHECKPOINT), error = function(e) NULL)
  if (!is.null(checkpoint_df) && all(names(df_raw) %in% names(checkpoint_df)) &&
      nrow(checkpoint_df) == nrow(df_raw)) {
    cat("Resuming from checkpoint:", LATEST_CHECKPOINT, "\n")
    df <- checkpoint_df
  } else {
    cat("Checkpoint unreadable or inconsistent; starting this model from scratch.\n")
  }
}

stopifnot(nrow(df) == nrow(df_raw), all(df$doc_id == df_raw$doc_id))

#==============================================================================
# 3. CHECKPOINTING
#==============================================================================

last_checkpoint_time <- Sys.time()
CHECKPOINT_INTERVAL_S <- 600  # also forced every 10 articles

save_progress_checkpoint <- function(data, column, force = FALSE) {
  now <- Sys.time()
  if (!force && difftime(now, last_checkpoint_time, units = "secs") < CHECKPOINT_INTERVAL_S) {
    return(invisible(FALSE))
  }
  tryCatch({
    stamped <- paste0(CHECKPOINT_PREFIX, "progress_", format(now, "%Y%m%d_%H%M%S"), ".rds")
    saveRDS(data, stamped)
    saveRDS(data, LATEST_CHECKPOINT)
    done <- sum(!is.na(data[[column]]))
    cat(sprintf("Checkpoint saved (%s: %d of %d articles, %.1f%%)\n",
                column, done, nrow(data), 100 * done / nrow(data)))
    last_checkpoint_time <<- now

    # Keep only the 10 most recent stamped checkpoints for this model
    stamped_files <- list.files("data/tmp",
                                pattern = sprintf("^corpus_sentiment_%s_progress_.*\\.rds$", MODEL_PREFIX),
                                full.names = TRUE)
    if (length(stamped_files) > 10) {
      stamped_files <- stamped_files[order(file.info(stamped_files)$mtime)]
      file.remove(stamped_files[seq_len(length(stamped_files) - 10)])
    }
    invisible(TRUE)
  }, error = function(e) {
    cat("ERROR: failed to save checkpoint:", conditionMessage(e), "\n")
    invisible(FALSE)
  })
}

#==============================================================================
# 4. MODEL CLIENT
#==============================================================================

# Mirrors the client construction in src/40_prompt.R, section 3.
build_client <- function(prefix) {
  system_prompt <- get_system_prompt()
  params <- ellmer::params(max_tokens = OUTPUT_MAX_TOKENS)
  model <- unname(model_mapping[prefix])

  if (prefix %in% names(model_provider_pin)) {
    return(ellmer::chat_openrouter(system_prompt = system_prompt, model = model,
                                   params = params, api_args = openrouter_args(prefix),
                                   echo = "none"))
  }
  switch(prefix,
    claudehaiku45 = ellmer::chat_anthropic(system_prompt = system_prompt, model = model,
                                           params = params, echo = "none"),
    gemini35      = ellmer::chat_google_gemini(system_prompt = system_prompt, model = model,
                                               params = params, echo = "none"),
    gpt56luna     = ellmer::chat_openai(system_prompt = system_prompt, model = model,
                                        params = params, echo = "none"),
    stop("No client constructor for prefix '", prefix, "'; add one in build_client().")
  )
}

client <- build_client(MODEL_PREFIX)
cat("Client initialized.\n")

#==============================================================================
# 5. SCORING LOOP
#==============================================================================

#' Score every article under one prompt-language condition
#'
#' @param data Corpus data frame
#' @param prompt_lang "fr" or "en": language of the instructions (text is
#'   always the French original)
#' @return `data` with the column `<prefix>_<prompt_lang>_fr` filled in
score_corpus <- function(data, prompt_lang) {
  condition <- paste0(prompt_lang, "_fr")
  column <- paste0(MODEL_PREFIX, "_", condition)
  make_prompt <- if (prompt_lang == "fr") fr_prompt_fr_text else en_prompt_fr_text

  if (!column %in% names(data)) data[[column]] <- NA_real_
  todo <- which(is.na(data[[column]]))
  cat(sprintf("\n=== %s: %d of %d articles to score ===\n", column, length(todo), nrow(data)))

  max_retries <- 3
  for (i in todo) {
    if (i %% 10 == 0) cat(sprintf("%s: article %d of %d\n", column, i, nrow(data)))

    prompt <- make_prompt(data$text_body[i])
    value <- NA_real_

    for (attempt in seq_len(max_retries)) {
      # `response` is assigned FROM tryCatch so an API failure yields NULL
      # here rather than leaving the previous article's text in place.
      response <- tryCatch({
        client$set_turns(list())  # each article is a fresh conversation
        retry_with_backoff({ client$chat(prompt) })
      }, error = function(e) {
        cat(sprintf("ERROR on article %d, attempt %d: %s\n", i, attempt, conditionMessage(e)))
        Sys.sleep(10)
        NULL
      })
      if (is.null(response)) next

      # Snapshot usage before anything else touches the client; fails soft
      call_usage <- capture_usage(client)
      Sys.sleep(1)  # pacing

      extracted <- clean_sentiment_value(response)
      is_valid <- !is.na(extracted) && extracted >= -1 && extracted <= 1

      # Every attempt is logged, including failed ones: providers bill them
      log_api_call(path = CORPUS_TOKEN_LOG_PATH, model_prefix = MODEL_PREFIX,
                   condition = condition, item = i, run = 1, attempt = attempt,
                   usage = call_usage, valid_response = is_valid)

      if (is_valid) {
        value <- extracted
        break
      }
      cat(sprintf("Attempt %d/%d: no usable value for article %d. Response: %s\n",
                  attempt, max_retries, i, substr(response, 1, 120)))
    }

    if (is.na(value)) {
      cat(sprintf("Warning: article %d left NA after %d attempts\n", i, max_retries))
    }
    data[[column]][i] <- value

    if (i %% 10 == 0) save_progress_checkpoint(data, column, force = TRUE)
    else save_progress_checkpoint(data, column)
  }

  save_progress_checkpoint(data, column, force = TRUE)
  data
}

# Both conditions on French text, in the same order as src/40_prompt.R
df <- score_corpus(df, "fr")
df <- score_corpus(df, "en")

#==============================================================================
# 6. SAVE AND SUMMARIZE
#==============================================================================

saveRDS(df, OUTPUT_PATH)
cat("\nSaved corpus scores to", OUTPUT_PATH, "\n")

for (condition in c("fr_fr", "en_fr")) {
  column <- paste0(MODEL_PREFIX, "_", condition)
  x <- df[[column]]
  cat(sprintf("%s: %d scored, %d NA (%.1f%%), mean %.3f, sd %.3f\n",
              column, sum(!is.na(x)), sum(is.na(x)), 100 * mean(is.na(x)),
              mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
}
cat("Done.\n")

}, error = function(e) {
  cat("\nScript interrupted or error occurred:", conditionMessage(e), "\n")
  cat("Progress is in the latest checkpoint:", LATEST_CHECKPOINT, "\n")
  stop(e)
}, finally = {
  if (file.exists(LOCK_PATH) &&
      identical(readLines(LOCK_PATH, warn = FALSE)[1], as.character(Sys.getpid()))) {
    file.remove(LOCK_PATH)
  }
})
