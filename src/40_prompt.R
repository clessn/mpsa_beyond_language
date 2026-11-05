# Scores the 200 benchmark sentences with every model in the three
# conditions (FR→FR, EN→FR, EN→EN), three runs each. Every call is written to
# the token log and its raw answer to results/analysis/raw_responses.jsonl.
# Resumes from the latest checkpoint.
#
# In:  data/tmp/data_manual_ranking.rds
# Out: data/tmp/data_manual_ranking_with_llm_scores.rds
#
# Environment: ONLY_MODELS or SKIP_MODELS (comma-separated prefixes).

options(warn = 1)  # Print warnings as they occur

# Two copies running at once overwrite each other's checkpoints and duplicate
# the token log, so a lock file holds the PID of the running process. A lock
# left by a killed run is ignored once that PID is gone.
LOCK_PATH <- "data/tmp/.rerun.lock"

if (file.exists(LOCK_PATH)) {
  old_pid <- suppressWarnings(as.integer(readLines(LOCK_PATH, warn = FALSE)[1]))
  alive <- !is.na(old_pid) &&
    length(system(sprintf("ps -p %d -o pid=", old_pid), intern = TRUE,
                  ignore.stderr = TRUE)) > 0
  if (alive) {
    stop(sprintf(paste0(
      "Another run of 40_prompt.R is already active (PID %d).\n",
      "  Stop it first:  pkill -f 40_prompt.R\n",
      "  Or, if you are sure it is dead:  rm %s"), old_pid, LOCK_PATH))
  }
  cat("Removing stale lock from PID", old_pid, "(no longer running).\n")
  file.remove(LOCK_PATH)
}
dir.create(dirname(LOCK_PATH), showWarnings = FALSE, recursive = TRUE)
writeLines(as.character(Sys.getpid()), LOCK_PATH)
cat("Lock acquired, PID", Sys.getpid(), "\n")

tryCatch({

library(ellmer)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

source("src/92_llm_helper_funcs.R")

source("src/94_models_map.R")
source("src/95_token_logging.R")
init_token_log()
check_price_coverage()

# Raw answers with the value read from each, to audit or redo the parsing
RAW_RESPONSE_LOG_PATH <- "results/analysis/raw_responses.jsonl"

df_raw <- readRDS("data/tmp/data_manual_ranking.rds")

df <- df_raw

latest_checkpoint <- list.files("data/tmp", pattern = "sentiment_analysis_progress_.*\\.rds", full.names = TRUE)
if (length(latest_checkpoint) > 0) {
  latest_checkpoint <- latest_checkpoint[order(file.info(latest_checkpoint)$mtime, decreasing = TRUE)][1]
  cat("Found checkpoint file:", latest_checkpoint, "\n")
  checkpoint_df <- tryCatch({
    readRDS(latest_checkpoint)
  }, error = function(e) {
    cat("Error reading checkpoint file:", e$message, "\n")
    NULL
  })

  if (!is.null(checkpoint_df)) {
    if (all(names(df_raw) %in% names(checkpoint_df))) {
      cat("Resuming from checkpoint...\n")
      df <- checkpoint_df
    } else {
      cat("Checkpoint file has unexpected structure. Starting fresh with original data.\n")
    }
  }
}

# Scores every sentence n_runs times with one model in one condition
run_sentiment_analysis <- function(model_client, model_name_prefix, prompt_type, text_field, n_runs = 3) {
  model_identifier <- paste0(model_name_prefix, "_", prompt_type, "_", ifelse(text_field == "sentences", "fr", "en"))

  initialize_model_columns(model_identifier)

  run_columns <- paste0(model_identifier, "_run", 1:n_runs)
  mean_column <- paste0(model_identifier, "_mean")

  for (i in seq_along(df[[text_field]])) {
    if (!is.na(df[[mean_column]][i])) {
      cat(sprintf("Skipping %s item %d (already processed)\n", model_identifier, i))
      next
    }

    cat(sprintf("Processing %s with %s prompt for %s: Item %d of %d\n",
                model_name_prefix, prompt_type, text_field, i, length(df[[text_field]])))

    current_time <- Sys.time()
    if (difftime(current_time, last_checkpoint_time, units = "secs") > checkpoint_interval) {
      save_progress_checkpoint()
      last_checkpoint_time <<- current_time  # Update the global variable
    }

    source("src/93_prompts.R")

    if (prompt_type == "en" && text_field == "sentences") {
      prompt <- en_prompt_fr_text(df$sentences[i])
    } else if (prompt_type == "en" && text_field == "sentences_en") {
      prompt <- en_prompt_en_text(df[[text_field]][i])
    } else if (prompt_type == "fr" && text_field == "sentences") {
      prompt <- fr_prompt_fr_text(df$sentences[i])
    } else {
      stop("Invalid prompt_type or text_field combination")
    }

    max_retries <- 3  # Maximum number of retry attempts per run
    run_results <- rep(NA_real_, n_runs)  # NA, not 0: an unset slot must not read as a neutral score

    for (run in 1:n_runs) {
      if (!is.na(df[[run_columns[run]]][i])) {
        cat(sprintf("Skipping run %d for %s item %d (already processed)\n", run, model_identifier, i))
        run_results[run] <- df[[run_columns[run]]][i]
        next
      }

      valid_value_obtained <- FALSE
      attempt <- 1

      while (!valid_value_obtained && attempt <= max_retries) {
        # Assigned from tryCatch so that a failed call gives NULL, not the
        # previous sentence's answer
        response <- tryCatch({
          model_client$set_turns(list())

          retry_with_backoff({
            model_client$chat(prompt)
          })
        }, error = function(e) {
          cat(sprintf("ERROR with %s on attempt %d: %s\n", model_identifier, attempt, e$message))
          Sys.sleep(10)
          NULL
        })

        if (is.null(response)) {
          attempt <- attempt + 1
          next
        }

        call_usage <- capture_usage(model_client)

        Sys.sleep(ifelse(model_name_prefix %in% names(model_provider_pin), 1.2, 1))

        extracted_value <- clean_sentiment_value(response)
        log_raw_response(
          path         = RAW_RESPONSE_LOG_PATH,
          model_prefix = model_name_prefix,
          condition    = paste0(prompt_type, "_",
                                ifelse(text_field == "sentences", "fr", "en")),
          item = i, run = run, attempt = attempt,
          doc_id = df$doc_id[i], response = response, parsed = extracted_value
        )

        # Every attempt is logged, failed ones included: providers bill them
        response_is_valid <- !is.na(extracted_value) &&
          extracted_value >= -1 && extracted_value <= 1
        log_api_call(
          model_prefix   = model_name_prefix,
          condition      = paste0(prompt_type, "_",
                                  ifelse(text_field == "sentences", "fr", "en")),
          item           = i,
          run            = run,
          attempt        = attempt,
          usage          = call_usage,
          valid_response = response_is_valid
        )

        if (response_is_valid) {
          run_results[run] <- extracted_value
          df[[run_columns[run]]][i] <<- extracted_value
          valid_value_obtained <- TRUE
          cat(sprintf("Valid value %.2f obtained for item %d, run %d on attempt %d\n",
                    extracted_value, i, run, attempt))
          break
        } else {
          cat(sprintf("Attempt %d/%d: Invalid value for item %d, run %d\n",
                    attempt, max_retries, i, run))
          cat("Response:", response, "\n")
          attempt <- attempt + 1
        }
      }

      if (!valid_value_obtained) {
        cat(sprintf("Warning: Failed to extract numerical value from responses for item %d, run %d after %d attempts\n",
                  i, run, max_retries))
        run_results[run] <- NA_real_
        df[[run_columns[run]]][i] <<- NA_real_
      }

    }

    if (all(is.na(run_results))) {
      df[[mean_column]][i] <<- NA_real_
      cat(sprintf("All runs for item %d returned invalid values. Skipping this item.\n", i))
    } else {
      mean_value <- mean(run_results, na.rm = TRUE)
      df[[mean_column]][i] <<- mean_value
      cat(sprintf("Final mean value for item %d: %.2f\n", i, mean_value))
    }

    if (i %% 20 == 0) {  # Save every 20 items for additional safety
      save_progress_checkpoint()
    }
  }

  return(df)
}

source("src/93_prompts.R")
system_prompt <- get_system_prompt()

# Output cap for every model; models that answer with a bare number use 3-7 tokens
OUTPUT_MAX_TOKENS <- 400

cat("Initializing all LLM clients...\n")

model_clients <- list()

# Open-weight models and Mistral Large 4, through OpenRouter

for (prefix in names(model_provider_pin)) {
  model_clients[[prefix]] <- ellmer::chat_openrouter(
    system_prompt = system_prompt,
    model = unname(model_mapping[prefix]),
    params = ellmer::params(max_tokens = OUTPUT_MAX_TOKENS),
    api_args = openrouter_args(prefix),
    echo = "none"
  )
  cat(sprintf("  %-14s %-34s [pin: %s]\n", prefix,
              model_mapping[prefix], model_provider_pin[prefix]))
}

# Closed-weight models, through each vendor's API

model_clients$claudehaiku45 <- ellmer::chat_anthropic(
  system_prompt = system_prompt,
  model = unname(model_mapping["claudehaiku45"]),
  params = ellmer::params(max_tokens = OUTPUT_MAX_TOKENS),
  echo = "none"
)

model_clients$gemini35 <- ellmer::chat_google_gemini(
  system_prompt = system_prompt,
  model = unname(model_mapping["gemini35"]),
  params = ellmer::params(max_tokens = OUTPUT_MAX_TOKENS),
  echo = "none"
)

model_clients$gpt56luna <- ellmer::chat_openai(
  system_prompt = system_prompt,
  model = unname(model_mapping["gpt56luna"]),
  params = ellmer::params(max_tokens = OUTPUT_MAX_TOKENS),
  echo = "none"
)

cat("All clients initialized:", length(model_clients), "models.\n")

initialize_model_columns <- function(model_prefix) {
  col_names <- c(
    paste0(model_prefix, "_run1"),
    paste0(model_prefix, "_run2"),
    paste0(model_prefix, "_run3"),
    paste0(model_prefix, "_mean")
  )

  for (col_name in col_names) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <<- NA_real_
      cat("Initialized column:", col_name, "\n")
    }
  }
}

save_progress_checkpoint <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  checkpoint_file <- paste0("data/tmp/sentiment_analysis_progress_", timestamp, ".rds")

  saveRDS(df, checkpoint_file)
  cat("Progress checkpoint saved to:", checkpoint_file, "\n")

  consolidated_file <- "data/tmp/sentiment_analysis_latest_checkpoint.rds"
  saveRDS(df, consolidated_file)
  cat("Also saved to consolidated checkpoint:", consolidated_file, "\n")

  # Keep the ten most recent checkpoints
  checkpoint_files <- list.files("data/tmp", pattern = "sentiment_analysis_progress_.*\\.rds", full.names = TRUE)

  if (length(checkpoint_files) > 10) {
    checkpoint_files <- checkpoint_files[order(file.info(checkpoint_files)$mtime)]

    files_to_remove <- checkpoint_files[1:(length(checkpoint_files) - 10)]

    for (file in files_to_remove) {
      file.remove(file)
      cat("Removed old checkpoint file:", file, "\n")
    }
  }
}

last_checkpoint_time <- Sys.time()
checkpoint_interval <- 600  # 10 minutes in seconds

conditions <- list(
  list(prompt = "en", text = "sentences"),     # English prompt, French text
  list(prompt = "fr", text = "sentences"),     # French prompt, French text
  list(prompt = "en", text = "sentences_en")   # English prompt, English translation
)

# SKIP_MODELS leaves models for a later run (their cells stay NA);
# ONLY_MODELS scores only the models named, e.g. to add a model to a finished
# run without retrying Llama 3.2 1B's many NA cells.
#   ONLY_MODELS=mistralsmall32,mistrallarge4 Rscript src/40_prompt.R
skip_models <- trimws(strsplit(Sys.getenv("SKIP_MODELS", ""), ",")[[1]])
skip_models <- skip_models[nzchar(skip_models)]

only_models <- trimws(strsplit(Sys.getenv("ONLY_MODELS", ""), ",")[[1]])
only_models <- only_models[nzchar(only_models)]

unknown <- setdiff(c(skip_models, only_models), names(model_clients))
if (length(unknown)) {
  stop("Unknown model prefix(es) in SKIP_MODELS/ONLY_MODELS: ",
       paste(unknown, collapse = ", "), ". Known: ",
       paste(names(model_clients), collapse = ", "))
}
if (length(only_models)) {
  skip_models <- union(skip_models, setdiff(names(model_clients), only_models))
}
if (length(skip_models)) {
  cat("Skipping on request:", paste(skip_models, collapse = ", "), "\n")
}

for (prefix in names(model_clients)) {
  if (prefix %in% skip_models) {
    cat(sprintf("\n=== Skipping %s (SKIP_MODELS) ===\n", prefix))
    next
  }
  cat(sprintf("\n=== Processing %s (%s) ===\n", prefix, model_mapping[prefix]))

  for (cond in conditions) {
    df <- run_sentiment_analysis(model_clients[[prefix]], prefix,
                                 cond$prompt, cond$text)
  }
}

cat("Saving the results...\n")
saveRDS(df, "data/tmp/data_manual_ranking_with_llm_scores.rds")

write_csv(df, "data/tmp/data_manual_ranking_with_llm_scores.csv")
save_progress_checkpoint()

}, error = function(e) {
  cat("\nScript interrupted or error occurred:", conditionMessage(e), "\n")

  emergency_file <- paste0("data/tmp/sentiment_analysis_INTERRUPTED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  saveRDS(df, emergency_file)
  cat("Emergency backup saved to:", emergency_file, "\n")
  cat("You can load this file with: df <- readRDS('", emergency_file, "')\n")

  stop(e)
}, finally = {
  if (exists("LOCK_PATH") && file.exists(LOCK_PATH)) {
    held <- suppressWarnings(as.integer(readLines(LOCK_PATH, warn = FALSE)[1]))
    if (!is.na(held) && held == Sys.getpid()) file.remove(LOCK_PATH)
  }
  cat("Script execution completed or interrupted. Check for saved checkpoints if needed.\n")
})
