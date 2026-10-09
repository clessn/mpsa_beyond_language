###############################################################################
# LLM Sentiment Analysis Evaluation
# 
# This script evaluates multiple LLMs on sentiment analysis tasks using
# different prompt types (English/French) and text inputs (French original/English translation)
#
#iThe script processes a dataset containing French sentences and their English translations,
# uses various LLMs to analyze sentiment, and stores the results for comparison.
#
# Author: Ral Zarek
# Date: March 2025
###############################################################################

#==============================================================================
# 1. SETUP AND DEPENDENCIES
#==============================================================================

# Set up error handling for script interruptions
options(warn = 1)  # Print warnings as they occur

#------------------------------------------------------------------------------
# SINGLE-INSTANCE LOCK
#------------------------------------------------------------------------------
# Two copies of this script running at once corrupt each other's work: they
# hold separate in-memory copies of `df` and overwrite each other's checkpoints,
# they append interleaved (and sometimes spliced) lines to the token log, and
# they pay twice for the same API calls. This happened on 2026-09-09 — three
# instances ran concurrently and 54% of the logged calls were duplicates.
#
# The lock stores this process's PID. A stale lock from a killed run is
# detected by checking whether that PID is still alive, so a crash does not
# require manual cleanup.
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

# Set up tryCatch to save progress on interrupt
tryCatch({

# Load required libraries for data manipulation, API calls, and file operations
library(ellmer)   # Package for LLM API interactions
library(dplyr)    # Data manipulation
library(purrr)    # Functional programming tools
library(readr)    # Reading/writing data
library(stringr)  # String manipulation

# Source helper functions file containing utility functions for sentiment analysis
source("src/92_llm_helper_funcs.R")

# Cost instrumentation: model_mapping supplies full API model ids, and
# 95_token_logging.R supplies the price table and per-call logging used to
# report cost per 1,000 sentences in the paper.
source("src/94_models_map.R")
source("src/95_token_logging.R")
init_token_log()
check_price_coverage()

# Load the original dataset containing sentences for sentiment analysis
# This dataset contains French sentences and their English translations
df_raw <- readRDS("data/tmp/data_manual_ranking.rds")

# Create a working copy of the data
df <- df_raw

# Check for existing checkpoint file and load if it exists
latest_checkpoint <- list.files("data/tmp", pattern = "sentiment_analysis_progress_.*\\.rds", full.names = TRUE)
if (length(latest_checkpoint) > 0) {
  # Sort by modification time to get the most recent
  latest_checkpoint <- latest_checkpoint[order(file.info(latest_checkpoint)$mtime, decreasing = TRUE)][1]
  cat("Found checkpoint file:", latest_checkpoint, "\n")
  # Load the checkpoint data
  checkpoint_df <- tryCatch({
    readRDS(latest_checkpoint)
  }, error = function(e) {
    cat("Error reading checkpoint file:", e$message, "\n")
    NULL
  })
  
  # If checkpoint loaded successfully, use it
  if (!is.null(checkpoint_df)) {
    # Check if checkpoint has expected structure (has the base columns of df_raw)
    if (all(names(df_raw) %in% names(checkpoint_df))) {
      cat("Resuming from checkpoint...\n")
      df <- checkpoint_df
    } else {
      cat("Checkpoint file has unexpected structure. Starting fresh with original data.\n")
    }
  }
}

#==============================================================================
# 2. CORE SENTIMENT ANALYSIS FUNCTION
#==============================================================================

#' Run sentiment analysis for a specific model, prompt type, and text field
#'
#' This function processes each sentence in the dataset with a specified model,
#' makes multiple runs for each sentence, and handles parsing of the results.
#'
#' @param model_client The initialized LLM client object
#' @param model_name_prefix String prefix to identify the model in logs and results
#' @param prompt_type The language of the prompt ("en" or "fr")
#' @param text_field The field in df containing the text to analyze ("sentences" or "sentences_en")
#' @param n_runs Number of runs to perform for each sentence (default: 3)
#' @return Updated dataframe with filled sentiment scores
run_sentiment_analysis <- function(model_client, model_name_prefix, prompt_type, text_field, n_runs = 3) {
  # Create full model identifier for column names
  model_identifier <- paste0(model_name_prefix, "_", prompt_type, "_", ifelse(text_field == "sentences", "fr", "en"))
  
  # Initialize columns for this model if they don't exist
  initialize_model_columns(model_identifier)
  
  # Create column names for storing individual run results and mean
  run_columns <- paste0(model_identifier, "_run", 1:n_runs)
  mean_column <- paste0(model_identifier, "_mean")
  
  # Process each sentence in the dataset
  for (i in seq_along(df[[text_field]])) {
    # Skip if we already have a valid mean result for this item
    if (!is.na(df[[mean_column]][i])) {
      cat(sprintf("Skipping %s item %d (already processed)\n", model_identifier, i))
      next
    }
    
    # Log progress information
    cat(sprintf("Processing %s with %s prompt for %s: Item %d of %d\n", 
                model_name_prefix, prompt_type, text_field, i, length(df[[text_field]])))
    
    # Check if it's time for a checkpoint save
    current_time <- Sys.time()
    if (difftime(current_time, last_checkpoint_time, units = "secs") > checkpoint_interval) {
      save_progress_checkpoint()
      last_checkpoint_time <<- current_time  # Update the global variable
    }
    
    #--------------------------------------------------------------------------
    # 2.1 PROMPT CONSTRUCTION
    #--------------------------------------------------------------------------
    
    # Source the prompts file
    source("src/93_prompts.R")
    
    # Create the appropriate prompt based on the prompt type and text field
    if (prompt_type == "en" && text_field == "sentences") {
      # English prompt for French text
      prompt <- en_prompt_fr_text(df$sentences[i])
    } else if (prompt_type == "en" && text_field == "sentences_en") {
      # English prompt for English text (translated from French)
      prompt <- en_prompt_en_text(df[[text_field]][i])
    } else if (prompt_type == "fr" && text_field == "sentences") {
      # French prompt for French text
      prompt <- fr_prompt_fr_text(df$sentences[i])
    } else {
      # Error handling for invalid combinations
      stop("Invalid prompt_type or text_field combination")
    }
    
    #--------------------------------------------------------------------------
    # 2.2 MODEL INVOCATION WITH RETRIES
    #--------------------------------------------------------------------------
    
    # Set up for multiple runs with retry logic for each run
    max_retries <- 3  # Maximum number of retry attempts per run
    run_results <- rep(NA_real_, n_runs)  # NA, not 0: an unset slot must not read as a neutral score
    
    # Perform n_runs for each sentence
    for (run in 1:n_runs) {
      # Skip if we already have a valid result for this run
      if (!is.na(df[[run_columns[run]]][i])) {
        cat(sprintf("Skipping run %d for %s item %d (already processed)\n", run, model_identifier, i))
        run_results[run] <- df[[run_columns[run]]][i]
        next
      }
      
      # Initialize values for retry logic
      valid_value_obtained <- FALSE
      attempt <- 1
      
      # Try multiple times to get a valid sentiment value
      while (!valid_value_obtained && attempt <= max_retries) {
        # Wrap in tryCatch to provide more detailed error handling and logging
        # `response` is assigned FROM tryCatch, so the error handler's NULL
        # actually reaches it. In the original form the assignment sat inside
        # the block and `return(NULL)` only exited the handler function — so on
        # any API failure `response` still held the PREVIOUS call's text, and
        # the previous sentence's score was silently attributed to this one.
        # Verified with a reproduction and fixed 2026-09-09.
        response <- tryCatch({
          # Reset chat history to ensure each prompt is treated as new
          model_client$set_turns(list())

          # Make API call with retry and backoff for connection issues
          retry_with_backoff({
            model_client$chat(prompt)
          })
        }, error = function(e) {
          cat(sprintf("ERROR with %s on attempt %d: %s\n", model_identifier, attempt, e$message))
          # Add a longer timeout after errors to let rate limits recover
          Sys.sleep(10)
          NULL
        })
        
        # Skip rest of loop if response is NULL (error occurred)
        if (is.null(response)) {
          attempt <- attempt + 1
          next
        }
        
        # Snapshot token usage for the call we just made, before anything else
        # can touch the client. Fails soft: returns NA rather than erroring.
        call_usage <- capture_usage(model_client)
        
        # Pace requests. OpenRouter's limits scale with account credit rather
        # than being fixed per model, so a uniform pause replaces the old
        # per-provider token accounting; the retry/backoff path handles 429s.
        Sys.sleep(ifelse(model_name_prefix %in% names(model_provider_pin), 1.2, 1))

        #----------------------------------------------------------------------
        # 2.3 RESPONSE PARSING
        #----------------------------------------------------------------------
        
        # Extract numerical sentiment value from the model's response
        extracted_value <- clean_sentiment_value(response)
        
        # Record this call. Every attempt gets its own row, including failed
        # ones: providers bill retries, so excluding them would understate cost.
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
        
        # Validate that we got a value in the acceptable range (-1 to 1)
        if (response_is_valid) {
          # Valid value obtained - store it and break the retry loop
          run_results[run] <- extracted_value
          # Store the result in the dataframe
          df[[run_columns[run]]][i] <<- extracted_value
          valid_value_obtained <- TRUE
          cat(sprintf("Valid value %.2f obtained for item %d, run %d on attempt %d\n", 
                    extracted_value, i, run, attempt))
          break
        } else {
          # Invalid response - log it and try again (up to max_retries)
          cat(sprintf("Attempt %d/%d: Invalid value for item %d, run %d\n", 
                    attempt, max_retries, i, run))
          cat("Response:", response, "\n")
          attempt <- attempt + 1
        }
      }
      
      # If all retry attempts failed for this run, store NA
      if (!valid_value_obtained) {
        cat(sprintf("Warning: Failed to extract numerical value from responses for item %d, run %d after %d attempts\n", 
                  i, run, max_retries))
        run_results[run] <- NA_real_
        df[[run_columns[run]]][i] <<- NA_real_
      }
      
      # Don't save checkpoint after each run as it creates too many files
      # (We'll save checkpoints based on item count outside this loop instead)
    }
    
    #--------------------------------------------------------------------------
    # 2.4 RESULT AGGREGATION
    #--------------------------------------------------------------------------
    
    # Calculate final sentiment value for this sentence from all runs
    if (all(is.na(run_results))) {
      # If all runs resulted in NA, store NA for this item
      df[[mean_column]][i] <<- NA_real_
      cat(sprintf("All runs for item %d returned invalid values. Skipping this item.\n", i))
    } else {
      # Calculate mean of valid values from all runs
      mean_value <- mean(run_results, na.rm = TRUE)
      df[[mean_column]][i] <<- mean_value
      cat(sprintf("Final mean value for item %d: %.2f\n", i, mean_value))
    }
    
    # Save checkpoint every 20 items
    if (i %% 20 == 0) {  # Save every 20 items for additional safety
      save_progress_checkpoint()
    }
  }
  
  # Return the updated dataframe
  return(df)
}

#==============================================================================
# 3. MODEL INITIALIZATION
#==============================================================================

# Load system prompt from the prompts file
source("src/93_prompts.R")
system_prompt <- get_system_prompt()

# Output cap, shared by every model. Raised from 100 to 400 on 2026-09-09:
# qwen3-32b returned an empty string at 100 even with reasoning suppressed, and
# the cap costs nothing when unused — providers bill tokens generated, not the
# ceiling, and compliant models answer in 3-4 tokens.
OUTPUT_MAX_TOKENS <- 400

cat("Initializing all LLM clients...\n")

# Clients are built from the mapping in src/94_models_map.R instead of being
# declared one by one, so the lineup lives in exactly one place.
model_clients <- list()

#------------------------------------------------------------------------------
# 3.1 OPEN-WEIGHT MODELS (OpenRouter, provider-pinned)
#------------------------------------------------------------------------------

# openrouter_args() supplies the provider pin (so the request cannot silently
# reroute to a different backend or quantization) and, for reasoning models,
# reasoning = list(effort = "none"). See src/94_models_map.R for why both matter.
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

#------------------------------------------------------------------------------
# 3.2 CLOSED-WEIGHT MODELS (each vendor's own API)
#------------------------------------------------------------------------------

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
#==============================================================================
# 4. RUN SENTIMENT ANALYSIS FOR ALL MODELS
#==============================================================================

# Initialize columns for model results if they don't exist
# For each model, add columns to store results and mean value if they don't exist
initialize_model_columns <- function(model_prefix) {
  # For run 1, 2, 3 and mean
  col_names <- c(
    paste0(model_prefix, "_run1"),
    paste0(model_prefix, "_run2"),
    paste0(model_prefix, "_run3"),
    paste0(model_prefix, "_mean")
  )
  
  # Add columns if they don't exist
  for (col_name in col_names) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <<- NA_real_
      cat("Initialized column:", col_name, "\n")
    }
  }
}

# Setup checkpoint function to save interim progress and manage checkpoint files
save_progress_checkpoint <- function() {
  # Create timestamp for the new checkpoint file
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  checkpoint_file <- paste0("data/tmp/sentiment_analysis_progress_", timestamp, ".rds")
  
  # Save the current progress
  saveRDS(df, checkpoint_file)
  cat("Progress checkpoint saved to:", checkpoint_file, "\n")
  
  # Also save a consolidated checkpoint file that always has the same name
  # This makes it easier to reference in other scripts
  consolidated_file <- "data/tmp/sentiment_analysis_latest_checkpoint.rds"
  saveRDS(df, consolidated_file)
  cat("Also saved to consolidated checkpoint:", consolidated_file, "\n")
  
  # Cleanup old checkpoint files - keep only 10 most recent files
  checkpoint_files <- list.files("data/tmp", pattern = "sentiment_analysis_progress_.*\\.rds", full.names = TRUE)
  
  # If we have more than 10 checkpoint files, remove the oldest ones
  if (length(checkpoint_files) > 10) {
    # Sort files by modification time (oldest first)
    checkpoint_files <- checkpoint_files[order(file.info(checkpoint_files)$mtime)]
    
    # Determine how many files to remove
    files_to_remove <- checkpoint_files[1:(length(checkpoint_files) - 10)]
    
    # Remove the oldest files
    for (file in files_to_remove) {
      file.remove(file)
      cat("Removed old checkpoint file:", file, "\n")
    }
  }
}

# Set up checkpoint timer to save every 10 minutes (more time between saves to reduce IO overhead)
last_checkpoint_time <- Sys.time()
checkpoint_interval <- 600  # 10 minutes in seconds

#------------------------------------------------------------------------------
# 4.1 RUN EVERY MODEL ACROSS ALL THREE LANGUAGE CONDITIONS
#------------------------------------------------------------------------------

# The three conditions the study compares: prompt language crossed with text
# language. Driven by a loop over model_clients rather than one block per model,
# so adding or dropping a model means editing src/94_models_map.R only.
conditions <- list(
  list(prompt = "en", text = "sentences"),     # English prompt, French text
  list(prompt = "fr", text = "sentences"),     # French prompt, French text
  list(prompt = "en", text = "sentences_en")   # English prompt, English translation
)

# Models named in SKIP_MODELS are passed over without being contacted. Set it
# when a provider is unavailable — a quota that resets tomorrow, an account
# awaiting billing — so the remaining models are not blocked behind it. Their
# columns stay NA and a later run picks them up from the checkpoint.
#   SKIP_MODELS=gemini35 Rscript src/40_prompt.R
skip_models <- trimws(strsplit(Sys.getenv("SKIP_MODELS", ""), ",")[[1]])
skip_models <- skip_models[nzchar(skip_models)]

# ONLY_MODELS does the reverse: every other model is passed over. Use it to add
# a model to a finished run. Otherwise every model's NA cells are retried, and
# Llama 3.2 1B alone has ~400 of them by design (see src/94_models_map.R).
#   ONLY_MODELS=mistralsmall32,mistrallarge4 Rscript src/40_prompt.R
only_models <- trimws(strsplit(Sys.getenv("ONLY_MODELS", ""), ",")[[1]])
only_models <- only_models[nzchar(only_models)]

# A misspelt name would silently skip nothing, or run everything: refuse it.
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

#==============================================================================
# 5. SAVE RESULTS
#==============================================================================

# Save the updated dataframe with all sentiment scores
cat("Saving the results...\n")
saveRDS(df, "data/tmp/data_manual_ranking_with_llm_scores.rds")

# Also save a CSV version for easier viewing in spreadsheet applications
write_csv(df, "data/tmp/data_manual_ranking_with_llm_scores.csv")

# Log the difference in columns between the original and processed data
new_columns <- setdiff(names(df), names(df_raw))
cat("Added", length(new_columns), "columns to the original dataset:\n")
cat(paste(new_columns, collapse=", "), "\n")

# Save one final checkpoint with completion timestamp
save_progress_checkpoint()

cat("Done! Results saved.\n")

#==============================================================================
# 6. ANALYSIS AND VISUALIZATION
#==============================================================================

# Basic summary of processed data
cat("\nSummary of results:\n")
cat("Number of sentences processed:", nrow(df), "\n")

#------------------------------------------------------------------------------
# 6.1 MISSING VALUES ANALYSIS
#------------------------------------------------------------------------------

# Identify model-related columns
model_columns <- grep("_en_|_fr_", names(df), value = TRUE)

# Count NA values for each model (failed sentiment evaluations)
na_summary <- sapply(df[model_columns], function(x) sum(is.na(x)))
cat("NA counts per model:\n")
print(na_summary)

#------------------------------------------------------------------------------
# 6.2 SENTIMENT DISTRIBUTION ANALYSIS
#------------------------------------------------------------------------------

# Calculate mean sentiment value for each model
mean_summary <- sapply(df[model_columns], function(x) mean(x, na.rm = TRUE))
cat("\nMean sentiment values per model:\n")
print(mean_summary)

#------------------------------------------------------------------------------
# 6.3 CORRELATION ANALYSIS
#------------------------------------------------------------------------------

# Calculate correlations between models to assess agreement level
cat("\nCalculating correlations between models...\n")
cor_matrix <- cor(df[model_columns], use = "pairwise.complete.obs")

# Save correlation matrix for further analysis
write.csv(cor_matrix, "data/tmp/llm_sentiment_correlations.csv")
cat("Correlation matrix saved to data/tmp/llm_sentiment_correlations.csv\n")

#------------------------------------------------------------------------------
# 6.4 VISUALIZATION FUNCTIONS (FOR OPTIONAL USE)
#------------------------------------------------------------------------------

#' Create diagnostic visualizations for sentiment analysis results
#' 
#' This function generates density plots and boxplots to visualize
#' the distribution of sentiment scores across models and languages
create_diagnostic_plots <- function() {
  library(ggplot2)
  library(tidyr)
  
  # Convert data to long format for easier plotting
  df_long <- df %>%
    select(sentences, all_of(model_columns)) %>%
    pivot_longer(cols = model_columns, 
                names_to = "model", 
                values_to = "sentiment")
  
  # Extract model name, prompt language, and text language from column names
  df_long <- df_long %>%
    mutate(
      model_name = sub("_[^_]+_[^_]+$", "", model),             # Extract model name
      prompt_lang = sub("^.*_([^_]+)_.*$", "\\1", model),       # Extract prompt language
      text_lang = sub("^.*_.*_([^_]+)$", "\\1", model)          # Extract text language
    )
  
  # Create density plot to show sentiment distribution by model and language
  p1 <- ggplot(df_long, aes(x = sentiment, fill = model_name)) +
    geom_density(alpha = 0.5) +
    facet_grid(prompt_lang ~ text_lang) +
    theme_minimal() +
    labs(title = "Distribution of Sentiment Scores by Model and Language",
         x = "Sentiment Score", y = "Density")
  
  # Save density plot
  ggsave("data/tmp/sentiment_distributions.png", p1, width = 12, height = 10)
  
  # Create boxplot to compare model distributions side by side
  p2 <- ggplot(df_long, aes(x = model, y = sentiment, fill = prompt_lang)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Sentiment Score Comparisons Across Models",
         x = "Model", y = "Sentiment Score")
  
  # Save boxplot
  ggsave("data/tmp/sentiment_boxplots.png", p2, width = 14, height = 10)
  
  cat("Diagnostic plots saved to data/tmp/sentiment_distributions.png and data/tmp/sentiment_boxplots.png\n")
}

#==============================================================================
# 7. ADVANCED ANALYSES (USING UTILITY FUNCTIONS)
#==============================================================================

cat("\nGenerating additional analysis...\n")

#------------------------------------------------------------------------------
# 7.1 SUMMARY STATISTICS
#------------------------------------------------------------------------------

# Generate detailed summary statistics for each model
summary_stats <- generate_summary_stats(df)
write_csv(summary_stats, "data/tmp/model_summary_statistics.csv")
cat("Model summary statistics saved to data/tmp/model_summary_statistics.csv\n")

#------------------------------------------------------------------------------
# 7.2 CORRELATION VISUALIZATION
#------------------------------------------------------------------------------

# Create a heatmap to visualize correlations between models
create_correlation_heatmap(df)

#------------------------------------------------------------------------------
# 7.3 LANGUAGE EFFECT ANALYSIS
#------------------------------------------------------------------------------

# Analyze how different language combinations affect model performance
language_performance <- analyze_language_performance(df)
write_csv(language_performance, "data/tmp/language_performance_analysis.csv")
cat("Language performance analysis saved to data/tmp/language_performance_analysis.csv\n")

#------------------------------------------------------------------------------
# 7.4 OPTIONAL ADVANCED ANALYSES (COMMENTED OUT)
#------------------------------------------------------------------------------

# Uncomment to perform bootstrap resampling for confidence intervals
# bootstrap_stats <- estimate_model_performance(df)
# write_csv(bootstrap_stats, "data/tmp/bootstrap_statistics.csv")
# cat("Bootstrap statistics saved to data/tmp/bootstrap_statistics.csv\n")

# Uncomment to generate visualization of sentiment distributions
# create_diagnostic_plots()

cat("\nAnalysis complete!\n")

}, error = function(e) {
  # Handle any errors/interruptions by saving current state of df
  cat("\nScript interrupted or error occurred:", conditionMessage(e), "\n")
  
  # Save the current state of the dataframe
  emergency_file <- paste0("data/tmp/sentiment_analysis_INTERRUPTED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  saveRDS(df, emergency_file)
  cat("Emergency backup saved to:", emergency_file, "\n")
  cat("You can load this file with: df <- readRDS('", emergency_file, "')\n")
  
  # Re-throw the error
  stop(e)
}, finally = {
  # Release the lock so the next run can start. Guarded so a crash midway
  # through still frees it.
  if (exists("LOCK_PATH") && file.exists(LOCK_PATH)) {
    held <- suppressWarnings(as.integer(readLines(LOCK_PATH, warn = FALSE)[1]))
    if (!is.na(held) && held == Sys.getpid()) file.remove(LOCK_PATH)
  }
  cat("Script execution completed or interrupted. Check for saved checkpoints if needed.\n")
})
