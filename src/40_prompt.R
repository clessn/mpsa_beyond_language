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
    run_results <- numeric(n_runs)  # Store results from each run
    
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
        tryCatch({
          # Reset chat history to ensure each prompt is treated as new
          model_client$set_turns(list())
          
          # Make API call with retry and backoff for connection issues
          response <- retry_with_backoff({
            model_client$chat(prompt)
          })
        }, error = function(e) {
          cat(sprintf("ERROR with %s on attempt %d: %s\n", model_identifier, attempt, e$message))
          # Add a longer timeout after errors to let rate limits recover
          Sys.sleep(10)
          return(NULL)
        })
        
        # Skip rest of loop if response is NULL (error occurred)
        if (is.null(response)) {
          attempt <- attempt + 1
          next
        }
        
        # Apply appropriate rate limiting based on model type and token usage
        if (grepl("groq", model_name_prefix, ignore.case = TRUE)) {
          # Check token usage for Groq models
          last_usage <- tail(model_client$tokens(), 1)
          token_count <- ifelse(is.null(last_usage) || nrow(last_usage) == 0, 0, 
                               sum(last_usage$prompt_tokens, last_usage$completion_tokens))
          
          # Adaptive sleep based on specific model token limits
          if (grepl("gemma29b", model_name_prefix, ignore.case = TRUE)) {
            # Gemma 2 9B: 15000 tokens per minute (250 per second)
            sleep_time <- ifelse(token_count > 500, max(2, token_count / 250), 2)
          } else if (grepl("llama321b", model_name_prefix, ignore.case = TRUE)) {
            # Llama 3.2 1B: 7000 tokens per minute (117 per second)
            sleep_time <- ifelse(token_count > 300, max(2, token_count / 117), 2)
          } else {
            # Mistral and DeepSeek: 6000 tokens per minute (100 per second)
            sleep_time <- ifelse(token_count > 300, max(3, token_count / 100), 2)
          }
          
          # Log and apply the sleep
          if (token_count > 300) {
            cat(sprintf("Model %s: High token usage (%d tokens), sleeping for %.1f seconds\n", 
                        model_name_prefix, token_count, sleep_time))
          }
          Sys.sleep(sleep_time)
        } else if (grepl("llama323b|qwq32b|deepseekr1|llama3370b", model_name_prefix, ignore.case = TRUE)) {
          # 1.5 second delay for Fireworks.ai hosted models
          Sys.sleep(1.5)
        } else {
          # 1 second delay for all other API-based models
          Sys.sleep(1)
        }
        
        #----------------------------------------------------------------------
        # 2.3 RESPONSE PARSING
        #----------------------------------------------------------------------
        
        # Extract numerical sentiment value from the model's response
        extracted_value <- clean_sentiment_value(response)
        
        # Validate that we got a value in the acceptable range (-1 to 1)
        if (!is.na(extracted_value) && extracted_value >= -1 && extracted_value <= 1) {
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

# Initialize all LLM clients
cat("Initializing all LLM clients...\n")

#------------------------------------------------------------------------------
# 3.1 FIREWORKS.AI HOSTED MODELS
#------------------------------------------------------------------------------

# Llama 3.2 3B parameters (instruct version)
llama323b <- ellmer::chat_openai(
  system_prompt = system_prompt,
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/llama-v3p2-3b-instruct",
  echo = "none"
)

# QWQ 32B parameters
qwq32b <- ellmer::chat_openai(
  system_prompt = system_prompt,
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/qwq-32b",
  echo = "none"
)

# DeepSeek R1 Basic
deepseekr1 <- ellmer::chat_openai(
  system_prompt = system_prompt,
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/deepseek-r1-basic",
  echo = "none"
)

# Llama 3.3 70B (instruct version)
llama3370b <- ellmer::chat_openai(
  system_prompt = system_prompt,
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/llama-v3p3-70b-instruct",
  echo = "none"
)

#------------------------------------------------------------------------------
# 3.2 GROQ-HOSTED MODELS
#------------------------------------------------------------------------------

# Gemma 2 (9B parameters)
gemma29b <- ellmer::chat_groq(
  system_prompt = system_prompt,
  model = "gemma2-9b-it",
  echo = "none"
)

# Llama 3.2 (1B parameters)
llama321b <- ellmer::chat_groq(
  system_prompt = system_prompt,
  model = "llama-3.2-1b-preview",
  echo = "none"
)

# Mistral (8x7B parameters, 32K context window)
mistral <- ellmer::chat_groq(
  system_prompt = system_prompt,
  model = "mistral-saba-24b",
  echo = "none"
)

# DeepSeek R1 Distill Llama (70B parameters)
deepseekr1distillllama <- ellmer::chat_groq(
  system_prompt = system_prompt,
  model = "deepseek-r1-distill-llama-70b",
  echo = "none"
)

#------------------------------------------------------------------------------
# 3.3 OTHER CLOUD API MODELS
#------------------------------------------------------------------------------

# Anthropic Claude 3.5 Haiku
claude35 <- ellmer::chat_claude(
  system_prompt = system_prompt,
  model = "claude-3-5-haiku-20241022",
  echo = "none"
)

# Google Gemini 2.0 Flash
gemini20 <- ellmer::chat_gemini(
  system_prompt = system_prompt,
  model = "gemini-2.0-flash",
  echo = "none"
)

# DeepSeek Chat 
deepseekchat <- ellmer::chat_deepseek(
  system_prompt = system_prompt,
  model = "deepseek-chat",
  echo = "none"
)

# OpenAI GPT-4o
gpt4o <- ellmer::chat_openai(
  system_prompt = system_prompt,
  model = "gpt-4o",
  echo = "none"
)

cat("All clients initialized. Starting sentiment analysis...\n")

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
# 4.1 FIREWORKS.AI MODELS
#------------------------------------------------------------------------------

# Llama 3.2 3B
cat("Processing Llama 3.2 3B model...\n")
# Initialize columns for all combinations
initialize_model_columns("llama323b_en_fr")
initialize_model_columns("llama323b_fr_fr")
initialize_model_columns("llama323b_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(llama323b, "llama323b", "en", "sentences")      # English prompt, French text
df <- run_sentiment_analysis(llama323b, "llama323b", "fr", "sentences")      # French prompt, French text  
df <- run_sentiment_analysis(llama323b, "llama323b", "en", "sentences_en")   # English prompt, English text

# QWQ 32B
cat("Processing QWQ 32B model...\n")
# Initialize columns for all combinations
initialize_model_columns("qwq32b_en_fr")
initialize_model_columns("qwq32b_fr_fr")
initialize_model_columns("qwq32b_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(qwq32b, "qwq32b", "en", "sentences")         # English prompt, French text
df <- run_sentiment_analysis(qwq32b, "qwq32b", "fr", "sentences")         # French prompt, French text
df <- run_sentiment_analysis(qwq32b, "qwq32b", "en", "sentences_en")      # English prompt, English text

# DeepSeek R1
cat("Processing DeepSeek R1 model...\n")
# Initialize columns for all combinations
initialize_model_columns("deepseekr1_en_fr")
initialize_model_columns("deepseekr1_fr_fr")
initialize_model_columns("deepseekr1_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(deepseekr1, "deepseekr1", "en", "sentences")     # English prompt, French text
df <- run_sentiment_analysis(deepseekr1, "deepseekr1", "fr", "sentences")     # French prompt, French text
df <- run_sentiment_analysis(deepseekr1, "deepseekr1", "en", "sentences_en")  # English prompt, English text

# Llama 3.3 70B
cat("Processing Llama 3.3 70B model...\n")
# Initialize columns for all combinations
initialize_model_columns("llama3370b_en_fr")
initialize_model_columns("llama3370b_fr_fr")
initialize_model_columns("llama3370b_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(llama3370b, "llama3370b", "en", "sentences")   # English prompt, French text
df <- run_sentiment_analysis(llama3370b, "llama3370b", "fr", "sentences")   # French prompt, French text
df <- run_sentiment_analysis(llama3370b, "llama3370b", "en", "sentences_en") # English prompt, English text

#------------------------------------------------------------------------------
# 4.2 GROQ MODELS
#------------------------------------------------------------------------------

# Gemma 2 9B
cat("Processing Gemma 2 9B model...\n")
# Initialize columns for all combinations
initialize_model_columns("gemma29b_en_fr")
initialize_model_columns("gemma29b_fr_fr")
initialize_model_columns("gemma29b_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(gemma29b, "gemma29b", "en", "sentences")    # English prompt, French text
df <- run_sentiment_analysis(gemma29b, "gemma29b", "fr", "sentences")    # French prompt, French text
df <- run_sentiment_analysis(gemma29b, "gemma29b", "en", "sentences_en") # English prompt, English text

# Llama 3.2 1B
cat("Processing Llama 3.2 1B model...\n")
# Initialize columns for all combinations
initialize_model_columns("llama321b_en_fr")
initialize_model_columns("llama321b_fr_fr")
initialize_model_columns("llama321b_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(llama321b, "llama321b", "en", "sentences")  # English prompt, French text
df <- run_sentiment_analysis(llama321b, "llama321b", "fr", "sentences")  # French prompt, French text
df <- run_sentiment_analysis(llama321b, "llama321b", "en", "sentences_en") # English prompt, English text

# Mistral
cat("Processing Mistral model...\n")
# Initialize columns for all combinations
initialize_model_columns("mistral_en_fr")
initialize_model_columns("mistral_fr_fr")
initialize_model_columns("mistral_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(mistral, "mistral", "en", "sentences")      # English prompt, French text
df <- run_sentiment_analysis(mistral, "mistral", "fr", "sentences")      # French prompt, French text
df <- run_sentiment_analysis(mistral, "mistral", "en", "sentences_en")   # English prompt, English text

# DeepSeek R1 Distill Llama
cat("Processing DeepSeek R1 Distill Llama model...\n")
# Initialize columns for all combinations
initialize_model_columns("deepseekr1distillllama_en_fr")
initialize_model_columns("deepseekr1distillllama_fr_fr")
initialize_model_columns("deepseekr1distillllama_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(deepseekr1distillllama, "deepseekr1distillllama", "en", "sentences")     # English prompt, French text
df <- run_sentiment_analysis(deepseekr1distillllama, "deepseekr1distillllama", "fr", "sentences")     # French prompt, French text
df <- run_sentiment_analysis(deepseekr1distillllama, "deepseekr1distillllama", "en", "sentences_en")  # English prompt, English text

#------------------------------------------------------------------------------
# 4.3 OTHER CLOUD API MODELS
#------------------------------------------------------------------------------

# Claude 3.5
cat("Processing Claude 3.5 model...\n")
# Initialize columns for all combinations
initialize_model_columns("claude35_en_fr")
initialize_model_columns("claude35_fr_fr")
initialize_model_columns("claude35_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(claude35, "claude35", "en", "sentences")   # English prompt, French text
df <- run_sentiment_analysis(claude35, "claude35", "fr", "sentences")   # French prompt, French text
df <- run_sentiment_analysis(claude35, "claude35", "en", "sentences_en") # English prompt, English text

# Gemini 2.0
cat("Processing Gemini 2.0 model...\n")
# Initialize columns for all combinations
initialize_model_columns("gemini20_en_fr")
initialize_model_columns("gemini20_fr_fr")
initialize_model_columns("gemini20_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(gemini20, "gemini20", "en", "sentences")   # English prompt, French text
df <- run_sentiment_analysis(gemini20, "gemini20", "fr", "sentences")   # French prompt, French text
df <- run_sentiment_analysis(gemini20, "gemini20", "en", "sentences_en") # English prompt, English text

# DeepSeek Chat
cat("Processing DeepSeek Chat model...\n")
# Initialize columns for all combinations
initialize_model_columns("deepseekchat_en_fr")
initialize_model_columns("deepseekchat_fr_fr")
initialize_model_columns("deepseekchat_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(deepseekchat, "deepseekchat", "en", "sentences")  # English prompt, French text
df <- run_sentiment_analysis(deepseekchat, "deepseekchat", "fr", "sentences")  # French prompt, French text
df <- run_sentiment_analysis(deepseekchat, "deepseekchat", "en", "sentences_en") # English prompt, English text

# GPT-4o
cat("Processing GPT-4o model...\n")
# Initialize columns for all combinations
initialize_model_columns("gpt4o_en_fr")
initialize_model_columns("gpt4o_fr_fr")
initialize_model_columns("gpt4o_en_en")

# Run sentiment analysis - results stored directly in df
df <- run_sentiment_analysis(gpt4o, "gpt4o", "en", "sentences")           # English prompt, French text
df <- run_sentiment_analysis(gpt4o, "gpt4o", "fr", "sentences")           # French prompt, French text
df <- run_sentiment_analysis(gpt4o, "gpt4o", "en", "sentences_en")        # English prompt, English text

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
  # This will execute regardless of whether there was an error or not
  cat("Script execution completed or interrupted. Check for saved checkpoints if needed.\n")
})
