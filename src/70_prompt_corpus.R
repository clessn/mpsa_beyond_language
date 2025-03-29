###############################################################################
# Full Corpus Sentiment Analysis with Gemini
# 
# This script runs sentiment analysis on the full corpus of French news articles
# using only the Gemini model with French prompts on French text.
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

# Source helper functions and prompt functions (only once at the beginning)
source("src/92_llm_helper_funcs.R")
source("src/93_prompts.R")

# Get system prompt for model initialization (defined in prompts.R)
system_prompt <- get_system_prompt()

#==============================================================================
# 2. DATA LOADING & CHECKPOINT HANDLING
#==============================================================================

# Check for existing checkpoint files
checkpoint_files <- list.files("data/tmp", pattern = "corpus_sentiment_(progress|latest_checkpoint|INTERRUPTED).*\\.rds", full.names = TRUE)

if (length(checkpoint_files) > 0) {
  # First try latest_checkpoint.rds if it exists (it's always the most current)
  consolidated_checkpoint <- grep("corpus_sentiment_latest_checkpoint\\.rds$", checkpoint_files, value = TRUE)
  
  # If consolidated checkpoint exists, use it
  if (length(consolidated_checkpoint) > 0) {
    latest_checkpoint <- consolidated_checkpoint[1]
    cat("Found consolidated checkpoint file:", latest_checkpoint, "\n")
  } else {
    # Otherwise, find the most recent progress or interrupted checkpoint
    # Sort by modification time to get the most recent
    latest_checkpoint <- checkpoint_files[order(file.info(checkpoint_files)$mtime, decreasing = TRUE)][1]
    cat("Found checkpoint file:", latest_checkpoint, "\n")
  }
  
  # Load the checkpoint data
  checkpoint_df <- tryCatch({
    readRDS(latest_checkpoint)
  }, error = function(e) {
    cat("Error reading checkpoint file:", e$message, "\n")
    NULL
  })
  
  # If checkpoint loaded successfully, use it
  if (!is.null(checkpoint_df)) {
    # We'll verify the structure once we load the raw data
    df <- checkpoint_df
    resuming_from_checkpoint <- TRUE
    cat("Loaded checkpoint data. Will verify structure before proceeding.\n")
  } else {
    resuming_from_checkpoint <- FALSE
    cat("Failed to load checkpoint data. Starting with fresh data.\n")
  }
} else {
  resuming_from_checkpoint <- FALSE
  cat("No checkpoint files found. Starting with fresh data.\n")
}

# Load the original dataset (only if needed or for verification)
df_raw <- readRDS("data/tmp/news_df_tone_index.rds")

# If resuming from checkpoint, verify it has the expected structure
if (resuming_from_checkpoint) {
  if (all(names(df_raw) %in% names(df))) {
    cat("Checkpoint data structure verified. Resuming from checkpoint...\n")
  } else {
    cat("Checkpoint file has unexpected structure. Starting fresh with original data.\n")
    df <- df_raw
  }
} else {
  df <- df_raw
  cat("Starting fresh with original data.\n")
}

#==============================================================================
# 3. CHECKPOINT MANAGEMENT FUNCTIONS
#==============================================================================

# Setup checkpoint function to save interim progress and manage checkpoint files
save_progress_checkpoint <- function(force = FALSE) {
  # Check if it's time for a checkpoint or if forced
  current_time <- Sys.time()
  time_to_save <- force || difftime(current_time, last_checkpoint_time, units = "secs") > checkpoint_interval
  
  if (time_to_save) {
    # Calculate progress percentage
    total_rows <- nrow(df)
    processed_rows <- sum(!is.na(df[["gemini_fr_fr"]]))
    progress_pct <- round((processed_rows / total_rows) * 100, 2)
    
    # Create timestamp for the new checkpoint file
    timestamp <- format(current_time, "%Y%m%d_%H%M%S")
    checkpoint_file <- paste0("data/tmp/corpus_sentiment_progress_", timestamp, ".rds")
    
    # Save the current progress
    tryCatch({
      saveRDS(df, checkpoint_file)
      cat(sprintf("Progress checkpoint saved to: %s (%.2f%% complete, %d of %d rows)\n", 
                 checkpoint_file, progress_pct, processed_rows, total_rows))
    
      # Also save a consolidated checkpoint file that always has the same name
      consolidated_file <- "data/tmp/corpus_sentiment_latest_checkpoint.rds"
      saveRDS(df, consolidated_file)
      cat("Also saved to consolidated checkpoint:", consolidated_file, "\n")
      
      # Update the last checkpoint time
      last_checkpoint_time <<- current_time
      
      # Cleanup old checkpoint files - keep only 10 most recent files
      checkpoint_files <- list.files("data/tmp", pattern = "corpus_sentiment_progress_.*\\.rds", full.names = TRUE)
      
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
      
      return(TRUE)  # Checkpoint was saved
    }, error = function(e) {
      cat("ERROR: Failed to save checkpoint:", conditionMessage(e), "\n")
      return(FALSE)
    })
  }
  
  return(FALSE)  # No checkpoint saved
}

# Set up checkpoint timer (10 minutes between automatic saves)
last_checkpoint_time <- Sys.time()
checkpoint_interval <- 600  # 10 minutes in seconds

#==============================================================================
# 4. MODEL INITIALIZATION
#==============================================================================

# Initialize Gemini client
cat("Initializing Gemini client...\n")

# Google Gemini 2.0 Flash
gemini <- ellmer::chat_gemini(
  system_prompt = system_prompt,
  model = "gemini-2.0-flash",
  echo = "none"
)

cat("Gemini client initialized. Ready for sentiment analysis.\n")

#==============================================================================
# 5. CORE SENTIMENT ANALYSIS FUNCTION
#==============================================================================

#' Run sentiment analysis for Gemini model on the whole corpus
#'
#' This function processes each article in the dataset with the Gemini model,
#' using French prompts on the French article text.
#'
#' @param data The dataframe containing articles to analyze
#' @param model_client The initialized LLM client object
#' @return Updated dataframe with filled sentiment scores
run_corpus_sentiment_analysis <- function(data, model_client) {
  # Create a working copy of the input data
  result_df <- data
  
  # Create model identifier for column names
  model_identifier <- "gemini_fr_fr"
  
  # Initialize column for this model if it doesn't exist
  if (!paste0(model_identifier) %in% names(result_df)) {
    result_df[[paste0(model_identifier)]] <- NA_real_
    cat("Initialized column:", paste0(model_identifier), "\n")
  }
  
  # Process each article in the dataset
  for (i in seq_along(result_df[["text_body"]])) {
    # Skip if we already have a valid result for this item
    if (!is.na(result_df[[model_identifier]][i])) {
      if (i %% 50 == 0) {  # Log less frequently to reduce output
        cat(sprintf("Skipping %s item %d (already processed)\n", model_identifier, i))
      }
      next
    }
    
    # Log progress information (only every 10 items to reduce console output)
    if (i %% 10 == 0) {
      cat(sprintf("Processing %s: Item %d of %d\n", 
                model_identifier, i, length(result_df[["text_body"]])))
    }
    
    # Check if it's time for a checkpoint save (based on time elapsed)
    current_time <- Sys.time()
    if (difftime(current_time, last_checkpoint_time, units = "secs") > checkpoint_interval) {
      # Update global df with current results first
      df <<- result_df
      # Then save checkpoint
      save_progress_checkpoint()
      last_checkpoint_time <<- current_time  # Update the global variable
    }
    
    # Create French prompt for French text
    prompt <- fr_prompt_fr_text(result_df$text_body[i])
    
    # Set up for multiple runs with retry logic for each run
    max_retries <- 3  # Maximum number of retry attempts per run
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
      
      # Apply rate limiting - 1 second delay for all API-based models
      Sys.sleep(1)
      
      # Extract numerical sentiment value from the model's response
      extracted_value <- clean_sentiment_value(response)
      
      # Validate that we got a value in the acceptable range (-1 to 1)
      if (!is.na(extracted_value) && extracted_value >= -1 && extracted_value <= 1) {
        # Valid value obtained - store it and break the retry loop
        result_df[[model_identifier]][i] <- extracted_value
        valid_value_obtained <- TRUE
        # Print for every item
        cat(sprintf("Valid value %.2f obtained for item %d on attempt %d\n", 
                  extracted_value, i, attempt))
        break
      } else {
        # Invalid response - log it and try again (up to max_retries)
        cat(sprintf("Attempt %d/%d: Invalid value for item %d\n", 
                  attempt, max_retries, i))
        cat("Response:", response, "\n")
        attempt <- attempt + 1
      }
    }
    
    # If all retry attempts failed, store NA
    if (!valid_value_obtained) {
      cat(sprintf("Warning: Failed to extract numerical value from responses for item %d after %d attempts\n", 
                i, max_retries))
      result_df[[model_identifier]][i] <- NA_real_
    }
    
    # Save checkpoint every 10 items for additional safety
    if (i %% 10 == 0) {
      # Update global df with current results first
      df <<- result_df
      # Force a checkpoint save regardless of time elapsed
      save_progress_checkpoint(force = TRUE)
    }
  }
  
  # Return the updated dataframe
  return(result_df)
}

#==============================================================================
# 6. RUN SENTIMENT ANALYSIS
#==============================================================================

# Process the entire corpus with Gemini using French prompts
cat("Processing full corpus with Gemini model...\n")

# Run sentiment analysis with properly isolated scope
df <- run_corpus_sentiment_analysis(df, gemini)

#==============================================================================
# 7. SAVE RESULTS
#==============================================================================

# Save the updated dataframe with all sentiment scores
cat("Saving the results...\n")
saveRDS(df, "data/clean/news_df_sentiment_gemini.rds")

# Log the difference in columns between the original and processed data
new_columns <- setdiff(names(df), names(df_raw))
cat("Added", length(new_columns), "columns to the original dataset:\n")
cat(paste(new_columns, collapse=", "), "\n")

# Save one final checkpoint with completion timestamp
save_progress_checkpoint(force = TRUE)

cat("Done! Results saved.\n")

#==============================================================================
# 8. BASIC ANALYSIS
#==============================================================================

# Basic summary of processed data
cat("\nSummary of results:\n")
cat("Number of articles processed:", nrow(df), "\n")

# Count NA values (failed sentiment evaluations)
na_count <- sum(is.na(df[["gemini_fr_fr"]]))
cat("NA count:", na_count, " (", round(na_count/nrow(df)*100, 2), "%)", "\n")

# Calculate mean sentiment value
mean_sentiment <- mean(df[["gemini_fr_fr"]], na.rm = TRUE)
cat("Mean sentiment value:", mean_sentiment, "\n")

}, error = function(e) {
  # Handle any errors/interruptions by saving current state of df
  cat("\nScript interrupted or error occurred:", conditionMessage(e), "\n")
  
  # Create a more descriptive emergency file name
  emergency_file <- paste0("data/tmp/corpus_sentiment_INTERRUPTED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  
  # Try to save the emergency file
  tryCatch({
    saveRDS(df, emergency_file)
    cat("Emergency backup saved to:", emergency_file, "\n")
    cat("You can load this file with: df <- readRDS('", emergency_file, "')\n")
  }, error = function(save_error) {
    cat("ERROR: Failed to save emergency backup:", conditionMessage(save_error), "\n")
  })
  
  # Save to the consolidated checkpoint file as well if possible
  tryCatch({
    consolidated_file <- "data/tmp/corpus_sentiment_latest_checkpoint.rds"
    saveRDS(df, consolidated_file)
    cat("Also saved to consolidated checkpoint:", consolidated_file, "\n")
  }, error = function(save_error) {
    cat("ERROR: Failed to save to consolidated checkpoint:", conditionMessage(save_error), "\n")
  })
  
  # Re-throw the original error
  stop(e)
}, finally = {
  # This will execute regardless of whether there was an error or not
  cat("Script execution completed or interrupted. Check for saved checkpoints if needed.\n")
})
