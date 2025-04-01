###############################################################################
# FULL CORPUS SENTIMENT ANALYSIS WITH GEMINI (PART 2: ENGLISH PROMPTS)
# 
# This script continues the sentiment analysis on the full corpus of French news 
# articles, using English prompts on French text with the Gemini model.
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

# First load the previously processed dataset with French prompt results
cat("Loading previously processed dataset (French prompt)...\n")
df <- readRDS("data/clean/news_df_sentiment_gemini.rds")

# Verify that the French prompt analysis has been completed
if (!"gemini_fr_fr" %in% names(df)) {
  stop("Error: The dataset does not contain the 'gemini_fr_fr' column. Please run script 70_prompt_corpus.R first.")
} else {
  # Check if the French prompt analysis is complete
  na_count_fr <- sum(is.na(df[["gemini_fr_fr"]]))
  if (na_count_fr > 0) {
    cat("Warning: French prompt analysis is incomplete. ", na_count_fr, " of ", nrow(df), " rows have NA values.\n")
    cat("Consider completing script 70_prompt_corpus.R first.\n")
    cat("Proceeding anyway with English prompt analysis...\n")
  } else {
    cat("French prompt analysis is complete. Proceeding with English prompt analysis...\n")
  }
}

# Check for English prompt column - if it exists, we might be resuming
if ("gemini_en_fr" %in% names(df)) {
  cat("Found existing 'gemini_en_fr' column. Might be resuming from a previous run...\n")
} else {
  cat("Initializing 'gemini_en_fr' column for English prompt results...\n")
  df[["gemini_en_fr"]] <- NA_real_
}

# Check for existing checkpoint files
checkpoint_files <- list.files("data/tmp", pattern = "corpus_sentiment_(progress|latest_checkpoint|INTERRUPTED).*\\.rds", full.names = TRUE)

# Set up checkpoint timer (10 minutes between automatic saves)
last_checkpoint_time <- Sys.time()
checkpoint_interval <- 600  # 10 minutes in seconds

#==============================================================================
# 3. CHECKPOINT MANAGEMENT FUNCTIONS
#==============================================================================

# Setup checkpoint function to save interim progress and manage checkpoint files
save_progress_checkpoint <- function(force = FALSE) {
  # Check if it's time for a checkpoint or if forced
  current_time <- Sys.time()
  time_to_save <- force || difftime(current_time, last_checkpoint_time, units = "secs") > checkpoint_interval
  
  if (time_to_save) {
    # Calculate progress percentage for English prompt
    total_rows <- nrow(df)
    processed_rows <- sum(!is.na(df[["gemini_en_fr"]]))
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

#' Run sentiment analysis for Gemini model on the whole corpus using English prompts
#'
#' This function processes each article in the dataset with the Gemini model,
#' using English prompts on the French article text.
#'
#' @param data The dataframe containing articles to analyze
#' @param model_client The initialized LLM client object
#' @return Updated dataframe with filled sentiment scores
run_corpus_sentiment_analysis_english <- function(data, model_client) {
  # Create a working copy of the input data
  result_df <- data
  
  # Create model identifier for column names
  model_identifier <- "gemini_en_fr"
  
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
    
    # Create English prompt for French text
    prompt <- en_prompt_fr_text(result_df$text_body[i])
    
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

# Process the entire corpus with Gemini using English prompts on French text
cat("Processing full corpus with Gemini model using English prompts...\n")

# Run sentiment analysis with properly isolated scope
df <- run_corpus_sentiment_analysis_english(df, gemini)

#==============================================================================
# 7. SAVE RESULTS
#==============================================================================

# Save the updated dataframe with all sentiment scores
cat("Saving the results...\n")
saveRDS(df, "data/clean/news_df_sentiment_gemini.rds")

# Log the difference in columns between the original load and processed data
cat("Dataset now includes the following sentiment columns: gemini_fr_fr, gemini_en_fr\n")

# Save one final checkpoint with completion timestamp
save_progress_checkpoint(force = TRUE)

cat("Done! Results saved.\n")

#==============================================================================
# 8. BASIC ANALYSIS
#==============================================================================

# Basic summary of processed data
cat("\nSummary of results:\n")
cat("Number of articles processed:", nrow(df), "\n")

# Count NA values (failed sentiment evaluations) for both columns
na_count_fr <- sum(is.na(df[["gemini_fr_fr"]]))
na_count_en <- sum(is.na(df[["gemini_en_fr"]]))

cat("Gemini French prompt NA count:", na_count_fr, " (", round(na_count_fr/nrow(df)*100, 2), "%)", "\n")
cat("Gemini English prompt NA count:", na_count_en, " (", round(na_count_en/nrow(df)*100, 2), "%)", "\n")

# Calculate mean sentiment values
mean_sentiment_fr <- mean(df[["gemini_fr_fr"]], na.rm = TRUE)
mean_sentiment_en <- mean(df[["gemini_en_fr"]], na.rm = TRUE)

cat("Mean sentiment value (French prompt):", mean_sentiment_fr, "\n")
cat("Mean sentiment value (English prompt):", mean_sentiment_en, "\n")

# Calculate correlation between the two prompt approaches
correlation <- cor(df[["gemini_fr_fr"]], df[["gemini_en_fr"]], use = "pairwise.complete.obs")
cat("Correlation between French and English prompt results:", correlation, "\n")

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