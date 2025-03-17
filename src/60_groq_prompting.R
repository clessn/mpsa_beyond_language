library(dplyr)
library(ellmer)
library(lubridate)
library(glue)

# Only print essential messages with simple prefixes
cat_message <- function(msg, level = "INFO") {
  if (level %in% c("WARNING", "ERROR", "CRITICAL")) {
    prefix <- switch(level,
                    "WARNING" = "⚠️ ",
                    "ERROR" = "❌ ",
                    "CRITICAL" = "🛑 ",
                    "")
    cat(glue("{prefix}{msg}\n"))
  } else {
    cat(glue("{msg}\n"))
  }
}

# Load data
df_path <- "data/tmp/data_manual_ranking.rds"
if (!file.exists(df_path)) {
  stop("Data file not found")
}
df <- readRDS(df_path)

# Setup
system_prompt <- "You are a helpful assistant that analyzes the sentiment of text. You provide accurate and consistent sentiment ratings according to the specified scale."
models <- c("gemma2-9b-it", "llama-3.3-70b-versatile", "mixtral-8x7b-32768", "deepseek-r1-distill-llama-70b")

# Create columns for storing results
for (model in models) {
  model_snake_case <- gsub("-", "_", model)
  df[[model_snake_case]] <- NA
  df[[paste0(model_snake_case, "_run_1")]] <- NA
  df[[paste0(model_snake_case, "_run_2")]] <- NA
  df[[paste0(model_snake_case, "_run_3")]] <- NA
}

# Save initial state
saveRDS(df, "data/tmp/checkpoint_initial.rds")

# Check for existing progress
progress_file <- "data/tmp/sentiment_analysis_progress.rds"
if (file.exists(progress_file)) {
  progress <- readRDS(progress_file)
  # Ensure required variables exist in the progress file
  if (!is.null(progress$current_model_index) && !is.null(progress$current_run)) {
    current_model_index <- progress$current_model_index
    current_run <- progress$current_run
    start_row <- progress$last_completed_row + 1
    cat_message(glue("Resuming with model {models[current_model_index]}, run {current_run}, from row {start_row}"))
  } else {
    # If progress file exists but has invalid structure, create new
    current_model_index <- 1
    current_run <- 1
    start_row <- 1
    progress <- list(
      current_model_index = current_model_index,
      current_run = current_run,
      last_completed_row = 0,
      errors = list()
    )
    cat_message("Progress file found but missing required fields. Starting from beginning.", "WARNING")
  }
} else {
  # If no progress file exists, start from beginning
  current_model_index <- 1
  current_run <- 1
  start_row <- 1
  progress <- list(
    current_model_index = current_model_index,
    current_run = current_run,
    last_completed_row = 0,
    errors = list()
  )
  cat_message("Starting new analysis run")
}

# Completely revised parse_sentiment function
parse_sentiment <- function(response) {
  # Clean up the response - remove all whitespace
  clean_response <- gsub("\\s", "", response)
  
  # Look for a value between -1 and 1, with priority to decimal numbers
  if (grepl("-0\\.[0-9]+|0\\.[0-9]+|-1\\.0|1\\.0|-1|1|0", clean_response)) {
    # Extract the match
    match <- regexpr("-0\\.[0-9]+|0\\.[0-9]+|-1\\.0|1\\.0|-1|1|0", clean_response)
    value_str <- regmatches(clean_response, match)[[1]]
    
    # Convert to numeric and return
    return(round(as.numeric(value_str), 2))
  }
  
  # If no valid value was found, look for any number
  if (grepl("-?[0-9]+(\\.[0-9]+)?", clean_response)) {
    match <- regexpr("-?[0-9]+(\\.[0-9]+)?", clean_response)
    value_str <- regmatches(clean_response, match)[[1]]
    value <- as.numeric(value_str)
    
    # Normalize out-of-range values
    if (value < -1.0 || value > 1.0) {
      if (abs(value) <= 10) {
        value <- value / 10
      } else if (abs(value) <= 100) {
        value <- value / 100
      } else {
        value <- sign(value) * 1.0  # Cap at -1 or 1
      }
    }
    
    return(round(value, 2))
  }
  
  # If no number was found at all
  return(NA)
}

# Ensure variables are defined
if (!exists("current_model_index")) current_model_index <- 1
if (!exists("current_run")) current_run <- 1
if (!exists("start_row")) start_row <- 1

# Main loop - process one model at a time through the entire dataset
while (current_model_index <= length(models)) {
  model <- models[current_model_index]
  model_snake_case <- gsub("-", "_", model)
  
  cat_message(glue("Processing model: {model}, run: {current_run}"))
  
  # For each run, process the entire dataset
  row_start <- if (current_run == progress$current_run) start_row else 1
  
  for (i in row_start:nrow(df)) {
    # Skip empty sentences
    if (is.na(df$sentences[i]) || df$sentences[i] == "") {
      next
    }
    
    # Create prompt
    prompt <- paste0("Please analyze the sentiment of the following French text and provide a single numerical rating according to this scale:
    Sentiment Scale:
    -1.0: Strong negative sentiment - highly critical, hostile, or pessimistic content
    -0.5: Moderate negative sentiment - somewhat negative, disapproving, or concerned content
    0.0: Neutral sentiment - factual, balanced, or neither positive nor negative content
    0.5: Moderate positive sentiment - somewhat positive, approving, or optimistic content
    1.0: Strong positive sentiment - highly supportive, enthusiastic, or optimistic content
    You may also use values between these points for intermediate sentiment levels (e.g., -0.7, 0.3).
    Important instructions:
    1. First, carefully read and understand the text, considering cultural and linguistic nuances in French.
    2. Analyze the emotional tone, word choice, and overall message.
    3. Respond ONLY with a single numerical value between -1.0 and 1.0 that best represents the sentiment.
    4. Do not include ANY explanations, analysis, or additional text in your response.
    Here is the text to analyze: ", df$sentences[i])
    
    tryCatch({
      # Create groq client
      groq <- chat_groq(
        system_prompt = system_prompt,
        model = model
      )
      
      if (is.null(groq)) {
        stop("Failed to initialize groq client")
      }
      
      # Get response
      response <- groq$chat(prompt)
      
      if (is.null(response) || length(response) == 0) {
        cat_message("Error: Empty response", "ERROR")
        run_column_name <- paste0(model_snake_case, "_run_", current_run)
        df[i, run_column_name] <- NA
        next
      }
      
      # Add verbose debugging for response parsing
      cat_message(glue("Parsing response: '{response}'"))
      sentiment_value <- parse_sentiment(response)
      cat_message(glue("Parsed value: {ifelse(is.na(sentiment_value), 'NA', sentiment_value)}"))
      
      # Log and store result
      sentence_snippet <- substr(df$sentences[i], 1, min(40, nchar(df$sentences[i])))
      if (nchar(df$sentences[i]) > 40) sentence_snippet <- paste0(sentence_snippet, "...")
      
      run_column_name <- paste0(model_snake_case, "_run_", current_run)
      df[i, run_column_name] <- sentiment_value
      
      cat_message(glue("Row {i}: \"{sentence_snippet}\" → {model}, run {current_run}: {ifelse(is.na(sentiment_value), 'NA', sentiment_value)}"))
      
    }, error = function(e) {
      cat_message(glue("Error in row {i}, model {model}, run {current_run}: {as.character(e)}"), "ERROR")
      run_column_name <- paste0(model_snake_case, "_run_", current_run)
      df[i, run_column_name] <- NA
      
      # Store error information
      error_info <- list(
        row = i,
        model = model,
        run = current_run,
        message = as.character(e),
        timestamp = Sys.time()
      )
      progress$errors <- c(progress$errors, list(error_info))
    })
    
    # Update progress
    progress$last_completed_row <- i
    saveRDS(progress, progress_file)
    
    # Save checkpoint every 5 rows
    if (i %% 5 == 0) {
      saveRDS(df, "data/tmp/checkpoint_latest.rds")
    }
    
    # Rate limit delay
    Sys.sleep(2)
  }
  
  # Calculate average after completing a full run
  if (current_run == 3) {
    cat_message(glue("Calculating averages for model {model}"))
    
    # Calculate average for each row
    for (i in 1:nrow(df)) {
      run1_val <- df[[paste0(model_snake_case, "_run_1")]][i]
      run2_val <- df[[paste0(model_snake_case, "_run_2")]][i]
      run3_val <- df[[paste0(model_snake_case, "_run_3")]][i]
      
      # Calculate average if at least one value is not NA
      if (!all(is.na(c(run1_val, run2_val, run3_val)))) {
        average_sentiment <- mean(c(run1_val, run2_val, run3_val), na.rm = TRUE)
        df[i, model_snake_case] <- round(average_sentiment, 2)
      }
    }
    
    # Advance to next model
    current_model_index <- current_model_index + 1
    current_run <- 1
  } else {
    # Advance to next run for the current model
    current_run <- current_run + 1
  }
  
  # Update progress
  progress$current_model_index <- current_model_index
  progress$current_run <- current_run
  progress$last_completed_row <- 0  # Reset for the next run
  saveRDS(progress, progress_file)
  
  # Save checkpoint after each complete run
  saveRDS(df, glue("data/tmp/checkpoint_model_{model}_run_{current_run-1}.rds"))
  saveRDS(df, "data/tmp/checkpoint_latest.rds")
}

# Save final results
saveRDS(df, "data/tmp/data_manual_ranking_with_model_ratings.rds")
cat_message("Analysis complete!")
