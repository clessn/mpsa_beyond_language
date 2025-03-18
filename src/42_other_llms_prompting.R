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
df_path <- "data/tmp/data_groq_done.rds"
if (!file.exists(df_path)) {
  stop("Data file not found")
}
df <- readRDS(df_path)

# Setup - all models to process
# New models to add to the existing data
new_models <- list(
  list(type = "claude", name = "claude-3-5-sonnet-20241022", func = "chat_claude"),
  list(type = "gemini", name = "gemini-2.0-flash", func = "chat_gemini"),
  list(type = "deepseek", name = "deepseek-chat", func = "chat_deepseek"),
  list(type = "openai", name = "gpt-4o", func = "chat_openai")
)

system_prompt <- "You are a helpful assistant that analyzes the sentiment of text. You provide accurate and consistent sentiment ratings according to the specified scale."

# Create columns for storing results for each new model
for (model_info in new_models) {
  model_type <- model_info$type
  model_name <- model_info$name
  
  # Create snake_case column name (removing version numbers and special chars)
  model_snake_case <- model_type
  if (model_type == "claude") model_snake_case <- "claude_3_5_sonnet"
  if (model_type == "gemini") model_snake_case <- "gemini_2_flash"
  if (model_type == "openai") model_snake_case <- "gpt_4o"
  if (model_type == "deepseek") model_snake_case <- "deepseek_chat"
  
  # Create columns
  df[[model_snake_case]] <- NA
  df[[paste0(model_snake_case, "_run_1")]] <- NA
  df[[paste0(model_snake_case, "_run_2")]] <- NA
  df[[paste0(model_snake_case, "_run_3")]] <- NA
  
  cat_message(glue("Created columns for {model_name} as {model_snake_case}"))
}

# Save initial state
saveRDS(df, "data/tmp/checkpoint_multi_model_initial.rds")

# Check for existing progress
progress_file <- "data/tmp/multi_model_progress.rds"
if (file.exists(progress_file)) {
  progress <- readRDS(progress_file)
  # Ensure required variables exist in the progress file
  if (!is.null(progress$current_model_index) && !is.null(progress$current_run)) {
    current_model_index <- progress$current_model_index
    current_run <- progress$current_run
    start_row <- progress$last_completed_row + 1
    cat_message(glue("Resuming with model {new_models[[current_model_index]]$name}, run {current_run}, from row {start_row}"))
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

# Parse sentiment function - only accepts values in the -1 to 1 range
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
  
  # If no valid value was found, look for any other number between -1 and 1
  if (grepl("-?[0-9]+(\\.[0-9]+)?", clean_response)) {
    match <- regexpr("-?[0-9]+(\\.[0-9]+)?", clean_response)
    value_str <- regmatches(clean_response, match)[[1]]
    value <- as.numeric(value_str)
    
    # Check if the value is in range
    if (value >= -1.0 && value <= 1.0) {
      return(round(value, 2))
    }
  }
  
  # If no valid number in range was found
  return(NA)
}

# Create a client based on model type
create_client <- function(model_info) {
  tryCatch({
    model_func <- model_info$func
    model_name <- model_info$name
    
    # Dynamically call the appropriate chat function
    if (model_func == "chat_claude") {
      client <- chat_claude(model = model_name)
    } else if (model_func == "chat_gemini") {
      client <- chat_gemini(model = model_name)
    } else if (model_func == "chat_deepseek") {
      client <- chat_deepseek(model = model_name)
    } else if (model_func == "chat_openai") {
      client <- chat_openai(model = model_name)
    } else {
      stop(glue("Unknown model function: {model_func}"))
    }
    
    return(client)
  }, error = function(e) {
    cat_message(glue("Error creating client for {model_info$name}: {as.character(e)}"), "ERROR")
    return(NULL)
  })
}

# Ensure variables are defined
if (!exists("current_model_index")) current_model_index <- 1
if (!exists("current_run")) current_run <- 1
if (!exists("start_row")) start_row <- 1

# Main loop - process one model at a time through the entire dataset
while (current_model_index <= length(new_models)) {
  model_info <- new_models[[current_model_index]]
  model_type <- model_info$type
  model_name <- model_info$name
  
  # Create snake_case column name
  model_snake_case <- model_type
  if (model_type == "claude") model_snake_case <- "claude_3_5_sonnet"
  if (model_type == "gemini") model_snake_case <- "gemini_2_flash"
  if (model_type == "openai") model_snake_case <- "gpt_4o"
  if (model_type == "deepseek") model_snake_case <- "deepseek_chat"
  
  cat_message(glue("Processing model: {model_name}, run: {current_run}"))
  
  # Create client for this model
  client <- create_client(model_info)
  if (is.null(client)) {
    cat_message(glue("Skipping model {model_name} due to client creation failure"), "ERROR")
    current_model_index <- current_model_index + 1
    next
  }
  
  # Test the client to make sure it's working
  test_response <- NULL
  tryCatch({
    test_response <- client$chat("Test connection")
    cat_message(glue("Connection test successful for {model_name}"))
  }, error = function(e) {
    cat_message(glue("Connection test failed for {model_name}: {as.character(e)}"), "ERROR")
  })
  
  if (is.null(test_response)) {
    cat_message(glue("Skipping model {model_name} due to failed connection test"), "ERROR")
    current_model_index <- current_model_index + 1
    next
  }
  
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
      # Get response with retry logic for invalid responses
      max_attempts <- 10
      attempt <- 1
      sentiment_value <- NA
      
      while (is.na(sentiment_value) && attempt <= max_attempts) {
        # Use the same prompt for all attempts
        response <- client$chat(prompt)
        
        if (is.null(response) || length(response) == 0) {
          cat_message(glue("Attempt {attempt}/{max_attempts}: Empty response"), "ERROR")
          attempt <- attempt + 1
          Sys.sleep(1)  # Short delay before retry
          next
        }
        
        # Parse the response
        cat_message(glue("Attempt {attempt}/{max_attempts}: Parsing response: '{response}'"))
        sentiment_value <- parse_sentiment(response)
        
        if (is.na(sentiment_value)) {
          cat_message(glue("Attempt {attempt}/{max_attempts}: Invalid value, not in [-1,1] range"), "WARNING")
          attempt <- attempt + 1
          Sys.sleep(1)  # Short delay before retry
        }
      }
      
      # Log and store result
      sentence_snippet <- substr(df$sentences[i], 1, min(40, nchar(df$sentences[i])))
      if (nchar(df$sentences[i]) > 40) {
        sentence_snippet <- paste0(sentence_snippet, "...")
      }
      
      # Store the result in the dataframe
      run_column_name <- paste0(model_snake_case, "_run_", current_run)
      df[i, run_column_name] <- sentiment_value
      
      # Log the outcome
      if (is.na(sentiment_value)) {
        cat_message(glue("Row {i}: \"{sentence_snippet}\" → {model_name}, run {current_run}: Failed after {max_attempts} attempts"), "WARNING")
      } else {
        attempt_msg <- paste0("Row ", i, ": \"", sentence_snippet, "\" → ", model_name, ", run ", current_run, ": ", sentiment_value, " (attempt ", attempt, "/", max_attempts, ")")
        cat_message(attempt_msg)
      }
      
    }, error = function(e) {
      cat_message(glue("Error in row {i}, model {model_name}, run {current_run}: {as.character(e)}"), "ERROR")
      run_column_name <- paste0(model_snake_case, "_run_", current_run)
      df[i, run_column_name] <- NA
      
      # Store error information
      error_info <- list(
        row = i,
        model = model_name,
        run = current_run,
        message = as.character(e),
        timestamp = Sys.time()
      )
      progress$errors <- c(progress$errors, list(error_info))
      
      # Recreate client in case of error
      client <- create_client(model_info)
    })
    
    # Update progress
    progress$last_completed_row <- i
    saveRDS(progress, progress_file)
    
    # Save checkpoint every 5 rows
    if (i %% 5 == 0) {
      saveRDS(df, "data/tmp/checkpoint_multi_model_latest.rds")
    }
    
    # Rate limit delay - different for each model type to respect their rate limits
    delay <- 2  # Default delay
    if (model_type == "claude") delay <- 3
    if (model_type == "openai") delay <- 1
    Sys.sleep(delay)
  }
  
  # Calculate average after completing a full run
  if (current_run == 3) {
    cat_message(glue("Calculating averages for model {model_name}"))
    
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
  saveRDS(df, glue("data/tmp/checkpoint_model_{model_snake_case}_run_{current_run-1}.rds"))
  saveRDS(df, "data/tmp/checkpoint_multi_model_latest.rds")
}

# Save final results
saveRDS(df, "data/tmp/data_all_models_sentiment.rds")
cat_message("Analysis complete!")
