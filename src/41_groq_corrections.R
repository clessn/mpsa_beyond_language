library(dplyr)
library(ellmer)
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

# Load the dataset with the NA values
result_path <- "data/tmp/data_manual_ranking_with_model_ratings.rds"
if (!file.exists(result_path)) {
  stop("Result file not found. Please make sure the path is correct.")
}
df <- readRDS(result_path)

# Check the number of NA values in the first 15 rows
models <- c("gemma2-9b-it", "llama-3.3-70b-versatile", "mixtral-8x7b-32768", "deepseek-r1-distill-llama-70b")
model_cols <- sapply(models, function(model) gsub("-", "_", model))
run_cols <- c()
for (model in model_cols) {
  run_cols <- c(run_cols, paste0(model, "_run_1"), paste0(model, "_run_2"), paste0(model, "_run_3"))
}
all_cols <- c(model_cols, run_cols)

# Improved parser for consistent, reliable sentiment extraction
parse_sentiment <- function(response) {
  # Clean up the response - remove all whitespace
  clean_response <- gsub("\\s", "", response)
  
  # First look for a decimal number between -1 and 1
  decimal_pattern <- "-0\\.[0-9]+|0\\.[0-9]+|-1\\.0|1\\.0"
  if (grepl(decimal_pattern, clean_response)) {
    match <- regexpr(decimal_pattern, clean_response)
    value_str <- regmatches(clean_response, match)[[1]]
    return(round(as.numeric(value_str), 2))
  }
  
  # Then look for whole numbers -1, 0, 1
  whole_pattern <- "(?<![0-9])(-1|0|1)(?![0-9])"
  if (grepl(whole_pattern, clean_response, perl = TRUE)) {
    match <- regexpr(whole_pattern, clean_response, perl = TRUE)
    value_str <- regmatches(clean_response, match)[[1]]
    return(as.numeric(value_str))
  }
  
  # If still no match, look for any number and normalize
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

# Function to retry a specific NA cell
retry_na_cell <- function(row_idx, model_name, run_number = NULL) {
  model_snake <- gsub("-", "_", model_name)
  
  # Skip if the sentence is empty
  if (is.na(df$sentences[row_idx]) || df$sentences[row_idx] == "") {
    cat_message(glue("Row {row_idx}: Empty sentence, skipping"), "WARNING")
    return(FALSE)
  }
  
  # Create the prompt
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
  Here is the text to analyze: ", df$sentences[row_idx])
  
  # Create the groq client
  system_prompt <- "You are a helpful assistant that analyzes the sentiment of text. You provide accurate and consistent sentiment ratings according to the specified scale."
  
  tryCatch({
    groq <- chat_groq(
      system_prompt = system_prompt,
      model = model_name
    )
    
    if (is.null(groq)) {
      cat_message(glue("Failed to initialize groq client for {model_name}"), "ERROR")
      return(FALSE)
    }
    
    # Get the response
    response <- groq$chat(prompt)
    
    # Check if response is valid
    if (is.null(response) || length(response) == 0) {
      cat_message(glue("Empty response from {model_name}"), "ERROR")
      return(FALSE)
    }
    
    # Parse the response
    cat_message(glue("Response: '{response}'"))
    sentiment_value <- parse_sentiment(response)
    
    if (is.na(sentiment_value)) {
      cat_message(glue("Failed to parse value from '{response}'"), "WARNING")
      return(FALSE)
    }
    
    # Log the result
    sentence_snippet <- substr(df$sentences[row_idx], 1, min(40, nchar(df$sentences[row_idx])))
    if (nchar(df$sentences[row_idx]) > 40) sentence_snippet <- paste0(sentence_snippet, "...")
    cat_message(glue("Row {row_idx}: \"{sentence_snippet}\" → {model_name}: {sentiment_value}"))
    
    # If run_number is provided, update that specific run
    if (!is.null(run_number)) {
      run_col <- paste0(model_snake, "_run_", run_number)
      df[row_idx, run_col] <<- sentiment_value
      cat_message(glue("Updated {run_col} for row {row_idx}"))
    } 
    
    # Always update the average column with the new values
    # Recalculate the average from all available runs
    run1_val <- df[[paste0(model_snake, "_run_1")]][row_idx]
    run2_val <- df[[paste0(model_snake, "_run_2")]][row_idx]
    run3_val <- df[[paste0(model_snake, "_run_3")]][row_idx]
    
    # Calculate average if at least one value is not NA
    if (!all(is.na(c(run1_val, run2_val, run3_val)))) {
      average_sentiment <- mean(c(run1_val, run2_val, run3_val), na.rm = TRUE)
      df[row_idx, model_snake] <<- round(average_sentiment, 2)
      cat_message(glue("Updated average for {model_snake}: {round(average_sentiment, 2)}"))
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat_message(glue("Error with {model_name} for row {row_idx}: {as.character(e)}"), "ERROR")
    return(FALSE)
  })
}

# Initialize counter for rate limiting
request_count <- 0

# Process the first 15 rows
for (row_idx in 1:15) {
  cat_message(glue("=== Processing row {row_idx} ==="))
  
  if (is.na(df$sentences[row_idx]) || df$sentences[row_idx] == "") {
    cat_message(glue("Row {row_idx}: Empty sentence, skipping"), "WARNING")
    next
  }
  
  for (model_idx in 1:length(models)) {
    model <- models[model_idx]
    model_snake <- model_cols[model_idx]
    
    # Check if the average is NA
    if (is.na(df[row_idx, model_snake])) {
      cat_message(glue("Row {row_idx}: Missing average for {model}"))
      
      # Check which runs are NA
      for (run in 1:3) {
        run_col <- paste0(model_snake, "_run_", run)
        
        if (is.na(df[row_idx, run_col])) {
          cat_message(glue("Retrying {run_col} for row {row_idx}"))
          
          # Rate limiting - count requests and sleep as needed
          request_count <- request_count + 1
          if (request_count %% 10 == 0) {
            cat_message("Pause for rate limiting...")
            Sys.sleep(5)  # Longer pause every 10 requests
          }
          
          success <- retry_na_cell(row_idx, model, run)
          
          # After each API call, sleep to respect rate limits
          Sys.sleep(2)
          
          # Save progress frequently
          if (request_count %% 5 == 0) {
            saveRDS(df, result_path)
            cat_message("Progress saved")
          }
        }
      }
    }
  }
  
  # Save after each row
  saveRDS(df, result_path)
  cat_message(glue("Completed row {row_idx}"))
}

# Final save
saveRDS(df, result_path)
cat_message("Correction complete!")

# Print summary of NA values after correction
na_counts <- sapply(model_cols, function(col) sum(is.na(df[1:15, col])))
cat_message("NA counts in first 15 rows after correction:")
for (i in 1:length(model_cols)) {
  cat_message(glue("{models[i]}: {na_counts[i]} NAs"))
}

saveRDS(df, "data/tmp/data_groq_done.rds")
