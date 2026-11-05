###############################################################################
# LLM HELPER FUNCTIONS
# 
# This script contains utility functions for working with language models,
# processing their responses, and analyzing sentiment data. It provides
# error handling, response parsing, and statistical analysis functions.
#
# Author: Ral Zarek
# Date: March 2025
###############################################################################

#==============================================================================
# 1. MODEL CONNECTION NOTES
#==============================================================================
# USING FIREWORKS.AI HOSTED MODELS
# ================================
# To use models from fireworks.ai:
#
# 1. Get an API key from fireworks.ai and set it as an environment variable:
#    Sys.setenv(FIREWORKS_API_KEY="your_api_key_here")
#
# 2. Initialize a model client with ellmer::chat_openai() using the structure:
#    fireworks <- ellmer::chat_openai(
#      system_prompt = "You are a helpful assistant",
#      base_url = "https://api.fireworks.ai/inference/v1",
#      api_key = Sys.getenv("FIREWORKS_API_KEY"),
#      model = "accounts/fireworks/models/model-name"
#    )
#
# 3. Available models include:
#    - accounts/fireworks/models/qwen3-235b-a22b
#    - accounts/fireworks/models/deepseek-v3p2
#    - accounts/fireworks/models/deepseek-v4-flash
#
# 4. Use the model client with the chat() function:
#    response <- fireworks$chat("Your prompt here")

#==============================================================================
# 2. API ERROR HANDLING
#==============================================================================

#' Retry function with exponential backoff
#'
#' @param expr Expression to evaluate
#' @param max_attempts Maximum number of retry attempts
#' @param base_delay Base delay in seconds
#' @param max_delay Maximum delay in seconds
#' @return Result of the expression
retry_with_backoff <- function(expr, max_attempts = 5, base_delay = 1, max_delay = 30) {
  attempt <- 1
  while (attempt <= max_attempts) {
    tryCatch({
      result <- eval(expr)
      return(result)
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("Max retry attempts reached: ", e$message)
      }
      delay <- min(base_delay * 2^(attempt - 1) + runif(1, 0, 1), max_delay)
      cat(sprintf("Attempt %d failed. Retrying in %.2f seconds...\n", attempt, delay))
      Sys.sleep(delay)
      attempt <<- attempt + 1
    })
  }
}

#==============================================================================
# 3. RESPONSE PROCESSING
#==============================================================================

#' Read the sentiment score out of an LLM's text answer
#'
#' The rule, in one sentence: an answer is accepted only if it contains exactly
#' one number, or exactly one number right after a label such as "Score:";
#' that number must lie in [-1, 1]. Anything else (no number, several
#' unlabelled numbers, out of range) returns NA, and the caller asks the
#' model again.
#'
#' Before looking for numbers, the text is normalized so that common
#' formatting does not change the value:
#'   - the minus sign is kept, including the Unicode minus (U+2212) and dashes
#'     used as minus signs (U+2013, U+2014);
#'   - a French decimal comma ("-0,5") is read as a decimal point;
#'   - Markdown emphasis and code marks (*, _, `) are removed.
#'
#' This replaces the parser used until November 2026, which dropped the minus
#' sign whenever a number was preceded by a space, colon or asterisk
#' ("Sentiment: -0.5" was read as 0.5) and read "-0,5" as 0. See
#' tests/test_clean_sentiment_value.R for the cases checked.
#'
#' @param response Text response from LLM
#' @return Numeric score in [-1, 1], or NA if the answer is not usable
clean_sentiment_value <- function(response) {
  if (is.null(response) || length(response) != 1 || is.na(response)) return(NA_real_)
  text <- as.character(response)

  # 1. Normalize: minus signs, Markdown marks, decimal commas
  text <- gsub("[−–—]", "-", text)
  text <- gsub("[*_`]", "", text)
  text <- gsub("(\\d),(\\d)", "\\1.\\2", text)

  # 2. Every number in the answer, with its sign. A sign counts only when it
  #    is not glued to a preceding letter or digit (so "GPT-5" gives 5, not -5).
  number <- "(?<![\\w.])[-+]?(?:\\d+(?:\\.\\d+)?|\\.\\d+)"
  numbers <- regmatches(text, gregexpr(number, text, perl = TRUE))[[1]]

  # 3. One number: take it. Several: accept only a single labelled one.
  if (length(numbers) == 1) {
    candidate <- numbers
  } else if (length(numbers) > 1) {
    label <- paste0("(?i)(?:sentiment(?:\\s+score)?|score|rating|value|note|",
                    "valeur|[ée]valuation|r[ée]ponse)\\s*[:=]\\s*(", number, ")")
    labelled <- regmatches(text, gregexpr(label, text, perl = TRUE))[[1]]
    if (length(labelled) != 1) return(NA_real_)
    candidate <- regmatches(labelled, regexpr(number, labelled, perl = TRUE))
  } else {
    return(NA_real_)
  }

  # 4. Range check
  value <- suppressWarnings(as.numeric(candidate))
  if (is.na(value) || value < -1 || value > 1) return(NA_real_)
  value
}

#' Append one model answer, as returned, to a JSON-lines archive
#'
#' Every API call is archived with the raw text and the value the parser read
#' from it, so that parsing can be audited and redone without new API calls.
#' Fails soft: an archiving error never stops a scoring run.
#'
#' @param path File to append to (one JSON object per line)
#' @param model_prefix,condition,item,run,attempt Identify the call
#' @param doc_id Identifier of the sentence or article scored
#' @param response Raw text returned by the model
#' @param parsed Value returned by clean_sentiment_value() (NA if rejected)
log_raw_response <- function(path, model_prefix, condition, item, run, attempt,
                             doc_id, response, parsed) {
  tryCatch({
    record <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      model_prefix = model_prefix,
      model_id = if (exists("model_mapping") && model_prefix %in% names(model_mapping))
        unname(model_mapping[model_prefix]) else NA_character_,
      condition = condition, item = item, run = run, attempt = attempt,
      doc_id = as.character(doc_id),
      response = as.character(response),
      parsed = parsed
    )
    line <- jsonlite::toJSON(record, auto_unbox = TRUE, na = "null")
    cat(line, "\n", file = path, sep = "", append = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#==============================================================================
# 4. STATISTICS AND VISUALIZATION
#==============================================================================

#' Generate summary statistics for the sentiment analysis results
#'
#' @param df Dataframe with sentiment analysis results
#' @return Dataframe with summary statistics
generate_summary_stats <- function(df) {
  # Extract model columns
  model_columns <- grep("_en_|_fr_", names(df), value = TRUE)
  
  # Create summary statistics
  summary_stats <- data.frame(
    model = model_columns,
    mean = sapply(df[model_columns], function(x) mean(x, na.rm = TRUE)),
    median = sapply(df[model_columns], function(x) median(x, na.rm = TRUE)),
    sd = sapply(df[model_columns], function(x) sd(x, na.rm = TRUE)),
    min = sapply(df[model_columns], function(x) min(x, na.rm = TRUE)),
    max = sapply(df[model_columns], function(x) max(x, na.rm = TRUE)),
    na_count = sapply(df[model_columns], function(x) sum(is.na(x))),
    na_percent = sapply(df[model_columns], function(x) sum(is.na(x)) / length(x) * 100)
  )
  
  # Extract model name, prompt language, and text language
  summary_stats <- summary_stats %>%
    mutate(
      model_name = sub("_[^_]+_[^_]+$", "", model),
      prompt_lang = sub("^.*_([^_]+)_.*$", "\\1", model),
      text_lang = sub("^.*_.*_([^_]+)$", "\\1", model)
    )
  
  return(summary_stats)
}

#' Create a heatmap of correlations between models
#'
#' @param df Dataframe with sentiment analysis results
#' @param output_file Path to save the heatmap
create_correlation_heatmap <- function(df, output_file = "data/tmp/correlation_heatmap.png") {
  library(ggplot2)
  library(reshape2)
  
  # Extract model columns
  model_columns <- grep("_en_|_fr_", names(df), value = TRUE)
  
  # Create correlation matrix
  cor_matrix <- cor(df[model_columns], use = "pairwise.complete.obs")
  
  # Create a dataframe for ggplot
  cor_df <- melt(cor_matrix)
  names(cor_df) <- c("Model1", "Model2", "Correlation")
  
  # Create the heatmap
  p <- ggplot(cor_df, aes(x = Model1, y = Model2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = "Correlation Matrix of Sentiment Scores")
  
  # Save the plot
  ggsave(output_file, p, width = 12, height = 10)
  
  cat("Correlation heatmap saved to", output_file, "\n")
}

#==============================================================================
# 5. BOOTSTRAP ANALYSIS
#==============================================================================

#' Estimate model performance using bootstrap resampling
#'
#' @param df Dataframe with sentiment analysis results
#' @param n_bootstraps Number of bootstrap samples
#' @return Dataframe with bootstrap statistics
estimate_model_performance <- function(df, n_bootstraps = 1000) {
  library(boot)
  
  # Extract model columns
  model_columns <- grep("_en_|_fr_", names(df), value = TRUE)
  
  # Define bootstrap function
  bootstrap_mean <- function(data, indices) {
    return(mean(data[indices], na.rm = TRUE))
  }
  
  # Run bootstrap for each model
  results <- list()
  for (col in model_columns) {
    if (sum(!is.na(df[[col]])) > 10) {  # Only run if we have enough data
      boot_result <- boot(df[[col]], bootstrap_mean, R = n_bootstraps)
      results[[col]] <- data.frame(
        model = col,
        mean = boot_result$t0,
        se = sd(boot_result$t, na.rm = TRUE),
        ci_lower = quantile(boot_result$t, 0.025, na.rm = TRUE),
        ci_upper = quantile(boot_result$t, 0.975, na.rm = TRUE)
      )
    }
  }
  
  # Combine results
  bootstrap_stats <- do.call(rbind, results)
  
  # Extract model name, prompt language, and text language
  bootstrap_stats <- bootstrap_stats %>%
    mutate(
      model_name = sub("_[^_]+_[^_]+$", "", model),
      prompt_lang = sub("^.*_([^_]+)_.*$", "\\1", model),
      text_lang = sub("^.*_.*_([^_]+)$", "\\1", model)
    )
  
  return(bootstrap_stats)
}

#==============================================================================
# 6. LANGUAGE PERFORMANCE ANALYSIS
#==============================================================================

#' Analyze model performance by language
#'
#' @param df Dataframe with sentiment analysis results
#' @return Dataframe with language performance statistics
analyze_language_performance <- function(df) {
  # Extract model columns
  model_columns <- grep("_en_|_fr_", names(df), value = TRUE)
  
  # Create a long format dataframe
  df_long <- df %>%
    select(all_of(model_columns)) %>%
    pivot_longer(cols = everything(), 
              names_to = "model", 
              values_to = "sentiment")
  
  # Extract model name, prompt language, and text language
  df_long <- df_long %>%
    mutate(
      model_name = sub("_[^_]+_[^_]+$", "", model),
      prompt_lang = sub("^.*_([^_]+)_.*$", "\\1", model),
      text_lang = sub("^.*_.*_([^_]+)$", "\\1", model)
    )
  
  # Calculate statistics by language combinations
  language_stats <- df_long %>%
    group_by(model_name, prompt_lang, text_lang) %>%
    summarize(
      mean = mean(sentiment, na.rm = TRUE),
      median = median(sentiment, na.rm = TRUE),
      sd = sd(sentiment, na.rm = TRUE),
      n = sum(!is.na(sentiment)),
      na_count = sum(is.na(sentiment)),
      .groups = "drop"
    )
  
  return(language_stats)
}