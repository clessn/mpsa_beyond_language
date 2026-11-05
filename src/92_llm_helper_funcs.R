# Helpers shared by the scoring scripts (40, 43, 70) and the analyses.

retry_with_backoff <- function(expr, max_attempts = 5, base_delay = 1, max_delay = 30) {
  expr <- substitute(expr)
  env <- parent.frame()
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(eval(expr, env), error = function(e) e)
    if (!inherits(result, "error")) return(result)
    if (attempt == max_attempts) stop("Max retry attempts reached: ", conditionMessage(result))
    delay <- min(base_delay * 2^(attempt - 1) + runif(1), max_delay)
    message(sprintf("Attempt %d failed, retrying in %.1f s", attempt, delay))
    Sys.sleep(delay)
  }
}

#' Read the sentiment score out of a model's answer.
#'
#' An answer is accepted only if it contains exactly one number, or exactly
#' one number right after a label such as "Score:", and that number lies in
#' [-1, 1]. Anything else returns NA and the caller asks again. The minus sign
#' is kept (including the Unicode minus and dashes), a decimal comma is read as
#' a point, and Markdown marks are ignored. Cases are in
#' tests/test_clean_sentiment_value.R.
#'
#' Until November 2026 the parser dropped the minus sign after a space, colon
#' or asterisk ("Sentiment: -0.5" read as 0.5) and read "-0,5" as 0; see
#' src/99x_parser_diagnostic.R.
clean_sentiment_value <- function(response) {
  if (is.null(response) || length(response) != 1 || is.na(response)) return(NA_real_)
  text <- as.character(response)
  text <- gsub("[−–—]", "-", text)
  text <- gsub("[*_`]", "", text)
  text <- gsub("(\\d),(\\d)", "\\1.\\2", text)

  # A sign counts only when it is not glued to a letter or digit ("GPT-5" is 5)
  number <- "(?<![\\w.])[-+]?(?:\\d+(?:\\.\\d+)?|\\.\\d+)"
  numbers <- regmatches(text, gregexpr(number, text, perl = TRUE))[[1]]

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

  value <- suppressWarnings(as.numeric(candidate))
  if (is.na(value) || value < -1 || value > 1) return(NA_real_)
  value
}

# Appends one raw answer and the value read from it to a JSON-lines archive.
# Never stops a scoring run.
log_raw_response <- function(path, model_prefix, condition, item, run, attempt,
                             doc_id, response, parsed) {
  tryCatch({
    model_id <- if (exists("model_mapping") && model_prefix %in% names(model_mapping))
      unname(model_mapping[model_prefix]) else NA_character_
    record <- list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                   model_prefix = model_prefix, model_id = model_id,
                   condition = condition, item = item, run = run, attempt = attempt,
                   doc_id = as.character(doc_id), response = as.character(response),
                   parsed = parsed)
    cat(jsonlite::toJSON(record, auto_unbox = TRUE, na = "null"), "\n",
        file = path, sep = "", append = TRUE)
  }, error = function(e) NULL)
  invisible(NULL)
}

# Weighted F1 on three classes (negative, exactly zero, positive), each class
# weighted by its share of the ground truth.
polarity_f1 <- function(pred, truth) {
  classes <- c("negative", "neutral", "positive")
  polarity <- function(x) factor(ifelse(x < 0, "negative", ifelse(x == 0, "neutral", "positive")),
                                 levels = classes)
  cm <- table(polarity(pred), polarity(truth))
  f1 <- sapply(seq_along(classes), function(i) {
    precision <- cm[i, i] / max(sum(cm[i, ]), 1)
    recall <- cm[i, i] / max(sum(cm[, i]), 1)
    if (precision + recall == 0) 0 else 2 * precision * recall / (precision + recall)
  })
  sum(f1 * colSums(cm)) / sum(cm)
}
