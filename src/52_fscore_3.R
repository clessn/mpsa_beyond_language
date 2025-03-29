# Grouped Sentiment F-Score Analysis
# This script calculates F-scores after grouping sentiment categories into positive, negative, and neutral

library(tidyverse)
library(caret)

# Load the data
df <- readRDS("data/clean/df_fscores.rds")

# Function to group sentiment categories
group_sentiment <- function(sentiment) {
  case_when(
    sentiment %in% c("very_positive", "positive", "somewhat_positive") ~ "positive",
    sentiment %in% c("very_negative", "negative", "somewhat_negative") ~ "negative",
    sentiment == "neutral" ~ "neutral",
    TRUE ~ NA_character_
  )
}

# Apply grouping to ground truth and all model predictions
# Start with ground truth
df_grouped <- data.frame(ground_truth = group_sentiment(df$ground_truth))

# Apply grouping to each model
model_columns <- setdiff(names(df), "ground_truth")
for(model in model_columns) {
  df_grouped[[model]] <- group_sentiment(df[[model]])
}

# Print the distribution of grouped ground truth
print(table(df_grouped$ground_truth))

# Define a function to calculate F1 scores for grouped categories
calculate_detailed_f1_scores <- function(predicted, actual) {
  # Convert inputs to factors with the same levels
  sentiment_levels <- c("negative", "neutral", "positive")
  
  predicted <- factor(predicted, levels = sentiment_levels)
  actual <- factor(actual, levels = sentiment_levels)
  
  # Create the confusion matrix
  cm <- table(predicted, actual)
  
  # Initialize vectors for precision, recall, and F1 for each class
  precision <- numeric(length(sentiment_levels))
  recall <- numeric(length(sentiment_levels))
  f1 <- numeric(length(sentiment_levels))
  support <- numeric(length(sentiment_levels))
  names(f1) <- sentiment_levels
  
  # Calculate precision, recall, and F1 for each class
  for(i in 1:length(sentiment_levels)) {
    # True positives
    tp <- cm[i, i]
    
    # False positives (sum of column i minus true positives)
    fp <- sum(cm[, i]) - tp
    
    # False negatives (sum of row i minus true positives)
    fn <- sum(cm[i, ]) - tp
    
    # Calculate precision and recall
    precision[i] <- if(tp + fp > 0) tp / (tp + fp) else 0
    recall[i] <- if(tp + fn > 0) tp / (tp + fn) else 0
    
    # Calculate F1 score
    f1[i] <- if(precision[i] + recall[i] > 0) 2 * precision[i] * recall[i] / (precision[i] + recall[i]) else 0
    
    # Store the number of actual cases for this class
    support[i] <- sum(cm[, i])
  }
  
  # Calculate the weighted average F1 score
  weighted_f1 <- sum(f1 * support) / sum(support)
  
  # Return both individual F1 scores and weighted average
  return(list(
    class_f1 = f1,
    weighted_f1 = weighted_f1
  ))
}

# Initialize results data frames
model_names <- character()
f1_scores <- numeric()

# For detailed class-level F1 scores
detailed_results <- data.frame(
  model = character(),
  negative = numeric(),
  neutral = numeric(),
  positive = numeric(),
  weighted_f1 = numeric(),
  stringsAsFactors = FALSE
)

# Calculate F-scores for each model
for(model in model_columns) {
  cat("\nProcessing model:", model, "\n")
  
  # Check for NA values
  na_count <- sum(is.na(df_grouped[[model]]))
  cat("NA count:", na_count, "out of", nrow(df_grouped), "\n")
  
  # Skip if all values are NA
  if(all(is.na(df_grouped[[model]]))) {
    cat("Skipping model due to all NA values\n")
    next
  }
  
  # Calculate the F1 scores with error handling
  tryCatch({
    scores <- calculate_detailed_f1_scores(df_grouped[[model]], df_grouped$ground_truth)
    cat("Weighted F1 score:", scores$weighted_f1, "\n")
    
    # Store the results for the simple dataframe
    model_names <- c(model_names, model)
    f1_scores <- c(f1_scores, scores$weighted_f1)
    
    # Add to detailed results
    detailed_results <- rbind(detailed_results, 
                            data.frame(
                              model = model,
                              negative = scores$class_f1["negative"],
                              neutral = scores$class_f1["neutral"],
                              positive = scores$class_f1["positive"],
                              weighted_f1 = scores$weighted_f1,
                              stringsAsFactors = FALSE
                            ))
  }, error = function(e) {
    cat("Error processing model:", e$message, "\n")
  })
}

# Create regular results dataframe
results <- data.frame(
  model = model_names,
  weighted_f1 = f1_scores,
  stringsAsFactors = FALSE
)

# Sort both results dataframes by weighted F1 score (descending)
results <- results %>% 
  arrange(desc(weighted_f1))

detailed_results <- detailed_results %>%
  arrange(desc(weighted_f1))

saveRDS(results, "results/fscores/f1_scores_3.rds")
saveRDS(detailed_results, "results/fscores/f1_scores_detailed_3.rds")

 

