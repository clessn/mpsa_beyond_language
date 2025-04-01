#################################################################
# 7-CATEGORY SENTIMENT ANALYSIS F-SCORE CALCULATION
#################################################################
# This script calculates weighted F1 scores for sentiment analysis models
# using the original 7-category sentiment scale (very negative to very positive).
# It computes both overall weighted F1 scores and detailed per-class scores.

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(caret)      # For confusion matrix utilities

#################################################################
# LOAD DATASET
#################################################################
# Load the prepared dataset with categorical sentiment values
df <- readRDS("data/clean/df_fscores.rds")

#################################################################
# F1 SCORE CALCULATION FUNCTION
#################################################################
# Define a function to manually calculate F1 scores for all sentiment classes
calculate_detailed_f1_scores <- function(predicted, actual) {
  # Define the ordered sentiment levels
  sentiment_levels <- c("very_negative", "negative", "somewhat_negative", 
                       "neutral", "somewhat_positive", "positive", "very_positive")
  
  # Convert inputs to factors with the same levels
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
  
  # Calculate precision, recall, and F1 for each sentiment class
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

#################################################################
# INITIALIZE RESULTS DATAFRAMES
#################################################################
# Prepare dataframes to store results
model_names <- character()
f1_scores <- numeric()

# For detailed class-level F1 scores
detailed_results <- data.frame(
  model = character(),
  very_negative = numeric(),
  negative = numeric(),
  somewhat_negative = numeric(),
  neutral = numeric(),
  somewhat_positive = numeric(),
  positive = numeric(),
  very_positive = numeric(),
  weighted_f1 = numeric(),
  stringsAsFactors = FALSE
)

#################################################################
# CALCULATE F-SCORES FOR EACH MODEL
#################################################################
# Get a list of model columns from the dataset
model_columns <- setdiff(names(df), "ground_truth")

# Process each model
for(model in model_columns) {
  cat("\nProcessing model:", model, "\n")
  
  # Check for NA values
  na_count <- sum(is.na(df[[model]]))
  cat("NA count:", na_count, "out of", nrow(df), "\n")
  
  # Skip if all values are NA
  if(all(is.na(df[[model]]))) {
    cat("Skipping model due to all NA values\n")
    next
  }
  
  # Calculate the F1 scores with error handling
  tryCatch({
    scores <- calculate_detailed_f1_scores(df[[model]], df$ground_truth)
    cat("Weighted F1 score:", scores$weighted_f1, "\n")
    
    # Store the results for the simple dataframe
    model_names <- c(model_names, model)
    f1_scores <- c(f1_scores, scores$weighted_f1)
    
    # Add to detailed results
    detailed_results <- rbind(detailed_results, 
                            data.frame(
                              model = model,
                              very_negative = scores$class_f1["very_negative"],
                              negative = scores$class_f1["negative"],
                              somewhat_negative = scores$class_f1["somewhat_negative"],
                              neutral = scores$class_f1["neutral"],
                              somewhat_positive = scores$class_f1["somewhat_positive"],
                              positive = scores$class_f1["positive"],
                              very_positive = scores$class_f1["very_positive"],
                              weighted_f1 = scores$weighted_f1,
                              stringsAsFactors = FALSE
                            ))
  }, error = function(e) {
    cat("Error processing model:", e$message, "\n")
  })
}

#################################################################
# CREATE AND SORT RESULTS DATAFRAMES
#################################################################
# Create a simple results dataframe with model names and weighted F1 scores
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

#################################################################
# SAVE RESULTS
#################################################################
# Save the F1 score results for further analysis and visualization
saveRDS(results, "results/analysis/f1_scores_7.rds")
saveRDS(detailed_results, "results/analysis/f1_scores_detailed_7.rds")