#################################################################
# CORRELATION ANALYSIS OF SENTIMENT MODELS
#################################################################
# This script analyzes the correlation between different sentiment analysis 
# models (LLMs and dictionary-based) and ground truth human ratings.
# It calculates Pearson correlations, MAE, and RMSE for each model.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization

#################################################################
# LOAD DATASET
#################################################################
# Read the cleaned dataset with all sentiment scores
df <- readRDS("data/clean/df.rds")

#################################################################
# SELECT MODEL COLUMNS
#################################################################
# Extract only the model output columns (excluding categorical versions)
# We include French-French, English-French, and English-English variations
# as well as dictionary-based scores (lsd_*)
model_columns <- names(df)[
  (grepl("_fr_fr$", names(df)) | 
   grepl("_en_fr$", names(df)) | 
   grepl("_en_en$", names(df)) | 
   grepl("^lsd_fr", names(df)) | 
   grepl("^lsd_en", names(df))) & 
  !grepl("_bin$", names(df)) & 
  names(df) != "ground_truth"
]

# Print the selected model columns for verification
cat("Selected model columns:", length(model_columns), "\n")
print(head(model_columns))

#################################################################
# INITIALIZE RESULTS DATAFRAME
#################################################################
# Create dataframe to store correlation and error metrics
results <- data.frame(
  model = character(),
  correlation = numeric(),
  p_value = numeric(),
  mae = numeric(),
  rmse = numeric(),
  n_obs = numeric(),
  data_pct = numeric(),
  stringsAsFactors = FALSE
)

#################################################################
# CALCULATE CORRELATION AND ERROR METRICS
#################################################################
# For each model, calculate correlation with ground truth and error metrics
for (model in model_columns) {
  # Create a temporary dataframe for this model, removing NA values
  temp_df <- df[, c("ground_truth", model)]
  temp_df <- temp_df[complete.cases(temp_df), ]
  
  # Calculate what percentage of data is available
  data_pct <- nrow(temp_df) / nrow(df) * 100
  
  # Only proceed if we have enough data points and both columns are numeric
  if (nrow(temp_df) >= 5 && 
      is.numeric(temp_df$ground_truth) && 
      is.numeric(temp_df[[model]])) {
    
    # Run correlation test with error handling
    tryCatch({
      # Calculate Pearson correlation
      cor_test <- cor.test(temp_df$ground_truth, temp_df[[model]], method = "pearson")
      
      # Calculate Mean Absolute Error (MAE)
      mae_value <- mean(abs(temp_df$ground_truth - temp_df[[model]]))
      
      # Calculate Root Mean Squared Error (RMSE)
      rmse_value <- sqrt(mean((temp_df$ground_truth - temp_df[[model]])^2))
      
      # Add row to results
      results <- rbind(results, data.frame(
        model = model,
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        mae = mae_value,
        rmse = rmse_value,
        n_obs = nrow(temp_df),
        data_pct = data_pct,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      cat("Error in analysis for model", model, ":", e$message, "\n")
    })
  }
}

#################################################################
# FORMAT RESULTS FOR VISUALIZATION
#################################################################
# Enhance results with additional information for plotting and analysis
plot_results <- results %>%
  mutate(
    # Add significance level indicators
    sig_level = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01 ~ "p < 0.01",
      p_value < 0.05 ~ "p < 0.05",
      TRUE ~ "Not significant"
    ),
    
    # Create descriptive labels with model type for better readability
    term_with_n = case_when(
      grepl("_en_fr$", model) ~ paste0(gsub("_en_fr$", " (EN→FR)", model)),
      grepl("_fr_fr$", model) ~ paste0(gsub("_fr_fr$", " (FR→FR)", model)),
      grepl("_en_en$", model) ~ paste0(gsub("_en_en$", " (EN→EN)", model)),
      grepl("^lsd_", model) ~ paste0(gsub("^lsd_", "Dictionary (", model), ")"),
      TRUE ~ model
    ),
    
    # Flag models with small sample sizes (less than 70% of data)
    is_small_sample = data_pct < 70,
    
    # Calculate absolute correlation for sorting
    abs_correlation = abs(correlation)
  )

# Sort by absolute correlation (strongest first)
plot_results <- plot_results %>%
  arrange(desc(abs_correlation))

#################################################################
# SAVE RESULTS
#################################################################
# Save the complete results for further analysis and visualization
saveRDS(plot_results, "data/clean/cor_results.rds")