#################################################################
# LIN'S CONCORDANCE CORRELATION COEFFICIENT ANALYSIS
#################################################################
# This script calculates Lin's Concordance Correlation Coefficient (CCC)
# for different sentiment analysis models against ground truth human ratings.
# CCC measures both correlation and agreement, providing a more comprehensive
# assessment of model performance than Pearson correlation alone.

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization

# Function to calculate Lin's Concordance Correlation Coefficient
lin_ccc <- function(x, y) {
  # Remove NA values
  complete_data <- complete.cases(x, y)
  x <- x[complete_data]
  y <- y[complete_data]
  
  if(length(x) < 2) return(list(ccc = NA, lower = NA, upper = NA))
  
  # Calculate means and variances
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  # Calculate covariance
  cov_xy <- cov(x, y)
  
  # Calculate Pearson correlation
  r <- cor(x, y)
  
  # Calculate Lin's CCC
  ccc <- (2 * cov_xy) / (var_x + var_y + (mean_x - mean_y)^2)
  
  # Calculate confidence interval using Fisher's z-transform approach
  n <- length(x)
  z_ccc <- 0.5 * log((1 + ccc) / (1 - ccc))
  se_z <- sqrt(1 / (n - 3))
  z_lower <- z_ccc - 1.96 * se_z
  z_upper <- z_ccc + 1.96 * se_z
  
  ccc_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  ccc_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  
  return(list(
    ccc = ccc,
    lower = ccc_lower,
    upper = ccc_upper,
    pearson = r
  ))
}

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
# Create dataframe to store CCC and related metrics
results <- data.frame(
  model = character(),
  ccc = numeric(),
  ccc_lower = numeric(),
  ccc_upper = numeric(),
  pearson_r = numeric(),
  bias_correction = numeric(),
  scale_shift = numeric(),
  location_shift = numeric(),
  n_obs = numeric(),
  data_pct = numeric(),
  stringsAsFactors = FALSE
)

#################################################################
# CALCULATE LIN'S CCC AND RELATED METRICS
#################################################################
# For each model, calculate CCC with ground truth and decomposition metrics
for (model in model_columns) {
  # Create a temporary dataframe for this model, removing NA values
  # Get the correct ground_truth column (the numeric one, not the first one which seems to be metadata)
  gt_col <- which(names(df) == "ground_truth")[2]  # Take the second instance which should be numeric
  if(is.na(gt_col)) gt_col <- which(names(df) == "ground_truth")[1]  # Fallback to first if only one exists
  
  temp_df <- data.frame(
    ground_truth = df[[gt_col]],
    model_values = df[[model]]
  )
  temp_df <- temp_df[complete.cases(temp_df), ]
  
  # Calculate what percentage of data is available
  data_pct <- nrow(temp_df) / nrow(df) * 100
  
  # Only proceed if we have enough data points and both columns are numeric
  if (nrow(temp_df) >= 5 && 
      is.numeric(temp_df$ground_truth) && 
      is.numeric(temp_df$model_values)) {
    
    # Run CCC analysis with error handling
    tryCatch({
      # Calculate Lin's CCC using our custom function
      ccc_result <- lin_ccc(temp_df$ground_truth, temp_df$model_values)
      
      # Extract CCC value and confidence interval
      ccc_value <- ccc_result$ccc
      ccc_lower <- ccc_result$lower
      ccc_upper <- ccc_result$upper
      
      # Extract Pearson correlation
      pearson_r <- ccc_result$pearson
      
      # Calculate bias correction factor (Cb)
      # CCC = r * Cb, where Cb measures how far the best-fit line deviates from the 45-degree line
      bias_correction <- ccc_value / pearson_r
      
      # Calculate scale shift (ratio of standard deviations)
      scale_shift <- sd(temp_df$model_values) / sd(temp_df$ground_truth)
      
      # Calculate location shift (difference in means)
      location_shift <- mean(temp_df$model_values) - mean(temp_df$ground_truth)
      
      # Add row to results
      results <- rbind(results, data.frame(
        model = model,
        ccc = ccc_value,
        ccc_lower = ccc_lower,
        ccc_upper = ccc_upper,
        pearson_r = pearson_r,
        bias_correction = bias_correction,
        scale_shift = scale_shift,
        location_shift = location_shift,
        n_obs = nrow(temp_df),
        data_pct = data_pct,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      cat("Error in CCC analysis for model", model, ":", e$message, "\n")
    })
  }
}

#################################################################
# FORMAT RESULTS FOR VISUALIZATION
#################################################################
# Enhance results with additional information for plotting and analysis
plot_results <- results %>%
  mutate(
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
    
    # Calculate absolute CCC for sorting
    abs_ccc = abs(ccc),
    
    # Classify agreement quality based on CCC values
    # Following interpretation guidelines: <0.90 poor, 0.90-0.95 moderate, 0.95-0.99 substantial, >0.99 almost perfect
    agreement_quality = case_when(
      abs_ccc < 0.90 ~ "Poor",
      abs_ccc < 0.95 ~ "Moderate", 
      abs_ccc < 0.99 ~ "Substantial",
      TRUE ~ "Almost Perfect"
    )
  )

# Sort by absolute CCC (strongest first)
plot_results <- plot_results %>%
  arrange(desc(abs_ccc))

#################################################################
# SAVE RESULTS
#################################################################
# Save the complete results for further analysis and visualization
saveRDS(plot_results, "data/clean/ccc_results.rds")

#################################################################
# DISPLAY SUMMARY STATISTICS
#################################################################
# Print summary of CCC results
cat("\n=== LIN'S CONCORDANCE CORRELATION COEFFICIENT SUMMARY ===\n")
cat("Number of models analyzed:", nrow(plot_results), "\n")
cat("CCC range:", sprintf("%.3f to %.3f", min(plot_results$ccc), max(plot_results$ccc)), "\n")
cat("Mean CCC:", sprintf("%.3f", mean(plot_results$ccc)), "\n")
cat("Median CCC:", sprintf("%.3f", median(plot_results$ccc)), "\n")

# Print top 10 models by CCC
cat("\nTop 10 models by CCC:\n")
print(plot_results %>%
  select(model, ccc, pearson_r, bias_correction, agreement_quality) %>%
  head(10))
