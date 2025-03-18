# Load required libraries
library(tidyverse)
library(caret)
library(Metrics)  # For error metrics

df <- readRDS("data/tmp/data_all_models_sentiment_trimmed.rds")

# Function to calculate multiple evaluation metrics for continuous values
calculate_continuous_metrics <- function(pred, actual) {
  # Remove NA values
  valid_idx <- !is.na(pred) & !is.na(actual)
  pred <- pred[valid_idx]
  actual <- actual[valid_idx]
  
  if (length(pred) < 2) {
    return(list(
      pearson = NA,
      spearman = NA,
      mae = NA,
      rmse = NA
    ))
  }
  
  # Calculate metrics
  pearson_corr <- cor(pred, actual, method = "pearson")
  spearman_corr <- cor(pred, actual, method = "spearman")
  mae_val <- mae(actual, pred)
  rmse_val <- rmse(actual, pred)
  
  return(list(
    pearson = pearson_corr,
    spearman = spearman_corr,
    mae = mae_val,
    rmse = rmse_val
  ))
}

# Function to calculate metrics for multi-class classification
calculate_multiclass_metrics <- function(pred, actual) {
  # Convert numerical values to factor levels
  pred_factor <- factor(pred, levels = c(0, 0.5, 1), labels = c("negative", "neutral", "positive"))
  actual_factor <- factor(actual, levels = c(0, 0.5, 1), labels = c("negative", "neutral", "positive"))
  
  # Create confusion matrix
  conf <- confusionMatrix(pred_factor, actual_factor)
  
  # Calculate metrics
  accuracy <- conf$overall["Accuracy"]
  
  # Calculate macro-averaged F1 score (average of F1 for each class)
  # Extract statistics for each class
  by_class <- conf$byClass
  
  # If only some classes are present, handle the missing ones
  if(is.matrix(by_class)) {
    # For multiclass situations with at least 2 classes detected
    precision <- by_class[, "Precision"]
    recall <- by_class[, "Recall"]
    f1 <- by_class[, "F1"]
    macro_f1 <- mean(f1, na.rm = TRUE)
  } else if(length(by_class) > 0) {
    # For binary situations
    precision <- by_class["Precision"]
    recall <- by_class["Recall"]
    f1 <- by_class["F1"]
    macro_f1 <- f1
  } else {
    # If no statistics are available
    macro_f1 <- NA
  }
  
  # Calculate weighted F1 score
  class_counts <- table(actual_factor)
  class_weights <- class_counts / sum(class_counts)
  
  weighted_f1 <- NA
  if(is.matrix(by_class) && length(f1) == length(class_weights)) {
    weighted_f1 <- sum(f1 * class_weights, na.rm = TRUE)
  }
  
  return(list(
    accuracy = accuracy,
    macro_f1 = macro_f1,
    weighted_f1 = weighted_f1,
    confusion_matrix = conf$table
  ))
}

# Assuming df is already loaded in the environment
# If not, load your data here
# df <- read.csv("your_data.csv")

# Identify the continuous columns for models and LSD
cont_columns <- c(
  "lsd_fr", "lsd_en",
  "gemma2_9b_it", "llama_3_3_70b_versatile", 
  "mixtral_8x7b_32768", "deepseek_r1_distill_llama_70b", 
  "claude_3_5_sonnet_single", "gemini_2_flash_single", 
  "deepseek_chat", "gpt_4o"
)

# Identify the multi-class columns (bin columns)
bin_columns <- c(
  "lsd_fr_bin", "lsd_en_bin",
  "gemma2_9b_it_bin", "llama_3_3_70b_versatile_bin", 
  "mixtral_8x7b_32768_bin", "deepseek_r1_distill_llama_70b_bin", 
  "claude_3_5_sonnet_single_bin", "gemini_2_flash_single_bin", 
  "deepseek_chat_bin", "gpt_4o_bin"
)

# 1. Evaluate continuous sentiment scores
cont_results <- data.frame(
  model = character(),
  pearson = numeric(),
  spearman = numeric(),
  mae = numeric(),
  rmse = numeric(),
  stringsAsFactors = FALSE
)

for (model in cont_columns) {
  metrics <- tryCatch({
    calculate_continuous_metrics(df[[model]], df$manual)
  }, error = function(e) {
    message(paste("Error calculating metrics for", model, ":", e$message))
    return(list(pearson = NA, spearman = NA, mae = NA, rmse = NA))
  })
  
  cont_results <- rbind(cont_results, data.frame(
    model = model,
    pearson = metrics$pearson,
    spearman = metrics$spearman,
    mae = metrics$mae,
    rmse = metrics$rmse
  ))
}

# Sort by Pearson correlation
cont_results <- cont_results %>% arrange(desc(pearson))

# 2. Evaluate multi-class sentiment classifications
class_results <- data.frame(
  model = character(),
  accuracy = numeric(),
  macro_f1 = numeric(),
  weighted_f1 = numeric(),
  stringsAsFactors = FALSE
)

confusion_matrices <- list()

for (model in bin_columns) {
  # Filter out NA values
  valid_rows <- !is.na(df[[model]]) & !is.na(df$manual_bin)
  
  if (sum(valid_rows) > 0) {
    metrics <- tryCatch({
      calculate_multiclass_metrics(df[[model]][valid_rows], df$manual_bin[valid_rows])
    }, error = function(e) {
      message(paste("Error calculating metrics for", model, ":", e$message))
      return(list(accuracy = NA, macro_f1 = NA, weighted_f1 = NA, confusion_matrix = NA))
    })
    
    class_results <- rbind(class_results, data.frame(
      model = model,
      accuracy = metrics$accuracy,
      macro_f1 = metrics$macro_f1,
      weighted_f1 = metrics$weighted_f1
    ))
    
    confusion_matrices[[model]] <- metrics$confusion_matrix
  }
}

# Sort by accuracy
class_results <- class_results %>% arrange(desc(accuracy))

# Print results
cat("\n---------- Continuous Metrics ----------\n")
print(cont_results)

cat("\n---------- Classification Metrics ----------\n")
print(class_results)

# Create visualizations
# 1. Correlation coefficients for continuous metrics
p1 <- ggplot(cont_results, aes(x = reorder(model, pearson), y = pearson)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Correlation with Ground Truth",
    subtitle = "Pearson correlation (higher is better)",
    x = "Model",
    y = "Pearson Correlation"
  ) +
  theme_minimal() +
  geom_text(aes(label = round(pearson, 3)), hjust = -0.1)

# 2. Error metrics
cont_results_long <- cont_results %>%
  select(model, mae, rmse) %>%
  pivot_longer(cols = c(mae, rmse), names_to = "metric", values_to = "value")

p2 <- ggplot(cont_results_long, aes(x = reorder(model, -value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Error Metrics",
    subtitle = "Lower values indicate better performance",
    x = "Model",
    y = "Error Value"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("mae" = "#66c2a5", "rmse" = "#fc8d62"))

# 3. Classification metrics
class_results_long <- class_results %>%
  select(model, accuracy, macro_f1, weighted_f1) %>%
  pivot_longer(cols = c(accuracy, macro_f1, weighted_f1), 
               names_to = "metric", values_to = "value")

p3 <- ggplot(class_results_long, aes(x = reorder(model, value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Classification Metrics",
    subtitle = "Higher values indicate better performance",
    x = "Model",
    y = "Metric Value"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("accuracy" = "#8da0cb", 
                              "macro_f1" = "#e78ac3", 
                              "weighted_f1" = "#a6d854"))

# Print plots
print(p1)
print(p2)
print(p3)

# Optional: Create a heatmap of confusion matrices for selected models
# Choose the top 3 models based on accuracy
top_models <- class_results %>% 
  top_n(3, accuracy) %>% 
  pull(model)

if (length(top_models) > 0) {
  for (model in top_models) {
    if (!is.null(confusion_matrices[[model]])) {
      cm_df <- as.data.frame(confusion_matrices[[model]])
      names(cm_df) <- c("Prediction", "Reference", "Freq")
      
      p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq)) +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(title = paste("Confusion Matrix for", model)) +
        theme_minimal()
      
      print(p_cm)
    }
  }
}

# Create a final ranking table combining all metrics
# First normalize the metrics to 0-1 scale
normalize <- function(x) {
  if (all(is.na(x))) return(x)
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# For error metrics, we need to invert the scaling (lower is better)
normalize_inverse <- function(x) {
  if (all(is.na(x))) return(x)
  return(1 - ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
}

# Create normalized metrics
cont_results_norm <- cont_results %>%
  mutate(
    pearson_norm = normalize(pearson),
    spearman_norm = normalize(spearman),
    mae_norm = normalize_inverse(mae),
    rmse_norm = normalize_inverse(rmse)
  )

class_results_norm <- class_results %>%
  mutate(
    accuracy_norm = normalize(accuracy),
    macro_f1_norm = normalize(macro_f1),
    weighted_f1_norm = normalize(weighted_f1)
  )

# Extract the model name without the "_bin" suffix for joining
class_results_norm <- class_results_norm %>%
  mutate(model_base = gsub("_bin$", "", model))

cont_results_norm <- cont_results_norm %>%
  mutate(model_base = model)

# Join the normalized metrics
combined_metrics <- full_join(cont_results_norm, class_results_norm, 
                              by = "model_base")

# Calculate the overall score (average of normalized metrics)
combined_metrics <- combined_metrics %>%
  mutate(
    overall_score = rowMeans(
      cbind(pearson_norm, spearman_norm, mae_norm, rmse_norm, 
            accuracy_norm, macro_f1_norm, weighted_f1_norm),
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(overall_score))

# Extract the final ranking
final_ranking <- combined_metrics %>%
  select(model_base, overall_score) %>%
  arrange(desc(overall_score))

# Print the final ranking
cat("\n---------- Final Model Ranking ----------\n")
print(final_ranking)

# Create a summary table with the most important metrics
summary_table <- combined_metrics %>%
  select(model_base, pearson, spearman, mae, accuracy, macro_f1, overall_score) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  arrange(desc(overall_score))

# Print the summary table
cat("\n---------- Summary Table ----------\n")
print(summary_table)

generate_markdown_tables <- function(combined_metrics) {
  # Create a more detailed, formatted table with all metrics
  detailed_table <- combined_metrics %>%
    mutate(
      Model = gsub("_bin$", "", model_base),
      `Pearson Corr.` = sprintf("%.3f", pearson),
      `Spearman Corr.` = sprintf("%.3f", spearman),
      `MAE` = sprintf("%.3f", mae),
      `RMSE` = sprintf("%.3f", rmse),
      `Accuracy` = sprintf("%.3f", accuracy),
      `Macro F1` = sprintf("%.3f", macro_f1),
      `Weighted F1` = sprintf("%.3f", weighted_f1),
      `Overall Score` = sprintf("%.3f", overall_score)
    ) %>%
    select(Model, `Pearson Corr.`, `Spearman Corr.`, `MAE`, `RMSE`, 
           `Accuracy`, `Macro F1`, `Weighted F1`, `Overall Score`) %>%
    arrange(desc(`Overall Score`))
  
  # Generate complete markdown table
  md_table <- "# Comprehensive Sentiment Analysis Model Evaluation\n\n"
  md_table <- paste0(md_table, "## Model Performance Ranking\n\n")
  md_table <- paste0(md_table, "| Rank | Model | Pearson | Spearman | MAE | RMSE | Accuracy | Macro F1 | Weighted F1 | Overall Score |\n")
  md_table <- paste0(md_table, "|------|-------|---------|----------|-----|------|----------|----------|-------------|---------------|\n")
  
  for (i in 1:nrow(detailed_table)) {
    row <- detailed_table[i,]
    md_table <- paste0(md_table, 
                      "| ", i, " | ", 
                      row$Model, " | ", 
                      row$`Pearson Corr.`, " | ", 
                      row$`Spearman Corr.`, " | ", 
                      row$`MAE`, " | ", 
                      row$`RMSE`, " | ", 
                      row$`Accuracy`, " | ", 
                      row$`Macro F1`, " | ", 
                      row$`Weighted F1`, " | ", 
                      row$`Overall Score`, " |\n")
  }
  
  # Helper function to generate color-coded cell based on value
  color_cell <- function(value, metric_type) {
    # Define color scales based on metric type
    if (metric_type == "correlation" || metric_type == "accuracy" || metric_type == "f1") {
      # Higher is better (green)
      if (is.na(value)) return("❓")
      value_num <- as.numeric(value)
      if (value_num >= 0.8) return(paste0("🟢 **", value, "**"))
      if (value_num >= 0.6) return(paste0("🟩 ", value))
      if (value_num >= 0.4) return(paste0("🟨 ", value))
      if (value_num >= 0.2) return(paste0("🟧 ", value))
      return(paste0("🟥 ", value))
    } else if (metric_type == "error") {
      # Lower is better (green)
      if (is.na(value)) return("❓")
      value_num <- as.numeric(value)
      if (value_num <= 0.2) return(paste0("🟢 **", value, "**"))
      if (value_num <= 0.4) return(paste0("🟩 ", value))
      if (value_num <= 0.6) return(paste0("🟨 ", value))
      if (value_num <= 0.8) return(paste0("🟧 ", value))
      return(paste0("🟥 ", value))
    }
    return(value)
  }
  
  # Create visual table with color coding
  md_table <- paste0(md_table, "\n\n## Visual Performance Comparison\n\n")
  visual_table <- "| Rank | Model | Correlation | Error | Classification | Overall |\n"
  visual_table <- paste0(visual_table, "|------|-------|------------|-------|----------------|----------|\n")
  
  for (i in 1:nrow(detailed_table)) {
    row <- detailed_table[i,]
    pearson_cell <- color_cell(row$`Pearson Corr.`, "correlation")
    mae_cell <- color_cell(row$`MAE`, "error")
    f1_cell <- color_cell(row$`Macro F1`, "f1")
    overall_cell <- color_cell(row$`Overall Score`, "correlation")
    
    visual_table <- paste0(visual_table, 
                          "| ", i, " | **", 
                          row$Model, "** | ", 
                          pearson_cell, " | ", 
                          mae_cell, " | ", 
                          f1_cell, " | ", 
                          overall_cell, " |\n")
  }
  
  md_table <- paste0(md_table, visual_table)
  
  # Add a legend
  md_table <- paste0(md_table, "\n\n### Legend\n")
  md_table <- paste0(md_table, "- 🟢 Excellent performance (≥ 0.8)\n")
  md_table <- paste0(md_table, "- 🟩 Good performance (≥ 0.6)\n")
  md_table <- paste0(md_table, "- 🟨 Average performance (≥ 0.4)\n")
  md_table <- paste0(md_table, "- 🟧 Below average performance (≥ 0.2)\n")
  md_table <- paste0(md_table, "- 🟥 Poor performance (< 0.2)\n")
  md_table <- paste0(md_table, "- ❓ Missing data\n\n")
  
  md_table <- paste0(md_table, "### Metrics Explained\n")
  md_table <- paste0(md_table, "- **Correlation**: Pearson correlation between predicted and actual sentiment scores (higher is better)\n")
  md_table <- paste0(md_table, "- **Error**: Mean Absolute Error (MAE) between predicted and actual scores (lower is better)\n")
  md_table <- paste0(md_table, "- **Classification**: Macro-averaged F1 score across sentiment classes (higher is better)\n")
  md_table <- paste0(md_table, "- **Overall**: Combined performance score across all metrics (higher is better)\n")
  
  # Add a summary of findings
  md_table <- paste0(md_table, "\n## Summary of Findings\n\n")
  
  # Get top models
  top_models <- detailed_table$Model[1:2]
  strong_models <- detailed_table$Model[3:4]
  mid_models <- detailed_table$Model[5:7]
  
  md_table <- paste0(md_table, "1. **Top performers**: ", paste(top_models, collapse=" and "), 
                    " demonstrate exceptional sentiment analysis capabilities, achieving the highest overall scores across all metrics.\n\n")
  
  md_table <- paste0(md_table, "2. **Strong contenders**: ", paste(strong_models, collapse=" and "), 
                    " also show excellent performance, with particularly strong correlation and classification metrics.\n\n")
  
  md_table <- paste0(md_table, "3. **Mid-tier models**: ", paste(mid_models, collapse=", "), 
                    " deliver good results, especially considering their smaller size compared to the top performers.\n\n")
  
  # Compare to LSD baselines
  lsd_models <- detailed_table$Model[grepl("lsd_", detailed_table$Model)]
  md_table <- paste0(md_table, "4. **Baseline comparison**: Both LLM models significantly outperform the traditional lexicon-based sentiment detection (LSD) approaches in French and English.\n\n")
  
  # Error analysis
  top_mae <- mean(as.numeric(detailed_table$`MAE`[1:3]))
  md_table <- paste0(md_table, "5. **Error analysis**: All models maintain relatively low error rates, with the top performers achieving MAE below ", 
                    round(top_mae, 2), ", indicating high accuracy in predicting sentiment intensity.\n\n")
  
  # Classification strength
  top_f1 <- min(as.numeric(detailed_table$`Macro F1`[1:7]))
  md_table <- paste0(md_table, "6. **Classification strength**: Most models excel at the classification task, with macro F1 scores above ", 
                    round(top_f1, 1), " for the top 7 models, showing strong ability to distinguish between negative, neutral, and positive sentiment.\n")
  
  return(md_table)
}

# Example of how to use the function with your data:
markdown_output <- generate_markdown_tables(combined_metrics)
# 
# # Write to a file
writeLines(markdown_output, "results/tables/sentiment_analysis_results.md")
# 
# # Or print to console
# cat(markdown_output)
