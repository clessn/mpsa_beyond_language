#################################################################
# PARAMETER SIZE VS PERFORMANCE REGRESSION ANALYSIS
#################################################################
# This script analyzes the relationship between model parameter size and 
# performance measured by Mean Absolute Error (MAE).

# Load required libraries
library(ggplot2)    # For visualization
library(tidyr)      # For data reshaping
library(stringr)    # For string manipulation
library(dplyr)      # For data manipulation

#################################################################
# LOAD DATA AND PREPARE FOR ANALYSIS
#################################################################
# Load the cleaned dataframe
df <- readRDS("data/clean/df.rds") %>% 
  # Remove categorical columns and language-specific columns we won't use
  select(-ends_with("cat")) %>%
  select(-ends_with("_fr_fr")) %>%
  select(-ends_with("_en_en")) %>%
  # Keep only cross-lingual (EN→FR) models and ground truth
  select(ends_with("_en_fr") | ends_with("truth")) %>%
  # Exclude closed-weight models, and DeepSeek V3.2 (dual reasoning/non-reasoning
  # mode, direct successor to DeepSeek R1 Basic, excluded from this batch for the
  # same reason: inconsistent output formatting when the model reasons before answering)
  select(-deepseekv32_en_fr, -claudehaiku45_en_fr, -gemini35_en_fr, -deepseekv4flash_en_fr, -gpt56luna_en_fr) %>%
  # Rename models to more readable format with parameter size (total parameters
  # for MoE architectures, consistent with the previous batch's convention)
  rename(
    qwen3_32b = qwen332b_en_fr,
    qwen3_235b = qwen3235b_en_fr,
    llama4_109b = llama4scout_en_fr,
    gptoss_20b = gptoss20b_en_fr,
  ) %>%
  # Reshape data to long format for regression analysis
  pivot_longer(
    cols = -ground_truth,
    names_to = c("model_name", "parameters"),
    names_pattern = "(.+)_([^_]+)$",
    values_to = "result"
  ) %>%
  # Calculate absolute error for each prediction
  mutate(
    mae = abs(result - ground_truth)
  ) %>%
  # Extract numeric parameter size in billions
  mutate(
    param_numeric = as.numeric(str_extract(parameters, "[0-9\\.]+"))
  )

#################################################################
# REGRESSION ANALYSIS
#################################################################
# Run linear regression model to test relationship between parameter size and MAE
model <- lm(mae ~ param_numeric, data = df)

# Create summary for display and reporting
regression_summary <- summary(model)

# Calculate average MAE by model and parameter size for visualization
model_params_summary <- df %>%
  group_by(model_name, parameters, param_numeric) %>%
  summarise(
    mean_mae = mean(mae, na.rm = TRUE),
    .groups = "drop"
  )

#################################################################
# CREATE PARAMETER SIZE VS. PERFORMANCE VISUALIZATION
#################################################################
# Professional black and white visualization for academic conference
params_plot <- ggplot(model_params_summary, aes(x = param_numeric, y = mean_mae)) +
  # Clean white background
  annotate("rect", 
           xmin = -Inf, xmax = Inf, 
           ymin = -Inf, ymax = Inf, 
           fill = "white", alpha = 1.0) +
  
  # Add subtle grid lines
  geom_hline(yintercept = seq(0.2, 0.4, by = 0.05), color = "gray90", linewidth = 0.4) +
  geom_vline(xintercept = seq(0, 250, by = 50), color = "gray90", linewidth = 0.4) +
  
  # Add regression line with confidence interval
  geom_smooth(method = "lm", se = TRUE, 
              color = "black", fill = "gray80", 
              linewidth = 0.8, alpha = 0.2) +
  
  # Add scatter points with professional appearance
  geom_point(aes(shape = model_name), size = 5, 
             color = "black", fill = "black", stroke = 0.8) +
  
  # Use different shapes for models
  scale_shape_manual(values = c(
    "qwen3" = 21,   # Circle for Qwen3 (32B and 235B-A22B)
    "llama4" = 22,  # Square for Llama 4 Scout
    "gptoss" = 23   # Diamond for GPT-OSS
  )) +
  
  # Add small parameter size labels below points
  geom_text(aes(label = paste0(parameters), y = mean_mae + 0.01), 
            color = "black", size = 3, vjust = -0.5) +
  
  # Add model names with professional styling
  geom_text(
    data = model_params_summary,
    aes(
      x = case_when(
        param_numeric < 30 ~ param_numeric + 6,
        TRUE ~ param_numeric - 6
      ),
      y = case_when(
        param_numeric < 30 ~ mean_mae - 0.02,
        param_numeric > 200 ~ mean_mae - 0.02,
        TRUE ~ mean_mae + 0.03
      ),
      label = model_name
    ),
    fontface = "italic", size = 3.5,
    hjust = case_when(
      model_params_summary$param_numeric < 30 ~ 0,
      model_params_summary$param_numeric > 200 ~ 1,
      TRUE ~ 0.5
    )
  ) +
  
  # Add equation annotation with academic styling
  annotate(
    "text",
    x = 125, y = 0.36,
    label = sprintf(
      "MAE = %.3f - %.4f × Parameters\nR² = %.3f, p < 0.001",
      coef(model)[1], 
      abs(coef(model)[2]),
      regression_summary$r.squared
    ),
    hjust = 0.5,
    size = 3.5,
    color = "black",
    fontface = "plain"
  ) +
  
  # Professional axis scales
  scale_y_continuous(
    limits = c(0.2, 0.4),
    breaks = seq(0.2, 0.4, by = 0.05),
    labels = function(x) sprintf("%.2f", x),
    expand = c(0.01, 0.01)
  ) +
  
  scale_x_continuous(
    limits = c(0, 250),
    breaks = c(20, 32, 109, 235),
    minor_breaks = NULL,
    expand = c(0.01, 0.01)
  ) +
  
  # Academic labels and titles 
  labs(
    title = "Parameter Size and Performance",
    subtitle = "Relationship between model size and mean absolute error",
    x = "Model Size (billions of parameters)",
    y = "Mean Absolute Error",
    caption = "Note: Analysis based on cross-lingual (EN->FR) sentiment analysis task."
  ) +
  
  # Academic black and white theme
  theme_bw() +
  theme(
    # Panel and plot background
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    
    # Axis styling
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(2, "pt"),
    
    # Text elements - academic styling
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10), face = "italic"),
    axis.title = element_text(size = 10, face = "plain", margin = margin(t = 5, b = 5)),
    axis.text = element_text(size = 9, color = "black"),
    
    # Legend (hidden)
    legend.position = "none",
    
    # Plot margins
    plot.margin = margin(15, 15, 15, 15)
  )

#################################################################
# DISPLAY AND SAVE VISUALIZATION
#################################################################
# Display the plot
print(params_plot)

# Save high-resolution version to file
ggsave("results/graphs/model_parameter_vs_mae.png", params_plot, width = 10, height = 8, dpi = 300)

# Create a presentation-optimized version (16:9 format)
params_plot_pres <- params_plot +
  # Adjust theme for presentation
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, face = "italic")
  )

# Save presentation version
ggsave("results/graphs/model_parameter_vs_mae_16x9.png", params_plot_pres, width = 16, height = 9, dpi = 300)

#################################################################
# SAVE REGRESSION RESULTS FOR REPORTING
#################################################################
# Save the regression results for potential later use
saveRDS(list(
  model = model,
  summary = regression_summary,
  data = model_params_summary
), "results/analysis/parameter_regression_results.rds")