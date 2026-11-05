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
source("src/94_models_map.R")  # model_params_total, get_model_display_name()

# All open-weight models in the final lineup, in the cross-lingual (EN->FR)
# condition. The lineup comes from the model map rather than a hand-kept list:
# this script used to name four models explicitly and broke when the lineup
# changed. Closed-weight models are excluded because their sizes are undisclosed.
#
# Size is TOTAL parameters, the convention of the previous batch. For the
# mixture-of-experts models active parameters would be the better regressor,
# but three active counts are still unverified (see 94_models_map.R).
open_prefixes <- names(model_params_total)

df <- readRDS("data/clean/df.rds") %>%
  select(ground_truth, all_of(paste0(open_prefixes, "_en_fr"))) %>%
  pivot_longer(
    cols = -ground_truth,
    names_to = "model_prefix",
    names_pattern = "(.+)_en_fr$",
    values_to = "result"
  ) %>%
  # llama321b scored only part of the sample; its unscored sentences drop out
  # here, so its mean MAE rests on a smaller, non-random n.
  filter(!is.na(result)) %>%
  mutate(
    mae = abs(result - ground_truth),
    param_numeric = unname(model_params_total[model_prefix]),
    model_name = sapply(model_prefix, get_model_display_name, with_condition = FALSE)
  )

#################################################################
# REGRESSION ANALYSIS
#################################################################
# Sentence-level regression of absolute error on log10 model size. Sizes span
# 1B to 671B, so a linear term would be driven almost entirely by DeepSeek V3.2.
model <- lm(mae ~ log10(param_numeric), data = df)

# Create summary for display and reporting
regression_summary <- summary(model)

# Calculate average MAE by model and parameter size for visualization
model_params_summary <- df %>%
  group_by(model_name, param_numeric) %>%
  summarise(
    mean_mae = mean(mae, na.rm = TRUE),
    n_obs = n(),
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
  
  # Add regression line with confidence interval
  geom_smooth(method = "lm", se = TRUE, 
              color = "black", fill = "gray80", 
              linewidth = 0.8, alpha = 0.2) +
  
  geom_point(size = 3.5, color = "black") +
  
  # Model name, with n where the model did not score the full sample
  geom_text(
    aes(label = ifelse(n_obs < max(n_obs),
                       paste0(model_name, " (n = ", n_obs, ")"),
                       model_name)),
    vjust = -1.1, fontface = "italic", size = 3.2
  ) +
  
  annotate(
    "text",
    x = 30, y = max(model_params_summary$mean_mae) * 1.08,
    label = sprintf(
      "MAE = %.3f %s %.3f \u00d7 log10(Parameters)\nR\u00b2 = %.3f, p %s (sentence level)",
      coef(model)[1],
      ifelse(coef(model)[2] < 0, "-", "+"),
      abs(coef(model)[2]),
      regression_summary$r.squared,
      ifelse(coef(regression_summary)[2, 4] < 0.001, "< 0.001",
             sprintf("= %.3f", coef(regression_summary)[2, 4]))
    ),
    hjust = 0.5,
    size = 3.5,
    color = "black"
  ) +
  
  scale_y_continuous(
    labels = function(x) sprintf("%.2f", x),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  scale_x_log10(
    breaks = c(1, 3, 10, 30, 100, 300, 1000),
    minor_breaks = NULL,
    expand = expansion(mult = 0.08)
  ) +
  
  # Academic labels and titles 
  labs(
    title = "Parameter Size and Performance",
    subtitle = "Relationship between model size and mean absolute error",
    x = "Model Size (billions of parameters, log scale)",
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