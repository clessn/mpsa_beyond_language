#################################################################
# CORRELATION VISUALIZATION OF MODEL PERFORMANCE
#################################################################
# This script creates a publication-quality visualization of the correlation
# between different sentiment analysis models and ground truth human ratings.
# It groups models by type, orders them by performance, and provides visual
# indications of statistical significance.

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization

#################################################################
# LOAD SUPPORT FILES AND DATA
#################################################################
# Load model mapping information
source("src/94_models_map.R")

# Load correlation results data
df_raw <- readRDS("data/clean/cor_results.rds")

#################################################################
# MODEL DISPLAY NAME PROCESSING
#################################################################
# Function to get human-friendly model display names from the technical model IDs
get_model_display_name <- function(model_name) {
  # Special handling for dictionary-based models
  if (grepl("^lsd_", model_name)) {
    return(model_name)  # Return dictionary models as is
  }
  
  # Extract the base prefix from the model name
  prefix <- sub("_[a-z]{2}_[a-z]{2}$", "", model_name)
  
  # Look up the full model name from the mapping
  if (prefix %in% names(model_mapping)) {
    full_name <- model_mapping[prefix]
    
    # Get manufacturer based on the full model name
    manufacturer <- case_when(
      full_name %in% meta_models ~ "Meta",
      full_name %in% alibaba_models ~ "Alibaba",
      full_name %in% mistral_models ~ "Mistral",
      full_name %in% anthropic_models ~ "Anthropic",
      full_name %in% google_models ~ "Google",
      full_name %in% deepseek_models ~ "DeepSeek",
      full_name %in% openai_models ~ "OpenAI",
      TRUE ~ "Other"
    )
    
    # Get display name based on the full model name
    display_name <- case_when(
      full_name == "accounts/fireworks/models/llama-v3p2-3b-instruct" ~ "Llama 3.2 3B",
      full_name == "accounts/fireworks/models/qwq-32b" ~ "QWen2 32B",
      full_name == "accounts/fireworks/models/deepseek-r1-basic" ~ "DeepSeek R1 Basic",
      full_name == "accounts/fireworks/models/llama-v3p3-70b-instruct" ~ "Llama 3.3 70B",
      full_name == "gemma2-9b-it" ~ "Gemma 2 9B",
      full_name == "llama-3.2-1b-preview" ~ "Llama 3.2 1B",
      full_name == "mistral-saba-24b" ~ "Mistral Saba 24B",
      full_name == "claude-3-5-haiku-20241022" ~ "Claude 3.5 Haiku",
      full_name == "gemini-2.0-flash" ~ "Gemini 2.0 Flash",
      full_name == "deepseek-chat" ~ "DeepSeek Chat",
      full_name == "gpt-4o" ~ "GPT-4o",
      TRUE ~ full_name
    )
    
    # Create a display name that includes the manufacturer
    return(paste0(manufacturer, ": ", display_name))
  }
  
  # Return original name if no mapping found
  return(model_name)
}

#################################################################
# DATA PREPARATION FOR VISUALIZATION
#################################################################
# Enhance the correlation data with additional information for plotting
df <- df_raw %>%
  mutate(
    # Determine if model has open-source weights
    is_open_source = sapply(model, is_open_source),
    
    # Classify models into categories
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is.na(is_open_source) ~ "Other",
      is_open_source ~ "Open weights",
      !is_open_source ~ "Closed weights"
    ),
    
    # Extract base model name, ensuring dictionary models are grouped together
    model_name = case_when(
      grepl("^lsd_", model) ~ "dict",  # Group all lsd_ models under "dict"
      TRUE ~ sapply(model, function(m) {
        # Extract base model name without language suffix
        sub("_[a-z]{2}_[a-z]{2}$", "", m)
      })
    ),
    
    # Get display name for the model using the mapping function
    display_name = sapply(model, get_model_display_name),
    
    # Create a simple prompt technique code
    prompt_mechanism = case_when(
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "FR",
      grepl("^lsd_en", model) ~ "EN",
      TRUE ~ "Other"
    ),
    
    # Create labels that include both model name and prompting mechanism
    model_label = case_when(
      grepl("^lsd_", model) ~ model,  # Keep original lsd_ model names for labels
      TRUE ~ paste0(display_name, " [", prompt_mechanism, "]")
    )
  )

#################################################################
# MODEL GROUPING AND ORDERING
#################################################################
# Calculate average correlation by model type for ordering
model_avg_cors <- df %>%
  group_by(model_name) %>%
  summarize(avg_correlation = mean(abs_correlation, na.rm = TRUE)) %>%
  arrange(desc(avg_correlation))

# Create ordered factors for proper plot ordering
df <- df %>%
  mutate(
    # Order factor for model_name (for grouping)
    model_name_ordered = factor(model_name, levels = model_avg_cors$model_name),
    
    # Assign group number to each unique model_name for alternating backgrounds
    model_group = as.numeric(factor(model_name, levels = model_avg_cors$model_name)),
    
    # Create a composite ordering value that keeps models from the same family together
    y_ordering = paste0(
      sprintf("%03d", model_group),
      "_",
      # Order prompting mechanisms consistently within each model (FR→FR, EN→FR, EN→EN)
      case_when(
        prompt_mechanism == "FR→FR" ~ "1", 
        prompt_mechanism == "EN→FR" ~ "2",
        prompt_mechanism == "EN→EN" ~ "3",
        prompt_mechanism == "FR" ~ "4",
        prompt_mechanism == "EN" ~ "5",
        TRUE ~ "9"
      )
    )
  ) %>%
  # Final ordering for plot - reverse to put highest performing at top
  arrange(y_ordering) %>%
  mutate(model_label_ordered = factor(model_label, levels = rev(unique(model_label))))

#################################################################
# CREATE BACKGROUND RECTANGLES FOR VISUAL GROUPING
#################################################################
# Get unique model groups in the order they will appear
model_groups <- df %>%
  select(model_name, model_group) %>%
  distinct() %>%
  arrange(model_group)

# Get the total number of rows in the dataframe for position calculation
total_rows <- nrow(df)

# Create background rectangles data for alternating model group backgrounds
bg_rects <- data.frame()
current_min <- 0.5

for (i in 1:nrow(model_groups)) {
  # Count how many rows this model takes up
  model_count <- sum(df$model_name == model_groups$model_name[i])
  
  # Need to reverse the y-coordinates since we're placing highest at top
  rect_ymin <- total_rows - (current_min + model_count) + 1
  rect_ymax <- total_rows - current_min + 1
  
  # For even numbered groups, use a light gray; for odd, use white
  bg_rects <- rbind(bg_rects, data.frame(
    ymin = rect_ymin,
    ymax = rect_ymax,
    shade = ifelse(i %% 2 == 1, "even", "odd"),
    model_name = model_groups$model_name[i]
  ))
  
  current_min <- current_min + model_count
}

# Create display names for model groups (special handling for dictionary models)
model_groups <- model_groups %>%
  mutate(display_name = case_when(
    model_name == "dict" ~ "Dictionary-based",
    TRUE ~ model_name
  ))

#################################################################
# CREATE CORRELATION PLOT
#################################################################
# Create the correlation plot with professional styling
plot_correlation <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  
  # Points colored by model type
  geom_point(data = df, aes(
    x = correlation, 
    y = model_label_ordered,
    color = model_type
  ), size = 3.5) +
  
  # Add error bars with matching colors
  geom_errorbarh(data = df, aes(
    y = model_label_ordered,
    xmin = correlation - 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    xmax = correlation + 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    color = model_type
  ), height = 0.2) +
  
  # Theme with white background
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  
  # Labels and titles
  labs(x = "\nCorrelation with Ground Truth\n",
       y = "",  # Remove y-axis label since we have direct labels
       title = "Sentiment Analysis Models Correlation with Ground Truth",
       subtitle = "Pearson correlation coefficients with 95% confidence intervals",
       caption = "Figure 1. Cross-lingual sentiment analysis performance across model architectures.\nModels grouped by type (color) and sorted by correlation strength. Prompting mechanisms within clusters ordered as\nFR→FR (French prompt on French text), EN→FR (English prompt on French text), and EN→EN (English prompt on translated text).\nError bars represent 95% confidence intervals; *p < 0.05, **p < 0.01, ***p < 0.001.") +
  
  # Scale and reference lines
  scale_x_continuous(limits = c(-1, 1)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # Fill scale for alternating backgrounds
  scale_fill_manual(values = c(
    "even" = "gray95",  # Almost white
    "odd" = "gray90"    # Very light gray
  )) +
  
  # Hide the fill legend (used only for background grouping)
  guides(fill = "none") +
  
  # Correlation value labels with significance stars
  geom_text(data = df, aes(
    x = ifelse(correlation >= 0, correlation + 0.15, correlation - 0.15),
    y = model_label_ordered,
    label = sprintf("r = %.2f%s", 
                    correlation,
                    ifelse(p_value < 0.001, "***", 
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", ""))))
  ), hjust = ifelse(df$correlation >= 0, 0, 1), size = 3.5) +
  
  # Color scale for model types
  scale_color_manual(values = c(
    "Open weights" = "#1F77B4",   # Blue 
    "Closed weights" = "#D62728", # Red
    "Dictionary" = "#2CA02C",    # Green
    "Other" = "#7F7F7F"          # Gray
  ), name = "Model Type") +
  
  # Theme formatting
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))

#################################################################
# DISPLAY AND SAVE VISUALIZATION
#################################################################
# Display the plot
print(plot_correlation)

# Save high-resolution version to file
ggsave("results/graphs/model_correlation.png", plot_correlation, width = 14, height = 12, dpi = 300)

#################################################################
# CREATE PRESENTATION-READY CORRELATION PLOT (16:9 FORMAT)
#################################################################
# Create the correlation plot with professional styling optimized for 16:9 presentations
plot_correlation <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.4) +
  
  # Points colored by model type
  geom_point(data = df, aes(
    x = correlation, 
    y = model_label_ordered,
    color = model_type
  ), size = 4) +
  
  # Add error bars with matching colors
  geom_errorbarh(data = df, aes(
    y = model_label_ordered,
    xmin = correlation - 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    xmax = correlation + 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    color = model_type
  ), height = 0.25) +
  
  # Theme with white background optimized for presentations
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank(),    # Remove all minor grid lines
    panel.grid.major.x = element_line(color = "gray90"),  # Lighter vertical grid lines
    plot.margin = margin(20, 40, 20, 20)   # Add more margin space for presentation
  ) +
  
  # Labels and titles - larger for presentations
  labs(
    x = "Correlation with Ground Truth",
    y = "",  # Remove y-axis label since we have direct labels
    title = "Sentiment Analysis Models: Correlation with Ground Truth",
    subtitle = "Pearson correlation coefficients with 95% confidence intervals",
    caption = "Models grouped by type (color) and sorted by correlation strength. Prompting mechanisms within clusters ordered as\nFR→FR (French prompt on French text), EN→FR (English prompt on French text), and EN→EN (English prompt on translated text).\nError bars represent 95% confidence intervals; *p < 0.05, **p < 0.01, ***p < 0.001."
  ) +
  
  # Scale and reference lines
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25),
    expand = c(0.02, 0.02)
  ) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # Fill scale for alternating backgrounds
  scale_fill_manual(values = c(
    "even" = "gray95",  # Almost white
    "odd" = "gray90"    # Very light gray
  )) +
  
  # Hide the fill legend (used only for background grouping)
  guides(fill = "none") +
  
  # Correlation value labels with significance stars - increase space for widescreen
  geom_text(data = df, aes(
    x = ifelse(correlation >= 0, correlation + 0.12, correlation - 0.12), 
    y = model_label_ordered,
    label = sprintf("r = %.2f%s", 
                    correlation,
                    ifelse(p_value < 0.001, "***", 
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", ""))))
  ), hjust = ifelse(df$correlation >= 0, 0, 1), size = 3.8) +
  
  # Color scale for model types - brighter colors for presentation visibility
  scale_color_manual(values = c(
    "Open weights" = "#2077B4",   # Brighter blue 
    "Closed weights" = "#E63946", # Brighter red
    "Dictionary" = "#2DC653",     # Brighter green
    "Other" = "#6C757D"           # Slightly brighter gray
  ), name = "Model Type") +
  
  # Theme formatting - optimized for presentation
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 14, margin = margin(t = 10, b = 5)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 11, hjust = 0, margin = margin(t = 15)),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 10),
    legend.key.size = unit(1.2, "cm")
  )

#################################################################
# SAVE VISUALIZATION IN 16:9 FORMAT
#################################################################
# Save high-resolution version in 16:9 format
# 16:9 aspect ratio = 1920x1080 pixels for standard HD presentations
ggsave("results/graphs/model_correlation_16x9.png", 
       plot_correlation, 
       width = 16, 
       height = 9, 
       dpi = 300)
