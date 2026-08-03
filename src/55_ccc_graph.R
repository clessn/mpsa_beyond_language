#################################################################
# LIN'S CCC VISUALIZATION OF MODEL PERFORMANCE
#################################################################
# This script creates a publication-quality visualization of Lin's 
# Concordance Correlation Coefficient (CCC) between different sentiment 
# analysis models and ground truth human ratings. It groups models by type, 
# orders them by performance, and provides visual indications of agreement quality.

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization

#################################################################
# LOAD SUPPORT FILES AND DATA
#################################################################
# Load model mapping information
source("src/94_models_map.R")

# Load CCC results data
df_raw <- readRDS("data/clean/ccc_results.rds")

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
      full_name %in% openai_oss_models ~ "OpenAI",
      full_name %in% anthropic_models ~ "Anthropic",
      full_name %in% google_models ~ "Google",
      full_name %in% deepseek_models ~ "DeepSeek",
      full_name %in% openai_models ~ "OpenAI",
      TRUE ~ "Other"
    )

    # Get display name based on the full model name
    display_name <- case_when(
      full_name == "accounts/fireworks/models/qwen3-235b-a22b" ~ "Qwen3 235B-A22B",
      full_name == "accounts/fireworks/models/deepseek-v3p2" ~ "DeepSeek V3.2",
      full_name == "accounts/fireworks/models/deepseek-v4-flash" ~ "DeepSeek V4 Flash",
      full_name == "meta-llama/llama-4-scout-17b-16e-instruct" ~ "Llama 4 Scout",
      full_name == "qwen/qwen3-32b" ~ "Qwen3 32B",
      full_name == "openai/gpt-oss-20b" ~ "GPT-OSS 20B",
      full_name == "claude-haiku-4-5-20251001" ~ "Claude Haiku 4.5",
      full_name == "gemini-3.5-flash" ~ "Gemini 3.5 Flash",
      full_name == "gpt-5.6-luna" ~ "GPT-5.6 Luna",
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
# Enhance the CCC data with additional information for plotting
df <- df_raw %>%
  mutate(
    # Determine if model has open-source weights
    is_open_source = sapply(model, function(x) {
      if (grepl("^lsd_", x)) return(NA)
      return(is_open_source(x))
    }),
    
    # Classify models into categories
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is.na(is_open_source) ~ "Other",
      is_open_source == TRUE ~ "Open weights",
      is_open_source == FALSE ~ "Closed weights",
      TRUE ~ "Other"
    ),
    
    # Extract base model name, ensuring dictionary models are grouped together
    model_name = case_when(
      grepl("^lsd_", model) ~ "dict",  # Group all lsd_ models under "dict"
      TRUE ~ sub("_[a-z]{2}_[a-z]{2}$", "", model)
    ),
    
    # Get display name for the model using the mapping function
    display_name = unlist(sapply(model, get_model_display_name)),
    
    # Create a simple prompt technique code
    prompt_mechanism = case_when(
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "FR",
      grepl("^lsd_en", model) ~ "EN",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    # Create labels that include both model name and prompting mechanism
    model_label = case_when(
      grepl("^lsd_", model) ~ model,  # Keep original lsd_ model names for labels
      TRUE ~ paste0(display_name, " [", prompt_mechanism, "]")
    )
  )

#################################################################
# MODEL GROUPING AND ORDERING
#################################################################
# Calculate average CCC by model type for ordering
model_avg_ccc <- df %>%
  group_by(model_name) %>%
  summarize(avg_ccc = mean(abs_ccc, na.rm = TRUE)) %>%
  arrange(desc(avg_ccc))

# Create ordered factors for proper plot ordering
df <- df %>%
  mutate(
    # Order factor for model_name (for grouping)
    model_name_ordered = factor(model_name, levels = model_avg_ccc$model_name),
    
    # Assign group number to each unique model_name for alternating backgrounds
    model_group = as.numeric(factor(model_name, levels = model_avg_ccc$model_name)),
    
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
# CREATE CCC PLOT
#################################################################
# Create the CCC plot with professional styling
plot_ccc <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  
  # Points colored by model type
  geom_point(data = df, aes(
    x = ccc, 
    y = model_label_ordered,
    color = model_type
  ), size = 3.5) +
  
  # Add error bars (confidence intervals) with matching colors
  geom_errorbarh(data = df, aes(
    y = model_label_ordered,
    xmin = ccc_lower,
    xmax = ccc_upper,
    color = model_type
  ), height = 0.2) +
  
  # Theme with white background
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  
  # Labels and titles
  labs(x = "\nLin's Concordance Correlation Coefficient (CCC)\n",
       y = "",  # Remove y-axis label since we have direct labels
       title = "Sentiment Analysis Models: Lin's Concordance Correlation Coefficient",
       subtitle = "CCC values with 95% confidence intervals (measures both correlation and agreement)",
       caption = "Figure 3. Cross-lingual sentiment analysis performance using Lin's CCC.\nModels grouped by type (color) and sorted by CCC strength. Prompting mechanisms within clusters ordered as\nFR→FR (French prompt on French text), EN→FR (English prompt on French text), and EN→EN (English prompt on translated text).\nError bars represent 95% confidence intervals. CCC combines correlation and agreement into a single metric.") +
  
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
  
  # CCC value labels
  geom_text(data = df, aes(
    x = ifelse(ccc >= 0, ccc + 0.15, ccc - 0.15),
    y = model_label_ordered,
    label = sprintf("CCC = %.3f", ccc)
  ), hjust = ifelse(df$ccc >= 0, 0, 1), size = 3.5) +
  
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
print(plot_ccc)

# Save high-resolution version to file
ggsave("results/graphs/model_ccc.png", plot_ccc, width = 14, height = 12, dpi = 300)

#################################################################
# CREATE PRESENTATION-READY CCC PLOT (16:9 FORMAT)
#################################################################
# Create the CCC plot with professional styling optimized for 16:9 presentations
plot_ccc_16x9 <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.4) +
  
  # Points colored by model type
  geom_point(data = df, aes(
    x = ccc, 
    y = model_label_ordered,
    color = model_type
  ), size = 4) +
  
  # Add error bars (confidence intervals) with matching colors
  geom_errorbarh(data = df, aes(
    y = model_label_ordered,
    xmin = ccc_lower,
    xmax = ccc_upper,
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
    x = "Lin's Concordance Correlation Coefficient (CCC)",
    y = "",  # Remove y-axis label since we have direct labels
    title = "Sentiment Analysis Models: Lin's Concordance Correlation Coefficient",
    subtitle = "CCC values with 95% confidence intervals (measures both correlation and agreement)",
    caption = "Models grouped by type (color) and sorted by CCC strength. Prompting mechanisms within clusters ordered as\nFR→FR (French prompt on French text), EN→FR (English prompt on French text), and EN→EN (English prompt on translated text).\nError bars represent 95% confidence intervals. CCC combines correlation and agreement into a single metric."
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
  
  # CCC value labels - increase space for widescreen
  geom_text(data = df, aes(
    x = ifelse(ccc >= 0, ccc + 0.12, ccc - 0.12), 
    y = model_label_ordered,
    label = sprintf("CCC = %.3f", ccc)
  ), hjust = ifelse(df$ccc >= 0, 0, 1), size = 3.8) +
  
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
ggsave("results/graphs/model_ccc_16x9.png", 
       plot_ccc_16x9, 
       width = 16, 
       height = 9, 
       dpi = 300)
