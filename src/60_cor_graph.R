library(dplyr)
library(ggplot2)
source("src/94_models_map.R")
df_raw <- readRDS("data/clean/cor_results.rds")

# Function to get model display name from the model_mapping
get_model_display_name <- function(model_name) {
  # Check if it's a dictionary model
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

# Add open source classification to the plot_results data frame
df <- df_raw %>%
  mutate(
    is_open_source = sapply(model, is_open_source),
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is.na(is_open_source) ~ "Other",
      is_open_source ~ "Open weights",
      !is_open_source ~ "Closed weights"
    ),
    # Extract base model name, ensuring lsd_ models are grouped together
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

# Calculate average correlation by model_name for ordering the groups
model_avg_cors <- df %>%
  group_by(model_name) %>%
  summarize(avg_correlation = mean(abs_correlation, na.rm = TRUE)) %>%
  arrange(desc(avg_correlation))

# Create ordered factors for both model_name and model_label
df <- df %>%
  mutate(
    # Order factor for model_name (for grouping)
    model_name_ordered = factor(model_name, levels = model_avg_cors$model_name),
    # Assign group number to each unique model_name for alternating backgrounds
    model_group = as.numeric(factor(model_name, levels = model_avg_cors$model_name)),
    # Create a composite ordering value for the y-axis that keeps models from the same family together
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

# Create a data frame for the group background rectangles with alternating shades
# Get unique model groups in the order they will appear
model_groups <- df %>%
  select(model_name, model_group) %>%
  distinct() %>%
  arrange(model_group)

# Get the total number of rows in the dataframe
total_rows <- nrow(df)

# Create background rectangles data
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

# Create display names for model groups (to handle the "dict" group)
model_groups <- model_groups %>%
  mutate(display_name = case_when(
    model_name == "dict" ~ "Dictionary-based",
    TRUE ~ model_name
  ))

# Create the correlation plot with simplified visual elements
plot_correlation <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  # Points colored by model type (no shape variation)
  geom_point(data = df, aes(
    x = correlation, 
    y = model_label_ordered,
    color = model_type
  ), size = 3.5) +
  # Add error bars with matching colors
  geom_errorbarh(data = df, aes(
    x = correlation,
    y = model_label_ordered,
    xmin = correlation - 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    xmax = correlation + 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
    color = model_type
  ), height = 0.2) +
  # REMOVED: The geom_text() block for model group labels
  
  # Theme with white background
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank()) +
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
  # Correlation value labels with significance stars only
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

print(plot_correlation)
# Save the plot
ggsave("results/graphs/model_correlation.png", plot_correlation, width = 14, height = 12, dpi = 300)
