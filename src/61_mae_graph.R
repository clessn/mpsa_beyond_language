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

# For MAE, we want to group by average MAE (LOWER is better)
model_avg_mae <- df %>%
  group_by(model_name) %>%
  summarize(avg_mae = mean(mae, na.rm = TRUE)) %>%
  arrange(avg_mae)  # Arrange from lowest (best) to highest MAE

# Create ordered factors for both model_name and model_label based on MAE
df <- df %>%
  mutate(
    # Order factor for model_name (for grouping) - lowest MAE first
    model_name_ordered = factor(model_name, levels = model_avg_mae$model_name),
    # Assign group number to each unique model_name for alternating backgrounds
    model_group = as.numeric(factor(model_name, levels = model_avg_mae$model_name)),
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
  # Final ordering for plot - lowest MAE at top (which is reverse of the ordering)
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

# Create the MAE plot with simplified visual elements (similar to correlation plot)
plot_mae <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  # Points colored by model type (no shape variation)
  geom_point(data = df, aes(
    x = mae, 
    y = model_label_ordered,
    color = model_type
  ), size = 3.5) +
  # No error bars for MAE as it's a direct calculation, not a statistical inference
  
  # Theme with white background
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
        panel.grid.minor.y = element_blank()) +
  labs(x = "\nMean Absolute Error (MAE)\n",
       y = "",  # Remove y-axis label since we have direct labels
       title = "Sentiment Analysis Models Mean Absolute Error",
       subtitle = "Lower values indicate better performance",
       caption = "Figure 2. Cross-lingual sentiment analysis performance by Mean Absolute Error.\nModels grouped by type (color) and sorted by MAE (lower is better). Prompting mechanisms within clusters ordered as\nFR→FR (French prompt on French text), EN→FR (English prompt on French text), and EN→EN (English prompt on translated text).") +
  # Scale and reference lines - adaptive limits for MAE
  scale_x_continuous(limits = c(0, max(df$mae) * 1.2)) +
  # Fill scale for alternating backgrounds
  scale_fill_manual(values = c(
    "even" = "gray95",  # Almost white
    "odd" = "gray90"    # Very light gray
  )) +
  # Hide the fill legend (used only for background grouping)
  guides(fill = "none") +
  # MAE value labels
  geom_text(data = df, aes(
    x = mae + max(df$mae) * 0.05,  # Add some padding to the right of points
    y = model_label_ordered,
    label = sprintf("MAE = %.3f", mae)
  ), hjust = 0, size = 3.5) +
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

print(plot_mae)

# Save the plot
ggsave("results/graphs/model_mae.png", plot_mae, width = 14, height = 12, dpi = 300)
