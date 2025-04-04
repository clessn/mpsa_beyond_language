#################################################################
# F-SCORE VISUALIZATION AND COMPARISON
#################################################################
# This script creates publication-quality visualizations comparing F1 scores
# for sentiment analysis models using both 7-category and 3-category schemes.
# It organizes models by performance and groups them for easier comparison.

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(tidyr)      # For data reshaping

#################################################################
# LOAD SUPPORT FILES AND DATA
#################################################################
# Source the model mapping file with model information
source("src/94_models_map.R")

# Read in the F1 score data for both categorization schemes
df_7 <- readRDS("results/analysis/f1_scores_7.rds")
df_3 <- readRDS("results/analysis/f1_scores_3.rds")

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
      full_name == "accounts/fireworks/models/qwq-32b" ~ "QwQ 32B",
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
# Process dataframes and add necessary columns for visualization
process_dataframe <- function(df, category_label) {
  df %>%
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
      
      # Extract base model name
      model_name = case_when(
        grepl("^lsd_", model) ~ "dict",  # Group all lsd_ models under "dict"
        TRUE ~ sapply(model, function(m) {
          # Extract base model name without language suffix
          sub("_[a-z]{2}_[a-z]{2}$", "", m)
        })
      ),
      
      # Create a simple prompt technique code
      prompt_mechanism = case_when(
        grepl("_fr_fr$", model) ~ "FR→FR",
        grepl("_en_fr$", model) ~ "EN→FR",
        grepl("_en_en$", model) ~ "EN→EN",
        grepl("^lsd_fr", model) ~ "FR",
        grepl("^lsd_en", model) ~ "EN",
        TRUE ~ "Other"
      ),
      
      # Create a sortable key that encodes both model and prompt
      sort_key = case_when(
        # For dictionary models, preserve original order
        grepl("^lsd_", model) ~ model,
        # For regular models: create a compound key with model_name first, then prompt code
        # The prompt code is prefixed to ensure desired ordering:
        # A: EN→FR first
        # B: FR→FR second  
        # C: EN→EN third
        grepl("_en_fr$", model) ~ paste0(sub("_[a-z]{2}_[a-z]{2}$", "", model), "_A"),
        grepl("_fr_fr$", model) ~ paste0(sub("_[a-z]{2}_[a-z]{2}$", "", model), "_B"),
        grepl("_en_en$", model) ~ paste0(sub("_[a-z]{2}_[a-z]{2}$", "", model), "_C"),
        TRUE ~ model # Fallback
      ),
      
      # Get display name for the model using the mapping function
      display_name = sapply(model, get_model_display_name),
      
      # Create labels that include both model name and prompting mechanism
      model_label = case_when(
        grepl("^lsd_", model) ~ model,  # Keep original lsd_ model names for labels
        TRUE ~ paste0(display_name, " [", prompt_mechanism, "]")
      ),
      
      # Add category type
      category_type = category_label
    )
}

# Process both dataframes
df_7 <- process_dataframe(df_7, "Detailed (7-category)")
df_3 <- process_dataframe(df_3, "Grouped (3-category)")

#################################################################
# MODEL PERFORMANCE GROUPING AND ORDERING
#################################################################
# Find the maximum 3-category F1 score for each model for sorting
best_model_scores <- df_3 %>%
  group_by(model_name) %>%
  summarize(
    best_f1 = max(weighted_f1, na.rm = TRUE),
    # Which variant had the best score
    best_variant = model[which.max(weighted_f1)]
  ) %>%
  # Sort by best F1 score in descending order (highest scores first)
  arrange(desc(best_f1))

# Create model ordering groups based on best 3-category performance
model_groups <- best_model_scores %>%
  mutate(model_group = row_number()) %>%
  select(model_name, model_group, best_f1)

# Debug output to confirm order
print("Models ordered by best 3-category F1 score (descending):")
print(model_groups)

#################################################################
# COMBINE AND FORMAT DATA FOR VISUALIZATION
#################################################################
# Combine both datasets and select only needed columns
df_combined <- bind_rows(
  df_7 %>% select(model, model_name, model_label, weighted_f1, category_type, prompt_mechanism, sort_key),
  df_3 %>% select(model, model_name, model_label, weighted_f1, category_type, prompt_mechanism, sort_key)
)

# Join to add model grouping information
df_combined <- df_combined %>%
  left_join(model_groups, by = "model_name")

# Create a manual ordering for the factor levels
model_label_ordered <- df_combined %>%
  select(model_label, model_name, model_group, sort_key) %>%
  distinct() %>%
  # Sort by: 1) model_group (performance), 2) sort_key (custom order)
  arrange(model_group, sort_key) %>%
  pull(model_label)

# Print the order to verify
print("Checking the final order of model labels (should have EN→FR, FR→FR, EN→EN pattern):")
model_check <- df_combined %>%
  select(model_label, prompt_mechanism, sort_key) %>%
  distinct() %>%
  arrange(sort_key)
print(head(model_check, 20))

# Reverse the order for the plot (best performers at the top)
model_labels_ordered <- rev(model_label_ordered)

# Explicitly check the final order (after reversing)
label_check <- data.frame(
  model_label = model_labels_ordered,
  position = 1:length(model_labels_ordered)
) %>%
  left_join(df_combined %>% select(model_label, prompt_mechanism) %>% distinct(), by = "model_label")

print("\nFinal display order (top to bottom):")
print(head(label_check, 20))

# Get unique model labels for each model name, maintaining order
model_label_mapping <- df_combined %>%
  select(model_label, model_name, model_group, sort_key, prompt_mechanism) %>%
  distinct()

# Apply factor levels to model_label with custom ordering
df_combined$model_label <- factor(df_combined$model_label, levels = model_labels_ordered)

#################################################################
# CREATE BACKGROUND RECTANGLES FOR VISUAL GROUPING
#################################################################
# Get the positions of each model label in the final plot
total_labels <- length(levels(df_combined$model_label))
label_positions <- data.frame(
  model_label = levels(df_combined$model_label),
  position = total_labels:1  # Reverse because we've reversed the factor levels
)

# Join with model information
label_positions <- label_positions %>%
  left_join(model_label_mapping %>% select(model_label, model_name, model_group, prompt_mechanism), by = "model_label")

# Print a sample to verify prompt ordering
print("\nVerifying final positioning (should see EN→FR at top of each model group):")
print(label_positions %>% arrange(model_name, desc(position)) %>% head(20))

# Create background rectangle bounds for each model_name group
bg_rects <- label_positions %>%
  group_by(model_name) %>%
  summarize(
    ymin = min(position) - 0.5,
    ymax = max(position) + 0.5,
    model_group = first(model_group)
  ) %>%
  # Add alternating shade pattern
  arrange(model_group) %>%
  mutate(shade = ifelse(row_number() %% 2 == 1, "even", "odd")) %>%
  # Add best score info
  left_join(best_model_scores %>% select(model_name, best_f1), by = "model_name")

#################################################################
# CREATE F1 SCORE COMPARISON PLOT
#################################################################
# Set expansion factor to minimize empty space
y_expansion <- c(0.01, 0.01)

# Professional grayscale palette for category types
category_colors <- c(
  "Detailed (7-category)" = "#777777",  # Medium gray
  "Grouped (3-category)" = "#BBBBBB"    # Light gray
)

# Create the enhanced F1 score plot
plot_f1 <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  
  # Add bars with grouped categories
  geom_bar(data = df_combined, aes(
    x = weighted_f1,
    y = model_label,
    fill = category_type
  ), 
  stat = "identity", 
  position = position_dodge(width = 0.9), 
  width = 0.6,
  color = "#444444",
  linewidth = 0.2) +
  
  # Professional minimal theme with more space between elements
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),    # Remove horizontal grid lines
    panel.grid.minor = element_blank(),      # Remove all minor grid lines
    panel.grid.major.x = element_line(color = "gray95"),  # Even more subtle vertical grid lines
    axis.line.x = element_line(color = "#555555", size = 0.4),  # Softer axis lines
    axis.line.y = element_line(color = "#555555", size = 0.4),
    panel.spacing = unit(1.5, "lines"),     # Increase spacing between panel elements
    axis.ticks = element_line(color = "#555555", size = 0.4)  # Matching tick marks
  ) +
  
  # Labels and titles
  labs(
    x = "Weighted F1 Score",
    y = "",  # Remove y-axis label since we have direct labels
    title = "Performance Comparison of Sentiment Analysis Models",
    subtitle = "F1 scores for both detailed and grouped sentiment classification schemes (sorted by 3-category performance)",
    caption = "Figure 2. F1 scores for sentiment analysis across model architectures.\nModels grouped by F1 score performance and sorted by 3-category classification. Within each model group, prompting mechanisms are\nordered as EN→FR (English prompt on French text), FR→FR (French prompt on French text), and EN→EN (English prompt on translated text).\nDetailed classification includes 7 distinct sentiment categories, while grouped classification uses 3 categories (Positive, Negative, Neutral)."
  ) +
  
  # Scale and reference lines
  scale_x_continuous(limits = c(0, 1)) +
  
  # Control y-axis expansion to eliminate wasted space
  scale_y_discrete(expand = y_expansion) +
  
  # Create a separate scale for fill that correctly handles both backgrounds and bars
  scale_fill_manual(
    values = c(
      "even" = "gray95",    # Almost white (for background)
      "odd" = "gray90",     # Very light gray (for background)
      "Detailed (7-category)" = "#777777",  # Medium gray (for bars)
      "Grouped (3-category)" = "#BBBBBB"    # Light gray (for bars)
    ),
    # Only include classification types in the legend
    breaks = c("Detailed (7-category)", "Grouped (3-category)"),
    name = "Classification Type"
  ) +
  
  # Hide the background shades from the legend, only show classification types
  guides(fill = guide_legend(title = "Classification Type")) +
  
  # Add F1 score value labels
  geom_text(data = df_combined, aes(
    label = sprintf("%.2f", weighted_f1),
    x = weighted_f1 + 0.01,
    y = model_label,
    group = category_type
  ), 
  position = position_dodge(width = 0.9), 
  size = 2.8, 
  hjust = 0,
  color = "#444444") +
  
  # Scientific journal style formatting
  theme(
    plot.caption.position = "plot",
    axis.title.x = element_text(hjust = 0.5, size = 10, face = "plain", color = "#333333"),
    axis.text.x = element_text(size = 9, color = "#333333"),
    axis.text.y = element_text(size = 9, face = "plain", color = "#333333", margin = margin(r = 10)),
    plot.title = element_text(size = 12, face = "bold", hjust = 0, color = "#333333"),
    plot.subtitle = element_text(size = 10, hjust = 0, face = "plain", color = "#333333"),
    plot.caption = element_text(size = 8, hjust = 0, face = "italic", color = "#555555"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "plain", color = "#333333"),
    legend.text = element_text(size = 8, face = "plain", color = "#333333"),
    legend.spacing.x = unit(0.5, "cm"),  # Add more space between legend items
    legend.margin = margin(t = 10),      # Add top margin to legend
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.8, "cm"),   # Slightly larger legend keys
    plot.margin = unit(c(1, 1.2, 1, 1), "cm"),  # Slightly larger right margin
    # Ensure the plot uses all available space
    aspect.ratio = NULL
  )

#################################################################
# DISPLAY AND SAVE VISUALIZATIONS
#################################################################
# Display the plot
print(plot_f1)

# Save the plot with publication quality settings
ggsave("results/graphs/model_f1_scores_comparison_grouped.png", 
       plot_f1, 
       width = 16,      # Width in inches
       height = 9,      # Height in inches
       dpi = 300,       # High DPI for publication quality
       units = "in",
       limitsize = FALSE) # Prevent R from warning about large dimensions

#################################################################
# CREATE PUBLICATION-OPTIMIZED VERSION
#################################################################
# Create a publication version optimized for letter-sized landscape paper
plot_f1_pub <- ggplot() +
  # Add alternating background for visual grouping
  geom_rect(data = bg_rects, aes(
    xmin = -Inf, xmax = Inf,
    ymin = ymin,
    ymax = ymax,
    fill = shade
  ), alpha = 0.5) +
  
  # Add bars with grouped categories
  geom_bar(data = df_combined, aes(
    x = weighted_f1,
    y = model_label,
    fill = category_type
  ), 
  stat = "identity", 
  position = position_dodge(width = 0.9), 
  width = 0.6,
  color = "#444444",
  linewidth = 0.2) +
  
  # Professional minimal theme
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),    # Remove horizontal grid lines
    panel.grid.minor = element_blank(),      # Remove all minor grid lines
    panel.grid.major.x = element_line(color = "gray95"),  # Even more subtle vertical grid lines
    axis.line.x = element_line(color = "#555555", size = 0.4),  # Softer axis lines
    axis.line.y = element_line(color = "#555555", size = 0.4),
    panel.spacing = unit(1.2, "lines"),     # Slightly reduced spacing for letter size
    axis.ticks = element_line(color = "#555555", size = 0.4)  # Matching tick marks
  ) +
  
  # Condensed labels and titles
  labs(
    x = "Weighted F1 Score",
    y = "",  # Remove y-axis label since we have direct labels
    title = "Performance Comparison of Sentiment Analysis Models",
    subtitle = "F1 scores for both detailed and grouped sentiment classification schemes",
    caption = "Figure 2. F1 scores for sentiment analysis across model architectures. Models grouped by F1 score performance and sorted by\n3-category classification. Prompting mechanisms: EN→FR (English prompt on French text), FR→FR (French prompt on French text),\nand EN→EN (English prompt on translated text). Detailed classification includes 7 categories, grouped classification uses 3."
  ) +
  
  # Scale and reference lines
  scale_x_continuous(limits = c(0, 1)) +
  
  # Control y-axis expansion to eliminate wasted space
  scale_y_discrete(expand = y_expansion) +
  
  # Create a separate scale for fill
  scale_fill_manual(
    values = c(
      "even" = "gray95",    # Almost white (for background)
      "odd" = "gray90",     # Very light gray (for background)
      "Detailed (7-category)" = "#777777",  # Medium gray (for bars)
      "Grouped (3-category)" = "#BBBBBB"    # Light gray (for bars)
    ),
    # Only include classification types in the legend
    breaks = c("Detailed (7-category)", "Grouped (3-category)"),
    name = "Classification Type"
  ) +
  
  # Hide the background shades from the legend
  guides(fill = guide_legend(title = "Classification Type")) +
  
  # Add F1 score value labels
  geom_text(data = df_combined, aes(
    label = sprintf("%.2f", weighted_f1),
    x = weighted_f1 + 0.01,
    y = model_label,
    group = category_type
  ), 
  position = position_dodge(width = 0.9), 
  size = 2.3,  # Slightly smaller text for letter size
  hjust = 0,
  color = "#444444") +
  
  # Publication-optimized styling for letter size
  theme(
    plot.caption.position = "plot",
    axis.title.x = element_text(hjust = 0.5, size = 9, face = "plain", color = "#333333"),
    axis.text.x = element_text(size = 8, color = "#333333"),
    axis.text.y = element_text(size = 7.5, face = "plain", color = "#333333", margin = margin(r = 5)),
    plot.title = element_text(size = 11, face = "bold", hjust = 0, color = "#333333"),
    plot.subtitle = element_text(size = 9, hjust = 0, face = "plain", color = "#333333"),
    plot.caption = element_text(size = 7, hjust = 0, face = "italic", color = "#555555"),
    legend.position = "bottom",
    legend.title = element_text(size = 8, face = "plain", color = "#333333"),
    legend.text = element_text(size = 7, face = "plain", color = "#333333"),
    legend.spacing.x = unit(0.4, "cm"),  # Slightly reduced spacing for letter size
    legend.margin = margin(t = 5),      # Reduced top margin for letter size
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.6, "cm"),   # Smaller legend keys for letter size
    plot.margin = unit(c(0.7, 0.9, 0.7, 0.7), "cm"),  # Adjusted margins for letter size
    # Ensure the plot uses all available space
    aspect.ratio = NULL
  )

# Save a landscape letter-sized version for publication
# US Letter landscape: 11×8.5 inches
ggsave("results/graphs/model_f1_scores_comparison_grouped_pub.png", 
       plot_f1_pub,  # Use the publication-optimized version
       width = 11,      # Width in inches - landscape letter
       height = 8.5,    # Height in inches - landscape letter
       dpi = 300,       # High DPI for publication quality
       units = "in",
       limitsize = FALSE)