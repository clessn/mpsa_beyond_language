library(dplyr)
library(ggplot2)
library(tidyr)

# Source the model mapping file
source("src/94_models_map.R")

# Read in the data
df_7 <- readRDS("results/fscores/f1_scores_7.rds")
df_3 <- readRDS("results/fscores/f1_scores_3.rds")

# Define the get_model_display_name function
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

# Process dataframes and add necessary columns
process_dataframe <- function(df, category_label) {
  df %>%
    mutate(
      is_open_source = sapply(model, is_open_source),
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
      # IMPORTANT: For ordering, create a sortable key that encodes both model and prompt
      # We'll sort directly on this composite key
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

# Extract model performance data for sorting
# Find the maximum 3-category F1 score for each model
best_model_scores <- df_3 %>%
  group_by(model_name) %>%
  summarize(
    best_f1 = max(weighted_f1, na.rm = TRUE),
    # Optional: Which variant had the best score
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

# Combine and reshape data for visualization
df_combined <- bind_rows(
  df_7 %>% select(model, model_name, model_label, weighted_f1, category_type, prompt_mechanism, sort_key),
  df_3 %>% select(model, model_name, model_label, weighted_f1, category_type, prompt_mechanism, sort_key)
)

# Join to add model grouping information
df_combined <- df_combined %>%
  left_join(model_groups, by = "model_name")

# COMPLETELY NEW APPROACH: Create a manual ordering for the factor levels
# First, get all the unique model labels organized by their sort_key within model_groups
model_label_ordered <- df_combined %>%
  select(model_label, model_name, model_group, sort_key) %>%
  distinct() %>%
  # Sort by: 1) model_group (performance), 2) sort_key (our custom order)
  arrange(model_group, sort_key) %>%
  pull(model_label)

# Print the order to verify
print("Checking the final order of model labels (should have EN→FR, FR→FR, EN→EN pattern):")
model_check <- df_combined %>%
  select(model_label, prompt_mechanism, sort_key) %>%
  distinct() %>%
  arrange(sort_key)
print(head(model_check, 20))

# Now reverse the order for the plot (since we want best at top)
model_labels_ordered <- rev(model_label_ordered)

# Explicitly check the final order (after reversing)
label_check <- data.frame(
  model_label = model_labels_ordered,
  position = 1:length(model_labels_ordered)
) %>%
  left_join(df_combined %>% select(model_label, prompt_mechanism) %>% distinct(), by = "model_label")

print("\nFinal display order (top to bottom):")
print(head(label_check, 20))

# Get unique model labels for each model name, maintaining the model_group order and custom sort order
model_label_mapping <- df_combined %>%
  select(model_label, model_name, model_group, sort_key, prompt_mechanism) %>%
  distinct()

# Apply factor levels to model_label with our custom ordering
df_combined$model_label <- factor(df_combined$model_label, levels = model_labels_ordered)

# Create background rectangles for alternating groups
# First get the positions of each model label in the final plot
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
  # but only shows classification types in the legend
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
  # Add F1 score value labels with softer styling
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
  # Scientific journal style formatting with more relaxed styling
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

# Print the plot
print(plot_f1)

# Save the plot with publication quality settings
ggsave("results/graphs/model_f1_scores_comparison_grouped.png", 
       plot_f1, 
       width = 16,      # Width in inches
       height = 9,      # Height in inches
       dpi = 300,       # High DPI for publication quality
       units = "in",
       limitsize = FALSE) # Prevent R from warning about large dimensions
