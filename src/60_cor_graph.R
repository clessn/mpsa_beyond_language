library(dplyr)
library(ggplot2)
source("src/94_models_map.R")
df_raw <- readRDS("data/clean/cor_results.rds")

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
    # Extract base model name
    model_name = case_when(
      grepl("_en_fr$", model) ~ gsub("_en_fr$", "", model),
      grepl("_fr_fr$", model) ~ gsub("_fr_fr$", "", model),
      grepl("_en_en$", model) ~ gsub("_en_en$", "", model),
      grepl("^lsd_", model) ~ gsub("^lsd_", "dict_", model),
      TRUE ~ model
    ),
    # Create a simple prompt technique code
    lang_code = case_when(
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "FR",
      grepl("^lsd_en", model) ~ "EN",
      TRUE ~ ""
    ),
    # Create concise label
    concise_label = paste0(model_name, " [", lang_code, "]")
  )

# Create the correlation plot with open-source coloring and concise labels
plot_correlation <- ggplot(df, aes(x = correlation, y = reorder(concise_label, abs_correlation))) +
  # Points colored by open/closed source
  geom_point(aes(color = model_type), size = 3.5) +
  # Add error bars (95% confidence intervals) with matching colors
  geom_errorbarh(aes(xmin = correlation - 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
                     xmax = correlation + 1.96 * sqrt((1 - correlation^2) / (n_obs - 2)),
                     color = model_type),
                 height = 0.2) +
  # Theme with white background
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(x = "\nCorrelation with Ground Truth\n",
       y = "\nSentiment Analysis Model\n",
       title = "Sentiment Analysis Models Correlation with Ground Truth",
       subtitle = "Pearson correlation coefficients with 95% confidence intervals",
       caption = "Models sorted by correlation strength. EN→FR: English prompt on French text, FR→FR: French prompt on French text, EN→EN: English prompt on translated text") +
  # Scale and reference lines
  scale_x_continuous(limits = c(-1, 1)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  # Correlation value labels with significance stars only (no p-value numbers)
  # Increased offset to prevent overlap with error bars
  geom_text(aes(x = ifelse(correlation >= 0, 
                          correlation + 0.15, 
                          correlation - 0.15),
                label = sprintf("r = %.2f%s", 
                                correlation,
                                ifelse(p_value < 0.001, "***", 
                                       ifelse(p_value < 0.01, "**",
                                              ifelse(p_value < 0.05, "*", ""))))),
            hjust = ifelse(df$correlation >= 0, 0, 1),
            size = 3.5) +
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
        axis.title.y = element_text(hjust = 0.5, size = 12),
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
