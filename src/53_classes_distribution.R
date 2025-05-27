#################################################################
# GROUND TRUTH SENTIMENT CLASSES DISTRIBUTION VISUALIZATION
#################################################################
# This script creates a publication-quality visualization of the distribution
# of ground truth sentiment classes in the dataset used for model evaluation.

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization

#################################################################
# LOAD DATA
#################################################################
# Load the F-scores dataset which contains ground truth labels
df <- readRDS("data/clean/df_fscores.rds")

#################################################################
# CREATE DISTRIBUTION PLOT
#################################################################
# Create the distribution plot with professional styling
plot_distribution <- ggplot(df, aes(x = ground_truth)) +
  # Create proportional bars with professional color scheme
  geom_bar(aes(y = ..count../sum(..count..)), 
           fill = "#777777",        # Medium gray consistent with other plots
           color = "#444444",       # Darker border
           linewidth = 0.3,
           alpha = 0.8) +
  
  # Professional minimal theme with white background
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "gray95"),  # Subtle horizontal grid lines
    panel.grid.minor = element_blank(),      # Remove all minor grid lines
    panel.grid.major.x = element_blank(),    # Remove vertical grid lines for bar charts
    axis.line.x = element_line(color = "#555555", size = 0.4),  # Softer axis lines
    axis.line.y = element_line(color = "#555555", size = 0.4),
    axis.ticks = element_line(color = "#555555", size = 0.4)  # Matching tick marks
  ) +
  
  # Labels and titles with academic style
  labs(
    title = "Distribution of Ground Truth Sentiment Classes",
    subtitle = "Proportion of each sentiment category in the evaluation dataset",
    x = "Sentiment Class",
    y = "Proportion",
    caption = "Figure 3. Distribution of human-annotated sentiment classes in the evaluation corpus.\nShows the relative frequency of each sentiment category used as ground truth for model performance assessment."
  ) +
  
  # Scale formatting
  scale_y_continuous(
    labels = scales::percent,
    expand = c(0, 0, 0.05, 0)  # Remove bottom padding, small top padding
  ) +
  
  # Rotate x-axis labels for readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#333333"),
    axis.text.y = element_text(size = 9, color = "#333333"),
    axis.title.x = element_text(hjust = 0.5, size = 10, color = "#333333"),
    axis.title.y = element_text(hjust = 0.5, size = 10, color = "#333333"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0, color = "#333333"),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "#333333"),
    plot.caption = element_text(size = 8, hjust = 0, face = "italic", color = "#555555"),
    plot.caption.position = "plot",
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

#################################################################
# DISPLAY AND SAVE VISUALIZATION
#################################################################
# Display the plot
print(plot_distribution)

# Save high-resolution version to file
ggsave("results/graphs/ground_truth_distribution.png", 
       plot_distribution, 
       width = 10, 
       height = 6, 
       dpi = 300,
       units = "in")
