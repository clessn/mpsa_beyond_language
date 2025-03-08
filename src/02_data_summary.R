library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(lubridate)

# Read data
news_df <- read.csv("data/raw/combined_news_articles.csv") %>%
  filter(!is.na(source_media)) %>%
  filter(!is.na(publication_date)) %>%
  rename(date = publication_date) %>%
  mutate(date = ymd(date)) 

# Display summary by source
table(news_df$source_media, useNA = "ifany")

# Create summary by source
summary_by_source <- news_df %>%
  group_by(source_media) %>%
  summarize(
    article_count = n(),
    earliest_date = min(date, na.rm = TRUE),
    latest_date = max(date, na.rm = TRUE)
  ) %>%
  arrange(desc(article_count))
print(summary_by_source)

# Create the time_analysis data frame  
time_analysis <- news_df %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(year, month)

# Add the proper date column for plotting
time_analysis <- time_analysis %>%
  mutate(date = ymd(paste(year, month, "01", sep = "-")))

# Create professional time series plot
p <- ggplot(time_analysis, aes(x = date, y = count)) +
  # Add gridlines but make them light
  geom_hline(yintercept = seq(0, max(time_analysis$count, na.rm = TRUE) + 10, 
                              by = 10), color = "gray90") +
  # Change to yearly vertical lines
  geom_vline(xintercept = as.numeric(seq(as.Date(paste0(min(time_analysis$year), "-01-01")), 
                                         as.Date(paste0(max(time_analysis$year)+1, "-01-01")), 
                                         by = "1 year")), 
             color = "gray90") +
  # Add the smoothed trend line with confidence interval
  geom_smooth(method = "loess", span = 0.3, color = "#1F78B4", fill = "#A6CEE3", 
              alpha = 0.2, linewidth = 1) +
  # Add the original data with connected lines
  geom_line(color = "#525252", linewidth = 0.6, alpha = 0.8) +
  # Add points with a white fill for a cleaner look
  geom_point(color = "#252525", fill = "white", shape = 21, size = 2.5, stroke = 0.8) +
  # Set elegant theme
  clessnize::theme_clean_light() +
  # Custom theme elements
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 15)),
    plot.subtitle = element_text(size = 12, color = "#636363", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#636363", hjust = 0, margin = margin(t = 15)),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "#252525"),
    # Changed to 45 degrees with proper justification
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Show only years on x-axis
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = c(0, 0.05)),
    labels = comma
  ) +
  # Descriptive labels
  labs(
    title = "Temporal Distribution of News Articles Discussing Open Source Software",
    subtitle = "Monthly article about open-source count with trend line (LOESS smoothing)",
    x = "",
    y = "Number of Articles",
    caption = "Data collected from Eureka."
  )
# Display the plot
print(p)

ggsave("results/graphs/time_series.png", p, width = 16, height = 9, dpi = 300)


