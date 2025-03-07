# Example script for processing news articles from multiple folders
# Save this as a separate file (e.g., "process_news.R")

# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Source the parser script
source("src/99_parser.R")

# Define paths to your folders containing HTML files
folder_paths <- c(
  "eureka_articles/1991-01-01_2005-05-13",
  "eureka_articles/2005-05-14_2013-09-04",
  "eureka_articles/2013-09-05_2025_01_01"
)

# Process all folders and combine results
news_df <- process_multiple_folders(
  folder_paths,
  output_csv = "combined_news_articles.csv",
  method = "auto"
)

# Display a summary of the results
summary_by_source <- news_df %>%
  group_by(source_media) %>%
  summarize(
    article_count = n(),
    earliest_date = min(publication_date, na.rm = TRUE),
    latest_date = max(publication_date, na.rm = TRUE)
  ) %>%
  arrange(desc(article_count))

print(summary_by_source)

# Optional: Convert dates to Date objects and analyze by time period
if (requireNamespace("lubridate", quietly = TRUE)) {
  library(lubridate)
  
  # Convert publication_date to Date objects
  news_df <- news_df %>%
    mutate(date = ymd(publication_date))
  
  # Count articles by year and month
  time_analysis <- news_df %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    group_by(year, month) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(year, month)
  
  print(head(time_analysis, 10))
  
  # Optional: create a plot if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    
    # Create time series plot
    p <- ggplot(time_analysis, aes(x = paste(year, month, sep = "-"), y = count)) +
      geom_line(group = 1) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(
        title = "Number of News Articles by Month",
        x = "Year-Month",
        y = "Article Count"
      )
    
    # Display plot
    print(p)
    
    # Save plot to file
    ggsave("articles_by_month.png", p, width = 10, height = 6)
  }
}

# Display the first few rows of the parsed data
print(head(news_df))

cat("Processing complete. Results saved to 'combined_news_articles.csv'\n")
