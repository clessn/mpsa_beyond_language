#################################################################
# HTML PARSING SCRIPT
#################################################################
# This script processes news articles from multiple folders containing HTML files
# collected from the Eureka database. It extracts structured data from the HTML
# content and combines it into a unified dataset.

# Load required libraries
library(rvest)      # For HTML parsing
library(dplyr)      # For data manipulation
library(stringr)    # For string operations
library(purrr)      # For functional programming operations
library(readr)      # For reading/writing data

# Source the custom parser script with specific parsing functions
source("src/99_parser.R")

#################################################################
# DATA SOURCE CONFIGURATION
#################################################################
# Define paths to folders containing HTML files organized by time periods
folder_paths <- c(
  "eureka_articles/1991-01-01_2005-05-13",
  "eureka_articles/2005-05-14_2013-09-04",
  "eureka_articles/2013-09-05_2025_01_01"
)

#################################################################
# PROCESS HTML FILES
#################################################################
# Process all folders and combine results
# The 'process_multiple_folders' function is defined in the 99_parser.R file
news_df <- process_multiple_folders(
  folder_paths,
  output_csv = "combined_news_articles.csv",
  method = "auto"  # Automatically determine the best parsing method
)

#################################################################
# SOURCE MEDIA SUMMARY ANALYSIS
#################################################################
# Create and display a summary of the results by source media
summary_by_source <- news_df %>%
  group_by(source_media) %>%
  summarize(
    article_count = n(),
    earliest_date = min(publication_date, na.rm = TRUE),
    latest_date = max(publication_date, na.rm = TRUE)
  ) %>%
  arrange(desc(article_count))

print(summary_by_source)

#################################################################
# TIME-BASED ANALYSIS (OPTIONAL)
#################################################################
# This section performs temporal analysis if the lubridate package is available
if (requireNamespace("lubridate", quietly = TRUE)) {
  library(lubridate)
  
  # Convert publication dates to proper Date objects
  news_df <- news_df %>%
    mutate(date = ymd(publication_date))
  
  # Analyze article distribution by year and month
  time_analysis <- news_df %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    group_by(year, month) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(year, month)
  
  # Display sample of temporal distribution
  print(head(time_analysis, 10))
  
  #################################################################
  # VISUALIZATION (IF GGPLOT2 AVAILABLE)
  #################################################################
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
    
    # Display the plot
    print(p)
    
    # Save plot to file for future reference
    ggsave("articles_by_month.png", p, width = 10, height = 6)
  }
}

#################################################################
# DATA VERIFICATION AND COMPLETION
#################################################################
# Display the first few rows of the parsed data to verify structure
print(head(news_df))

# Confirmation message
cat("Processing complete. Results saved to 'combined_news_articles.csv'\n")