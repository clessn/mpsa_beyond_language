# Parses the Eureka HTML files into one table of articles (date, source,
# title, text) with the functions in src/90_html_parser_fun.R.
#
# In:  data/eureka_articles/ (licensed, not shared)
# Out: combined_news_articles.csv (then moved to data/raw/)

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

source("src/99_parser.R")

folder_paths <- c(
  "eureka_articles/1991-01-01_2005-05-13",
  "eureka_articles/2005-05-14_2013-09-04",
  "eureka_articles/2013-09-05_2025_01_01"
)

news_df <- process_multiple_folders(
  folder_paths,
  output_csv = "combined_news_articles.csv",
  method = "auto"  # Automatically determine the best parsing method
)

summary_by_source <- news_df %>%
  group_by(source_media) %>%
  summarize(
    article_count = n(),
    earliest_date = min(publication_date, na.rm = TRUE),
    latest_date = max(publication_date, na.rm = TRUE)
  ) %>%
  arrange(desc(article_count))

print(summary_by_source)

if (requireNamespace("lubridate", quietly = TRUE)) {
  library(lubridate)

  news_df <- news_df %>%
    mutate(date = ymd(publication_date))

  time_analysis <- news_df %>%
    mutate(
      year = year(date),
      month = month(date)
    ) %>%
    group_by(year, month) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(year, month)

  print(head(time_analysis, 10))

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)

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

    print(p)

    ggsave("articles_by_month.png", p, width = 10, height = 6)
  }
}

print(head(news_df))

cat("Processing complete. Results saved to 'combined_news_articles.csv'\n")
