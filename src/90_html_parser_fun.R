# Functions to parse Eureka article pages (metadata and full text), used by
# src/01_html_parsing.R.

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Function to parse a single HTML file
parse_news_article <- function(file_path) {
  tryCatch({
    html_content <- read_file(file_path)

    doc <- read_html(html_content)

    source_media_raw <- doc %>%
      html_node(".DocPublicationName") %>%
      html_text() %>%
      str_trim()

    source_media <- clean_source_media(source_media_raw)

    # For debugging, output the raw text and the matched source
    # cat("Raw source:", source_media_raw, "| Matched to:", source_media, "\n")

    date_text <- doc %>%
      html_node(".DocHeader") %>%
      html_text() %>%
      str_trim()

    # Parse date from the text
    # The format in the example is "vendredi 23 janvier 1998"
    date_match <- str_extract(date_text, "[[:alpha:]]+ [0-9]+ [[:alpha:]]+ [0-9]{4}")

    publication_date <- convert_french_date(date_match)

    title <- doc %>%
      html_node(".titreArticleVisu") %>%
      html_text() %>%
      str_trim()

    text_body <- doc %>%
      html_nodes(".docOcurrContainer p") %>%
      html_text() %>%
      paste(collapse = " ") %>%
      str_trim()

    return(list(
      source_media = source_media,
      publication_date = publication_date,
      title = title,
      text_body = text_body,
      file_name = basename(file_path)  # We'll update this with parent folder later
    ))
  }, error = function(e) {
    # If parsing fails, return NA values and log the error
    warning(paste("Error parsing file:", file_path, "- Error:", e$message))
    return(list(
      source_media = NA,
      publication_date = NA,
      title = NA,
      text_body = NA,
      file_name = basename(file_path)  # We'll update this with parent folder later
    ))
  })
}

# Normalizes the source name to the list of dailies
clean_source_media <- function(source_media_raw) {
  media_patterns <- list(
    "Devoir, Le" = "(?i)\\b(le)?\\s*devoir\\b",
    "Droit, Le (Ottawa, ON)" = "(?i)\\b(le)?\\s*droit\\b",
    "Figaro, Le" = "(?i)\\b(le)?\\s*figaro\\b",
    "Journal de Montréal, Le" = "(?i)\\b(le)?\\s*journal\\s*de\\s*montr(é|e)al\\b",
    "Journal de Québec, Le" = "(?i)\\b(le)?\\s*journal\\s*de\\s*qu(é|e)bec\\b",
    "Libération" = "(?i)\\blib(é|e)ration\\b",
    "Monde, Le" = "(?i)\\b(le)?\\s*monde\\b",
    "Nouvelliste, Le (Trois-Rivières, QC)" = "(?i)\\b(le)?\\s*nouvelliste\\b",
    "Presse, La" = "(?i)\\b(la)?\\s*presse\\b",
    "Quotidien, Le (Saguenay, QC)" = "(?i)\\b(le)?\\s*quotidien\\b",
    "Soleil, Le (Québec, QC)" = "(?i)\\b(le)?\\s*soleil\\b",
    "Tribune, La (Sherbrooke, QC)" = "(?i)\\b(la)?\\s*tribune\\b",
    "Voix de l'Est, La (Granby, QC)" = "(?i)\\b(la)?\\s*voix\\s*de\\s*l'est\\b"
  )

  clean_text <- trimws(source_media_raw)

  # Try to match each pattern
  for (media_name in names(media_patterns)) {
    if (str_detect(clean_text, media_patterns[[media_name]])) {
      return(media_name)
    }
  }

  # Otherwise retry on the most significant part of the name
  clean_text_simple <- str_extract(clean_text, "\\b[A-Za-zÀ-ÿ]+\\b")
  if (!is.na(clean_text_simple)) {
    for (media_name in names(media_patterns)) {
      if (str_detect(tolower(media_name), tolower(clean_text_simple))) {
        return(media_name)
      }
    }
  }

  # Last resort: the raw input
  cat("Warning: Could not match source name '", clean_text, "' to known media sources\n", sep="")
  return(clean_text)
}

# "12 mars 2003" -> "2003-03-12"
convert_french_date <- function(date_match) {
  if (is.na(date_match)) return(NA)

  months_fr <- c(
    "janvier" = "01", "février" = "02", "mars" = "03", "avril" = "04",
    "mai" = "05", "juin" = "06", "juillet" = "07", "août" = "08",
    "septembre" = "09", "octobre" = "10", "novembre" = "11", "décembre" = "12"
  )

  day <- str_extract(date_match, "[0-9]+")
  month_name <- str_extract(date_match, "[[:alpha:]]+(?= [0-9]{4})")
  year <- str_extract(date_match, "[0-9]{4}")

  # Pad day with leading zero if needed
  day <- sprintf("%02d", as.integer(day))

  month_num <- months_fr[tolower(month_name)]

  if (!is.na(month_num)) {
    return(paste(year, month_num, day, sep = "-"))
  } else {
    return(date_match)  # Keep original if conversion fails
  }
}

# Alternative parser that reads the article from the page's JavaScript content
parse_news_article_js <- function(file_path) {
  tryCatch({
    content <- read_file(file_path)

    js_content <- str_extract(content, "var documentText = `.*?`;")
    if (!is.na(js_content)) {
      js_content <- str_replace(js_content, "var documentText = `", "")
      js_content <- str_replace(js_content, "`;$", "")

      # Unescape HTML entities
      js_content <- stringi::stri_replace_all_fixed(js_content, "&lt;", "<")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&gt;", ">")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&amp;", "&")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&quot;", "\"")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&#39;", "'")

      js_doc <- read_html(js_content)

      source_media_raw <- str_extract(js_content, "(?<=DocPublicationName\">).*?(?=</span>)") %>% str_trim()

      source_media <- clean_source_media(source_media_raw)

      date_text <- str_extract(js_content, "(?<=DocHeader\">).*?(?=</span>)") %>% str_trim()
      date_match <- str_extract(date_text, "[[:alpha:]]+ [0-9]+ [[:alpha:]]+ [0-9]{4}")

      publication_date <- convert_french_date(date_match)

      title <- str_extract(js_content, "(?<=titreArticleVisu rdp__articletitle\">).*?(?=</p>)") %>% str_trim()

      text_body_parts <- str_extract_all(js_content, "(?<=<p>).*?(?=</p>)")
      if (length(text_body_parts[[1]]) > 0) {
        text_body <- paste(text_body_parts[[1]], collapse = " ") %>% str_trim()
      } else {
        text_body <- NA
      }

      return(list(
        source_media = source_media,
        publication_date = publication_date,
        title = title,
        text_body = text_body,
        file_name = basename(file_path)  # We'll update this with parent folder later
      ))
    } else {
      # Fall back to HTML parsing if JS content not found
      return(parse_news_article(file_path))
    }
  }, error = function(e) {
    warning(paste("Error parsing JS in file:", file_path, "- Error:", e$message))
    # Fall back to HTML parsing if JS parsing fails
    return(parse_news_article(file_path))
  })
}

# Parses every HTML file of one folder
process_news_files_advanced <- function(directory_path, output_csv = "news_articles_parsed.csv", method = "auto") {
  file_paths <- list.files(directory_path, pattern = "\\.(html|htm)$", full.names = TRUE)

  cat("Found", length(file_paths), "HTML files to process.\n")

  results <- map(file_paths, function(file_path) {
    cat("Processing:", basename(file_path), "\n")
    if (method == "html") {
      return(parse_news_article(file_path))
    } else if (method == "js") {
      return(parse_news_article_js(file_path))
    } else { # "auto" - try JS first, then fall back to HTML
      result <- parse_news_article_js(file_path)
      if (is.na(result$title) && is.na(result$text_body)) {
        result <- parse_news_article(file_path)
      }
      return(result)
    }
  })

  news_df <- bind_rows(results)

  parent_folder <- basename(directory_path)
  news_df$file_name <- paste(parent_folder, news_df$file_name, sep = "/")

  write_csv(news_df, output_csv)

  cat("Processing complete. Results saved to", output_csv, "\n")
  cat("Summary: Processed", nrow(news_df), "files.\n")

  return(news_df)
}

# Parses several folders into one table
process_multiple_folders <- function(folder_paths, output_csv = "news_articles_parsed.csv", method = "auto") {
  all_results <- data.frame()

  for (folder_path in folder_paths) {
    cat("Processing folder:", folder_path, "\n")

    file_paths <- list.files(folder_path, pattern = "\\.(html|htm)$", full.names = TRUE)

    cat("Found", length(file_paths), "HTML files in folder.\n")

    results <- map(file_paths, function(file_path) {
      cat("Processing:", basename(file_path), "\n")

      parent_folder <- basename(folder_path)

      if (method == "html") {
        result <- parse_news_article(file_path)
      } else if (method == "js") {
        result <- parse_news_article_js(file_path)
      } else { # "auto" - try JS first, then fall back to HTML
        result <- parse_news_article_js(file_path)
        if (is.na(result$title) && is.na(result$text_body)) {
          result <- parse_news_article(file_path)
        }
      }

      # Include parent folder in file_name
      result$file_name <- paste(parent_folder, basename(file_path), sep = "/")

      return(result)
    })

    folder_df <- bind_rows(results)

    all_results <- bind_rows(all_results, folder_df)
  }

  write_csv(all_results, output_csv)

  cat("Processing complete. Results saved to", output_csv, "\n")
  cat("Summary: Processed", nrow(all_results), "files from", length(folder_paths), "folders.\n")

  return(all_results)
}

# Legacy function for backward compatibility
process_news_files <- function(directory_path, output_csv = "news_articles_parsed.csv") {
  return(process_news_files_advanced(directory_path, output_csv, method = "auto"))
}
