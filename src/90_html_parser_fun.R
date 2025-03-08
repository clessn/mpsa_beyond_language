# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Function to parse a single HTML file
parse_news_article <- function(file_path) {
  tryCatch({
    # Read the HTML file
    html_content <- read_file(file_path)
    
    # Parse the HTML
    doc <- read_html(html_content)
    
    # Extract source media name (publication name)
    source_media_raw <- doc %>%
      html_node(".DocPublicationName") %>%
      html_text() %>%
      str_trim()
    
    # Clean source media using helper function
    source_media <- clean_source_media(source_media_raw)
    
    # For debugging, output the raw text and the matched source
    # cat("Raw source:", source_media_raw, "| Matched to:", source_media, "\n")
    
    # Extract publication date from header
    date_text <- doc %>%
      html_node(".DocHeader") %>%
      html_text() %>%
      str_trim()
    
    # Parse date from the text
    # The format in the example is "vendredi 23 janvier 1998"
    date_match <- str_extract(date_text, "[[:alpha:]]+ [0-9]+ [[:alpha:]]+ [0-9]{4}")
    
    # Convert to yyyy-mm-dd format using helper function
    publication_date <- convert_french_date(date_match)
    
    # Extract title
    title <- doc %>%
      html_node(".titreArticleVisu") %>%
      html_text() %>%
      str_trim()
    
    # Extract text body
    text_body <- doc %>%
      html_nodes(".docOcurrContainer p") %>%
      html_text() %>%
      paste(collapse = " ") %>%
      str_trim()
    
    # Return a named list with the extracted data
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

# Helper function to clean source media name
clean_source_media <- function(source_media_raw) {
  # Define regex patterns for each media source
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
  
  # Clean the source text (remove extra spaces, normalize accents if needed)
  clean_text <- trimws(source_media_raw)
  
  # Try to match each pattern
  for (media_name in names(media_patterns)) {
    if (str_detect(clean_text, media_patterns[[media_name]])) {
      return(media_name)
    }
  }
  
  # If nothing matches, extract the most significant part and try again
  # This is a fallback in case the automatic detection fails
  clean_text_simple <- str_extract(clean_text, "\\b[A-Za-zÀ-ÿ]+\\b")
  if (!is.na(clean_text_simple)) {
    for (media_name in names(media_patterns)) {
      if (str_detect(tolower(media_name), tolower(clean_text_simple))) {
        return(media_name)
      }
    }
  }
  
  # If still nothing matches, return the raw input as a last resort
  # This should rarely happen since you mentioned all sources will be from your list
  cat("Warning: Could not match source name '", clean_text, "' to known media sources\n", sep="")
  return(clean_text)
}

# Helper function to convert French date to yyyy-mm-dd format
convert_french_date <- function(date_match) {
  if (is.na(date_match)) return(NA)
  
  # First, create a mapping for French month names to numbers
  months_fr <- c(
    "janvier" = "01", "février" = "02", "mars" = "03", "avril" = "04",
    "mai" = "05", "juin" = "06", "juillet" = "07", "août" = "08",
    "septembre" = "09", "octobre" = "10", "novembre" = "11", "décembre" = "12"
  )
  
  # Extract day, month name, and year
  day <- str_extract(date_match, "[0-9]+")
  month_name <- str_extract(date_match, "[[:alpha:]]+(?= [0-9]{4})")
  year <- str_extract(date_match, "[0-9]{4}")
  
  # Pad day with leading zero if needed
  day <- sprintf("%02d", as.integer(day))
  
  # Convert month name to number
  month_num <- months_fr[tolower(month_name)]
  
  # Create yyyy-mm-dd date string
  if (!is.na(month_num)) {
    return(paste(year, month_num, day, sep = "-"))
  } else {
    return(date_match)  # Keep original if conversion fails
  }
}

# Alternative parsing method using the JavaScript content
# This can be used if the HTML parsing doesn't work well
parse_news_article_js <- function(file_path) {
  tryCatch({
    # Read the file
    content <- read_file(file_path)
    
    # Extract the JavaScript documentText variable content
    js_content <- str_extract(content, "var documentText = `.*?`;")
    if (!is.na(js_content)) {
      # Remove the variable declaration and backticks
      js_content <- str_replace(js_content, "var documentText = `", "")
      js_content <- str_replace(js_content, "`;$", "")
      
      # Unescape HTML entities
      js_content <- stringi::stri_replace_all_fixed(js_content, "&lt;", "<")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&gt;", ">")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&amp;", "&")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&quot;", "\"")
      js_content <- stringi::stri_replace_all_fixed(js_content, "&#39;", "'")
      
      # Parse as HTML
      js_doc <- read_html(js_content)
      
      # Extract information from the JavaScript content
      source_media_raw <- str_extract(js_content, "(?<=DocPublicationName\">).*?(?=</span>)") %>% str_trim()
      
      # Clean source media using helper function
      source_media <- clean_source_media(source_media_raw)
      
      date_text <- str_extract(js_content, "(?<=DocHeader\">).*?(?=</span>)") %>% str_trim()
      date_match <- str_extract(date_text, "[[:alpha:]]+ [0-9]+ [[:alpha:]]+ [0-9]{4}")
      
      # Convert to yyyy-mm-dd format using helper function
      publication_date <- convert_french_date(date_match)
      
      title <- str_extract(js_content, "(?<=titreArticleVisu rdp__articletitle\">).*?(?=</p>)") %>% str_trim()
      
      # Extract text body using a more complex pattern
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

# Function to process a single folder
process_news_files_advanced <- function(directory_path, output_csv = "news_articles_parsed.csv", method = "auto") {
  # Get all HTML files in the directory
  file_paths <- list.files(directory_path, pattern = "\\.(html|htm)$", full.names = TRUE)
  
  cat("Found", length(file_paths), "HTML files to process.\n")
  
  # Process each file and collect results
  results <- map(file_paths, function(file_path) {
    cat("Processing:", basename(file_path), "\n")
    if (method == "html") {
      return(parse_news_article(file_path))
    } else if (method == "js") {
      return(parse_news_article_js(file_path))
    } else { # "auto" - try JS first, then fall back to HTML
      result <- parse_news_article_js(file_path)
      # Check if important fields are NA, and if so, try HTML parsing
      if (is.na(result$title) && is.na(result$text_body)) {
        result <- parse_news_article(file_path)
      }
      return(result)
    }
  })
  
  # Convert results to a dataframe
  news_df <- bind_rows(results)
  
  # Add parent folder to file_name
  parent_folder <- basename(directory_path)
  news_df$file_name <- paste(parent_folder, news_df$file_name, sep = "/")
  
  # Write to CSV
  write_csv(news_df, output_csv)
  
  cat("Processing complete. Results saved to", output_csv, "\n")
  cat("Summary: Processed", nrow(news_df), "files.\n")
  
  return(news_df)
}

# Function to process multiple folders
process_multiple_folders <- function(folder_paths, output_csv = "news_articles_parsed.csv", method = "auto") {
  # Initialize an empty dataframe to store all results
  all_results <- data.frame()
  
  # Process each folder
  for (folder_path in folder_paths) {
    cat("Processing folder:", folder_path, "\n")
    
    # Get all HTML files in the directory
    file_paths <- list.files(folder_path, pattern = "\\.(html|htm)$", full.names = TRUE)
    
    cat("Found", length(file_paths), "HTML files in folder.\n")
    
    # Process each file and collect results
    results <- map(file_paths, function(file_path) {
      cat("Processing:", basename(file_path), "\n")
      
      # Get the parent folder name
      parent_folder <- basename(folder_path)
      
      # Process the file
      if (method == "html") {
        result <- parse_news_article(file_path)
      } else if (method == "js") {
        result <- parse_news_article_js(file_path)
      } else { # "auto" - try JS first, then fall back to HTML
        result <- parse_news_article_js(file_path)
        # Check if important fields are NA, and if so, try HTML parsing
        if (is.na(result$title) && is.na(result$text_body)) {
          result <- parse_news_article(file_path)
        }
      }
      
      # Include parent folder in file_name
      result$file_name <- paste(parent_folder, basename(file_path), sep = "/")
      
      return(result)
    })
    
    # Convert results to a dataframe
    folder_df <- bind_rows(results)
    
    # Append to the combined results
    all_results <- bind_rows(all_results, folder_df)
  }
  
  # Write to CSV
  write_csv(all_results, output_csv)
  
  cat("Processing complete. Results saved to", output_csv, "\n")
  cat("Summary: Processed", nrow(all_results), "files from", length(folder_paths), "folders.\n")
  
  return(all_results)
}

# Legacy function for backward compatibility
process_news_files <- function(directory_path, output_csv = "news_articles_parsed.csv") {
  return(process_news_files_advanced(directory_path, output_csv, method = "auto"))
}
