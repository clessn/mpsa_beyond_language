# Machine translation of the corpus into English with Google Translate's
# document translation: writes the articles into Word batches, then reads the
# translated batches back and aligns them by document id. The upload and
# download are done by hand between the two steps.
#
# In:  data/tmp/news_df.rds, data/translation_files/
# Out: data/tmp/news_df_translated.rds

library(tidyverse)
library(officer)

news_df <- readRDS("data/tmp/news_df.rds")

print(paste("Number of articles:", nrow(news_df)))
print(names(news_df))  # Check column names

dir.create("translation_files", showWarnings = FALSE)

BATCH_SIZE <- 900  # Number of articles per batch

num_batches <- ceiling(nrow(news_df) / BATCH_SIZE)
print(paste("Creating", num_batches, "batch files with up to", BATCH_SIZE, "articles each"))

batch_files <- c()
for (batch_num in 1:num_batches) {
  start_idx <- ((batch_num - 1) * BATCH_SIZE) + 1
  end_idx <- min(batch_num * BATCH_SIZE, nrow(news_df))

  print(paste("Creating batch", batch_num, "with articles", start_idx, "to", end_idx))

  doc <- read_docx()

  doc <- doc %>%
    body_add_par(value = paste("News Articles Batch", batch_num), style = "heading 1") %>%
    body_add_par(value = "DO NOT REMOVE THE ###ARTICLE_START### MARKERS", style = "Normal")

  for (i in start_idx:end_idx) {
    article <- news_df$text_body[i]

    doc <- doc %>%
      body_add_par(value = paste0("###ARTICLE_START### ID:", i), style = "heading 2") %>%
      body_add_par(value = article, style = "Normal") %>%
      body_add_par(value = " ", style = "Normal")  # Add some space

    if (i %% 100 == 0) {
      print(paste("Added article", i))
    }
  }

  file_name <- paste0("translation_files/articles_batch_", batch_num, ".docx")
  print(doc, target = file_name)
  batch_files <- c(batch_files, file_name)
  print(paste("Batch", batch_num, "saved to", file_name))
}

cat("\n=== NEXT STEPS ===\n")
cat("1. Upload each Word document from the 'translation_files' folder to Google Translate (document translation)\n")
cat("2. Translate from French to English\n")
cat("3. Download each translated document\n")
cat("4. Save them with '_translated' added to the filename\n")
cat("   Example: 'articles_batch_1.docx' -> 'articles_batch_1_translated.docx'\n")
cat("5. Place all translated documents in the 'translation_files' folder\n")
cat("6. Run the second part of this script\n")

process_all_text_files <- function() {
  if(!exists("news_df")) {
    news_df <- readRDS("data/tmp/news_df.rds")
    print(paste("Loaded dataframe with", nrow(news_df), "articles"))
  }

  if(!"translated_text" %in% names(news_df)) {
    news_df$translated_text <- NA_character_
  }

  text_files <- list.files("translation_files/text_files",
                          pattern = "\\.txt$",
                          full.names = TRUE)

  print(paste("Found", length(text_files), "text files to process"))

  delimiter_pattern <- "###ARTICLE_START###"

  total_updated <- 0

  for(text_file in text_files) {
    print(paste("Processing text file:", text_file))

    file_content <- readLines(text_file, warn = FALSE)
    print(paste("Read", length(file_content), "lines from", text_file))

    delimiter_lines <- grep(delimiter_pattern, file_content)
    print(paste("Found", length(delimiter_lines), "article delimiters"))

    # If no exact matches, try more flexible matching
    if(length(delimiter_lines) == 0) {
      print("No exact delimiters found, trying more flexible matching...")
      delimiter_lines <- grep("ARTICLE.*START", file_content)
      print(paste("Found", length(delimiter_lines), "potential article delimiters"))
    }

    file_updated_count <- 0

    for(i in 1:length(delimiter_lines)) {
      delimiter_line <- file_content[delimiter_lines[i]]

      id_match <- regexpr("ID:\\s*\\d+", delimiter_line)
      if(id_match > 0) {
        id_text <- regmatches(delimiter_line, id_match)
        id <- as.numeric(gsub("ID:\\s*", "", id_text))

        start_line <- delimiter_lines[i] + 1
        end_line <- if(i < length(delimiter_lines))
                      delimiter_lines[i+1] - 1
                    else
                      length(file_content)

        article_lines <- file_content[start_line:end_line]
        article_text <- paste(article_lines, collapse = "\n")

        if(!is.na(id) && id > 0 && id <= nrow(news_df)) {
          news_df$translated_text[id] <- article_text
          file_updated_count <- file_updated_count + 1
        }
      }

      if(i %% 100 == 0) {
        print(paste("Processed", i, "articles so far in this file"))
      }
    }

    print(paste("Updated", file_updated_count, "articles from", text_file))
    total_updated <- total_updated + file_updated_count
  }

  saveRDS(news_df, "data/tmp/news_df_translated.rds")

  print(paste("Translation processing complete! Total of", total_updated, "articles translated"))
  print(paste("Saved to data/tmp/news_df_translated.rds"))

  return(news_df)
}

translated_df <- process_all_text_files()

saveRDS(news_df, "data/tmp/news_df_translated.rds")
