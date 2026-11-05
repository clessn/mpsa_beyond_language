# Builds the French Lexicoder dictionary (data/dict/frlsd_dict.rds) from the
# distributed word lists (data/raw/frlsd.cat).

library(quanteda)

lexicon_raw <- readLines("data/raw/frlsd.cat", warn = FALSE)

# Identify where the POSITIVE section begins
positive_start <- which(lexicon_raw == "POSITIVE")

negative_words <- lexicon_raw[2:(positive_start-1)]
positive_words <- lexicon_raw[(positive_start+1):length(lexicon_raw)]

clean_entry <- function(entry) {
  entry <- trimws(gsub("\t", "", entry))

  word <- trimws(gsub("\\s*\\([0-9]+\\).*$", "", entry))

  weight <- as.numeric(gsub(".*\\(([0-9]+)\\).*", "\\1", entry))

  return(data.frame(word = word, weight = weight, stringsAsFactors = FALSE))
}

negative_df <- do.call(rbind, lapply(negative_words, clean_entry))
positive_df <- do.call(rbind, lapply(positive_words, clean_entry))

negative_df$sentiment <- "negative"
positive_df$sentiment <- "positive"

lexicon_df <- rbind(negative_df, positive_df)

positive_words_list <- lexicon_df$word[lexicon_df$sentiment == "positive"]
negative_words_list <- lexicon_df$word[lexicon_df$sentiment == "negative"]

frlsd_dict <- quanteda::dictionary(
  list(
    positive = positive_words_list,
    negative = negative_words_list
  )
)

saveRDS(lexicon_df, "data/dict/lexicon_df.rds")
saveRDS(frlsd_dict, "data/dict/frlsd_dict.rds")

cat("Dictionary created successfully!\n")
cat("Positive words:", length(positive_words_list), "\n")
cat("Negative words:", length(negative_words_list), "\n")
cat("Saved to data/dict/frlsd_dict.rds\n")
