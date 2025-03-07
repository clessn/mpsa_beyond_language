# TEXT= ("logiciel libre" OU "logiciels libres" OU "open source" OU "open-source" OU "logiciel open source" OU "logiciels open source" OU "code source ouvert" OU "software libre" OU "free software" OU "code source libre" OU "Free Software Foundation" OU "Richard Stallman")

# Load the necessary libraries
library(stringr)
library(dplyr)
library(jsonlite)
library(utils) # For URLencode function

# Read the JavaScript content from the file
js_content <- readLines("data/paste_2005-05-02_1991_01_01.txt", warn = FALSE) %>% paste(collapse = "\n")

# Extract the _docKeyList array from the JavaScript
pattern <- "_docKeyList = (\\[.+?\\]);"
match <- str_match(js_content, pattern)

# The second element of the match will contain the JSON array
json_array <- match[2]

# Parse the JSON array to get the document keys
doc_keys <- fromJSON(json_array)

# Show how many document keys we found
cat("Found", length(doc_keys), "document keys\n")

# Function to properly encode a document key for use in a URL
encode_doc_key <- function(doc_key) {
  # First, replace the special characters with their URL-encoded equivalents
  # This is done manually to ensure consistent encoding
  encoded <- doc_key
  encoded <- gsub("·", "%C2%B7", encoded)
  encoded <- gsub("×", "%C3%97", encoded)
  
  # Return the encoded string
  return(encoded)
}

# Create URLs for all document keys
urls_df <- data.frame(
  doc_key = doc_keys,
  stringsAsFactors = FALSE
)

# Add index starting from 0
urls_df$index <- 0:(nrow(urls_df) - 1)

# Create URLs with proper encoding
urls_df$url <- sapply(urls_df$doc_key, function(key) {
  encoded_key <- encode_doc_key(key)
  paste0(
    "https://nouveau-eureka-cc.acces.bibl.ulaval.ca/Document/View?viewEvent=1&docRefId=0&docName=",
    encoded_key,
    "&docIndex=0" # We'll adjust this later
  )
})

# Now update the docIndex parameter in each URL to match the index
for (i in 1:nrow(urls_df)) {
  urls_df$url[i] <- gsub("docIndex=0", paste0("docIndex=", urls_df$index[i]), urls_df$url[i])
}

# Verify the first URL
cat("First URL:", urls_df$url[1], "\n")

# Verify it matches the expected pattern
expected_url <- "https://nouveau-eureka-cc.acces.bibl.ulaval.ca/Document/View?viewEvent=1&docRefId=0&docName=news%C2%B720241226%C2%B7LM%C2%B7202412262%C3%9720%C3%9722578961160%C3%97215&docIndex=0"
cat("Does it match the expected URL?", urls_df$url[1] == expected_url, "\n")

# Save the URLs to a CSV file
write.csv(urls_df, "data/article_urls_2.csv", row.names = FALSE)
cat("Saved", nrow(urls_df), "URLs to lemonde_article_urls.csv\n")

head(urls_df)


df <- read.csv("data/article_urls.csv")
head(df)
df2 <- read.csv("data/article_urls_1.csv")
head(df2)
df3 <- read.csv("data/article_urls_2.csv")
