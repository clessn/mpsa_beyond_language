#!/usr/bin/env Rscript

# Script to rename HTML files with proper zero padding
# Renames files like "article_1.html" to "article_0001.html"

# Function to safely rename files with error handling
rename_files <- function(directory = "eureka_articles") {
  
  # Check if directory exists
  if (!dir.exists(directory)) {
    stop(paste("Directory", directory, "does not exist. Please provide a valid directory path."))
  }
  
  # List all HTML files in the directory
  files <- list.files(directory, pattern = "\\.html$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop(paste("No HTML files found in", directory))
  }
  
  cat("Found", length(files), "HTML files to process.\n")
  
  # Create a backup directory
  backup_dir <- file.path(directory, "backup_before_rename")
  dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Extract file numbers and prepare rename operations
  operations <- data.frame(
    original_path = character(0),
    new_path = character(0),
    stringsAsFactors = FALSE
  )
  
  # Regular expression to match article_X.html pattern
  pattern <- "^(.*article_)(\\d+)(\\.html)$"
  
  # Prepare rename operations and validate
  for (file_path in files) {
    file_name <- basename(file_path)
    
    # Skip if file doesn't match our pattern
    if (!grepl(pattern, file_name)) {
      cat("Skipping", file_name, "- doesn't match expected pattern\n")
      next
    }
    
    # Extract components
    prefix <- gsub(pattern, "\\1", file_name)
    number <- as.numeric(gsub(pattern, "\\2", file_name))
    suffix <- gsub(pattern, "\\3", file_name)
    
    # Create new filename with zero-padded number
    new_filename <- sprintf("%s%04d%s", prefix, number, suffix)
    new_path <- file.path(dirname(file_path), new_filename)
    
    # Add to operations list if the name would actually change
    if (basename(file_path) != new_filename) {
      operations <- rbind(operations, data.frame(
        original_path = file_path,
        new_path = new_path,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(operations) == 0) {
    cat("No files need renaming.\n")
    return(invisible(NULL))
  }
  
  # Check for potential conflicts (where renaming would overwrite existing files)
  potential_conflicts <- operations$new_path[operations$new_path %in% files]
  if (length(potential_conflicts) > 0) {
    stop("Potential conflicts detected. Some renamed files would overwrite existing files.")
  }
  
  # Create backup copies
  cat("Creating backups in", backup_dir, "\n")
  backup_success <- TRUE
  for (file_path in unique(operations$original_path)) {
    backup_path <- file.path(backup_dir, basename(file_path))
    if (!file.copy(file_path, backup_path, overwrite = TRUE)) {
      backup_success <- FALSE
      warning(paste("Failed to backup", file_path))
    }
  }
  
  if (!backup_success) {
    stop("Failed to create complete backup. Aborting rename operation.")
  }
  
  # Proceed with rename operations
  cat("Renaming files...\n")
  rename_results <- logical(nrow(operations))
  
  for (i in 1:nrow(operations)) {
    rename_results[i] <- file.rename(operations$original_path[i], operations$new_path[i])
    if (rename_results[i]) {
      cat("Renamed:", basename(operations$original_path[i]), "->", basename(operations$new_path[i]), "\n")
    } else {
      warning(paste("Failed to rename", operations$original_path[i]))
    }
  }
  
  # Summary
  success_count <- sum(rename_results)
  cat("\nSummary:\n")
  cat("  Files processed:", length(files), "\n")
  cat("  Files renamed successfully:", success_count, "/", nrow(operations), "\n")
  cat("  Backups created in:", backup_dir, "\n")
  
  if (success_count < nrow(operations)) {
    warning("Some rename operations failed. Check the output for details.")
  } else if (nrow(operations) > 0) {
    cat("All rename operations completed successfully.\n")
  }
}

# Execute the rename function
tryCatch({
  rename_files()
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
}, warning = function(w) {
  cat("WARNING:", conditionMessage(w), "\n")
})