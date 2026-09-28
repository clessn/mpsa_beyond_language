#################################################################
# CLEAR ONE MODEL FROM THE CHECKPOINT SO IT IS SCORED AGAIN
#################################################################
# src/40_prompt.R resumes by skipping any sentence that already has a mean, so
# a model whose columns are populated is never re-contacted. This clears one
# model's columns, which is what a full re-score requires.
#
# WHEN THIS IS NEEDED: the model's configuration changed in a way that makes
# old and new scores non-comparable — a different provider endpoint, a
# different quantization, a changed prompt. Recovering only the failed items
# would then mix two backends within one model's results.
#
# Usage:
#   Rscript src/99x_reset_model.R qwen332b
#
# The previous checkpoint is left untouched on disk, so this is reversible.
#################################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript src/99x_reset_model.R <model_prefix>")
}
prefix <- args[1]

source("src/94_models_map.R")
if (!prefix %in% names(model_mapping)) {
  stop("Unknown model prefix: ", prefix,
       "\nKnown: ", paste(names(model_mapping), collapse = ", "))
}

# Refuse to run while a scoring run holds the lock: that process has its own
# copy of df in memory and would write it back over this edit.
LOCK_PATH <- "data/tmp/.rerun.lock"
if (file.exists(LOCK_PATH)) {
  pid <- suppressWarnings(as.integer(readLines(LOCK_PATH, warn = FALSE)[1]))
  alive <- !is.na(pid) &&
    length(system(sprintf("ps -p %d -o pid=", pid), intern = TRUE,
                  ignore.stderr = TRUE)) > 0
  if (alive) {
    stop(sprintf(paste0(
      "src/40_prompt.R is running (PID %d) and would overwrite this edit.\n",
      "Wait for it to finish, or stop it with:  pkill -f 40_prompt.R"), pid))
  }
}

checkpoints <- list.files("data/tmp",
                          pattern = "^sentiment_analysis_progress_.*\\.rds$",
                          full.names = TRUE)
if (length(checkpoints) == 0) stop("No checkpoint found in data/tmp/.")

latest <- checkpoints[which.max(file.info(checkpoints)$mtime)]
df <- readRDS(latest)
cat("Checkpoint:", basename(latest), "-", nrow(df), "rows\n\n")

cols <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
if (length(cols) == 0) {
  cat("No columns for", prefix, "- nothing to clear.\n")
  quit(status = 0)
}

cat("Clearing", length(cols), "columns for", prefix, ":\n")
for (col in cols) {
  filled <- sum(!is.na(df[[col]]))
  df[[col]] <- NA_real_
  cat(sprintf("  %-26s %d scores cleared\n", col, filled))
}

# Written as a fresh checkpoint rather than over the old one, so the previous
# state stays recoverable.
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out <- sprintf("data/tmp/sentiment_analysis_progress_%s.rds", stamp)
saveRDS(df, out)
saveRDS(df, "data/tmp/sentiment_analysis_latest_checkpoint.rds")

cat("\nWritten:", basename(out), "\n")
cat("Previous checkpoint left in place:", basename(latest), "\n\n")
cat("Now re-score just this model:\n")
cat(sprintf("  SKIP_MODELS=%s Rscript src/40_prompt.R\n",
            paste(setdiff(names(model_mapping), prefix), collapse = ",")))
