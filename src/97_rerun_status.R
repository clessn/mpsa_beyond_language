###############################################################################
# RERUN PROGRESS DASHBOARD
#
# Reads the live token log written by src/40_prompt.R and reports how far the
# run has got, how each model is behaving, and what it has cost so far.
#
# Run it any time while a rerun is in progress:
#     Rscript src/97_rerun_status.R
#
# Or refresh it continuously:
#     while true; do clear; Rscript src/97_rerun_status.R; sleep 30; done
#
# Read-only: it never touches the run.
###############################################################################

suppressMessages({
  source("src/94_models_map.R")
  source("src/95_token_logging.R")
})

N_SENTENCES  <- 200
N_CONDITIONS <- 3
N_RUNS       <- 3
CALLS_NOMINAL <- N_SENTENCES * N_CONDITIONS * N_RUNS   # per model, no retries

bar <- function(frac, width = 24) {
  frac <- max(0, min(1, frac))
  n <- round(frac * width)
  paste0("[", strrep("#", n), strrep(".", width - n), "]")
}

if (!file.exists(TOKEN_LOG_PATH)) {
  cat("No token log yet at", TOKEN_LOG_PATH, "\n")
  cat("Either the rerun has not started, or it has not made its first call.\n")
  quit(status = 0)
}

log_df <- utils::read.csv(TOKEN_LOG_PATH, stringsAsFactors = FALSE)
if (nrow(log_df) == 0) { cat("Token log is empty.\n"); quit(status = 0) }

log_df$total_in <- rowSums(log_df[, c("input_tokens", "cached_input_tokens")], na.rm = TRUE)
ts     <- as.POSIXct(log_df$timestamp)
began  <- min(ts, na.rm = TRUE)
last   <- max(ts, na.rm = TRUE)
mins   <- as.numeric(difftime(last, began, units = "mins"))
rate   <- if (mins > 0) nrow(log_df) / mins else NA_real_

#------------------------------------------------------------------------------
# Is the process still alive?
#------------------------------------------------------------------------------
# Count instances, don't just test for one. On 2026-09-09 three copies ran
# concurrently, each holding its own in-memory df and overwriting the others'
# checkpoints; 54% of logged calls were duplicates. A dashboard that only says
# "RUNNING" hides exactly that failure, so the count is reported explicitly.
# Match the R process itself, not the shell that launched it: a wrapper's
# command line also contains "40_prompt.R" and would be counted as a second
# instance, turning the concurrency alarm into a false positive.
pids <- suppressWarnings(
  system("pgrep -f 'exec/R.*40_prompt[.]R'", intern = TRUE, ignore.stderr = TRUE))
n_instances <- length(pids)
alive <- n_instances > 0
idle_s <- as.numeric(difftime(Sys.time(), last, units = "secs"))

# Duplicate keys in the log have two very different causes, and conflating them
# makes the alarm useless:
#
#   - CONCURRENT RUNS: two processes scoring the same sentence at the same
#     moment. This is the 9 September failure and it corrupts data.
#   - DELIBERATE RE-SCORING: a model cleared and scored again after its
#     configuration changed (qwen332b moved from DeepInfra to SiliconFlow on
#     10 September). The old calls are superseded, not corrupted.
#
# They are told apart by the gap between occurrences: concurrent processes
# produce duplicates seconds apart, a re-score minutes or hours apart.
CONCURRENT_GAP_S <- 120

dup_keys <- paste(log_df$model_prefix, log_df$condition,
                  log_df$item, log_df$run, log_df$attempt)
dup_names <- names(which(table(dup_keys) > 1))

n_concurrent <- 0L
rescored <- character(0)
if (length(dup_names) > 0) {
  for (k in dup_names) {
    times <- sort(ts[dup_keys == k])
    gaps <- as.numeric(diff(times), units = "secs")
    if (any(gaps < CONCURRENT_GAP_S)) {
      n_concurrent <- n_concurrent + sum(gaps < CONCURRENT_GAP_S)
    } else {
      rescored <- c(rescored, sub(" .*", "", k))
    }
  }
}
rescored <- sort(unique(rescored))

# Success rates are reported over RETAINED calls only. Deduplicating on the
# exact key is not enough: two scoring waves of the same sentence use different
# numbers of retries, so the extra failed attempts of a superseded wave have no
# counterpart in the new one and would survive deduplication, dragging the rate
# down. Instead, group by (model, condition, item, run) and keep the attempts
# belonging to the most recent burst — everything within a few minutes of that
# triple's last call.
WAVE_WINDOW_S <- 300

triple <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
last_of_triple <- tapply(ts, triple, max)
retained <- log_df[
  as.numeric(difftime(last_of_triple[triple], ts, units = "secs")) <= WAVE_WINDOW_S, ]

cat("===============================================================\n")
cat(" RERUN STATUS  —", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("===============================================================\n\n")
cat(sprintf("  process        %s\n",
    ifelse(alive, "RUNNING",
           ifelse(idle_s > 120, "NOT RUNNING (finished, or stopped)", "starting/stopping"))))
if (n_instances > 1) {
  cat(sprintf("  !! %d INSTANCES RUNNING AT ONCE — they are corrupting each other.\n",
              n_instances))
  cat("     Stop all of them now:  pkill -f 40_prompt.R\n")
  cat(sprintf("     PIDs: %s\n", paste(pids, collapse = ", ")))
} else if (alive) {
  cat(sprintf("  instances      1  (PID %s)\n", pids[1]))
}
if (n_concurrent > 0) {
  cat(sprintf("  !! %s calls made concurrently on the same sentence — data may be corrupt.\n",
              format(n_concurrent, big.mark = ",")))
}
if (length(rescored) > 0) {
  cat(sprintf("  re-scored     %s  (earlier wave superseded, not an error)\n",
              paste(rescored, collapse = ", ")))
}
cat(sprintf("  started        %s  (%.1f h ago)\n",
    format(began, "%H:%M:%S"), mins / 60))
cat(sprintf("  last call      %s  (%.0f s ago)\n",
    format(last, "%H:%M:%S"), idle_s))
cat(sprintf("  calls logged   %s\n", format(nrow(log_df), big.mark = ",")))
cat(sprintf("  pace           %.1f calls/min\n\n", rate))

#------------------------------------------------------------------------------
# Per-model progress
#------------------------------------------------------------------------------
# Progress is measured in DISTINCT (condition, item, run) triples completed,
# not raw calls: a model that retries three times per sentence would otherwise
# look three times further along than it is.
key <- paste(log_df$model_prefix, log_df$condition, log_df$item, log_df$run)
done_by_model <- tapply(key, log_df$model_prefix, function(k) length(unique(k)))

cat("---------------------------------------------------------------\n")
cat(" PER MODEL\n")
cat("---------------------------------------------------------------\n")
cat(sprintf("  %-14s %-26s %6s %7s %8s\n",
            "model", "progress", "done", "ok", "calls/ok"))

ordered <- names(model_mapping)
for (m in ordered) {
  if (!m %in% names(done_by_model)) {
    cat(sprintf("  %-14s %-26s %6s %7s %8s\n", m, bar(0), "-", "-", "-"))
    next
  }
  sub   <- retained[retained$model_prefix == m, ]
  done  <- done_by_model[[m]]
  okn   <- sum(sub$valid_response == "TRUE", na.rm = TRUE)
  ratio <- if (okn > 0) sprintf("%.2f", nrow(sub) / okn) else "none ok"
  cat(sprintf("  %-14s %-26s %6s %6.0f%% %8s\n",
              m, bar(done / CALLS_NOMINAL),
              format(done, big.mark = ","),
              100 * okn / nrow(sub), ratio))
}

#------------------------------------------------------------------------------
# Overall progress and ETA
#------------------------------------------------------------------------------
total_done <- sum(done_by_model)
total_need <- CALLS_NOMINAL * length(model_mapping)
frac <- total_done / total_need
# ETA is based on observed call pace, so it already includes retry overhead
# for the models seen so far; it will drift if later models retry differently.
remaining_calls <- (total_need - total_done) * (nrow(log_df) / max(total_done, 1))
eta_h <- if (!is.na(rate) && rate > 0) remaining_calls / rate / 60 else NA_real_

cat("\n---------------------------------------------------------------\n")
cat(sprintf(" OVERALL  %s  %.1f%%   (%s / %s scores)\n", bar(frac, 30), 100 * frac,
            format(total_done, big.mark = ","), format(total_need, big.mark = ",")))
if (alive && !is.na(eta_h)) {
  cat(sprintf("          ~%.1f h remaining  (finishes around %s)\n",
              eta_h, format(Sys.time() + eta_h * 3600, "%a %H:%M")))
}
cat("---------------------------------------------------------------\n")

#------------------------------------------------------------------------------
# Cost so far
#------------------------------------------------------------------------------
spent <- merge(
  stats::aggregate(cbind(total_in, output_tokens) ~ model_prefix,
                   data = log_df, FUN = sum, na.rm = TRUE),
  MODEL_PRICES, by.x = "model_prefix", by.y = "prefix", all.x = TRUE)
spent$cost <- spent$total_in / 1e6 * spent$price_in_per_mtok +
              spent$output_tokens / 1e6 * spent$price_out_per_mtok

cat(sprintf("\n  tokens so far   %s in  /  %s out\n",
            format(sum(log_df$total_in, na.rm = TRUE), big.mark = ","),
            format(sum(log_df$output_tokens, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  spent so far    USD %.3f", sum(spent$cost, na.rm = TRUE)))
unpriced <- sum(is.na(spent$price_in_per_mtok))
if (unpriced > 0) cat(sprintf("   (excludes %d model(s) with no price set)", unpriced))
cat("\n  NOTE: cached prompt tokens are charged at the full input rate, so this\n")
cat("        is an upper bound — the real invoice will be lower.\n\n")
