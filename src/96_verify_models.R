###############################################################################
# API KEY AND MODEL ID VERIFICATION
#
# Sends one minimal request to every model in the study and reports whether the
# key works, the model id resolves, the pinned provider accepts the request, and
# the model returns a parseable number.
#
# Run this BEFORE src/40_prompt.R, every time the lineup changes.
#
# Why: in September 2026 two model ids in this project had silently stopped
# being served, and two more returned empty strings because they reasoned past
# the output cap. Neither failure is visible until the pipeline reaches that
# model — potentially hours into a run. This turns that into a ten-second check.
#
# Cost: twelve requests of ~400 tokens each. Fractions of a cent.
###############################################################################

library(ellmer)
library(stringr)   # clean_sentiment_value() uses str_extract()

# Must match the cap used in src/40_prompt.R. Testing under a tighter cap
# produces false failures: Gemini 3.5 Flash returns "" at 20 tokens but answers
# correctly at 100, because the cap is consumed before the number is emitted.
VERIFY_MAX_TOKENS <- 400
source("src/93_prompts.R")
source("src/94_models_map.R")

#==============================================================================
# 1. KEY PRESENCE
#==============================================================================

# Which credential each model needs. ellmer resolves Gemini from GOOGLE_API_KEY
# first and falls back to GEMINI_API_KEY, hence the pair.
required_keys <- c(
  rep("OPENROUTER_API_KEY", length(model_provider_pin)),
  "ANTHROPIC_API_KEY", "GOOGLE_API_KEY|GEMINI_API_KEY", "OPENAI_API_KEY"
)
names(required_keys) <- c(names(model_provider_pin),
                          "claudehaiku45", "gemini35", "gpt56luna")

#' Is at least one of the named environment variables set?
key_present <- function(spec) {
  any(nzchar(Sys.getenv(strsplit(spec, "|", fixed = TRUE)[[1]])))
}

cat("=== KEY PRESENCE ===\n\n")
for (k in unique(required_keys)) {
  cat(sprintf("  %-32s %s\n", k,
              ifelse(key_present(k), "present", "MISSING")))
}

#==============================================================================
# 2. CLIENT CONSTRUCTION
#==============================================================================

# Built lazily so a missing key skips that model instead of aborting the script.
build_client <- function(prefix) {
  if (prefix %in% names(model_provider_pin)) {
    return(chat_openrouter(
      system_prompt = get_system_prompt(),
      model = unname(model_mapping[prefix]),
      params = params(max_tokens = VERIFY_MAX_TOKENS),
      api_args = openrouter_args(prefix),
      echo = "none"))
  }
  switch(prefix,
    claudehaiku45 = chat_anthropic(
      system_prompt = get_system_prompt(),
      model = unname(model_mapping[prefix]),
      params = params(max_tokens = VERIFY_MAX_TOKENS), echo = "none"),
    gemini35 = chat_google_gemini(
      system_prompt = get_system_prompt(),
      model = unname(model_mapping[prefix]),
      params = params(max_tokens = VERIFY_MAX_TOKENS), echo = "none"),
    gpt56luna = chat_openai(
      system_prompt = get_system_prompt(),
      model = unname(model_mapping[prefix]),
      params = params(max_tokens = VERIFY_MAX_TOKENS), echo = "none"),
    stop("No client constructor for prefix: ", prefix)
  )
}

#==============================================================================
# 3. LIVE CHECK
#==============================================================================

# A real sentiment prompt rather than a generic ping: the point is not only that
# the request is accepted, but that the reply survives clean_sentiment_value().
# A model that answers with prose or an empty string fails here, which is
# exactly the failure mode that wasted a run in the previous batch.
source("src/92_llm_helper_funcs.R")
TEST_SENTENCE <- "Le logiciel libre connait un succes remarquable."

results <- data.frame()

cat("\n=== LIVE MODEL CHECK ===\n\n")
for (prefix in names(required_keys)) {
  model_id <- unname(model_mapping[prefix])
  pin <- if (prefix %in% names(model_provider_pin))
    unname(model_provider_pin[prefix]) else "-"

  if (!key_present(required_keys[prefix])) {
    cat(sprintf("  %-14s SKIP     (no key)\n", prefix))
    results <- rbind(results, data.frame(
      model = prefix, model_id = model_id, pin = pin,
      status = "skipped - no key", parsed = NA_real_,
      detail = NA_character_, stringsAsFactors = FALSE))
    next
  }

  outcome <- tryCatch({
    reply <- build_client(prefix)$chat(en_prompt_fr_text(TEST_SENTENCE))
    value <- clean_sentiment_value(reply)
    usable <- !is.na(value) && value >= -1 && value <= 1
    list(status = ifelse(usable, "OK", "UNPARSEABLE"),
         parsed = value, detail = trimws(substr(reply, 1, 50)))
  }, error = function(e) {
    list(status = "FAILED", parsed = NA_real_,
         detail = trimws(substr(conditionMessage(e), 1, 110)))
  })

  cat(sprintf("  %-14s %-11s %-7s %s\n", prefix, outcome$status,
              ifelse(is.na(outcome$parsed), "", sprintf("%.2f", outcome$parsed)),
              outcome$detail))
  results <- rbind(results, data.frame(
    model = prefix, model_id = model_id, pin = pin,
    status = outcome$status, parsed = outcome$parsed,
    detail = outcome$detail, stringsAsFactors = FALSE))
}

#==============================================================================
# 4. VERDICT
#==============================================================================

ok      <- sum(results$status == "OK")
bad     <- sum(results$status %in% c("FAILED", "UNPARSEABLE"))
skipped <- sum(grepl("skipped", results$status))

cat("\n=== SUMMARY ===\n")
cat(sprintf("  %d usable, %d broken, %d skipped (no key)\n", ok, bad, skipped))

if (bad > 0) {
  cat("\n  Broken models:\n")
  for (i in which(results$status %in% c("FAILED", "UNPARSEABLE"))) {
    cat(sprintf("    %-14s %s  [pin: %s]\n", results$model[i],
                results$model_id[i], results$pin[i]))
    cat(sprintf("      %s: %s\n", results$status[i], results$detail[i]))
  }
  cat("\n  FAILED      -> the id or the pinned provider is wrong; check the\n")
  cat("                 catalogue and correct src/94_models_map.R.\n")
  cat("  UNPARSEABLE -> the model replied but not with a number. Usually a\n")
  cat("                 reasoning model: add it to models_needing_reasoning_off.\n")
}

if (ok == nrow(results)) {
  cat("\n  All models usable. Safe to run src/40_prompt.R.\n")
} else {
  cat("\n  Do not start the full run until every model is OK or deliberately dropped.\n")
}

dir.create("results/analysis", showWarnings = FALSE, recursive = TRUE)
saveRDS(results, "results/analysis/model_verification.rds")
cat("\nSaved: results/analysis/model_verification.rds\n")
