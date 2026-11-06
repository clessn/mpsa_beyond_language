# Sends one short request to every model and checks that the key, the model
# id and the pinned provider work and that the answer parses. Run before
# src/40 whenever the lineup changes.
#
# Out: results/analysis/model_verification.rds

library(ellmer)
library(stringr)

# Same cap as src/40_prompt.R: under a tighter one, reasoning models return nothing
VERIFY_MAX_TOKENS <- 400
source("src/93_prompts.R")
source("src/94_models_map.R")

# Which credential each model needs. ellmer resolves Gemini from GOOGLE_API_KEY
# first and falls back to GEMINI_API_KEY, hence the pair.
required_keys <- c(
  rep("OPENROUTER_API_KEY", length(model_provider_pin)),
  "ANTHROPIC_API_KEY", "GOOGLE_API_KEY|GEMINI_API_KEY", "OPENAI_API_KEY"
)
names(required_keys) <- c(names(model_provider_pin),
                          "claudehaiku45", "gemini35", "gpt56luna")

key_present <- function(spec) {
  any(nzchar(Sys.getenv(strsplit(spec, "|", fixed = TRUE)[[1]])))
}

cat("=== KEY PRESENCE ===\n\n")
for (k in unique(required_keys)) {
  cat(sprintf("  %-32s %s\n", k,
              ifelse(key_present(k), "present", "MISSING")))
}

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

# A real sentiment prompt, so that the check covers parsing the answer
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
