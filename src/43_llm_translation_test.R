# Translator check: is the loss in the EN→EN condition the cost of
# translation or of Google Translate? The 200 sentences are translated again
# by GPT-5.6 Luna and rescored by every model under the English prompt.
#
# In:  data/clean/df.rds, data/tmp/data_manual_ranking_with_llm_scores.rds
# Out: data/tmp/llm_translation/ (translations and scores),
#      results/analysis/llm_translation_test.rds, results/tables/translator_table.md
#
# Environment: SKIP_MODELS (prefixes to leave for later), FALLBACK_MODELS
# (prefixes whose OpenRouter provider pin is relaxed), N_RUNS_OPENROUTER.

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ellmer)
})
source("src/92_llm_helper_funcs.R")
source("src/93_prompts.R")
source("src/94_models_map.R")

out_dir <- "data/tmp/llm_translation"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
raw_log <- "results/analysis/raw_responses_llm_translation.jsonl"
max_tokens <- 400
max_retries <- 3
n_runs <- 3
# OpenRouter throttles heavily, so the models it serves are scored once
n_runs_openrouter <- as.integer(Sys.getenv("N_RUNS_OPENROUTER", "1"))

df <- readRDS("data/clean/df.rds") %>% select(doc_id, sentences, sentences_en, ground_truth)

# Translation ------------------------------------------------------------

translation_file <- file.path(out_dir, "sentences_en_llm.rds")
if (!file.exists(translation_file)) {
  translator <- chat_openai(
    system_prompt = paste("You are a professional French-to-English translator.",
                          "Translate faithfully, preserving tone and nuance.",
                          "Return only the English translation, with no quotation marks and no comment."),
    model = unname(model_mapping["gpt56luna"]), params = params(max_tokens = max_tokens), echo = "none")
  translations <- vapply(df$sentences, function(s) {
    translator$set_turns(list())
    Sys.sleep(0.5)
    tryCatch(str_squish(translator$chat(paste0("Translate into English: ", s))),
             error = function(e) NA_character_)
  }, character(1), USE.NAMES = FALSE)
  saveRDS(translations, translation_file)
}
df$sentences_en_llm <- readRDS(translation_file)

# Scoring ----------------------------------------------------------------

system_prompt <- get_system_prompt()
skip_models <- strsplit(Sys.getenv("SKIP_MODELS", ""), ",")[[1]]
fallback_models <- strsplit(Sys.getenv("FALLBACK_MODELS", ""), ",")[[1]]

clients <- list()
if (nzchar(Sys.getenv("OPENAI_API_KEY")))
  clients$gpt56luna <- chat_openai(system_prompt = system_prompt, model = unname(model_mapping["gpt56luna"]),
                                   params = params(max_tokens = max_tokens), echo = "none")
if (nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
  clients$claudehaiku45 <- chat_anthropic(system_prompt = system_prompt, model = unname(model_mapping["claudehaiku45"]),
                                          params = params(max_tokens = max_tokens), echo = "none")
if (nzchar(Sys.getenv("GEMINI_API_KEY")) || nzchar(Sys.getenv("GOOGLE_API_KEY")))
  clients$gemini35 <- chat_google_gemini(system_prompt = system_prompt, model = unname(model_mapping["gemini35"]),
                                         params = params(max_tokens = max_tokens), echo = "none")
if (nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
  for (prefix in setdiff(names(model_provider_pin), c("llama321b", skip_models))) {
    args <- openrouter_args(prefix)
    if (prefix %in% fallback_models) args$provider$allow_fallbacks <- TRUE
    clients[[prefix]] <- chat_openrouter(system_prompt = system_prompt, model = unname(model_mapping[prefix]),
                                         params = params(max_tokens = max_tokens),
                                         api_args = args, echo = "none")
  }
}

score_file <- file.path(out_dir, "scores_en_llm.rds")
scores <- if (file.exists(score_file)) readRDS(score_file) else data.frame(doc_id = df$doc_id)

for (m in names(clients)) {
  runs <- if (m %in% names(model_provider_pin)) n_runs_openrouter else n_runs
  for (run in seq_len(runs)) {
    col <- sprintf("%s_en_llm_run%d", m, run)
    if (!col %in% names(scores)) scores[[col]] <- NA_real_
    todo <- which(is.na(scores[[col]]) & !is.na(df$sentences_en_llm))
    for (i in todo) {
      value <- NA_real_
      for (attempt in seq_len(max_retries)) {
        clients[[m]]$set_turns(list())
        response <- tryCatch(clients[[m]]$chat(en_prompt_en_text(df$sentences_en_llm[i])),
                             error = function(e) { Sys.sleep(10); NULL })
        if (is.null(response)) next
        value <- clean_sentiment_value(response)
        log_raw_response(raw_log, model_prefix = m, condition = "en_llm", item = i, run = run,
                         attempt = attempt, doc_id = df$doc_id[i], response = response, parsed = value)
        if (!is.na(value)) break
      }
      scores[[col]][i] <- value
      Sys.sleep(if (m %in% names(model_provider_pin)) 1.5 else 1)
    }
    saveRDS(scores, score_file)
  }
}

# Comparison -------------------------------------------------------------

# The OpenRouter models scored the LLM translation once, and averaging runs
# raises the correlation with the benchmark, so every condition is compared
# on its first run.
per_run <- readRDS("data/tmp/data_manual_ranking_with_llm_scores.rds")
key <- function(d) paste(d$doc_id, d$sentences)
per_run <- per_run[match(key(df), key(per_run)), ]
stopifnot(identical(key(df), key(per_run)))

evaluate <- function(x) {
  ok <- !is.na(x)
  truth <- df$ground_truth[ok]
  data.frame(n = sum(ok), r = cor(x[ok], truth), mae = mean(abs(x[ok] - truth)),
             f1_3 = polarity_f1(x[ok], truth))
}

results <- bind_rows(lapply(names(clients), function(m) {
  bind_rows(
    `EN prompt, French original` = evaluate(per_run[[paste0(m, "_en_fr_run1")]]),
    `EN prompt, Google translation` = evaluate(per_run[[paste0(m, "_en_en_run1")]]),
    `EN prompt, GPT-5.6 Luna translation` = evaluate(scores[[paste0(m, "_en_llm_run1")]]),
    .id = "condition"
  ) %>% mutate(model = model_display_name[[m]], .before = 1)
}))
print(results, digits = 3, row.names = FALSE)
saveRDS(list(results = results, translations = df$sentences_en_llm),
        "results/analysis/llm_translation_test.rds")

# Table 1 ----------------------------------------------------------------

model_order <- c("GPT-5.6 Luna", "Claude Haiku 4.5", "Gemini 3.5 Flash", "Qwen3 235B-A22B",
                 "GPT-OSS 120B", "GPT-OSS 20B", "Mistral Small 3.2", "DeepSeek V3.2",
                 "Mistral Large 4", "Qwen3 32B", "Llama 4 Scout", "Llama 3.1 8B", "Llama 3.2 3B")
table_1 <- results %>%
  mutate(condition = recode(condition,
                            `EN prompt, French original` = "orig",
                            `EN prompt, Google translation` = "google",
                            `EN prompt, GPT-5.6 Luna translation` = "llm")) %>%
  select(model, condition, r, f1_3) %>%
  pivot_wider(names_from = condition, values_from = c(r, f1_3)) %>%
  arrange(factor(model, levels = model_order))

writeLines(c(
  "| Model | r, original | r, Google | r, LLM | F1, original | F1, Google | F1, LLM |",
  "|:------------------|------:|------:|------:|------:|------:|------:|",
  sprintf("| %s | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |", table_1$model,
          table_1$r_orig, table_1$r_google, table_1$r_llm,
          table_1$f1_3_orig, table_1$f1_3_google, table_1$f1_3_llm)
), "results/tables/translator_table.md")
