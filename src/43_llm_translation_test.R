# Translator check: is the loss in the EN→EN condition the cost of
# translation or of Google Translate? The 200 sentences are translated again
# by GPT-5.6 Luna and rescored by every model under the English prompt.
#
# In:  data/clean/df.rds, data/tmp/data_manual_ranking_with_llm_scores.rds
# Out: data/tmp/llm_translation/ (translations and scores),
#      results/analysis/llm_translation_test.rds, results/tables/translator_table.md
#
# Environment: N_RUNS_OPENROUTER (runs for the OpenRouter models, 1 in
# October 2026, completed to 3 in November); SKIP_MODELS; FALLBACK_MODELS
# (prefixes whose provider pin is relaxed); PIN_OVERRIDES ("prefix=provider",
# comma-separated); ONLY_MODELS (score these models only, each in its own
# file, and stop: used by src/43b_run_parallel.sh to score models in parallel;
# a run without it merges those files); KEEP_RUNS (prefixes left at the
# runs they already have).

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
n_runs_openrouter <- as.integer(Sys.getenv("N_RUNS_OPENROUTER", "1"))
only_models <- strsplit(Sys.getenv("ONLY_MODELS", ""), ",")[[1]]
# Models kept at the runs they already have (e.g. an endpoint whose settings changed)
keep_runs <- strsplit(Sys.getenv("KEEP_RUNS", ""), ",")[[1]]
pin_overrides <- strsplit(strsplit(Sys.getenv("PIN_OVERRIDES", ""), ",")[[1]], "=")

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
    for (o in pin_overrides) if (o[1] == prefix) args$provider$order <- list(o[2])
    clients[[prefix]] <- chat_openrouter(system_prompt = system_prompt, model = unname(model_mapping[prefix]),
                                         params = params(max_tokens = max_tokens),
                                         api_args = args, echo = "none")
  }
}

if (length(only_models) > 0) clients <- clients[intersect(names(clients), only_models)]

score_file <- file.path(out_dir, "scores_en_llm.rds")
model_file <- function(m) file.path(out_dir, sprintf("scores_en_llm_%s.rds", m))
scores <- if (file.exists(score_file)) readRDS(score_file) else data.frame(doc_id = df$doc_id)
# Scores from parallel runs (one file per model) replace the model's columns
for (f in list.files(out_dir, pattern = "^scores_en_llm_.+\\.rds$", full.names = TRUE)) {
  part <- readRDS(f)
  stopifnot(identical(part$doc_id, scores$doc_id))
  for (col in setdiff(names(part), "doc_id")) scores[[col]] <- part[[col]]
}
save_scores <- function(m) {
  if (length(only_models) == 0) return(saveRDS(scores, score_file))
  saveRDS(scores[, c("doc_id", grep(paste0("^", m, "_en_llm_run"), names(scores), value = TRUE))],
          model_file(m))
}

for (m in names(clients)) {
  runs <- if (m %in% names(model_provider_pin)) n_runs_openrouter else n_runs
  if (m %in% keep_runs) runs <- sum(grepl(paste0("^", m, "_en_llm_run"), names(scores)))
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
        log_raw_response(if (length(only_models) > 0) sub("\\.jsonl$", paste0("_", m, ".jsonl"), raw_log) else raw_log,
                         model_prefix = m, condition = "en_llm", item = i, run = run,
                         attempt = attempt, doc_id = df$doc_id[i], response = response, parsed = value)
        if (!is.na(value)) break
      }
      scores[[col]][i] <- value
      if (match(i, todo) %% 20 == 0) save_scores(m)
      Sys.sleep(if (m %in% names(model_provider_pin)) 1.5 else 1)
    }
    save_scores(m)
  }
}
if (length(only_models) > 0) {
  message("SCORING COMPLETE")
  quit(save = "no")
}
saveRDS(scores, score_file)

# Comparison -------------------------------------------------------------

# Mean of the three runs in every condition, as in the main analysis. Models
# without three runs on the LLM translation (Mistral Large 4, whose preview
# endpoint no longer accepts the reasoning setting used for its first run) are
# left out of the comparison.
evaluate <- function(x) {
  ok <- !is.na(x)
  truth <- df$ground_truth[ok]
  data.frame(n = sum(ok), r = cor(x[ok], truth), mae = mean(abs(x[ok] - truth)),
             f1_3 = polarity_f1(x[ok], truth), f1_7 = category_f1(x[ok], truth))
}

full <- readRDS("data/clean/df.rds")
stopifnot(identical(full$doc_id, df$doc_id))
complete <- names(clients)[sapply(names(clients), function(m)
  all(sprintf("%s_en_llm_run%d", m, 1:3) %in% names(scores)))]

results <- bind_rows(lapply(complete, function(m) {
  llm <- rowMeans(scores[sprintf("%s_en_llm_run%d", m, 1:3)], na.rm = TRUE)
  llm[is.nan(llm)] <- NA
  bind_rows(
    `EN prompt, French original` = evaluate(full[[paste0(m, "_en_fr")]]),
    `EN prompt, Google translation` = evaluate(full[[paste0(m, "_en_en")]]),
    `EN prompt, GPT-5.6 Luna translation` = evaluate(llm),
    .id = "condition"
  ) %>% mutate(model = model_display_name[[m]], .before = 1)
}))
print(results, digits = 3, row.names = FALSE)
saveRDS(list(results = results, translations = df$sentences_en_llm),
        "results/analysis/llm_translation_test.rds")

# Summary table (by model) ----------------------------------------------

model_order <- c("GPT-5.6 Luna", "Claude Haiku 4.5", "Gemini 3.5 Flash", "Qwen3 235B-A22B",
                 "GPT-OSS 120B", "GPT-OSS 20B", "Mistral Small 3.2", "DeepSeek V3.2",
                 "Qwen3 32B", "Llama 4 Scout", "Llama 3.1 8B", "Llama 3.2 3B")
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
