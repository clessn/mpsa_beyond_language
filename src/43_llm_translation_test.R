#################################################################
# ROBUSTNESS TEST: LLM TRANSLATION INSTEAD OF GOOGLE TRANSLATE
#################################################################
# The EN->EN condition uses Google Translate. A reviewer may object that
# the loss it shows is the cost of that translator, not of translation.
# This script translates the 200 benchmark sentences with GPT-5.6 Luna,
# rescores them with every model whose API key is available (English
# prompt on the LLM translation, three runs, same parsing and averaging
# as src/40_prompt.R), and compares the result with the EN->FR and the
# Google EN->EN conditions already in data/clean/df.rds.
#
# Output: data/tmp/llm_translation/ (translations, scores, checkpoints)
#         results/analysis/llm_translation_test.rds

suppressMessages({library(dplyr); library(stringr); library(ellmer)})
source("src/93_prompts.R")
source("src/94_models_map.R")
source("src/92_llm_helper_funcs.R")

OUT_DIR <- "data/tmp/llm_translation"; dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
N_RUNS <- 3; MAX_RETRIES <- 3; OUTPUT_MAX_TOKENS <- 400
df <- readRDS("data/clean/df.rds") %>% select(doc_id, sentences, sentences_en, ground_truth)

# ---- 1. Translate with GPT-5.6 Luna --------------------------------------
tr_file <- file.path(OUT_DIR, "sentences_en_llm.rds")
if (file.exists(tr_file)) {
  df$sentences_en_llm <- readRDS(tr_file)
} else {
  translator <- chat_openai(
    system_prompt = "You are a professional French-to-English translator. Translate faithfully, preserving tone and nuance. Return only the English translation, with no quotation marks and no comment.",
    model = unname(model_mapping["gpt56luna"]), params = params(max_tokens = 400), echo = "none")
  df$sentences_en_llm <- NA_character_
  for (i in seq_len(nrow(df))) {
    translator$set_turns(list())
    df$sentences_en_llm[i] <- tryCatch(str_squish(translator$chat(paste0("Translate into English: ", df$sentences[i]))),
                                       error = function(e) NA_character_)
    if (i %% 25 == 0) cat(sprintf("translated %d/200\n", i))
    Sys.sleep(0.5)
  }
  saveRDS(df$sentences_en_llm, tr_file)
}
cat(sprintf("Translations available: %d/200\n", sum(!is.na(df$sentences_en_llm))))

# ---- 2. Clients for every model whose key is present --------------------
system_prompt <- get_system_prompt()
clients <- list()
if (nzchar(Sys.getenv("OPENAI_API_KEY")))
  clients$gpt56luna <- chat_openai(system_prompt = system_prompt, model = unname(model_mapping["gpt56luna"]),
                                   params = params(max_tokens = OUTPUT_MAX_TOKENS), echo = "none")
if (nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
  clients$claudehaiku45 <- chat_anthropic(system_prompt = system_prompt, model = unname(model_mapping["claudehaiku45"]),
                                          params = params(max_tokens = OUTPUT_MAX_TOKENS), echo = "none")
if (nzchar(Sys.getenv("GEMINI_API_KEY")) || nzchar(Sys.getenv("GOOGLE_API_KEY")))
  clients$gemini35 <- chat_google_gemini(system_prompt = system_prompt, model = unname(model_mapping["gemini35"]),
                                         params = params(max_tokens = OUTPUT_MAX_TOKENS), echo = "none")
if (nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
  for (prefix in names(model_provider_pin)) {
    if (prefix == "llama321b") next
    clients[[prefix]] <- chat_openrouter(system_prompt = system_prompt, model = unname(model_mapping[prefix]),
                                         params = params(max_tokens = OUTPUT_MAX_TOKENS),
                                         api_args = openrouter_args(prefix), echo = "none")
  }
}
cat("Models to score:", paste(names(clients), collapse = ", "), "\n")

# ---- 3. Score: English prompt on the LLM translation, three runs --------
score_file <- file.path(OUT_DIR, "scores_en_llm.rds")
scores <- if (file.exists(score_file)) readRDS(score_file) else data.frame(doc_id = df$doc_id)
for (m in names(clients)) {
  for (run in seq_len(N_RUNS)) {
    col <- sprintf("%s_en_llm_run%d", m, run)
    if (col %in% names(scores) && all(!is.na(scores[[col]]))) next
    if (!col %in% names(scores)) scores[[col]] <- NA_real_
    cat(sprintf("%s run %d\n", m, run))
    for (i in seq_len(nrow(df))) {
      if (!is.na(scores[[col]][i]) || is.na(df$sentences_en_llm[i])) next
      value <- NA_real_; attempt <- 1
      while (is.na(value) && attempt <= MAX_RETRIES) {
        clients[[m]]$set_turns(list())
        response <- tryCatch(clients[[m]]$chat(en_prompt_en_text(df$sentences_en_llm[i])),
                             error = function(e) { Sys.sleep(10); NULL })
        if (!is.null(response)) value <- clean_sentiment_value(response)
        attempt <- attempt + 1
      }
      scores[[col]][i] <- value
      Sys.sleep(1)
    }
    saveRDS(scores, score_file)
  }
}

# ---- 4. Compare with the two conditions already in df.rds --------------
to3 <- function(x) ifelse(x < 0, "negative", ifelse(x == 0, "neutral", "positive"))
f1_3 <- function(pred, truth) {
  lv <- c("negative", "neutral", "positive"); cm <- table(factor(pred, lv), factor(truth, lv))
  f1 <- sapply(seq_along(lv), function(i) { tp <- cm[i, i]; p <- tp / max(sum(cm[i, ]), 1); r <- tp / max(sum(cm[, i]), 1)
    if (p + r == 0) 0 else 2 * p * r / (p + r) })
  sum(f1 * colSums(cm)) / sum(cm)
}
full <- readRDS("data/clean/df.rds")
res <- lapply(names(clients), function(m) {
  runs <- scores[, sprintf("%s_en_llm_run%d", m, 1:N_RUNS), drop = FALSE]
  llm <- rowMeans(runs, na.rm = TRUE); llm[is.nan(llm)] <- NA
  conds <- list(`EN prompt, French original` = full[[paste0(m, "_en_fr")]],
                `EN prompt, Google translation` = full[[paste0(m, "_en_en")]],
                `EN prompt, GPT-5.6 Luna translation` = llm)
  bind_rows(lapply(names(conds), function(k) {
    x <- conds[[k]]; ok <- !is.na(x) & !is.na(full$ground_truth)
    data.frame(model = model_display_name[[m]], condition = k, n = sum(ok),
               r = cor(x[ok], full$ground_truth[ok]), mae = mean(abs(x[ok] - full$ground_truth[ok])),
               f1_3 = f1_3(to3(x[ok]), to3(full$ground_truth[ok])))
  }))
}) %>% bind_rows()
print(res, digits = 3, row.names = FALSE)
saveRDS(list(results = res, translations = df$sentences_en_llm), "results/analysis/llm_translation_test.rds")
cat("Saved: results/analysis/llm_translation_test.rds\n")

# ---- 5. Table for the manuscript -----------------------------------------
tbl <- res %>%
  mutate(condition = recode(condition,
                            `EN prompt, French original` = "French original",
                            `EN prompt, Google translation` = "Google Translate",
                            `EN prompt, GPT-5.6 Luna translation` = "GPT-5.6 Luna translation"))
lines <- c("| Model | Text analyzed | r | MAE | F1 (3 categories) |",
           "|:------------------|:------------------------|-----:|------:|------:|",
           sprintf("| %s | %s | %.2f | %.3f | %.3f |", tbl$model, tbl$condition, tbl$r, tbl$mae, tbl$f1_3))
writeLines(lines, "results/tables/translator_table.md")
cat("Wrote results/tables/translator_table.md\n")
