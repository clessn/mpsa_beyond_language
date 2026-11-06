# Checks in which format five models answer, on 20 sentences in each
# condition, with the settings of src/40, and compares the parser used until
# November 2026 with the current one. Gemini 3.5 Flash was collected again
# after this check.
#
# Usage: Rscript src/99x_parser_diagnostic.R <model_prefix>
# Out: results/analysis/parser_diagnostic/<model_prefix>.jsonl

suppressMessages({ library(dplyr); library(stringr); library(ellmer) })
source("src/92_llm_helper_funcs.R")
source("src/93_prompts.R")
source("src/94_models_map.R")

prefix <- commandArgs(trailingOnly = TRUE)[1]
stopifnot(prefix %in% c("gpt56luna", "gemini35", "gptoss120b", "gptoss20b", "qwen332b"))
OUT <- sprintf("results/analysis/parser_diagnostic/%s.jsonl", prefix)
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
if (file.exists(OUT)) file.remove(OUT)

# The parser in use until November 2026, kept verbatim for the comparison
old_clean_sentiment_value <- function(response) {
  patterns <- c("^\\s*(-?\\d+\\.?\\d*)\\s*$", "\\b(-?\\d+\\.?\\d*)\\b", "(-?\\d+\\.?\\d*)/\\d+",
                "^\\s*Sentiment:\\s*(-?\\d+\\.?\\d*)", "^\\s*Rating:\\s*(-?\\d+\\.?\\d*)",
                "^\\s*Score:\\s*(-?\\d+\\.?\\d*)", "^\\s*Value:\\s*(-?\\d+\\.?\\d*)",
                "^\\s*sentiment score\\s*[=:]\\s*(-?\\d+\\.?\\d*)", "^\\s*sentiment\\s*[=:]\\s*(-?\\d+\\.?\\d*)")
  for (pattern in patterns) {
    match <- str_extract(response, pattern)
    if (!is.na(match)) {
      value <- as.numeric(str_extract(match, "-?\\d+\\.?\\d*"))
      if (!is.na(value) && value >= -1 && value <= 1) return(value)
    }
  }
  NA_real_
}

# 20 sentences: 10 with negative human scores, 10 others (fixed seed)
df <- readRDS("data/clean/df.rds")
set.seed(20261105)
idx <- sort(c(sample(which(df$ground_truth < 0), 10), sample(which(df$ground_truth >= 0), 10)))

# Client built exactly as in src/40_prompt.R
system_prompt <- get_system_prompt()
OUTPUT_MAX_TOKENS <- 400
# PIN_OVERRIDE: provider to use when the original pin no longer serves the
# model (GPT-OSS 120B: akashml/bf16 returned 404 on 2026-11-05; deepinfra/bf16
# serves the same weights at the same precision).
or_args <- openrouter_args(prefix)
if (nzchar(Sys.getenv("PIN_OVERRIDE"))) or_args$provider$order <- list(Sys.getenv("PIN_OVERRIDE"))
client <- switch(prefix,
  gemini35 = chat_google_gemini(system_prompt = system_prompt, model = unname(model_mapping["gemini35"]),
                                params = params(max_tokens = OUTPUT_MAX_TOKENS), echo = "none"),
  gpt56luna = chat_openai(system_prompt = system_prompt, model = unname(model_mapping["gpt56luna"]),
                          params = params(max_tokens = OUTPUT_MAX_TOKENS), echo = "none"),
  chat_openrouter(system_prompt = system_prompt, model = unname(model_mapping[prefix]),
                  params = params(max_tokens = OUTPUT_MAX_TOKENS),
                  api_args = or_args, echo = "none"))

conditions <- list(
  fr_fr = function(i) fr_prompt_fr_text(df$sentences[i]),
  en_fr = function(i) en_prompt_fr_text(df$sentences[i]),
  en_en = function(i) en_prompt_en_text(df$sentences_en[i])
)

for (cond in names(conditions)) {
  for (i in idx) {
    response <- tryCatch({
      client$set_turns(list())
      retry_with_backoff({ client$chat(conditions[[cond]](i)) })
    }, error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NA_character_ })
    record <- list(model_prefix = prefix, condition = cond, item = i,
                   doc_id = as.character(df$doc_id[i]), ground_truth = df$ground_truth[i],
                   response = as.character(response),
                   old_parsed = if (is.na(response)) NA_real_ else old_clean_sentiment_value(response),
                   new_parsed = if (is.na(response)) NA_real_ else clean_sentiment_value(response))
    cat(jsonlite::toJSON(record, auto_unbox = TRUE, na = "null"), "\n", file = OUT, sep = "", append = TRUE)
    cat(sprintf("%s %s item %d: %s\n", prefix, cond, i, substr(gsub("\n", " ", response), 1, 60)))
    Sys.sleep(1)
  }
}
cat("Done:", OUT, "\n")
