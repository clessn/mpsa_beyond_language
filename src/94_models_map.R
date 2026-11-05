# The model lineup: API identifiers, OpenRouter provider pins, reasoning
# settings, licence and display names. Sourced by the scoring and analysis
# scripts.

# Prefixes contain no underscore: result columns are "<prefix>_<prompt>_<text>"
model_mapping <- c(
  "llama321b"      = "meta-llama/llama-3.2-1b-instruct",
  "llama323b"      = "meta-llama/llama-3.2-3b-instruct",
  "llama318b"      = "meta-llama/llama-3.1-8b-instruct",
  "gptoss20b"      = "openai/gpt-oss-20b",
  "qwen332b"       = "qwen/qwen3-32b",
  "llama4scout"    = "meta-llama/llama-4-scout",
  "gptoss120b"     = "openai/gpt-oss-120b",
  "qwen3235b"      = "qwen/qwen3-235b-a22b-2507",
  "deepseekv32"    = "deepseek/deepseek-v3.2",
  "mistralsmall32" = "mistralai/mistral-small-3.2-24b-instruct",
  # Preview release, no published weights yet: counted as closed-weight
  "mistrallarge4"  = "mistralai/mistral-large-4-0",
  "claudehaiku45"  = "claude-haiku-4-5",
  "gemini35"       = "gemini-3.5-flash",
  "gpt56luna"      = "gpt-5.6-luna",
  # Scored by src/42_jev_prompt.R through TypeSafe's API (src/98_jev_helpers.R)
  "jev"            = "jev-1.13.0"
)

# OpenRouter can serve a model through several providers at different
# precisions, so each model is pinned to one endpoint ("<provider>/<quant>"),
# at the highest precision offered, with fallback disabled. Checked against
# /api/v1/models/{id}/endpoints in September and October 2026. On 2026-11-05
# akashml served gpt-oss-120b only in fp4; deepinfra/bf16 serves the same
# weights at the same precision.
model_provider_pin <- c(
  "llama321b"      = "cloudflare",       # only provider, quantization undeclared
  "llama323b"      = "parasail/bf16",
  "llama318b"      = "coreweave/bf16",
  "gptoss20b"      = "deepinfra/bf16",
  # DeepInfra ignores reasoning = "none" for this model; SiliconFlow honours it
  "qwen332b"       = "siliconflow/fp8",
  "llama4scout"    = "novita/bf16",
  "gptoss120b"     = "akashml/bf16",
  "qwen3235b"      = "gmicloud/fp8",     # no bf16 endpoint
  "deepseekv32"    = "gmicloud/fp8",     # no bf16 endpoint
  "mistralsmall32" = "parasail/bf16",
  "mistrallarge4"  = "mistral"           # Mistral's own endpoint, quantization undeclared
)

# Models that reason by default spend the 400-token cap on reasoning and
# return no answer. The GPT-OSS endpoints refuse "none", so "low" is the
# minimum there.
model_reasoning_config <- list(
  gptoss20b     = list(effort = "low"),
  gptoss120b    = list(effort = "low"),
  qwen332b      = list(effort = "none"),
  deepseekv32   = list(effort = "none"),
  mistrallarge4 = list(effort = "none")
)

open_models <- c(
  "meta-llama/llama-3.2-1b-instruct",
  "meta-llama/llama-3.2-3b-instruct",
  "meta-llama/llama-3.1-8b-instruct",
  "openai/gpt-oss-20b",
  "qwen/qwen3-32b",
  "meta-llama/llama-4-scout",
  "openai/gpt-oss-120b",
  "qwen/qwen3-235b-a22b-2507",
  "deepseek/deepseek-v3.2",
  "mistralai/mistral-small-3.2-24b-instruct"
)

model_display_name <- c(
  "llama321b"      = "Llama 3.2 1B",
  "llama323b"      = "Llama 3.2 3B",
  "llama318b"      = "Llama 3.1 8B",
  "gptoss20b"      = "GPT-OSS 20B",
  "qwen332b"       = "Qwen3 32B",
  "llama4scout"    = "Llama 4 Scout",
  "gptoss120b"     = "GPT-OSS 120B",
  "qwen3235b"      = "Qwen3 235B-A22B",
  "deepseekv32"    = "DeepSeek V3.2",
  "claudehaiku45"  = "Claude Haiku 4.5",
  "gemini35"       = "Gemini 3.5 Flash",
  "gpt56luna"      = "GPT-5.6 Luna",
  "jev"            = "Jev 1.13",
  "mistralsmall32" = "Mistral Small 3.2",
  "mistrallarge4"  = "Mistral Large 4"
)

# "qwen3235b_en_fr" -> "Qwen3 235B-A22B (EN→FR)"; "lsd_en" -> "Lexicoder (EN)"
get_model_display_name <- function(model_name, with_condition = TRUE) {
  if (grepl("^lsd_", model_name)) {
    return(paste0("Lexicoder (", toupper(sub("^lsd_", "", model_name)), ")"))
  }
  prefix <- sub("_[a-z]{2}_[a-z]{2}$", "", model_name)
  label <- if (prefix %in% names(model_display_name)) unname(model_display_name[prefix]) else prefix
  condition <- regmatches(model_name, regexpr("_[a-z]{2}_[a-z]{2}$", model_name))
  if (!with_condition || length(condition) == 0) return(label)
  condition <- switch(sub("^_", "", condition),
                      "fr_fr" = "FR→FR", "en_fr" = "EN→FR", "en_en" = "EN→EN",
                      condition)
  paste0(label, " (", condition, ")")
}

# TRUE for open-weight models, FALSE for closed-weight, NA for the dictionaries
is_open_source <- function(model_column) {
  for (prefix in names(model_mapping)) {
    if (grepl(prefix, model_column, fixed = TRUE)) {
      return(unname(model_mapping[prefix]) %in% open_models)
    }
  }
  NA
}

# Request arguments for a model served through OpenRouter. The output cap is
# set separately with ellmer's params(max_tokens = ...).
openrouter_args <- function(prefix) {
  args <- list(provider = list(order = list(unname(model_provider_pin[prefix])),
                               allow_fallbacks = FALSE))
  if (!is.null(model_reasoning_config[[prefix]])) {
    args$reasoning <- model_reasoning_config[[prefix]]
  }
  args
}
