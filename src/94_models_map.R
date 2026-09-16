###############################################################################
# MODEL MAPPING AND CATEGORIZATION
#
# Single source of truth for the model lineup: API identifiers, provider pins,
# parameter counts, and the open/closed split used throughout the analysis.
#
# Author: Ral Zarek
# Date: March 2025
# Updated: September 2026 — migrated the open-weight lineup to OpenRouter.
#
# WHY OPENROUTER: the previous lineup was split across Groq and Fireworks. A
# September 2026 check of Groq's catalogue found only 14 models served, two of
# which the study depended on (qwen/qwen3-32b, meta-llama/llama-4-scout) had
# been withdrawn, and no small general-purpose model remained. OpenRouter
# serves 431 models including every model here plus the 1B-8B range that
# restores continuity with the original 2024-2025 batch.
#
# THE COST OF ROUTING: OpenRouter dispatches to underlying providers, and the
# same model can be served at different quantizations (llama-3.2-3b is offered
# at bf16 by one provider and at an undeclared quantization by another). For a
# benchmark that is a reproducibility hazard, so every open-weight model below
# is PINNED to a named provider endpoint via `model_provider_pin`, preferring
# the highest available precision. Report these pins in the paper's Methods.
#
# FINAL LINEUP (fixed 2026-09-16): the twelve models below, all scored in the
# September 2026 rerun. Two decisions to carry into the manuscript:
#   - llama321b is KEPT with a reduced n, on purpose: small models (3B, 8B) can
#     do the task, but at 1B the model is too small to follow the instruction
#     at all, and the paper uses it to show that lower bound. It returns a
#     usable number on ~4% of calls and ends up scored on only 58-74 of 200
#     sentences per condition;
#     its only OpenRouter provider leaves no alternative endpoint. Its metrics
#     are reported with their n and are not comparable to the full-n models,
#     since the sentences it did score are not a random subset.
#   - DeepSeek V4 Flash, in the August lineup, is DROPPED. It was never scored
#     after the OpenRouter migration; DeepSeek remains represented by V3.2.
###############################################################################

#==============================================================================
# 1. MODEL IDENTIFIERS
#==============================================================================

# Prefixes must not contain underscores: result columns are built as
# "<prefix>_<prompt_lang>_<text_lang>" and downstream scripts split on "_".
model_mapping <- c(
  # --- Open-weight, served through OpenRouter -------------------------------
  "llama321b"     = "meta-llama/llama-3.2-1b-instruct",
  "llama323b"     = "meta-llama/llama-3.2-3b-instruct",
  "llama318b"     = "meta-llama/llama-3.1-8b-instruct",
  "gptoss20b"     = "openai/gpt-oss-20b",
  "qwen332b"      = "qwen/qwen3-32b",
  "llama4scout"   = "meta-llama/llama-4-scout",
  "gptoss120b"    = "openai/gpt-oss-120b",
  "qwen3235b"     = "qwen/qwen3-235b-a22b-2507",
  "deepseekv32"   = "deepseek/deepseek-v3.2",

  # --- Closed-weight, served through each vendor's own API ------------------
  "claudehaiku45" = "claude-haiku-4-5",
  "gemini35"      = "gemini-3.5-flash",
  "gpt56luna"     = "gpt-5.6-luna"
)

#==============================================================================
# 2. PROVIDER PINNING (OPEN-WEIGHT MODELS ONLY)
#==============================================================================

# OpenRouter endpoint tags, of the form "<provider>" or "<provider>/<quant>".
# Passed as provider$order with allow_fallbacks = FALSE, so a request either
# runs on this exact endpoint or fails loudly rather than silently rerouting.
# Verified against /api/v1/models/{id}/endpoints on 2026-09-09.
model_provider_pin <- c(
  "llama321b"   = "cloudflare",      # NOTE: only provider; quantization undeclared
  "llama323b"   = "parasail/bf16",
  "llama318b"   = "coreweave/bf16",
  "gptoss20b"   = "deepinfra/bf16",
  # SiliconFlow rather than DeepInfra, both fp8: DeepInfra silently ignores
  # reasoning = list(effort = "none") and always reasons before answering,
  # spending 284-392 output tokens and overrunning the cap on 37% of calls.
  # SiliconFlow honours it and returns a bare number in 3 tokens. Verified
  # head-to-head 2026-09-10; the switch cost 140 lost scores to recover.
  "qwen332b"    = "siliconflow/fp8",
  "llama4scout" = "novita/bf16",
  "gptoss120b"  = "akashml/bf16",
  "qwen3235b"   = "gmicloud/fp8",    # no bf16 endpoint offered
  "deepseekv32" = "gmicloud/fp8"     # no bf16 endpoint offered
)

#==============================================================================
# 3. REASONING SUPPRESSION
#==============================================================================

# Models that reason before answering. Left unchecked they spend the output
# budget on chain-of-thought and return either an empty string or raw <think>
# text, which the sentiment parser cannot read.
#
# The setting is NOT uniform, and the difference was found by testing each
# endpoint on 2026-09-09:
#
#   effort = "none"  Qwen and DeepSeek endpoints accept full suppression.
#   effort = "low"   The pinned gpt-oss endpoints reject "none" outright
#                    ("Reasoning is mandatory for this endpoint and cannot be
#                    disabled", HTTP 400). "low" is the least they allow and
#                    returns a clean number in ~28 output tokens.
#
# Models absent from this list declare no reasoning parameters in the
# OpenRouter catalogue and need no suppression.
model_reasoning_config <- list(
  gptoss20b   = list(effort = "low"),
  gptoss120b  = list(effort = "low"),
  qwen332b    = list(effort = "none"),
  deepseekv32 = list(effort = "none")
)

#==============================================================================
# 4. PARAMETER COUNTS
#==============================================================================

# Total and active parameters in billions. The two differ for mixture-of-experts
# models, where only a fraction of weights activate per token — active count is
# what drives inference cost, so it is the more meaningful regressor. Report
# both in the paper and state which the regression uses.
#
# NA = not reliably documented; fill from the model card before publishing.
model_params_total <- c(
  "llama321b" = 1, "llama323b" = 3, "llama318b" = 8,
  "gptoss20b" = 20, "qwen332b" = 32, "llama4scout" = 109,
  "gptoss120b" = 120, "qwen3235b" = 235, "deepseekv32" = 671
)

model_params_active <- c(
  "llama321b" = 1,          # dense
  "llama323b" = 3,          # dense
  "llama318b" = 8,          # dense
  "gptoss20b" = NA_real_,   # MoE — active count not verified
  "qwen332b" = 32,          # dense
  "llama4scout" = 17,       # 17B active / 109B total
  "gptoss120b" = NA_real_,  # MoE — active count not verified
  "qwen3235b" = 22,         # "a22b" in the model name = 22B active
  "deepseekv32" = NA_real_  # MoE — active count not verified
)

#==============================================================================
# 5. LICENCE CATEGORIZATION
#==============================================================================

open_models <- c(
  "meta-llama/llama-3.2-1b-instruct",
  "meta-llama/llama-3.2-3b-instruct",
  "meta-llama/llama-3.1-8b-instruct",
  "openai/gpt-oss-20b",
  "qwen/qwen3-32b",
  "meta-llama/llama-4-scout",
  "openai/gpt-oss-120b",
  "qwen/qwen3-235b-a22b-2507",
  "deepseek/deepseek-v3.2"
)

closed_models <- c(
  "claude-haiku-4-5",
  "gemini-3.5-flash",
  "gpt-5.6-luna"
)

#==============================================================================
# 6. DISPLAY NAMES AND PROVIDERS
#==============================================================================

# Kept here rather than in each plotting script. Four scripts previously carried
# their own copy of this lookup, hard-coded against the Fireworks and Groq model
# ids; the move to OpenRouter silently broke all four at once, since every
# lookup fell through to "Other". One definition, one place to update.

model_display_name <- c(
  "llama321b"     = "Llama 3.2 1B",
  "llama323b"     = "Llama 3.2 3B",
  "llama318b"     = "Llama 3.1 8B",
  "gptoss20b"     = "GPT-OSS 20B",
  "qwen332b"      = "Qwen3 32B",
  "llama4scout"   = "Llama 4 Scout",
  "gptoss120b"    = "GPT-OSS 120B",
  "qwen3235b"     = "Qwen3 235B-A22B",
  "deepseekv32"   = "DeepSeek V3.2",
  "claudehaiku45" = "Claude Haiku 4.5",
  "gemini35"      = "Gemini 3.5 Flash",
  "gpt56luna"     = "GPT-5.6 Luna"
)

model_provider <- c(
  "llama321b" = "Meta", "llama323b" = "Meta", "llama318b" = "Meta",
  "llama4scout" = "Meta",
  "gptoss20b" = "OpenAI", "gptoss120b" = "OpenAI", "gpt56luna" = "OpenAI",
  "qwen332b" = "Alibaba", "qwen3235b" = "Alibaba",
  "deepseekv32" = "DeepSeek",
  "claudehaiku45" = "Anthropic",
  "gemini35" = "Google"
)

#' Readable label for a result column such as "qwen3235b_en_fr"
#'
#' Dictionary columns become "Lexicoder (EN)" / "(FR)". Unknown prefixes fall back to the
#' column name rather than to a silent "Other", so a broken lookup is visible
#' on the plot instead of collapsing several models into one label.
#'
#' @param model_name A result column name or bare model prefix
#' @param with_condition Append the language condition, e.g. "(FR->FR)"
#' @return A character label
get_model_display_name <- function(model_name, with_condition = TRUE) {
  # Lexicoder Sentiment Dictionary baselines: "lsd_en" -> "Lexicoder (EN)"
  if (grepl("^lsd_", model_name)) {
    return(paste0("Lexicoder (", toupper(sub("^lsd_", "", model_name)), ")"))
  }

  prefix <- sub("_[a-z]{2}_[a-z]{2}$", "", model_name)
  label <- if (prefix %in% names(model_display_name)) {
    unname(model_display_name[prefix])
  } else {
    prefix
  }

  if (!with_condition) return(label)

  cond <- regmatches(model_name, regexpr("_[a-z]{2}_[a-z]{2}$", model_name))
  if (length(cond) == 0) return(label)
  cond <- switch(sub("^_", "", cond),
                 "fr_fr" = "FR\u2192FR", "en_fr" = "EN\u2192FR",
                 "en_en" = "EN\u2192EN", cond)
  paste0(label, " (", cond, ")")
}

#' Manufacturer for a result column
get_model_provider <- function(model_name) {
  prefix <- sub("_[a-z]{2}_[a-z]{2}$", "", model_name)
  if (prefix %in% names(model_provider)) unname(model_provider[prefix]) else "Other"
}

#==============================================================================
# 7. HELPER FUNCTIONS
#==============================================================================

#' Determine whether a result column belongs to an open-weight model
#'
#' @param model_column A column name or bare model prefix
#' @return TRUE, FALSE, or NA for columns outside the mapping (e.g. dictionaries)
is_open_source <- function(model_column) {
  for (prefix in names(model_mapping)) {
    if (grepl(prefix, model_column, fixed = TRUE)) {
      return(unname(model_mapping[prefix]) %in% open_models)
    }
  }
  NA
}

#' Extra request arguments for one open-weight model on OpenRouter
#'
#' Combines the provider pin with reasoning suppression where required.
#'
#' The output cap is NOT set here: pass it via ellmer's portable
#' `params(max_tokens = ...)`, which translates to each provider's native
#' parameter name (OpenAI now rejects a raw `max_tokens` in the request body).
#'
#' @param prefix Model prefix, e.g. "gptoss20b"
#' @return A list suitable for ellmer's `api_args`
openrouter_args <- function(prefix) {
  args <- list(
    provider = list(
      order = list(unname(model_provider_pin[prefix])),
      allow_fallbacks = FALSE
    )
  )
  if (!is.null(model_reasoning_config[[prefix]])) {
    args$reasoning <- model_reasoning_config[[prefix]]
  }
  args
}
