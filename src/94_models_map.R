###############################################################################
# MODEL MAPPING AND CATEGORIZATION
#
# This script defines the mapping between model identifiers used in this project
# and their corresponding full names. It also categorizes models as open or closed
# source for analysis purposes.
#
# Author: Ral Zarek
# Date: March 2025
# Updated: July 2026 (model refresh - see docs/pub for rationale)
###############################################################################

#==============================================================================
# 1. MODEL GROUPINGS BY PROVIDER
#==============================================================================

# Alibaba models (Qwen)
alibaba_models <- c(
  "qwen/qwen3-32b",
  "accounts/fireworks/models/qwen3-235b-a22b"
)

# Meta models
meta_models <- c(
  "meta-llama/llama-4-scout-17b-16e-instruct"
)

# OpenAI open-weight models
openai_oss_models <- c(
  "openai/gpt-oss-20b"
)

# Anthropic models
anthropic_models <- c(
  "claude-haiku-4-5-20251001"
)

# Google models
google_models <- c(
  "gemini-3.5-flash"
)

# DeepSeek models
deepseek_models <- c(
  "accounts/fireworks/models/deepseek-v4-flash",
  "accounts/fireworks/models/deepseek-v3p2"
)

# OpenAI models (closed)
openai_models <- c(
  "gpt-5.6-luna"
)

#==============================================================================
# 2. MODEL CATEGORIZATION BY LICENSE
#==============================================================================

# Open source (open-weight) models
open_models <- c(
  meta_models,
  alibaba_models,
  openai_oss_models,
  "accounts/fireworks/models/deepseek-v3p2"
)

# Closed source models
closed_models <- c(
  anthropic_models,
  openai_models,
  "gemini-3.5-flash",
  "accounts/fireworks/models/deepseek-v4-flash"
)

#==============================================================================
# 3. MODEL MAPPING FOR CODE SIMPLIFICATION
#==============================================================================

# Simple mapping from model prefix to actual model name
model_mapping <- c(
  # Fireworks models
  "qwen3235b" = "accounts/fireworks/models/qwen3-235b-a22b",
  "deepseekv32" = "accounts/fireworks/models/deepseek-v3p2",
  "deepseekv4flash" = "accounts/fireworks/models/deepseek-v4-flash",

  # Groq models
  "llama4scout" = "meta-llama/llama-4-scout-17b-16e-instruct",
  "qwen332b" = "qwen/qwen3-32b",
  "gptoss20b" = "openai/gpt-oss-20b",

  # Other API models
  "claudehaiku45" = "claude-haiku-4-5-20251001",
  "gemini35" = "gemini-3.5-flash",
  "gpt56luna" = "gpt-5.6-luna"
)

#==============================================================================
# 4. HELPER FUNCTIONS
#==============================================================================

# Function to determine if a model is open source
is_open_source <- function(model_column) {
  # Extract model prefix from column name
  for (prefix in names(model_mapping)) {
    if (grepl(prefix, model_column, fixed = TRUE)) {
      # Look up the full model name
      full_name <- model_mapping[prefix]
      # Check if it's in the open_models list
      return(full_name %in% open_models)
    }
  }
  # Default for models not in our mapping (like dictionary models)
  return(NA)
}
