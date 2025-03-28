alibaba_models <- c(
  "accounts/fireworks/models/qwq-32b"
)

meta_models <- c(
  "accounts/fireworks/models/llama-v3p2-3b-instruct",
  "accounts/fireworks/models/llama-v3p3-70b-instruct",
  "llama-3.2-1b-preview"
)

mistral_models <- c(
  "mistral-saba-24b"
)

anthropic_models <- c(
  "claude-3-5-haiku-20241022"
)

google_models <- c(
  "gemini-2.0-flash",
  "gemma2-9b-it"
)

deepseek_models <- c(
  "deepseek-chat",
  "accounts/fireworks/models/deepseek-r1-basic"
)

openai_models <- c(
  "gpt-4o"
)

open_models <- c(
  meta_models,
  alibaba_models,
  mistral_models,
  "gemma2-9b-it",
  "accounts/fireworks/models/deepseek-r1-basic"
)

closed_models <- c(
  anthropic_models,
  openai_models,
  "gemini-2.0-flash",
  "deepseek-chat"
)

# Simple mapping from model prefix to actual model name
model_mapping <- c(
  # Fireworks models
  "llama323b" = "accounts/fireworks/models/llama-v3p2-3b-instruct",
  "qwq32b" = "accounts/fireworks/models/qwq-32b",
  "deepseekr1" = "accounts/fireworks/models/deepseek-r1-basic",
  "llama3370b" = "accounts/fireworks/models/llama-v3p3-70b-instruct",
  
  # Groq models
  "gemma29b" = "gemma2-9b-it",
  "llama321b" = "llama-3.2-1b-preview",
  "mistral" = "mistral-saba-24b",
  
  # Other API models
  "claude35" = "claude-3-5-haiku-20241022",
  "gemini20" = "gemini-2.0-flash",
  "deepseekchat" = "deepseek-chat",
  "gpt4o" = "gpt-4o"
)

# Define how to determine if a model is open source
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


