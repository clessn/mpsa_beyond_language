library(dplyr)

fireworks <- ellmer::chat_openai(
  system_prompt = "You are a helpful assistant",
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/llama-v3p1-8b-instruct",
)

response <- fireworks$chat("What is the capital of France?")
