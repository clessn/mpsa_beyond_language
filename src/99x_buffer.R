library(dplyr)

fireworks <- ellmer::chat_openai(
  system_prompt = "You are a helpful assistant",
  base_url = "https://api.fireworks.ai/inference/v1",
  api_key = Sys.getenv("FIREWORKS_API_KEY"),
  model = "accounts/fireworks/models/llama-v3p1-8b-instruct",
)

response <- fireworks$chat("What is the capital of France?")

df <- readRDS("data/tmp/data_all_models_sentiment.rds")

groq <- ellmer::chat_groq(
  system_prompt = "Your role is to answer simple questions",
  model = "llama-3.3-70b-versatile",
)

deepseekchat <- ellmer::chat_deepseek(
  system_prompt = system_prompt,
  model = "deepseek-chat",
  api_args = list(max_tokens = 20),  # Set maximum output tokens to 20
  echo = "none"
)

response <- deepseekchat$chat("How is the capital of France?")
print(response)

response <- groq$chat("What is the capital of France?")
response <- groq$chat("Which country I asked you about the capital?")
print(groq$tokens())
groq$set_turns(list())
tokens <- groq$tokens()
print(tokens)
response <- groq$chat("What is the capital of France?")
groq$set_turns(list())
response <- groq$chat("What is the capital of France?")
tokens <- groq$tokens()
print(tokens)
response <- groq$chat("Which country I asked you about the capital?")
tokens <- groq$tokens()

print(response)

gemini20 <- ellmer::chat_gemini(
  system_prompt = "You are a helpful assistant",
  model = "gemini-2.0-flash",
  echo = "none"
)

response <- gemini20$chat("What is the capital of France?")
print(response)
response <- gemini20$chat("Which country I asked you about the capital?")
print(response)
gemini20$set_turns(list())
response <- gemini20$chat("Which country I asked you about the capital?")
print(response)
