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

The script 40_prompt.R will handle prompting models for a long period of time.
│   estimate the time it takes to make all of the prompt about 8 hours. I want it to run as  │

│   smoothly as possible during this time. All the groq prompts have a 2 sec system sleep    │
│   to account for a max 30 prompts per minute cap. However, groq also has a limit of 6000   │
│   tokens per minutes which is low.
