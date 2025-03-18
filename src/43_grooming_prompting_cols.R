library(dplyr)

df <- readRDS("data/tmp/data_all_models_sentiment.rds")

# List of columns to keep
columns <- c(
  "doc_id",
  "date",
  "source_media",
  "sentences",
  "sentences_en",
  "lsd_fr",
  "lsd_fr_bin",
  "lsd_en",
  "lsd_en_bin",
  "manual",
  "manual_bin",
  "gemma2_9b_it",
  "llama_3.3_70b_versatile",
  "mixtral_8x7b_32768",
  "deepseek_r1_distill_llama_70b",
  "claude_3_5_sonnet_run_1",
  "gemini_2_flash",
  "deepseek_chat",
  "gpt_4o"
)


# Keep only the columns needed
df_trimmed <- df %>%
  select(all_of(columns)) %>%
  rename(
    llama_3_3_70b_versatile = llama_3.3_70b_versatile,
    claude_3_5_sonnet_single = claude_3_5_sonnet_run_1,
    gemini_2_flash_single = gemini_2_flash
  ) %>%  
  mutate(
    gemma2_9b_it_bin = ifelse(gemma2_9b_it > 0, 1, ifelse(gemma2_9b_it < 0, 0, 0.5)),
    llama_3_3_70b_versatile_bin = ifelse(llama_3_3_70b_versatile > 0, 1, ifelse(llama_3_3_70b_versatile < 0, 0, 0.5)),
    mixtral_8x7b_32768_bin = ifelse(mixtral_8x7b_32768 > 0, 1, ifelse(mixtral_8x7b_32768 < 0, 0, 0.5)),
    deepseek_r1_distill_llama_70b_bin = ifelse(deepseek_r1_distill_llama_70b > 0, 1, ifelse(deepseek_r1_distill_llama_70b < 0, 0, 0.5)),
    claude_3_5_sonnet_single_bin = ifelse(claude_3_5_sonnet_single > 0, 1, ifelse(claude_3_5_sonnet_single < 0, 0, 0.5)),
    gemini_2_flash_single_bin = ifelse(gemini_2_flash_single > 0, 1, ifelse(gemini_2_flash_single < 0, 0, 0.5)),
    deepseek_chat_bin = ifelse(deepseek_chat > 0, 1, ifelse(deepseek_chat < 0, 0, 0.5)),
    gpt_4o_bin = ifelse(gpt_4o > 0, 1, ifelse(gpt_4o < 0, 0, 0.5)),
    lsd_fr_bin = ifelse(lsd_fr > 0, 1, ifelse(lsd_fr < 0, 0, 0.5)),
    lsd_en_bin = ifelse(lsd_en > 0, 1, ifelse(lsd_en < 0, 0, 0.5)),
    manual_bin = ifelse(manual > 0, 1, ifelse(manual < 0, 0, 0.5))
  ) 

saveRDS(df_trimmed, "data/tmp/data_all_models_sentiment_trimmed.rds")
