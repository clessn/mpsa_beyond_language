# Figure 1: GPT-5.6 Luna's score on each of the 2,683 articles under the
# English prompt (left) and the French Lexicoder score (right), against the
# same model's score under the French prompt. Also prints the agreement
# statistics reported with the figure.
#
# In:  data/clean/news_df_sentiment_corpus.rds
# Out: results/graphs/corpus_agreement.png

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

MODEL_PREFIX <- Sys.getenv("CORPUS_MODEL", "gpt56luna")
MODEL_NAME <- unname(model_display_name[MODEL_PREFIX])

df <- readRDS("data/clean/news_df_sentiment_corpus.rds") %>%
  rename(llm_fr_fr = all_of(paste0(MODEL_PREFIX, "_fr_fr")),
         llm_en_fr = all_of(paste0(MODEL_PREFIX, "_en_fr"))) %>%
  filter(!is.na(llm_fr_fr), !is.na(llm_en_fr), !is.na(fr_tone_index))

r_prompt <- cor(df$llm_fr_fr, df$llm_en_fr)
r_dict   <- cor(df$llm_fr_fr, df$fr_tone_index)
same_sign <- mean(sign(df$llm_fr_fr) == sign(df$llm_en_fr))
same_sign_dict <- mean(sign(df$llm_fr_fr) == sign(df$fr_tone_index))
opposite_dict  <- mean(sign(df$llm_fr_fr) * sign(df$fr_tone_index) == -1)
cat(sprintf("n = %d; prompts: r = %.3f, same sign %.1f%%; dictionary: r = %.3f, same sign %.1f%%, opposite %.1f%%\n",
            nrow(df), r_prompt, 100 * same_sign, r_dict, 100 * same_sign_dict, 100 * opposite_dict))

PANEL_A <- paste0(MODEL_NAME, ", English prompt")
PANEL_B <- "French Lexicoder dictionary"

long <- df %>%
  select(llm_fr_fr, llm_en_fr, fr_tone_index) %>%
  pivot_longer(c(llm_en_fr, fr_tone_index), names_to = "panel", values_to = "y") %>%
  mutate(panel = factor(ifelse(panel == "llm_en_fr", PANEL_A, PANEL_B),
                        levels = c(PANEL_A, PANEL_B)))

labels <- data.frame(
  panel = factor(c(PANEL_A, PANEL_B), levels = c(PANEL_A, PANEL_B)),
  label = c(sprintf("r = %.2f\n%.0f%% same sign", r_prompt, 100 * same_sign),
            sprintf("r = %.2f\n%.0f%% same sign, %.0f%% opposite", r_dict, 100 * same_sign_dict, 100 * opposite_dict))
)
identity_line <- data.frame(panel = factor(PANEL_A, levels = c(PANEL_A, PANEL_B)))

p <- ggplot(long, aes(x = llm_fr_fr, y = y)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
  # Scores fall on a coarse grid, so cells are shaded by count
  geom_bin2d(binwidth = c(0.1, 0.1), aes(fill = after_stat(count))) +
  scale_fill_gradient(low = "#DCE9F5", high = "#08306B", trans = "sqrt",
                      name = "Articles per cell",
                      breaks = c(1, 10, 50, 150)) +
  geom_abline(data = identity_line, aes(slope = 1, intercept = 0),
              linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  facet_wrap(~ panel, scales = "free_y", strip.position = "left") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(x = paste0(MODEL_NAME, ", French prompt"), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text.y.left = element_text(angle = 90, size = 11, face = "plain"),
    strip.placement = "outside",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.spacing.x = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.margin = margin(5, 8, 14, 5)
  )

ggsave("results/graphs/corpus_agreement.png", p, width = 8, height = 4.8, dpi = 300)
