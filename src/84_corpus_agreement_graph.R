#################################################################
# CORPUS AGREEMENT GRAPH
#################################################################
# Two article-level scatterplots for the 2,683 full articles:
#   A. English-prompt score against French-prompt score (same model)
#   B. French Lexicoder score against French-prompt score
# Shows directly that prompt language barely matters at corpus scale,
# and that the dictionary agrees weakly with the model and compresses
# its scores near zero.
#
# Title and note are set in the manuscript (APA figure caption and note).

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
cat(sprintf("dictionary vs model: same sign = %.1f%%, opposite sign = %.1f%%\n",
            100 * same_sign_dict, 100 * opposite_dict))
cat(sprintf("n = %d | r prompt = %.3f | same sign = %.1f%% | r dictionary = %.3f\n",
            nrow(df), r_prompt, 100 * same_sign, r_dict))
cat(sprintf("SD model FR = %.3f | SD dictionary = %.3f | dictionary range = [%.2f, %.2f]\n",
            sd(df$llm_fr_fr), sd(df$fr_tone_index), min(df$fr_tone_index), max(df$fr_tone_index)))

PANEL_A <- paste0("A. ", MODEL_NAME, ", English prompt")
PANEL_B <- "B. French Lexicoder dictionary"

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
  # Scores fall on a coarse grid, so the number of articles in each cell is
  # what matters: tiles shaded by count instead of overplotted points
  geom_bin2d(binwidth = c(0.1, 0.1), aes(fill = after_stat(count))) +
  scale_fill_gradient(low = "#DCE9F5", high = "#08306B", trans = "sqrt",
                      name = "Articles per cell",
                      breaks = c(1, 10, 50, 150)) +
  geom_abline(data = identity_line, aes(slope = 1, intercept = 0),
              linetype = "dashed", colour = "grey30", linewidth = 0.4) +
  geom_text(data = labels, aes(x = -0.95, y = Inf, label = label),
            hjust = 0, vjust = 1.4, size = 3.4, colour = "grey20", lineheight = 0.95) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
  labs(x = paste0(MODEL_NAME, ", French prompt"), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.spacing.x = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.margin = margin(5, 8, 14, 5)
  )

ggsave("results/graphs/corpus_agreement.png", p, width = 8, height = 4.8, dpi = 300)
