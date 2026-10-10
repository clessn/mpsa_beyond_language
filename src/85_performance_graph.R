#################################################################
# PERFORMANCE GRAPH: BARS WITH EXACT VALUES
#################################################################
# One row per model, three bars per row (one per condition), the
# exact value at the end of each bar. Left panel correlation, right
# panel MAE. Bar length is the most direct encoding there is, and the
# number removes any need to read the axis.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

df_raw <- readRDS("data/clean/cor_results.rds")

# Llama 3.2 1B scored a different partial subset in each condition; it
# stays in the appendix table only
df <- df_raw %>%
  filter(!grepl("^llama321b_", model)) %>%
  mutate(
    is_open_source = sapply(model, is_open_source),
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is_open_source ~ "Open weights",
      TRUE ~ "Closed weights"
    ),
    display_name = sapply(model, get_model_display_name, with_condition = FALSE),
    row_label = case_when(
      grepl("^lsd_fr", model) ~ "French Lexicoder",
      grepl("^lsd_en", model) ~ "English Lexicoder",
      TRUE ~ display_name
    ),
    condition = case_when(
      grepl("_fr_fr$", model) | grepl("^lsd_fr", model) ~ "French text, French prompt",
      grepl("_en_fr$", model) ~ "French text, English prompt",
      grepl("_en_en$", model) | grepl("^lsd_en", model) ~ "Translated text, English prompt"
    )
  )

row_order <- df %>%
  group_by(row_label) %>%
  summarize(avg = mean(correlation), .groups = "drop") %>%
  arrange(avg) %>%
  pull(row_label)

cond_levels <- c("French text, French prompt", "French text, English prompt", "Translated text, English prompt")
cond_colours <- c("French text, French prompt" = "#1B4F72",
                  "French text, English prompt" = "#5DADE2",
                  "Translated text, English prompt" = "#AEB6BF")

PANEL_R   <- "Correlation (r), higher is better"
PANEL_MAE <- "MAE, lower is better"

long <- df %>%
  select(row_label, model_type, condition, correlation, mae) %>%
  pivot_longer(c(correlation, mae), names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(ifelse(metric == "correlation", PANEL_R, PANEL_MAE), levels = c(PANEL_R, PANEL_MAE)),
    label = ifelse(metric == PANEL_R, sprintf("%.2f", value), sprintf("%.3f", value)),
    row_label = factor(row_label, levels = row_order),
    model_type = factor(model_type, levels = c("Closed weights", "Open weights", "Dictionary")),
    # reversed so that the first condition is drawn on top within each row
    condition = factor(condition, levels = rev(cond_levels))
  )

p <- ggplot(long, aes(x = value, y = row_label, fill = condition)) +
  geom_col(position = position_dodge2(width = 0.85, preserve = "single", reverse = FALSE),
           width = 0.8) +
  geom_text(aes(label = label),
            position = position_dodge2(width = 0.85, preserve = "single", reverse = FALSE),
            hjust = -0.15, size = 2.5, colour = "grey15") +
  facet_grid(model_type ~ metric, scales = "free", space = "free_y", switch = "y") +
  scale_fill_manual(values = cond_colours, breaks = cond_levels, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(face = "bold", hjust = 0, size = 9.5),
    strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 1, size = 9),
    strip.placement = "outside",
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8.5),
    legend.position = "bottom",
    legend.text = element_text(size = 8.5),
    legend.key.size = unit(0.4, "cm"),
    panel.spacing.x = unit(1.4, "lines"),
    panel.spacing.y = unit(0.6, "lines"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave("results/graphs/model_performance.png", p, width = 8.3, height = 9.4, dpi = 300)
