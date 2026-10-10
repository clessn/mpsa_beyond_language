#################################################################
# PERFORMANCE GRAPH: CORRELATION AND MAE, ONE ROW PER MODEL
#################################################################
# One row per model, sorted by mean correlation across conditions.
# The three conditions are three markers on the same row, so the
# prompt-language and translation effects are read within a row.
# Left panel Pearson r, right panel MAE. Exact values and 95% CIs are
# in the appendix table written by src/86_performance_table.R.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

df_raw <- readRDS("data/clean/cor_results.rds")

# Llama 3.2 1B scored a different partial subset of sentences in each
# condition, so its metrics are not comparable; it stays in the appendix tables
df <- df_raw %>%
  filter(!grepl("^llama321b_", model)) %>%
  mutate(
    is_open_source = sapply(model, is_open_source),
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is_open_source ~ "Open weights",
      TRUE ~ "Closed weights"
    ),
    model_name = ifelse(grepl("^lsd_", model), "dict",
                        sub("_[a-z]{2}_[a-z]{2}$", "", model)),
    display_name = sapply(model, get_model_display_name, with_condition = FALSE),
    condition = case_when(
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "French dictionary on original",
      grepl("^lsd_en", model) ~ "English dictionary on translation"
    ),
    row_label = ifelse(grepl("^lsd_", model), "Lexicoder", display_name)
  )

model_order <- df %>%
  group_by(row_label) %>%
  summarize(avg = mean(correlation), .groups = "drop") %>%
  arrange(avg) %>%
  pull(row_label)

df <- df %>%
  mutate(
    row_label  = factor(row_label, levels = model_order),
    model_type = factor(model_type, levels = c("Closed weights", "Open weights", "Dictionary")),
    condition  = factor(condition, levels = c("FR→FR", "EN→FR", "EN→EN",
                                              "French dictionary on original",
                                              "English dictionary on translation"))
  )

PANEL_R   <- "Correlation (r), higher is better"
PANEL_MAE <- "MAE, lower is better"

long <- df %>%
  select(row_label, model_type, condition, correlation, mae) %>%
  pivot_longer(c(correlation, mae), names_to = "panel", values_to = "value") %>%
  mutate(panel = factor(ifelse(panel == "correlation", PANEL_R, PANEL_MAE),
                        levels = c(PANEL_R, PANEL_MAE)))

cond_colours <- c("FR→FR" = "#1B4F72", "EN→FR" = "#E67E22", "EN→EN" = "#7F8C8D",
                  "French dictionary on original" = "#1B4F72",
                  "English dictionary on translation" = "#7F8C8D")
cond_shapes  <- c("FR→FR" = 16, "EN→FR" = 17, "EN→EN" = 15,
                  "French dictionary on original" = 1,
                  "English dictionary on translation" = 0)

p <- ggplot(long, aes(x = value, y = row_label)) +
  geom_line(aes(group = row_label), colour = "grey75", linewidth = 0.5) +
  geom_point(aes(colour = condition, shape = condition), size = 2.8) +
  facet_grid(model_type ~ panel, scales = "free", space = "free_y", switch = "y") +
  scale_colour_manual(values = cond_colours, name = NULL) +
  scale_shape_manual(values = cond_shapes, name = NULL) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  labs(x = NULL, y = NULL) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    strip.text.x = element_text(face = "bold", hjust = 0, size = 9.5),
    strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 1, size = 9),
    strip.placement = "outside",
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.spacing.x = unit(1.4, "lines"),
    panel.spacing.y = unit(0.8, "lines"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave("results/graphs/model_performance.png", p, width = 8, height = 6, dpi = 300)
