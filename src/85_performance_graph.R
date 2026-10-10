#################################################################
# PERFORMANCE GRAPH: SHADED TABLE OF r AND MAE
#################################################################
# One row per model, one column per condition, the exact number in
# every cell. Cells are shaded so that darker means better, for both
# metrics (higher r, lower MAE). Read a row to see whether the
# condition matters, a column to rank the models.
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
    # Dictionaries have no prompt: the French one reads the original text,
    # the English one the translation, so they sit under those text columns
    condition = case_when(
      grepl("_fr_fr$", model) | grepl("^lsd_fr", model) ~ "French text,\nFrench prompt",
      grepl("_en_fr$", model) ~ "French text,\nEnglish prompt",
      grepl("_en_en$", model) | grepl("^lsd_en", model) ~ "Translated text,\nEnglish prompt"
    )
  )

row_order <- df %>%
  group_by(row_label) %>%
  summarize(avg = mean(correlation), .groups = "drop") %>%
  arrange(avg) %>%
  pull(row_label)

cond_levels <- c("French text,\nFrench prompt", "French text,\nEnglish prompt", "Translated text,\nEnglish prompt")

long <- df %>%
  select(row_label, model_type, condition, correlation, mae) %>%
  pivot_longer(c(correlation, mae), names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  # Rank within each metric, 0 = worst, 1 = best, so one fill scale serves
  # both metrics and a single outlier does not flatten the shading
  mutate(goodness = if (first(metric) == "correlation") percent_rank(value)
         else percent_rank(-value)) %>%
  ungroup() %>%
  mutate(
    metric = factor(ifelse(metric == "correlation",
                           "Correlation with human judgments (r)",
                           "Mean absolute error (MAE)"),
                    levels = c("Correlation with human judgments (r)", "Mean absolute error (MAE)")),
    label = ifelse(grepl("Correlation", metric), sprintf("%.2f", value), sprintf("%.3f", value)),
    row_label = factor(row_label, levels = row_order),
    model_type = factor(model_type, levels = c("Closed weights", "Open weights", "Dictionary")),
    condition = factor(condition, levels = cond_levels)
  )

p <- ggplot(long, aes(x = condition, y = row_label, fill = goodness)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = label, colour = goodness > 0.55), size = 3.1) +
  facet_grid(model_type ~ metric, scales = "free", space = "free_y", switch = "y") +
  scale_fill_gradient(low = "#F1F6FB", high = "#1B4F72", guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    strip.text.x = element_text(face = "bold", hjust = 0, size = 10),
    strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 1, size = 9),
    strip.placement = "outside",
    axis.text.y = element_text(size = 9),
    axis.text.x.top = element_text(size = 8.5, lineheight = 0.9),
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(0.6, "lines"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave("results/graphs/model_performance.png", p, width = 8, height = 6.6, dpi = 300)
