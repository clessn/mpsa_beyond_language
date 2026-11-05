#################################################################
# PERFORMANCE GRAPH: CORRELATION AND MAE SIDE BY SIDE
#################################################################
# Replaces the separate correlation (src/60) and MAE (src/61) figures.
# One row per model-condition combination, sorted once by mean
# correlation; left panel Pearson r with 95% CI, right panel MAE.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

df_raw <- readRDS("data/clean/cor_results.rds")

df <- df_raw %>%
  mutate(
    is_open_source = sapply(model, is_open_source),
    model_type = case_when(
      grepl("^lsd_", model) ~ "Dictionary",
      is.na(is_open_source) ~ "Other",
      is_open_source ~ "Open weights",
      !is_open_source ~ "Closed weights"
    ),
    model_name = ifelse(grepl("^lsd_", model), "dict",
                        sub("_[a-z]{2}_[a-z]{2}$", "", model)),
    display_name = sapply(model, get_model_display_name, with_condition = FALSE),
    prompt_mechanism = case_when(
      grepl("_en_fr$", model) ~ "EN→FR",
      grepl("_fr_fr$", model) ~ "FR→FR",
      grepl("_en_en$", model) ~ "EN→EN",
      grepl("^lsd_fr", model) ~ "FR",
      grepl("^lsd_en", model) ~ "EN",
      TRUE ~ "Other"
    ),
    model_label = ifelse(
      grepl("^lsd_", model), display_name,
      paste0(display_name, " [", prompt_mechanism, "]",
             ifelse(n_obs < max(n_obs), paste0(" (n = ", n_obs, ")"), ""))
    ),
    # 95% CI of r by Fisher's z transformation, as returned by cor.test().
    # (Until November 2026 this used r +/- 1.96*sqrt((1-r^2)/(n-2)), the
    # standard error for testing rho = 0, which is too wide when r is high.)
    ci_lo = tanh(atanh(correlation) - qnorm(0.975) / sqrt(n_obs - 3)),
    ci_hi = tanh(atanh(correlation) + qnorm(0.975) / sqrt(n_obs - 3)),
    stars = ifelse(p_value < 0.001, "***",
                   ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
  )

# Sort models by mean correlation across conditions, conditions in a fixed order
model_order <- df %>%
  group_by(model_name) %>%
  summarize(avg = mean(abs_correlation, na.rm = TRUE)) %>%
  arrange(desc(avg)) %>%
  pull(model_name)

df <- df %>%
  mutate(
    model_group = match(model_name, model_order),
    cond_rank = match(prompt_mechanism, c("FR→FR", "EN→FR", "EN→EN", "FR", "EN"))
  ) %>%
  arrange(model_group, cond_rank) %>%
  mutate(model_label_ordered = factor(model_label, levels = rev(unique(model_label))))

# Alternating background bands, one per model
bands <- df %>%
  mutate(row = rev(seq_len(n()))) %>%
  group_by(model_group) %>%
  summarize(ymin = min(row) - 0.5, ymax = max(row) + 0.5) %>%
  mutate(shade = ifelse(model_group %% 2 == 1, "a", "b"))

PANEL_R   <- "Correlation with humans (r), higher is better"
PANEL_MAE <- "Distance from humans (MAE), lower is better"

long <- bind_rows(
  df %>% transmute(model_label_ordered, model_type,
                   panel = PANEL_R, value = correlation,
                   lo = ci_lo, hi = ci_hi,
                   label_x = hi + 0.03,
                   label = sprintf("%.2f%s", correlation, stars)),
  df %>% transmute(model_label_ordered, model_type,
                   panel = PANEL_MAE, value = mae,
                   lo = NA_real_, hi = NA_real_,
                   label_x = mae + 0.02,
                   label = sprintf("%.3f", mae))
) %>%
  mutate(panel = factor(panel, levels = c(PANEL_R, PANEL_MAE)))

bands_long <- bind_rows(mutate(bands, panel = PANEL_R), mutate(bands, panel = PANEL_MAE)) %>%
  mutate(panel = factor(panel, levels = c(PANEL_R, PANEL_MAE)))

zero_line <- data.frame(panel = factor(PANEL_R, levels = c(PANEL_R, PANEL_MAE)), x = 0)
arrows <- data.frame(
  panel = factor(c(PANEL_R, PANEL_MAE), levels = c(PANEL_R, PANEL_MAE)),
  x = c(1.0, 0.02), label = c("closer to humans \u2192", "\u2190 closer to humans")
)

p <- ggplot() +
  geom_rect(data = bands_long,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = shade),
            alpha = 0.5) +
  geom_vline(data = zero_line, aes(xintercept = x),
             linetype = "dotted", colour = "grey40", linewidth = 0.4) +
  geom_errorbarh(data = filter(long, !is.na(lo)),
                 aes(y = model_label_ordered, xmin = lo, xmax = hi, colour = model_type),
                 height = 0.25) +
  geom_point(data = long,
             aes(x = value, y = model_label_ordered, colour = model_type), size = 2.6) +
  geom_text(data = long,
            aes(x = label_x, y = model_label_ordered, label = label),
            hjust = 0, size = 2.6) +
  geom_text(data = arrows, aes(x = x, y = 0.4, label = label),
            hjust = c(1, 0), size = 2.7, colour = "grey35", fontface = "italic") +
  scale_y_discrete(expand = expansion(add = c(1.1, 0.6))) +
  facet_wrap(~ panel, scales = "free_x") +
  scale_fill_manual(values = c(a = "grey95", b = "grey88"), guide = "none") +
  scale_colour_manual(values = c("Open weights" = "#1F77B4",
                                 "Closed weights" = "#D62728",
                                 "Dictionary" = "#2CA02C",
                                 "Other" = "#7F7F7F"),
                      name = "Model type") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 8.5),
    axis.text.y = element_text(size = 7.5),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.spacing.x = unit(1.2, "lines"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Room for the labels: extend each panel's x range past the last value
p <- p + scale_x_continuous(expand = expansion(mult = c(0.05, 0.35)), breaks = seq(-0.5, 1, by = 0.25))

ggsave("results/graphs/model_performance.png", p, width = 7.5, height = 8.8, dpi = 300)
