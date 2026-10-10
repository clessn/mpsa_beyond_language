#################################################################
# EQUIVALENCE GRAPH: MEAN PERFORMANCE UNDER EACH CONDITION
#################################################################
# For each metric, the mean across the 14 models that scored all 200
# sentences under each of the three conditions, with a 90% confidence
# interval. The verdict of the equivalence test (src/82) for each pair
# of neighbouring conditions is written between the points, so the
# reader sees both the sizes and whether the difference counts.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# --- Per-model, per-condition performance, exactly as in src/82 ---------
df <- readRDS("data/clean/df.rds")
model_cols <- names(df)[grepl("_(fr_fr|en_fr|en_en)$", names(df)) & !grepl("_cat$|_bin$", names(df))]
model_cols <- model_cols[!grepl("lsd|llama321b", model_cols)]
perf <- data.frame(
  col = model_cols,
  base_model = str_remove(model_cols, "_(fr_fr|en_fr|en_en)$"),
  condition  = str_extract(model_cols, "(fr_fr|en_fr|en_en)$"),
  stringsAsFactors = FALSE
)
perf$correlation <- sapply(perf$col, function(m) {
  ok <- !is.na(df[[m]]) & !is.na(df$ground_truth); cor(df[[m]][ok], df$ground_truth[ok])
})
perf$mae <- sapply(perf$col, function(m) {
  ok <- !is.na(df[[m]]) & !is.na(df$ground_truth); mean(abs(df[[m]][ok] - df$ground_truth[ok]))
})
f1_7 <- readRDS("results/analysis/f1_scores_7.rds")
f1_3 <- readRDS("results/analysis/f1_scores_3.rds")
perf$f1_7 <- f1_7$weighted_f1[match(perf$col, f1_7$model)]
perf$f1_3 <- f1_3$weighted_f1[match(perf$col, f1_3$model)]
n_models <- length(unique(perf$base_model))

# --- Means and 90% CI per condition -------------------------------------
metric_labels <- c(
  correlation = "Correlation with humans (r), higher is better",
  mae         = "Distance from humans (MAE), lower is better",
  f1_3        = "F1, 3 categories, higher is better",
  f1_7        = "F1, 7 categories, higher is better"
)
cond_labels <- c(fr_fr = "French prompt,\nFrench text",
                 en_fr = "English prompt,\nFrench text",
                 en_en = "English prompt,\ntranslated text")

means <- perf %>%
  pivot_longer(c(correlation, mae, f1_7, f1_3), names_to = "metric", values_to = "value") %>%
  group_by(metric, condition) %>%
  summarize(mean = mean(value), se = sd(value) / sqrt(n()), n = n(), .groups = "drop") %>%
  mutate(ci_low = mean - qt(0.95, n - 1) * se, ci_high = mean + qt(0.95, n - 1) * se,
         metric_label = factor(metric_labels[metric], levels = metric_labels),
         cond_label = factor(cond_labels[condition], levels = cond_labels),
         x = as.numeric(cond_label))

# --- Equivalence verdicts between neighbouring conditions ---------------
eq <- readRDS("results/analysis/equivalence_tests.rds")
verdicts <- eq$results %>%
  filter(comparison %in% c("fr_fr vs en_fr", "en_fr vs en_en")) %>%
  mutate(x = ifelse(comparison == "fr_fr vs en_fr", 1.5, 2.5),
         metric_label = factor(metric_labels[metric], levels = metric_labels),
         # Three verdicts: equivalent (TOST p < .05), differs (t-test p < .05),
         # or neither established (the data cannot tell)
         verdict = case_when(equivalent ~ "equivalent",
                             p_nhst < 0.05 ~ "differs",
                             TRUE ~ "undetermined"),
         label = case_when(verdict == "equivalent" ~ "≈ equivalent",
                           verdict == "differs" ~ "≠ differs",
                           TRUE ~ "? not established")) %>%
  left_join(means %>% group_by(metric) %>%
              summarize(y = max(ci_high) + 0.35 * (max(ci_high) - min(ci_low)), .groups = "drop"),
            by = "metric")

p <- ggplot(means, aes(x = x, y = mean)) +
  geom_line(colour = "grey60", linewidth = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, colour = "#1B4F72", linewidth = 0.6) +
  geom_point(size = 3.2, colour = "#1B4F72") +
  geom_text(aes(label = sprintf("%.3f", mean)), vjust = -1.1, size = 3, colour = "grey15") +
  geom_text(data = verdicts, aes(x = x, y = y, label = label, colour = verdict),
            size = 3, fontface = "italic") +
  scale_colour_manual(values = c(equivalent = "#1E8449", differs = "#C0392B", undetermined = "grey45"),
                      guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = levels(means$cond_label), limits = c(0.6, 3.4)) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.45))) +
  facet_wrap(~ metric_label, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = sprintf("Mean across %d models (90%% CI)", n_models)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = 10),
    panel.spacing = unit(1.2, "lines"),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(size = 9, lineheight = 0.9, margin = margin(t = 4)),
    plot.margin = margin(5, 8, 14, 5)
  )

ggsave("results/graphs/equivalence_tests.png", p, width = 8, height = 6.2, dpi = 300)
