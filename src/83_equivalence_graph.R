#################################################################
# EQUIVALENCE TEST GRAPH
#################################################################
# Plots the paired mean differences from src/82_equivalence_tests.R
# with their 90% confidence intervals against the declared
# equivalence bound (±0.05). A contrast is equivalent when its
# whole interval lies inside the shaded band.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(ggplot2)

eq <- readRDS("results/analysis/equivalence_tests.rds")
res <- eq$results
bound <- eq$declared_bound

metric_labels <- c(
  correlation = "Correlation (r), higher is better",
  mae         = "Mean absolute error, lower is better",
  f1_3        = "F1, 3 categories, higher is better",
  f1_7        = "F1, 7 categories, higher is better"
)

# In every contrast the first condition is the more French option, so a
# positive difference means that option scored higher
contrast_labels <- c(
  "fr_fr vs en_fr" = "Prompt language
French vs. English prompt,
same French text",
  "en_fr vs en_en" = "Text language
French original vs. English
translation, same English prompt",
  "fr_fr vs en_en" = "Full workflow
all-French vs. all-English"
)

df <- res %>%
  mutate(
    metric_label   = factor(metric_labels[metric], levels = metric_labels),
    contrast_label = factor(contrast_labels[comparison], levels = rev(contrast_labels)),
    verdict        = ifelse(equivalent, "Equivalent within ±0.05", "Not established")
  )

# Reading direction, written under the rows of each panel
arrows <- data.frame(
  metric_label = factor(metric_labels, levels = metric_labels),
  left  = ifelse(names(metric_labels) == "mae", "\u2190 French better", "\u2190 English better"),
  right = ifelse(names(metric_labels) == "mae", "English better \u2192", "French better \u2192")
)

p <- ggplot(df, aes(x = mean_diff, y = contrast_label)) +
  annotate("rect", xmin = -bound, xmax = bound, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25,
                 colour = "#1F77B4", linewidth = 0.6) +
  geom_point(aes(shape = verdict), size = 3, colour = "#1F77B4", fill = "white", stroke = 1) +
  geom_text(aes(x = ci_high + 0.004,
                label = sprintf("%+.3f", mean_diff)),
            hjust = 0, size = 3, colour = "grey20") +
  geom_text(data = arrows, aes(x = -0.004, y = 0.35, label = left),
            hjust = 1, size = 2.6, colour = "grey35", fontface = "italic") +
  geom_text(data = arrows, aes(x = 0.004, y = 0.35, label = right),
            hjust = 0, size = 2.6, colour = "grey35", fontface = "italic") +
  scale_shape_manual(values = c("Equivalent within ±0.05" = 16, "Not established" = 21),
                     name = NULL) +
  scale_x_continuous(limits = c(-0.06, 0.13), breaks = seq(-0.05, 0.10, by = 0.05)) +
  facet_wrap(~ metric_label, ncol = 2) +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.6))) +
  labs(x = sprintf("Mean paired difference, French option minus English option, across %d models (90%% CI)", unique(res$n)[1]), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    axis.text.y = element_text(size = 9, lineheight = 0.9),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.spacing.x = unit(1.2, "lines")
  )

ggsave("results/graphs/equivalence_tests.png", p, width = 8.5, height = 5.6, dpi = 300)
