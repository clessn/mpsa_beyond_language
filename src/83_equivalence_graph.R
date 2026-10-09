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
  correlation = "Correlation (r)",
  mae         = "Mean absolute error",
  f1_3        = "F1, 3 categories",
  f1_7        = "F1, 7 categories"
)

contrast_labels <- c(
  "fr_fr vs en_fr" = "Prompt language\nFR→FR minus EN→FR",
  "en_fr vs en_en" = "Text language\nEN→FR minus EN→EN",
  "fr_fr vs en_en" = "Full workflow\nFR→FR minus EN→EN"
)

df <- res %>%
  mutate(
    metric_label   = factor(metric_labels[metric], levels = metric_labels),
    contrast_label = factor(contrast_labels[comparison], levels = rev(contrast_labels)),
    verdict        = ifelse(equivalent, "Equivalent within ±0.05", "Not established")
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
  scale_shape_manual(values = c("Equivalent within ±0.05" = 16, "Not established" = 21),
                     name = NULL) +
  scale_x_continuous(limits = c(-0.06, 0.13), breaks = seq(-0.05, 0.10, by = 0.05)) +
  facet_wrap(~ metric_label, ncol = 2) +
  labs(x = "Mean paired difference across 11 models (90% CI)", y = NULL) +
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

ggsave("results/graphs/equivalence_tests.png", p, width = 8, height = 5, dpi = 300)
