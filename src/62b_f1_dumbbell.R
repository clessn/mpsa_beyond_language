#################################################################
# F1 GRAPH: POLARITY VERSUS FINE-GRAINED CLASSIFICATION
#################################################################
# One row per model, two points: weighted F1 on 3 categories and on
# 7 categories, each averaged over the three conditions. The gap is
# what fine-grained classification costs. Per-condition values are in
# the appendix tables. Replaces the 94-bar version from src/62.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

f3 <- readRDS("results/analysis/f1_scores_3.rds")
f7 <- readRDS("results/analysis/f1_scores_7.rds")

d <- inner_join(f3 %>% select(model, f1_3 = weighted_f1),
                f7 %>% select(model, f1_7 = weighted_f1), by = "model") %>%
  filter(!grepl("^llama321b_", model)) %>%
  mutate(name = sapply(model, get_model_display_name, with_condition = FALSE)) %>%
  group_by(name) %>%
  summarize(f1_3 = mean(f1_3), f1_7 = mean(f1_7), .groups = "drop") %>%
  arrange(f1_3) %>%
  mutate(name = factor(name, levels = name))

SCHEME_3 <- "Polarity (3 categories)"
SCHEME_7 <- "Fine-grained (7 categories)"
long <- d %>%
  pivot_longer(c(f1_3, f1_7), names_to = "scheme", values_to = "f1") %>%
  mutate(scheme = factor(ifelse(scheme == "f1_3", SCHEME_3, SCHEME_7), levels = c(SCHEME_3, SCHEME_7)))

p <- ggplot(d, aes(y = name)) +
  geom_segment(aes(x = f1_7, xend = f1_3, yend = name), colour = "grey78", linewidth = 1.3) +
  geom_point(data = long, aes(x = f1, colour = scheme), size = 3.2) +
  geom_text(data = long, aes(x = f1, label = sprintf("%.2f", f1), colour = scheme),
            vjust = -1.0, size = 2.7, show.legend = FALSE) +
  scale_colour_manual(values = setNames(c("#1B4F72", "#E67E22"), c(SCHEME_3, SCHEME_7)), name = NULL) +
  scale_x_continuous(limits = c(0.15, 0.85), breaks = seq(0.2, 0.8, 0.2)) +
  labs(x = "Weighted F1, mean of the three conditions", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        legend.position = "top",
        axis.title.x = element_text(margin = margin(t = 8)),
        plot.margin = margin(5, 8, 14, 5),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/graphs/f1_dumbbell.png", p, width = 7, height = 6.5, dpi = 300)
