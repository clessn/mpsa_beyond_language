# Figure 3: weighted F1 on polarity (3 categories) and on the seven
# categories, averaged over the three conditions, one row per model.
#
# In:  results/analysis/f1_scores_{3,7}.rds
# Out: results/graphs/f1_dumbbell.png

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

f3 <- readRDS("results/analysis/f1_scores_3.rds")
f7 <- readRDS("results/analysis/f1_scores_7.rds")

d <- inner_join(f3 %>% select(model, f1_3 = weighted_f1),
                f7 %>% select(model, f1_7 = weighted_f1), by = "model") %>%
  filter(!grepl("^llama321b_", model)) %>%
  mutate(name = sapply(model, get_model_display_name, with_condition = FALSE),
         is_open = sapply(model, is_open_source),
         model_type = case_when(grepl("^lsd_", model) ~ "Dictionary",
                                is_open ~ "Open weights",
                                TRUE ~ "Closed weights")) %>%
  group_by(name, model_type) %>%
  summarize(f1_3 = mean(f1_3), f1_7 = mean(f1_7), .groups = "drop") %>%
  arrange(f1_3) %>%
  mutate(name = factor(name, levels = name))

SCHEME_3 <- "Polarity (3 categories)"
SCHEME_7 <- "Fine-grained (7 categories)"
long <- d %>%
  pivot_longer(c(f1_3, f1_7), names_to = "scheme", values_to = "f1") %>%
  mutate(scheme = factor(ifelse(scheme == "f1_3", SCHEME_3, SCHEME_7), levels = c(SCHEME_3, SCHEME_7)))

type_colours <- c("Closed weights" = "#D62728", "Open weights" = "#1F77B4", "Dictionary" = "#2CA02C")

p <- ggplot(d, aes(y = name)) +
  geom_segment(aes(x = f1_7, xend = f1_3, yend = name, colour = model_type),
               linewidth = 1.2, alpha = 0.5, show.legend = FALSE) +
  geom_point(data = long, aes(x = f1, shape = scheme, colour = model_type), size = 3.2,
             fill = "white", stroke = 1.1) +
  geom_text(data = long, aes(x = f1, label = sprintf("%.2f", f1)),
            vjust = -1.0, size = 2.7, colour = "grey25") +
  scale_colour_manual(values = type_colours, name = NULL) +
  scale_shape_manual(values = setNames(c(16, 21), c(SCHEME_3, SCHEME_7)), name = NULL) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16)),
         shape = guide_legend(order = 2)) +
  scale_x_continuous(limits = c(0.15, 0.85), breaks = seq(0.2, 0.8, 0.2)) +
  labs(x = "Weighted F1, mean of the three conditions", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(4, 0, 0, 0),
        legend.text = element_text(size = 9),
        axis.title.x = element_text(margin = margin(t = 8)),
        plot.margin = margin(5, 8, 14, 5),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/graphs/f1_dumbbell.png", p, width = 7, height = 6.5, dpi = 300)
