#################################################################
# CONDITION GRAPH: ONE LINE PER MODEL ACROSS THE THREE CONDITIONS
#################################################################
# Correlation with human judgments under the three conditions. A thin
# grey line per model, the mean across models in bold. A flat line
# means the condition made no difference; a falling line means it
# cost accuracy. The equivalence tests (src/82) are reported in the
# text; this figure shows what they summarize.
#
# Title and note are set in the manuscript (APA figure caption and note).

library(dplyr)
library(ggplot2)
source("src/94_models_map.R")

# Models that scored all 200 sentences; Llama 3.2 3B is left out because
# its swing (r = 0.48 to 0.13) would compress every other line. It and
# Llama 3.2 1B stay in the appendix table.
cr <- readRDS("data/clean/cor_results.rds") %>%
  filter(!grepl("^lsd_|^llama321b_|^llama323b_", model)) %>%
  mutate(base = sub("_[a-z]{2}_[a-z]{2}$", "", model),
         cond = case_when(grepl("_fr_fr$", model) ~ 1,
                          grepl("_en_fr$", model) ~ 2,
                          TRUE ~ 3))
n_models <- length(unique(cr$base))

m <- cr %>% group_by(cond) %>% summarize(correlation = mean(correlation), .groups = "drop")
ymax <- max(cr$correlation) + 0.06

p <- ggplot(cr, aes(x = cond, y = correlation, group = base)) +
  annotate("rect", xmin = 0.85, xmax = 2.15, ymin = -Inf, ymax = Inf, fill = "#EEF3F8", alpha = 0.7) +
  annotate("text", x = 1.5, y = ymax, label = "Change the prompt language",
           size = 3.6, colour = "grey25", fontface = "bold") +
  annotate("text", x = 2.5, y = ymax, label = "Translate the text",
           size = 3.6, colour = "grey25", fontface = "bold") +
  geom_line(colour = "grey72", linewidth = 0.5) +
  geom_point(colour = "grey60", size = 1.5) +
  geom_line(data = m, aes(group = 1), colour = "#1B4F72", linewidth = 1.8) +
  geom_point(data = m, aes(group = 1), colour = "#1B4F72", size = 4) +
  geom_text(data = m, aes(label = sprintf("mean %.2f", correlation), group = 1),
            vjust = 2.1, colour = "#1B4F72", fontface = "bold", size = 3.6) +
  scale_x_continuous(breaks = 1:3, limits = c(0.75, 3.25),
                     labels = c("French prompt,\nFrench text",
                                "English prompt,\nFrench text",
                                "English prompt,\ntranslated text")) +
  scale_y_continuous(breaks = seq(0.5, 0.9, 0.05)) +
  labs(x = NULL,
       y = sprintf("Correlation with human judgments (r)\nthin lines: %d models; bold line: their mean", n_models)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 10.5, lineheight = 0.9, margin = margin(t = 6)),
        axis.title.y = element_text(size = 10, lineheight = 1.1),
        plot.margin = margin(5, 8, 12, 5),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/graphs/equivalence_tests.png", p, width = 7, height = 5.6, dpi = 300)
