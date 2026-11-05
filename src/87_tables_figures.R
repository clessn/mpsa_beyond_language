# Tables and figures of the Results section.
#
# Table 1  description of the corpus, the benchmark and the models
# Figure 1 RQ1: prompt and text language (A), translator check (B)
# Figure 2 RQ2: models against the two dictionaries
# Table 2  paired tests between conditions (13 models of about 8B and more)
#
# In:  data/clean/df.rds, data/clean/news_df_sentiment_corpus.rds,
#      data/clean/cor_results.rds, results/analysis/f1_scores_{3,7}.rds,
#      results/analysis/intercoder_reliability.rds, results/analysis/llm_translation_test.rds,
#      results/analysis/lsd_on_llm_translation.rds
# Out: results/tables/table1_data.md, results/tables/table2_tests.md,
#      results/graphs/fig1_language.png, fig2_dictionaries.png

library(dplyr)
library(tidyr)
library(ggplot2)
source("src/94_models_map.R")

bound <- 0.05
small_models <- c("llama321b", "llama323b")

theme_paper <- theme_classic(base_size = 11) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(5, 10, 5, 5))

save_figure <- function(plot, file, height) {
  ggsave(file.path("results/graphs", file), plot, width = 6.5, height = height, dpi = 300)
}

# Table 1 -------------------------------------------------------------------

df <- readRDS("data/clean/df.rds")
corpus <- readRDS("data/clean/news_df_sentiment_corpus.rds")
coders <- readRDS("results/analysis/intercoder_reliability.rds")
words <- lengths(strsplit(trimws(df$sentences), "[[:space:]]+"))
truth <- df$ground_truth
open_weight <- sapply(names(model_display_name), is_open_source)

table_1 <- tribble(
  ~Characteristic, ~Value,
  "**Corpus**", "",
  "Articles", format(nrow(corpus), big.mark = ","),
  "Daily newspapers", sprintf("%d (9 Québec, 1 Ontario, 3 France)", n_distinct(corpus$source_media)),
  "Period", sprintf("%s to %s", format(min(corpus$date), "%B %Y"), format(max(corpus$date), "%B %Y")),
  "**Benchmark**", "",
  "Sentences (articles)", sprintf("%d (%d)", nrow(df), n_distinct(df$doc_id)),
  "Words per sentence, mean (range)", sprintf("%.0f (%d to %d)", mean(words), min(words), max(words)),
  "Coders", "3, native French speakers",
  "Krippendorff's alpha (interval)", sprintf("%.2f", coders$krippendorff_alpha),
  "Ground truth, mean (SD)", sprintf("%.2f (%.2f)", mean(truth), sd(truth)),
  "Negative / neutral / positive sentences", sprintf("%d / %d / %d", sum(truth < 0), sum(truth == 0), sum(truth > 0)),
  "**Models**", "",
  "Open-weight", sprintf("%d (1B to 671B parameters)", sum(open_weight)),
  "Closed-weight", sprintf("%d", sum(!open_weight)),
  "Conditions x runs per sentence", "3 x 3"
)
writeLines(c("| Characteristic | Value |", "|:---------------------------|:---------------------------|",
             sprintf("| %s | %s |", table_1$Characteristic, table_1$Value)),
           "results/tables/table1_data.md")

# Performance by model and condition ----------------------------------------

conditions <- c(fr_fr = "FR→FR", en_fr = "EN→FR", en_en = "EN→EN")
f1_3 <- readRDS("results/analysis/f1_scores_3.rds")
f1_7 <- readRDS("results/analysis/f1_scores_7.rds")
performance <- readRDS("data/clean/cor_results.rds") %>%
  filter(grepl("_(fr_fr|en_fr|en_en)$", model)) %>%
  transmute(model,
            prefix = sub("_(fr_fr|en_fr|en_en)$", "", model),
            condition = sub(".*_(fr_fr|en_fr|en_en)$", "\\1", model),
            name = unname(model_display_name[prefix]),
            r = correlation, mae,
            f1_3 = f1_3$weighted_f1[match(model, f1_3$model)],
            f1_7 = f1_7$weighted_f1[match(model, f1_7$model)])
main <- filter(performance, !prefix %in% small_models)

# Figure 1: RQ1 --------------------------------------------------------------
# Panel A: the three conditions (mean of three runs), as tested in Table 2.
# Panel B: the translator check (first run; most models scored the LLM
# translation once), with the Lexicoder. A symbol means the same condition in
# both panels.

symbols <- c("French prompt, French text", "English prompt, French text",
             "English prompt, Google translation", "English prompt, LLM translation")
panels <- c("A. Prompt and text language", "B. Translator")

panel_a <- main %>%
  transmute(name, r, panel = panels[1],
            condition = symbols[match(condition, c("fr_fr", "en_fr", "en_en"))])
panel_b <- bind_rows(
  readRDS("results/analysis/llm_translation_test.rds")$results %>%
    filter(model != "Llama 3.2 3B") %>%
    transmute(name = model, r, condition = symbols[match(condition, c(
      "EN prompt, French original", "EN prompt, Google translation", "EN prompt, GPT-5.6 Luna translation")) + 1]),
  readRDS("results/analysis/lsd_on_llm_translation.rds")$results %>%
    transmute(name = "Lexicoder dictionary", r, condition = symbols[2:4])
) %>%
  mutate(panel = panels[2])

model_order <- panel_a %>% filter(condition == symbols[1]) %>% arrange(r) %>% pull(name)
fig1_data <- bind_rows(panel_a, panel_b) %>%
  mutate(name = factor(name, levels = c("Lexicoder dictionary", model_order)),
         condition = factor(condition, levels = symbols),
         panel = factor(panel, levels = panels))

fig1 <- ggplot(fig1_data, aes(x = r, y = name)) +
  geom_line(aes(group = name), colour = "grey70") +
  geom_point(aes(shape = condition, fill = condition), size = 2.2) +
  facet_wrap(~ panel, nrow = 1) +
  scale_shape_manual(values = c(21, 21, 24, 22), drop = FALSE) +
  scale_fill_manual(values = c("black", "white", "white", "grey55"), drop = FALSE) +
  scale_x_continuous(limits = c(0.3, 0.85), breaks = seq(0.3, 0.8, 0.1)) +
  guides(shape = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
  labs(x = "Correlation with human coders", y = NULL) +
  theme_paper +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 10),
        panel.spacing.x = unit(1.2, "lines"))
save_figure(fig1, "fig1_language.png", 5)

# Figure 2: RQ2 --------------------------------------------------------------

dictionaries <- readRDS("data/clean/cor_results.rds") %>%
  filter(model %in% c("lsd_fr", "lsd_en")) %>%
  transmute(label = ifelse(model == "lsd_fr", "French Lexicoder", "English Lexicoder (translation)"),
            r = correlation)
fig2_data <- performance %>%
  filter(prefix != "llama321b") %>%
  group_by(name, prefix) %>%
  summarise(mean_r = mean(r), low = min(r), high = max(r), .groups = "drop") %>%
  mutate(weights = ifelse(sapply(prefix, is_open_source), "Open-weight", "Closed-weight"),
         name = reorder(name, mean_r))

dictionaries$label <- c("French\nLexicoder", "English Lexicoder\n(translation)")[match(dictionaries$label, c("French Lexicoder", "English Lexicoder (translation)"))]
fig2 <- ggplot(fig2_data, aes(x = mean_r, y = name)) +
  geom_vline(data = dictionaries, aes(xintercept = r), linetype = "dashed", colour = "grey40") +
  geom_text(data = dictionaries, aes(x = r, y = nrow(fig2_data) + 1.1, label = label),
            hjust = 1.05, size = 3, lineheight = 0.9, colour = "grey25") +
  geom_errorbar(aes(xmin = low, xmax = high), width = 0, orientation = "y") +
  geom_point(aes(shape = weights), size = 2.4, fill = "white") +
  scale_shape_manual(values = c(`Closed-weight` = 16, `Open-weight` = 21)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.8))) +
  scale_x_continuous(limits = c(0, 0.85), breaks = seq(0, 0.8, 0.2)) +
  labs(x = "Correlation with human coders, mean and range of the three conditions", y = NULL) +
  theme_paper
save_figure(fig2, "fig2_dictionaries.png", 4.6)

# Table 2 --------------------------------------------------------------------

metrics <- c(r = "Correlation", mae = "MAE", f1_3 = "F1, 3 categories", f1_7 = "F1, 7 categories")
comparisons <- list(
  `Prompt language` = c("fr_fr", "en_fr"),
  `Translation` = c("en_fr", "en_en"),
  `Complete workflows` = c("fr_fr", "en_en")
)

table_2 <- bind_rows(lapply(names(comparisons), function(cmp) {
  bind_rows(lapply(names(metrics), function(m) {
    wide <- main %>% select(name, condition, value = all_of(m)) %>%
      pivot_wider(names_from = condition, values_from = value)
    d <- wide[[comparisons[[cmp]][1]]] - wide[[comparisons[[cmp]][2]]]
    test <- t.test(d)
    se <- sd(d) / sqrt(length(d))
    p_tost <- max(pt((mean(d) + bound) / se, length(d) - 1, lower.tail = FALSE),
                  pt((mean(d) - bound) / se, length(d) - 1))
    tibble(Comparison = cmp, Metric = metrics[[m]], diff = mean(d),
           low = test$conf.int[1], high = test$conf.int[2], p = test$p.value, p_tost = p_tost)
  }))
}))

format_p <- function(p) ifelse(p < 0.001, "< .001", sub("^0", "", sprintf("%.3f", p)))
writeLines(c(
  "| Comparison | Metric | Difference | 95% CI | *p* | *p* (equivalence) |",
  "|:------------------|:-----------------|---------:|:----------------|------:|-------:|",
  with(table_2, sprintf("| %s | %s | %.3f | [%.3f, %.3f] | %s | %s |",
                        ifelse(duplicated(Comparison), "", Comparison), Metric, diff, low, high,
                        format_p(p), format_p(p_tost)))
), "results/tables/table2_tests.md")

print(table_2, n = Inf)
