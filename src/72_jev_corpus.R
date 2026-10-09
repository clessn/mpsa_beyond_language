#################################################################
# FULL CORPUS SENTIMENT WITH JEV, COMPARED WITH GPT-5.6 LUNA AND LEXICODER
#################################################################
# Scores all 2,683 articles with Jev in the two conditions src/70_prompt_corpus.R
# runs (FR->FR and EN->FR, whole article as input), then compares Jev with the
# GPT-5.6 Luna corpus run behind Figure 5 and with Lexicoder, at the article,
# month and year level.
#
# Exploratory: separate files only. The corpus file, src/80 and the
# manuscript's Figure 5 are untouched.
#
# Differences from the GPT-5.6 Luna run, to keep in mind when comparing:
#   - both conditions are asked in one request (same state, two questions);
#     on the validation sample this moved scores by no more than repeat noise
#   - one run per article, as for GPT-5.6 Luna's corpus pass
#   - Jev was validated on single sentences; whole articles (~1,400 tokens on
#     average) are outside that validation, and TypeSafe notes accuracy can drop
#     as the state grows
#   - Jev's articles were scored on 2026-09-28, GPT-5.6 Luna's in October 2026
#
# Usage: Rscript src/72_jev_corpus.R
# Resumable: progress is saved every CHUNK articles.
#################################################################

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

source("src/98_jev_helpers.R")

PROGRESS_PATH <- "data/tmp/jev_corpus_progress.rds"
OUTPUT_PATH <- "data/clean/news_df_sentiment_jev.rds"
CHUNK <- 250
JEV_PRICE_PER_MTOK <- 0.042  # input only; output is free

stopifnot(nzchar(Sys.getenv("TYPESAFE_API_KEY")))

news <- readRDS("data/tmp/news_df_tone_index.rds")
stopifnot(nrow(news) == 2683, !anyDuplicated(news$doc_id))

#################################################################
# 1. SCORE (RESUMABLE)
#################################################################

done <- if (file.exists(PROGRESS_PATH)) readRDS(PROGRESS_PATH) else NULL
todo <- which(!news$doc_id %in% done$doc_id &
                !is.na(news$text_body) & nchar(news$text_body) > 0)
cat(sprintf("%d articles to score (%d already done)\n",
            length(todo), length(unique(done$doc_id))))

t0 <- Sys.time()
for (chunk in split(todo, ceiling(seq_along(todo) / CHUNK))) {
  res <- run_jev(news$text_body[chunk], c("en_fr", "fr_fr"), field = "article")
  res$doc_id <- news$doc_id[chunk][res$item]
  res$item <- NULL
  # Failed calls are dropped rather than saved, so the next run retries them
  failed <- unique(res$doc_id[is.na(res$level)])
  done <- bind_rows(done, filter(res, !doc_id %in% failed))
  saveRDS(done, PROGRESS_PATH)
  cat(sprintf("  %d / %d articles, %d failed in this chunk, %.0f s elapsed\n",
              length(unique(done$doc_id)), nrow(news), length(failed),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

jev <- done %>%
  mutate(value = jev_to_scale(level)) %>%
  select(doc_id, condition, value, confidence) %>%
  pivot_wider(names_from = condition, values_from = c(value, confidence),
              names_glue = "jev_{condition}_{.value}") %>%
  rename(jev_fr_fr = jev_fr_fr_value, jev_en_fr = jev_en_fr_value)

luna <- readRDS("data/clean/news_df_sentiment_corpus.rds") %>%
  select(doc_id, luna_fr_fr = gpt56luna_fr_fr, luna_en_fr = gpt56luna_en_fr)

corpus <- news %>%
  select(doc_id, date, source_media, text_body, lsd_fr = fr_tone_index,
         lsd_en = en_tone_index) %>%
  left_join(jev, by = "doc_id") %>%
  left_join(luna, by = "doc_id") %>%
  mutate(date = as.Date(date), n_chars = nchar(text_body)) %>%
  select(-text_body)
stopifnot(nrow(corpus) == nrow(news))
saveRDS(corpus, OUTPUT_PATH)

#################################################################
# 2. ARTICLE LEVEL
#################################################################

methods <- c(jev_fr_fr = "Jev FR→FR", jev_en_fr = "Jev EN→FR",
             luna_fr_fr = "GPT-5.6 Luna FR→FR", luna_en_fr = "GPT-5.6 Luna EN→FR",
             lsd_fr = "Lexicoder FR", lsd_en = "Lexicoder EN")

cat("\n=== COVERAGE ===\n")
for (m in names(methods)) {
  cat(sprintf("  %-18s %4d / %d articles scored\n", methods[[m]], sum(!is.na(corpus[[m]])),
              nrow(corpus)))
}

article_cor <- cor(corpus[names(methods)], use = "pairwise.complete.obs")
dimnames(article_cor) <- list(methods, methods)
cat("\n=== ARTICLE-LEVEL CORRELATIONS (Pearson) ===\n")
print(round(article_cor, 2))

cat("\n=== SHARE OF ARTICLES BY SIGN ===\n")
sign_share <- sapply(names(methods), \(m) {
  x <- na.omit(corpus[[m]])
  c(negative = mean(x < 0), neutral = mean(x == 0), positive = mean(x > 0), mean = mean(x))
})
colnames(sign_share) <- methods
print(round(sign_share, 2))

# Does confidence fall on long articles, as TypeSafe's docs would predict?
conf_length <- cor.test(corpus$jev_fr_fr_confidence, log(corpus$n_chars),
                        method = "spearman", exact = FALSE)
cat(sprintf("\n  Jev confidence vs log article length (FR->FR): rho = %.3f, p = %.3g\n",
            conf_length$estimate, conf_length$p.value))
cat(sprintf("  median Jev confidence: articles %.2f\n",
            median(corpus$jev_fr_fr_confidence, na.rm = TRUE)))

#################################################################
# 3. OVER TIME
#################################################################

series <- function(unit) {
  corpus %>%
    mutate(period = floor_date(date, unit)) %>%
    group_by(period) %>%
    summarise(across(all_of(names(methods)), \(x) mean(x, na.rm = TRUE)),
              n_articles = n(), .groups = "drop")
}
monthly <- series("month")
yearly <- series("year")

cat("\n=== CORRELATION OF THE TIME SERIES ===\n")
pairs <- list(c("jev_fr_fr", "luna_fr_fr"), c("jev_fr_fr", "lsd_fr"),
              c("luna_fr_fr", "lsd_fr"), c("jev_fr_fr", "jev_en_fr"),
              c("luna_fr_fr", "luna_en_fr"))
# Periods with at least 10 articles, where a mean is not one or two articles.
# 1995-1997 hold a handful of articles and otherwise dominate the yearly series.
busy_months <- filter(monthly, n_articles >= 10)
busy_years <- filter(yearly, n_articles >= 10)
time_cor <- bind_rows(lapply(pairs, \(p) tibble(
  pair = paste(methods[[p[1]]], "vs", methods[[p[2]]]),
  r_monthly = cor(monthly[[p[1]]], monthly[[p[2]]], use = "complete.obs"),
  r_yearly = cor(yearly[[p[1]]], yearly[[p[2]]], use = "complete.obs"),
  r_monthly_n10 = cor(busy_months[[p[1]]], busy_months[[p[2]]], use = "complete.obs"),
  r_yearly_n10 = cor(busy_years[[p[1]]], busy_years[[p[2]]], use = "complete.obs")
)))
print(mutate(time_cor, across(where(is.double), \(x) round(x, 3))))
cat(sprintf("  (%d months, %d with at least 10 articles; %d years, %d with at least 10)\n",
            nrow(monthly), nrow(busy_months), nrow(yearly), nrow(busy_years)))

plot_df <- busy_years %>%
  select(period, n_articles, jev_fr_fr, luna_fr_fr, lsd_fr) %>%
  pivot_longer(c(jev_fr_fr, luna_fr_fr, lsd_fr), names_to = "method",
               values_to = "sentiment") %>%
  mutate(method = factor(methods[method], levels = methods[c("jev_fr_fr", "luna_fr_fr", "lsd_fr")]))

p <- ggplot(plot_df, aes(period, sentiment, linetype = method, shape = method)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(colour = "grey20") +
  geom_point(aes(size = n_articles), colour = "grey20") +
  scale_size_area(max_size = 3.5, name = "Articles per year") +
  labs(
    title = sprintf("Yearly Mean Sentiment of Open-Source Coverage, %s–%s",
                    format(min(busy_years$period), "%Y"), format(max(busy_years$period), "%Y")),
    subtitle = "Whole articles scored on the -1 to 1 scale, French text with the French prompt",
    x = NULL, y = "Mean sentiment", linetype = NULL, shape = NULL,
    caption = paste("Years with at least 10 articles. GPT-5.6 Luna scores from",
                    "src/70_prompt_corpus.R; Jev 1.13 scores from src/72_jev_corpus.R.")
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("results/graphs/jev_corpus_time_series.png", p, width = 11, height = 6.5, dpi = 300)

#################################################################
# 4. COST, SPEED, SAVE
#################################################################

tokens <- done %>% distinct(doc_id, input_tokens)
cost <- sum(tokens$input_tokens, na.rm = TRUE) / 1e6 * JEV_PRICE_PER_MTOK
cat(sprintf("\n=== COST AND SPEED ===\n  %s input tokens, $%.3f for %d articles x 2 conditions\n",
            format(sum(tokens$input_tokens, na.rm = TRUE), big.mark = ","), cost, nrow(tokens)))
if (length(todo) > 0) cat(sprintf("  wall time this session: %.0f s\n", wall))

saveRDS(list(article_cor = article_cor, sign_share = sign_share, time_cor = time_cor,
             monthly = monthly, yearly = yearly, conf_length = conf_length,
             cost_usd = cost, input_tokens = sum(tokens$input_tokens, na.rm = TRUE)),
        "results/analysis/jev_corpus_comparison.rds")
cat("\nSaved", OUTPUT_PATH, ", results/analysis/jev_corpus_comparison.rds",
    "and results/graphs/jev_corpus_time_series.png\n")
