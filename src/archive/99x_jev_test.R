#################################################################
# JEV (TYPESAFE) SENTIMENT TEST
#################################################################
# Exploratory test of Jev, TypeSafe's System One model, on the 200-sentence
# validation sample. SUPERSEDED for publication by src/42_jev_prompt.R, which
# scores Jev with one condition per request and feeds the main pipeline; kept
# for its checks (determinism, batching, confidence analysis). Kept OUT of the main pipeline on purpose: it writes only
# jev_* files and never touches df.rds or the committed result tables.
#
# Jev does not generate text. Each condition is one Score question whose five
# levels are the five anchors of the paper's prompt (src/93_prompts.R), with the
# same wording in the same language. The answer is a probability per level, so:
#   - jev        = probability-weighted level mapped back to -1..1. Every
#                  metric uses it, binned for F1 with the same rule as the other
#                  models (== 0 -> neutral). The API rounds to two decimals, so
#                  a confident neutral lands on exactly 0. The docs warn this
#                  expectation is not a precise magnitude: read MAE/CCC with care.
#   - argmax     = most probable level, reported as a secondary F1 only. It
#                  calls ~95 of 200 sentences neutral, while the three-coder
#                  mean is exactly 0 for 14, so it answers a different question.
#   - confidence = how concentrated the distribution is. No other model in the
#                  paper returns one; section 6 tests whether it means anything.
#
# EN->FR and FR->FR share the same state (the French sentence), so both
# questions go in one request. Section 2 checks this does not change answers.
#
# Usage: Rscript src/99x_jev_test.R
# API responses are cached in data/tmp/jev_*.rds; delete them to re-query.
#################################################################

library(httr2)
library(dplyr)
library(tidyr)
library(purrr)

JEV_MODEL <- "jev-1.13.0"   # pinned, not jev-latest, so results stay reproducible
JEV_URL <- "https://api.typesafe.ai/v1/systemone"
JEV_PRICE_PER_MTOK <- 0.042 # input tokens only; output is free (docs.typesafe.ai/models, 2026-09-28)
CHECKS_PATH <- "data/tmp/jev_checks.rds"
SCORES_PATH <- "data/tmp/jev_scores.rds"

stopifnot(nzchar(Sys.getenv("TYPESAFE_API_KEY")))

#################################################################
# 1. QUESTIONS AND API HELPERS
#################################################################

levels_en <- list(
  "Strong negative sentiment: highly critical, hostile, or pessimistic content",
  "Moderate negative sentiment: somewhat negative, disapproving, or concerned content",
  "Neutral sentiment: factual, balanced, or neither positive nor negative content",
  "Moderate positive sentiment: somewhat positive, approving, or optimistic content",
  "Strong positive sentiment: highly supportive, enthusiastic, or optimistic content"
)

levels_fr <- list(
  "Sentiment négatif fort : contenu très critique, hostile ou pessimiste",
  "Sentiment négatif modéré : contenu plutôt négatif, désapprobateur ou préoccupant",
  "Sentiment neutre : contenu factuel, équilibré, ou ni positif ni négatif",
  "Sentiment positif modéré : contenu plutôt positif, approbateur ou optimiste",
  "Sentiment positif fort : contenu très favorable, enthousiaste ou optimiste"
)

questions <- list(
  en_fr = list(
    type = "score",
    instructions = paste(
      "Analyze the sentiment of the French text in `sentence`, considering",
      "cultural and linguistic nuances in French. Consider its emotional tone,",
      "word choice, and overall message."),
    criteria = levels_en
  ),
  fr_fr = list(
    type = "score",
    instructions = paste(
      "Analysez le sentiment du texte français dans `sentence`, en tenant compte",
      "des nuances culturelles et linguistiques en français. Considérez son ton",
      "émotionnel, le choix des mots et le message global."),
    criteria = levels_fr
  ),
  en_en = list(
    type = "score",
    instructions = paste(
      "Analyze the sentiment of the English text in `sentence`, considering",
      "cultural and linguistic nuances in English. Consider its emotional tone,",
      "word choice, and overall message."),
    criteria = levels_en
  )
)

jev_request <- function(text, qids) {
  request(JEV_URL) |>
    req_auth_bearer_token(Sys.getenv("TYPESAFE_API_KEY")) |>
    req_body_json(list(
      model = JEV_MODEL,
      state = list(sentence = text),
      questions = questions[qids]
    )) |>
    req_timeout(60) |>
    req_retry(max_tries = 5,
              is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504, 520, 529)) |>
    req_error(is_error = \(r) FALSE)
}

# One row per question. A failed call yields NA scores, never a stale value.
parse_jev <- function(resp, qids) {
  failed <- inherits(resp, "error") || resp_status(resp) != 200
  if (failed) {
    status <- if (inherits(resp, "error")) conditionMessage(resp) else resp_status(resp)
    warning("Jev call failed: ", status)
    return(tibble(condition = qids, level = NA_real_, confidence = NA_real_,
                  probs = list(rep(NA_real_, 5)), model = NA_character_,
                  input_tokens = NA_real_))
  }
  body <- resp_body_json(resp)
  map_dfr(qids, \(q) {
    a <- body$answers[[q]]
    tibble(
      condition = q,
      level = a$score,
      confidence = a$confidence,
      probs = list(unlist(a$probabilities[as.character(0:4)])),
      model = body$model,
      input_tokens = body$usage$input_tokens
    )
  })
}

# Runs one request per text, in parallel, and returns the parsed answers.
run_jev <- function(texts, qids, max_active = 10) {
  reqs <- map(texts, \(t) jev_request(t, qids))
  resps <- req_perform_parallel(reqs, on_error = "continue", max_active = max_active,
                                progress = FALSE)
  map_dfr(seq_along(resps), \(i) parse_jev(resps[[i]], qids) |> mutate(item = i))
}

#################################################################
# 2. PRE-FLIGHT CHECKS: DETERMINISM AND BATCHING
#################################################################
# The paper averages three runs per sentence and measures a prompt-language
# effect of about 0.006, so two things must hold before trusting one batched run:
# repeated calls agree, and asking EN->FR and FR->FR together gives the same
# answers as asking them separately.

sample_df <- readRDS("data/tmp/data_manual_ranking.rds") |>
  select(doc_id, sentences, sentences_en)
stopifnot(nrow(sample_df) == 200)

if (!file.exists(CHECKS_PATH)) {
  set.seed(42)
  check_idx <- sample(nrow(sample_df), 15)
  check_texts <- sample_df$sentences[check_idx]

  # Sequential calls, so per-request latency is measured without contention
  latency <- map_dbl(check_texts, \(t) {
    t0 <- Sys.time()
    req_perform(jev_request(t, c("en_fr", "fr_fr")))
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  })

  checks <- list(
    idx = check_idx,
    latency = latency,
    together = map(1:3, \(r) run_jev(check_texts, c("en_fr", "fr_fr"))),
    en_fr_alone = run_jev(check_texts, "en_fr"),
    fr_fr_alone = run_jev(check_texts, "fr_fr")
  )
  saveRDS(checks, CHECKS_PATH)
}
checks <- readRDS(CHECKS_PATH)

rep_levels <- sapply(checks$together, \(x) x$level)
max_rep_diff <- max(apply(rep_levels, 1, \(x) diff(range(x))))

together_1 <- checks$together[[1]]
alone <- bind_rows(checks$en_fr_alone, checks$fr_fr_alone)
batch_diff <- together_1 |>
  inner_join(alone, by = c("item", "condition"), suffix = c("_tog", "_alone")) |>
  summarise(max_diff = max(abs(level_tog - level_alone)))

cat("=== PRE-FLIGHT CHECKS (15 sentences) ===\n")
cat(sprintf("  latency per request      median %.2f s, max %.2f s\n",
            median(checks$latency), max(checks$latency)))
cat(sprintf("  repeat-to-repeat spread  max %.4f levels over 3 repeats\n", max_rep_diff))
cat(sprintf("  batched vs separate      max %.4f levels\n\n", batch_diff$max_diff))

#################################################################
# 3. SCORE THE 200 SENTENCES
#################################################################

# Three runs, averaged, as for the other models: Jev is not fully deterministic
# (see the repeat-to-repeat spread above).
N_RUNS <- 3
runs <- map(seq_len(N_RUNS), \(run) {
  path <- sub("\\.rds$", paste0("_run", run, ".rds"), SCORES_PATH)
  if (!file.exists(path)) {
    t0 <- Sys.time()
    fr <- run_jev(sample_df$sentences, c("en_fr", "fr_fr"))
    en <- run_jev(sample_df$sentences_en, "en_en")
    wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    scores <- bind_rows(fr, en) |>
      mutate(doc_id = sample_df$doc_id[item], sentences = sample_df$sentences[item],
             run = run)
    attr(scores, "wall_seconds") <- wall
    saveRDS(scores, path)
  }
  scores <- readRDS(path)

  # Re-request only the items whose call failed, then update the cache
  failed <- scores |> filter(is.na(level)) |> distinct(item, condition) |>
    mutate(request = if_else(condition == "en_en", "en", "fr")) |> distinct(item, request)
  if (nrow(failed) > 0) {
    cat(sprintf("Run %d: re-requesting %d failed item(s)\n", run, nrow(failed)))
    for (k in seq_len(nrow(failed))) {
      i <- failed$item[k]
      qids <- if (failed$request[k] == "en") "en_en" else c("en_fr", "fr_fr")
      text <- if (failed$request[k] == "en") sample_df$sentences_en[i] else sample_df$sentences[i]
      fresh <- parse_jev(req_perform(jev_request(text, qids)), qids) |>
        mutate(item = i, doc_id = sample_df$doc_id[i], sentences = sample_df$sentences[i],
               run = run)
      scores <- scores |> filter(!(item == i & condition %in% qids)) |> bind_rows(fresh)
    }
    attr(scores, "wall_seconds") <- attr(readRDS(path), "wall_seconds")
    saveRDS(scores, path)
  }
  scores
})
wall_per_run <- map_dbl(runs, \(x) attr(x, "wall_seconds"))

scores <- bind_rows(runs) |>
  group_by(doc_id, sentences, condition) |>
  summarise(
    level = mean(level),
    confidence = mean(confidence),
    probs = list(Reduce(`+`, probs) / n()),
    input_tokens = mean(input_tokens),
    model = paste(unique(model), collapse = ","),
    .groups = "drop"
  ) |>
  mutate(
    jev = (level - 2) / 2,
    argmax = map_int(probs, \(p) if (anyNA(p)) NA_integer_ else which.max(p) - 1L)
  )

cat("=== SCORING RUNS ===\n")
cat(sprintf("  model %s, %d runs x %d requests, %.1f s wall time per run (10 in flight)\n",
            paste(unique(scores$model), collapse = ","), N_RUNS, 2 * nrow(sample_df),
            mean(wall_per_run)))
cat(sprintf("  failed answers: %d of %d\n\n",
            sum(map_int(runs, \(x) sum(is.na(x$level)))), N_RUNS * nrow(scores)))

#################################################################
# 4. METRICS, IDENTICAL FOR JEV AND THE TWELVE MODELS
#################################################################
# Same definitions as src/50_cor.R (Pearson, MAE), src/54_ccc.R (Lin's CCC) and
# src/52_fscore_3.R (3-category weighted F1, weights = true class support).

lin_ccc <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  (2 * cov(x, y)) / (var(x) + var(y) + (mean(x) - mean(y))^2)
}

three_cat <- function(x) {
  case_when(is.na(x) ~ NA_character_, x < 0 ~ "negative",
            x == 0 ~ "neutral", TRUE ~ "positive")
}

weighted_f1_3 <- function(pred, truth) {
  lv <- c("negative", "neutral", "positive")
  ok <- !is.na(pred) & !is.na(truth)
  cm <- table(factor(pred[ok], lv), factor(truth[ok], lv))
  f1 <- sapply(seq_along(lv), \(i) {
    tp <- cm[i, i]; p <- tp / sum(cm[i, ]); r <- tp / sum(cm[, i])
    if (is.nan(p) || is.nan(r) || p + r == 0) 0 else 2 * p * r / (p + r)
  })
  sum(f1 * colSums(cm)) / sum(cm)
}

metrics <- function(pred, truth) {
  ok <- complete.cases(pred, truth)
  tibble(
    r = cor(pred[ok], truth[ok]),
    mae = mean(abs(pred[ok] - truth[ok])),
    ccc = lin_ccc(pred, truth),
    f1_3 = weighted_f1_3(three_cat(pred), three_cat(truth)),
    n = sum(ok)
  )
}

df <- readRDS("data/clean/df.rds")
jev_wide <- scores |>
  select(doc_id, sentences, condition, jev, argmax, confidence) |>
  pivot_wider(names_from = condition, values_from = c(jev, argmax, confidence))

# df.rds carries the pipeline's own Jev columns (src/42_jev_prompt.R); drop them
# so this test's batched scores are the only Jev columns here
eval_df <- df |>
  select(-starts_with("jev_")) |>
  select(doc_id, sentences, ground_truth, matches("_(en_fr|fr_fr|en_en)$"), lsd_fr, lsd_en) |>
  inner_join(jev_wide, by = c("doc_id", "sentences"))
if (nrow(eval_df) != 200) stop("Join changed the row count: ", nrow(eval_df))

source("src/94_models_map.R")

conditions <- c(en_fr = "EN→FR", fr_fr = "FR→FR", en_en = "EN→EN")

argmax_3cat <- function(a) {
  case_when(is.na(a) ~ NA_character_, a <= 1 ~ "negative",
            a == 2 ~ "neutral", TRUE ~ "positive")
}

jev_metrics <- map_dfr(names(conditions), \(cnd) {
  metrics(eval_df[[paste0("jev_", cnd)]], eval_df$ground_truth) |>
    mutate(
      model = "Jev 1.13", condition = conditions[[cnd]],
      f1_3_argmax = weighted_f1_3(argmax_3cat(eval_df[[paste0("argmax_", cnd)]]),
                                  three_cat(eval_df$ground_truth))
    )
})

other_cols <- grep("^jev_", grep("_(en_fr|fr_fr|en_en)$", names(df), value = TRUE),
                   value = TRUE, invert = TRUE)
other_metrics <- map_dfr(other_cols, \(col) {
  prefix <- sub("_(en_fr|fr_fr|en_en)$", "", col)
  cnd <- sub(paste0("^", prefix, "_"), "", col)
  metrics(eval_df[[col]], eval_df$ground_truth) |>
    mutate(model = get_model_display_name(prefix, with_condition = FALSE),
           condition = conditions[[cnd]])
})
lsd_metrics <- bind_rows(
  metrics(eval_df$lsd_fr, eval_df$ground_truth) |> mutate(model = "Lexicoder (FR)", condition = "—"),
  metrics(eval_df$lsd_en, eval_df$ground_truth) |> mutate(model = "Lexicoder (EN)", condition = "—")
)

all_metrics <- bind_rows(jev_metrics, other_metrics, lsd_metrics) |>
  arrange(desc(r)) |>
  mutate(rank_r = row_number()) |>
  arrange(desc(f1_3)) |>
  mutate(rank_f1 = row_number()) |>
  arrange(desc(r))

cat("=== JEV BY CONDITION (", nrow(all_metrics), " model-condition rows ranked) ===\n", sep = "")
jev_metrics |>
  left_join(select(all_metrics, model, condition, rank_r, rank_f1), by = c("model", "condition")) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  select(condition, r, rank_r, mae, ccc, f1_3, rank_f1, f1_3_argmax, n) |>
  print()

#################################################################
# 5. HUMAN CEILING (same definition as src/56_intercoder_reliability.R)
#################################################################

coders <- readRDS("data/tmp/data_manual_ranking.rds") |>
  select(doc_id, sentences, coder_1 = manual) |>
  inner_join(readRDS("data/clean/annotator_1.rds") |>
               select(doc_id, sentences, coder_2 = manual_sentiment_cam),
             by = c("doc_id", "sentences")) |>
  inner_join(readRDS("data/clean/annotator_2.rds") |>
               select(doc_id, sentences, coder_3 = manual_sentiment_etienne),
             by = c("doc_id", "sentences"))
if (nrow(coders) != 200) stop("Coder join changed the row count: ", nrow(coders))

M <- as.matrix(coders[, c("coder_1", "coder_2", "coder_3")])
human_ceiling <- mean(sapply(1:3, \(k) cor(M[, k], rowMeans(M[, -k]), use = "complete.obs")))
coders$coder_sd <- apply(M, 1, sd, na.rm = TRUE)

cat(sprintf("\n  human ceiling r = %.3f; model-condition rows above it: %d (Jev: %d)\n",
            human_ceiling, sum(all_metrics$r > human_ceiling),
            sum(jev_metrics$r > human_ceiling)))

#################################################################
# 6. DOES JEV'S CONFIDENCE MEAN ANYTHING?
#################################################################
# Two tests no other model in the paper allows: whether low confidence flags
# (a) Jev's own errors and (b) sentences on which the human coders disagree,
# and how accuracy changes if only the most confident sentences are kept.

conf_df <- eval_df |>
  inner_join(select(coders, doc_id, sentences, coder_sd), by = c("doc_id", "sentences"))

confidence_tests <- map_dfr(names(conditions), \(cnd) {
  conf <- conf_df[[paste0("confidence_", cnd)]]
  err <- abs(conf_df[[paste0("jev_", cnd)]] - conf_df$ground_truth)
  tibble(
    condition = conditions[[cnd]],
    rho_conf_error = cor(conf, err, method = "spearman", use = "complete.obs"),
    p_conf_error = cor.test(conf, err, method = "spearman", exact = FALSE)$p.value,
    rho_conf_coder_sd = cor(conf, conf_df$coder_sd, method = "spearman", use = "complete.obs"),
    p_conf_coder_sd = cor.test(conf, conf_df$coder_sd, method = "spearman", exact = FALSE)$p.value
  )
})

selective <- map_dfr(names(conditions), \(cnd) {
  conf <- conf_df[[paste0("confidence_", cnd)]]
  map_dfr(c(1, 0.75, 0.5), \(keep) {
    sel <- conf >= quantile(conf, 1 - keep, na.rm = TRUE)
    metrics(conf_df[[paste0("jev_", cnd)]][sel], conf_df$ground_truth[sel]) |>
      mutate(condition = conditions[[cnd]], kept = keep)
  })
})

# Is the gain on the confident half about Jev, or about easy sentences? If the
# other models improve on the same subset too, confidence is a difficulty flag
# (clear-cut sentences) rather than evidence Jev is more accurate when sure.
top_half <- conf_df$confidence_fr_fr >= median(conf_df$confidence_fr_fr)
difficulty_check <- map_dfr(c("jev_fr_fr", "gpt56luna_fr_fr", "claudehaiku45_fr_fr",
                              "qwen3235b_fr_fr", "gemini35_fr_fr"), \(col) tibble(
  model = col,
  r_all = cor(conf_df[[col]], conf_df$ground_truth, use = "complete.obs"),
  r_confident_half = cor(conf_df[[col]][top_half], conf_df$ground_truth[top_half],
                         use = "complete.obs"),
  mae_all = mean(abs(conf_df[[col]] - conf_df$ground_truth), na.rm = TRUE),
  mae_confident_half = mean(abs(conf_df[[col]][top_half] - conf_df$ground_truth[top_half]),
                            na.rm = TRUE)
))
cat(sprintf("\n  sd(ground truth): all %.3f, Jev-confident half (FR->FR) %.3f\n",
            sd(conf_df$ground_truth), sd(conf_df$ground_truth[top_half])))

cat("\n=== CONFIDENCE vs ERROR AND vs HUMAN DISAGREEMENT (Spearman) ===\n")
print(mutate(confidence_tests, across(where(is.double), \(x) signif(x, 3))))
cat("\n=== DOES THE CONFIDENT HALF HELP OTHER MODELS TOO? (FR->FR) ===\n")
print(mutate(difficulty_check, across(where(is.double), \(x) round(x, 3))))
cat("\n=== SELECTIVE PREDICTION: KEEP ONLY THE MOST CONFIDENT SENTENCES ===\n")
print(selective |> select(condition, kept, n, r, mae, f1_3) |>
        mutate(across(c(r, mae, f1_3), \(x) round(x, 3))))

#################################################################
# 7. PROMPT LANGUAGE: PAIRED BOOTSTRAP ON THE CORRELATION
#################################################################
# Jev's docs say English is its strongest language, which is the opposite of
# what the paper's null result would predict. Bootstrap the difference in r.

set.seed(2026)
boot_diff <- function(a, b, B = 2000) {
  gt <- eval_df$ground_truth
  d <- replicate(B, {
    i <- sample(nrow(eval_df), replace = TRUE)
    cor(eval_df[[a]][i], gt[i]) - cor(eval_df[[b]][i], gt[i])
  })
  c(diff = cor(eval_df[[a]], gt) - cor(eval_df[[b]], gt), quantile(d, c(0.025, 0.975)))
}
lang_tests <- rbind(
  "EN→EN minus FR→FR" = boot_diff("jev_en_en", "jev_fr_fr"),
  "EN→EN minus EN→FR" = boot_diff("jev_en_en", "jev_en_fr"),
  "EN→FR minus FR→FR" = boot_diff("jev_en_fr", "jev_fr_fr")
)
cat("\n=== PROMPT/TEXT LANGUAGE EFFECT ON r (95% bootstrap CI) ===\n")
print(round(lang_tests, 3))

#################################################################
# 8. COST AND SPEED
#################################################################
# Same unit as results/tables/cost_table.md: scoring 1,000 sentences once in
# one condition. The EN->EN requests carry one question, so they give it directly.

en_tokens <- scores |> filter(condition == "en_en") |> pull(input_tokens)
fr_tokens <- scores |> filter(condition == "en_fr") |> pull(input_tokens)
cost <- tibble(
  usd_per_1k_one_condition = mean(en_tokens, na.rm = TRUE) * 1000 / 1e6 * JEV_PRICE_PER_MTOK,
  usd_per_1k_two_conditions_batched = mean(fr_tokens, na.rm = TRUE) * 1000 / 1e6 * JEV_PRICE_PER_MTOK,
  mean_input_tokens_one_question = mean(en_tokens, na.rm = TRUE),
  mean_input_tokens_two_questions = mean(fr_tokens, na.rm = TRUE),
  wall_seconds_400_requests = mean(wall_per_run),
  median_latency_s = median(checks$latency)
)
cat("\n=== COST AND SPEED ===\n")
print(t(mutate(cost, across(everything(), \(x) signif(x, 3)))))

#################################################################
# 9. SAVE
#################################################################

saveRDS(list(metrics = all_metrics, jev = jev_metrics, confidence = confidence_tests,
             difficulty_check = difficulty_check,
             selective = selective, language = lang_tests, cost = cost,
             human_ceiling = human_ceiling,
             checks = list(max_rep_diff = max_rep_diff, batch_diff = batch_diff$max_diff)),
        "results/analysis/jev_results.rds")

table_md <- all_metrics |>
  mutate(across(c(r, mae, ccc, f1_3), \(x) sprintf("%.3f", x)),
         model = if_else(model == "Jev 1.13", "**Jev 1.13**", model)) |>
  mutate(row = sprintf("| %d | %s | %s | %s | %s | %s | %s | %d |",
                       rank_r, model, condition, r, mae, ccc, f1_3, n)) |>
  pull(row)
writeLines(c(
  "| Rank (r) | Model | Condition | r | MAE | CCC | F1 (3-cat) | n |",
  "|---|---|---|---|---|---|---|---|",
  table_md
), "results/tables/jev_comparison.md")
cat("\nSaved results/analysis/jev_results.rds and results/tables/jev_comparison.md\n")
