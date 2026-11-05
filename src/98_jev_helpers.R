# Requests to Jev (TypeSafe's System One model). Jev answers a typed Score
# question with a probability for each of five levels; the levels are the five
# anchors of the text prompt, in the same wording and language. The returned
# score (0 to 4, probability-weighted) is mapped onto -1..1 by jev_to_scale().
# API reference: https://docs.typesafe.ai/api.md

library(httr2)

JEV_MODEL <- "jev-1.13.0"  # pinned: the jev-latest alias moves with releases
JEV_URL <- "https://api.typesafe.ai/v1/systemone"


JEV_LEVELS_EN <- list(
  "Strong negative sentiment: highly critical, hostile, or pessimistic content",
  "Moderate negative sentiment: somewhat negative, disapproving, or concerned content",
  "Neutral sentiment: factual, balanced, or neither positive nor negative content",
  "Moderate positive sentiment: somewhat positive, approving, or optimistic content",
  "Strong positive sentiment: highly supportive, enthusiastic, or optimistic content"
)

JEV_LEVELS_FR <- list(
  "Sentiment négatif fort : contenu très critique, hostile ou pessimiste",
  "Sentiment négatif modéré : contenu plutôt négatif, désapprobateur ou préoccupant",
  "Sentiment neutre : contenu factuel, équilibré, ou ni positif ni négatif",
  "Sentiment positif modéré : contenu plutôt positif, approbateur ou optimiste",
  "Sentiment positif fort : contenu très favorable, enthousiaste ou optimiste"
)

# One Score question per condition; `field` names the text in the request state
jev_questions <- function(field = "sentence") {
  list(
    en_fr = list(
      type = "score",
      instructions = paste0(
        "Analyze the sentiment of the French text in `", field, "`, considering ",
        "cultural and linguistic nuances in French. Consider its emotional tone, ",
        "word choice, and overall message."),
      criteria = JEV_LEVELS_EN
    ),
    fr_fr = list(
      type = "score",
      instructions = paste0(
        "Analysez le sentiment du texte français dans `", field, "`, en tenant ",
        "compte des nuances culturelles et linguistiques en français. Considérez ",
        "son ton émotionnel, le choix des mots et le message global."),
      criteria = JEV_LEVELS_FR
    ),
    en_en = list(
      type = "score",
      instructions = paste0(
        "Analyze the sentiment of the English text in `", field, "`, considering ",
        "cultural and linguistic nuances in English. Consider its emotional tone, ",
        "word choice, and overall message."),
      criteria = JEV_LEVELS_EN
    )
  )
}

jev_to_scale <- function(level) (level - 2) / 2


# Transient failures are retried; the throttle stays under the documented
# limit of 1,200 requests per minute.
jev_request <- function(text, qids, field = "sentence") {
  state <- setNames(list(text), field)
  request(JEV_URL) |>
    req_auth_bearer_token(Sys.getenv("TYPESAFE_API_KEY")) |>
    req_body_json(list(model = JEV_MODEL, state = state,
                       questions = jev_questions(field)[qids])) |>
    req_timeout(60) |>
    req_throttle(capacity = 900, fill_time_s = 60, realm = "typesafe") |>
    req_retry(max_tries = 5,
              is_transient = \(r) resp_status(r) %in% c(429, 500, 502, 503, 504, 520, 529)) |>
    req_error(is_error = \(r) FALSE)
}

# One row per question; a failed call gives NA answers
parse_jev <- function(resp, qids) {
  failed <- inherits(resp, "error") || resp_status(resp) != 200
  if (failed) {
    status <- if (inherits(resp, "error")) conditionMessage(resp) else resp_status(resp)
    warning("Jev call failed: ", status)
    return(data.frame(
      condition = qids, level = NA_real_, confidence = NA_real_,
      probs = I(rep(list(rep(NA_real_, 5)), length(qids))), model = NA_character_,
      input_tokens = NA_real_, output_tokens = NA_real_
    ))
  }
  body <- resp_body_json(resp)
  do.call(rbind, lapply(qids, \(q) {
    a <- body$answers[[q]]
    data.frame(
      condition = q, level = a$score, confidence = a$confidence,
      probs = I(list(unlist(a$probabilities[as.character(0:4)]))),
      model = body$model,
      input_tokens = body$usage$input_tokens,
      output_tokens = body$usage$output_tokens
    )
  }))
}

# One request per text, several in flight; `item` indexes `texts`
run_jev <- function(texts, qids, field = "sentence", max_active = 10) {
  reqs <- lapply(texts, \(t) jev_request(t, qids, field))
  resps <- req_perform_parallel(reqs, on_error = "continue",
                                max_active = max_active, progress = FALSE)
  do.call(rbind, lapply(seq_along(resps), \(i) {
    out <- parse_jev(resps[[i]], qids)
    out$item <- i
    out
  }))
}
