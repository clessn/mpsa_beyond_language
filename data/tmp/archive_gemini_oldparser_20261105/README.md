# Gemini 3.5 Flash scores collected with the old parser (archived 2026-11-05)

The parser used until November 2026 dropped the minus sign of formatted answers
("-0.4 or -0.3 (…)" was read as 0.4). A diagnostic on 60 answers
(src/99x_parser_diagnostic.R) found that Gemini 3.5 Flash sometimes hedges
between two values, so its scores were recollected with the corrected parser.
The other models answered with bare numbers and were not recollected.

- gemini35_validation_scores.rds: the 200-sentence scores (3 conditions x 3 runs), September 2026
- gemini35_llm_translation_scores.rds: the scores on the GPT-5.6 Luna translation (src/43), October 2026
- token_usage_log_gemini35.csv: the token-log rows of the September collection, removed from
  results/analysis/token_usage_log.csv so the cost table reflects the new collection only
