# Prompt Language Doesn't Matter, but Keep the Original Text

Code, model scores, annotated benchmark and prompts for the article "Prompt
Language Doesn't Matter, but Keep the Original Text: Large Language Models and
Sentiment Analysis in French News".

Fifteen language models scored 200 hand-coded sentences from French-language
news under three conditions: a French prompt on the French text (FR→FR), an
English prompt on the French text (EN→FR), and an English prompt on a machine
translation (EN→EN). They are compared with the French and English Lexicoder
dictionaries, and the best model also scored the full corpus of 2,683 articles.

## Data

The corpus is every article on open-source software in the "Major daily
newspapers (FR)" category of the Eureka database from 1991 to 2025 (13 dailies:
9 from Québec, 1 from Ontario, 3 from France). The full texts are licensed
from Eureka and cannot be redistributed; the article identifiers (`doc_id`,
date, source) are in the data files.

- `data/clean/df.rds`: the 200 benchmark sentences, their Google translation,
  the three coders' mean rating (`ground_truth`) and every model's score.
- `data/clean/annotator_1.rds`, `annotator_2.rds`: the second and third coders.
- `data/clean/news_df_sentiment_corpus.rds`, `news_df_sentiment_jev.rds`:
  full-corpus scores.
- `results/analysis/raw_responses*.jsonl`: raw model answers collected with
  the current parser, one JSON object per call.
- `results/analysis/token_usage_log*.csv`: tokens of every API call.

## Pipeline

Run from the project root, in this order. Scripts 00 to 31 need the Eureka
files; scripts 40, 42, 43, 70 and 72 call model APIs and resume from their
checkpoints; everything from 41 onward can be rerun from the files in the
repository.

| Step | Scripts |
|---|---|
| Collection and preparation | `00_eureka_scraper.py`, `01_html_parsing.R`, `02_data_summary.R`, `10_dataframe_cleaning.R` |
| Dictionaries on the corpus | `20_frlsd.R`, `21_translate_to_english.R`, `22_lsd_prep.R`, `23_lsd.R`, `24_merging_french_english.R` |
| Benchmark sentences | `30_create_sample.R`, `31_validate_manual_anotation.R` |
| Model scores | `40_prompt.R`, `42_jev_prompt.R` (after any rerun of 40), `41_prompt_cleaning.R` |
| Translator check | `43_llm_translation_test.R` (`43b_run_parallel.sh` for the second and third runs), then `44_lsd_on_llm_translation.R` |
| Metrics | `50_cor.R`, `51_f1_scores.R`, `56_intercoder_reliability.R`, `66_f1_scores_averages.R` |
| Tests between conditions | `81_performance_by_condition.R`, `82_equivalence_tests.R` |
| Full corpus | `70_prompt_corpus.R`, `72_jev_corpus.R`, `84_corpus_agreement_graph.R` (supplemental figure) |
| Article tables and figures | `87_tables_figures.R` (Tables 1 and 2, Figures 1 and 2) |
| Supplemental tables and figures | `85_performance_graph.R`, `62b_f1_dumbbell.R`, `86_performance_table.R`, `63_fscore_tables.R`, `68_cost_table.R`, `69_tokens_by_language.R` |

Shared code: `src/92_llm_helper_funcs.R` (answer parser, retries),
`93_prompts.R`, `94_models_map.R` (model lineup and provider pins),
`95_token_logging.R`, `98_jev_helpers.R`, and `91_LSDprep_dec2017.R`, the
Lexicoder preparation functions distributed with the dictionary.
`96_verify_models.R` checks every model before a run and `97_rerun_status.R`
follows a run in progress. `tests/test_clean_sentiment_value.R` tests the
answer parser. `src/archive/` holds scripts from earlier versions of the study.

The manuscript is `docs/pub/pub.qmd`; `docs/pub/render_sage.sh` renders the
full and anonymized manuscripts, the title page and the supplemental material.

## API keys

Set in `~/.Renviron`: `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`
(or `GOOGLE_API_KEY`), `OPENROUTER_API_KEY` and `TYPESAFE_API_KEY`.

## Software

R with dplyr, tidyr, stringr, ggplot2, quanteda, ellmer, httr2, jsonlite and
polyglotr; Python dependencies of the scraper are in `pyproject.toml`
(`uv sync`).

## License

See `LICENSE`.
