# Checkpoints de la cuvée 2024-2025

Déplacés ici le 2026-09-09, avant le rerun avec la nouvelle liste de modèles.

## Pourquoi

`src/40_prompt.R` reprend automatiquement depuis tout fichier
`data/tmp/sentiment_analysis_progress_*.rds` et saute les phrases déjà notées.
Ces checkpoints contiennent `llama321b_*` et `llama323b_*` remplis à 200/200
par la cuvée de mars 2025 — deux modèles que la cuvée 2026 réintroduit.

Laissés en place, ils auraient fait sauter ces deux modèles au rerun, et des
scores de 2025 auraient été publiés comme des résultats de 2026, sans erreur
ni avertissement.

Ne pas remettre dans `data/tmp/` tant que le rerun n'est pas terminé.

## Ajout du 2026-10-07 : analyse du corpus complet

Les 12 fichiers `corpus_sentiment_*.rds` (mars–mai 2025) ont été déplacés ici
avant le rejeu de `src/70_prompt_corpus.R` avec Gemini 3.5 Flash, pour la même
raison : le script reprend depuis `corpus_sentiment_latest_checkpoint.rds`, où
`gemini_fr_fr` est rempli à 2 679/2 683 par l'ancien Gemini, et aurait sauté
tous les articles.

`news_df_sentiment_gemini_2025.rds` et `gemini_corpus_analysis_2025.rds` sont
des copies des sorties 2025 de `data/clean/`, que le rejeu écrase.
