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
