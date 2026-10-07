# Rejeu du corpus avec Gemini 3.5 Flash — interrompu le 2026-10-07

48 articles notés (condition fr_fr) avant l'arrêt. Le rejeu a été interrompu
parce que Gemini 3.5 Flash n'est plus le meilleur modèle de la cuvée 2026
(4e en corrélation, derrière GPT-5.6 Luna, Claude Haiku 4.5 et Qwen3 235B) et
coûte environ 20 fois plus cher sur ce corpus (~17 USD par passage contre
~0,80 USD) à cause de ses jetons de raisonnement.

Les 54 appels correspondants restent dans
`results/analysis/token_usage_log_corpus.csv` (model_prefix = gemini35),
pour 0,29 USD. Le corpus a été rejoué avec GPT-5.6 Luna.
