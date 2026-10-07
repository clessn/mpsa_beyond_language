# Plan détaillé de la revue de littérature

Établi le 16 septembre 2026, à partir de `cartographie_litterature.md`.

**Règle d'écriture.** Aucune affirmation n'est rédigée tant que la référence
qui la porte n'a pas été lue au niveau indiqué. Colonne « À lire » :
✅ déjà lu (L ou passages clés P) · 📖 à lire avant rédaction.

**Fil conducteur.** Les LLM rendent l'annotation accessible aux sciences
sociales (§1), mais la preuve est surtout anglophone (§2). Hors de l'anglais,
les chercheurs ont deux réflexes hérités : traduire le texte (§3) et prompter
en anglais (§4). Aucun n'a été testé pour l'analyse de sentiment par LLM sur
du texte écrit nativement dans une langue bien dotée. D'où les deux questions
et H1.

**Longueur visée.** 900 à 1 200 mots, 4 sections courtes. Environ 45
références pour tout l'article.

---

## §1 — Les LLM comme outil d'annotation en sciences sociales (C1, C5)

| # | Affirmation | Références | À lire |
|---|---|---|---|
| 1.1 | Des LLM utilisés sans entraînement égalent ou dépassent les annotateurs de plateformes et les experts sur des tâches courantes | Gilardi et al. 2023 ; Törnberg 2025 ; Heseltine & Clemm von Hohenberg 2024 | ✅ |
| 1.2 | Ils sont présentés comme une rupture pour la recherche en sciences sociales | Ziems et al. 2024 ; Bail 2024 | 📖 |
| 1.3 | Ils abaissent la barrière d'entrée par rapport au fine-tuning, coûteux en données annotées et en expertise | Laurer et al. 2023/2024 (*Political Analysis*, 164 cit.) ; Chae & Davidson 2025 | 📖 |
| 1.4 | Mais ils exigent une validation systématique contre un étalon humain | Grimmer & Stewart 2013 ; Pangakis et al. 2023 ; Lin & Zhang 2026 | 📖 (Lin & Zhang ✅ P) |
| 1.5 | Les modèles propriétaires posent des problèmes de reproductibilité ; les modèles ouverts sont recommandés | Spirling 2023 ; Ollion et al. 2024 ; Palmer et al. 2023 ; Alizadeh et al. 2024 | 📖 |

**Transition.** « Cette preuve est cependant largement anglophone. »

## §2 — Au-delà de l'anglais : ce que l'on sait (C1 multilingue, C4)

| # | Affirmation | Références | À lire |
|---|---|---|---|
| 2.1 | Les méthodes textuelles des sciences sociales sont biaisées vers l'anglais | Baden et al. 2022 ; Nicholas & Bhatia 2023 | 📖 |
| 2.2 | Les études multilingues trouvent des performances élevées, mais légèrement moindres hors de l'anglais | Rathje et al. 2024 ; Heseltine & Clemm von Hohenberg 2024 ; Törnberg 2025 | ✅ |
| 2.3 | La comparaison LLM–dictionnaire de Rathje et al. ne porte que sur des titres **anglais** avec des dictionnaires anglais | Rathje et al. 2024 | ✅ |
| 2.4 | Les dictionnaires ont une validité limitée face à l'annotation humaine | van Atteveldt et al. 2021 ; Young & Soroka 2012 | ✅ P / R |
| 2.5 | Pour d'autres langues, les modèles à base de transformers surpassent les dictionnaires | Widmann & Wich 2023 (allemand) ; « From dictionaries to LLMs… German » 2025 ; ParlaSent 2025 | 📖 |
| 2.6 | Les dictionnaires gardent des défenseurs (transparence, stabilité) | « The advantages of lexicon-based sentiment analysis… » 2025 | 📖 ✗ |
| 2.7 | Un dictionnaire français existe : c'est le point de comparaison natif | Duval & Pétry 2016 | ✅ R |

**Écart repéré.** Peu d'études comparent un LLM à un dictionnaire **natif** sur
du texte non anglais.

## §3 — Premier réflexe : traduire le texte (C3)

| # | Affirmation | Références | À lire |
|---|---|---|---|
| 3.1 | La traduction automatique est une stratégie établie de l'analyse textuelle comparée | Lucas et al. 2015 ; de Vries et al. 2018 ; Reber 2018 | 📖 (de Vries ✅ P) |
| 3.2 | Elle a servi à étendre des dictionnaires à d'autres langues | Proksch et al. 2019 ; Maier et al. 2021 | 📖 |
| 3.3 | Les plongements multilingues offrent une alternative à la traduction | Licht 2023 | 📖 |
| 3.4 | Pour le sentiment, la traduction altère le contenu affectif… | Mohammad et al. 2016 | 📖 |
| 3.5 | …mais traduire puis analyser en anglais restait compétitif avant les LLM | Araújo et al. 2019 | 📖 ✗ |
| 3.6 | Avec les LLM, traduire l'entrée en anglais améliore la performance… | Etxaniz et al. 2024 | ✅ ✗ |
| 3.7 | …mais sur des jeux de données eux-mêmes traduits, et les auteurs appellent à tester des textes natifs | Etxaniz et al. 2024 | ✅ |
| 3.8 | La pratique existe dans des travaux appliqués | Venerito et al. 2024 | ✅ P |

**Écart repéré.** La traduction avant analyse de sentiment par LLM n'a pas été
testée sur du texte écrit nativement.

## §4 — Second réflexe : prompter en anglais (C2)

| # | Affirmation | Références | À lire |
|---|---|---|---|
| 4.1 | Les LLM sont entraînés surtout sur de l'anglais et performent moins bien ailleurs | Touvron et al. 2023 ; Bang et al. 2023 ; Lai et al. 2023 ; Ahuja et al. 2023 | 📖 |
| 4.2 | Leur traitement interne passerait par une représentation proche de l'anglais | Wendler et al. 2024 | 📖 |
| 4.3 | Les gabarits et prompts anglais améliorent la performance multilingue | Lin et al. 2022 ; Muennighoff et al. 2023 ; Huang et al. 2023 ; Shi et al. 2023 | ✅ P (Lin, Muennighoff) / 📖 |
| 4.4 | En classification (arabe), les prompts anglais sont meilleurs en moyenne ; l'écart est minime pour le modèle le plus fort | Kmainasi et al. 2025 | ✅ ✗ |
| 4.5 | La langue du prompt change la forme des réponses, pas leur sens | Nguyen et al. 2026 | ✅ P |
| 4.6 | Le seul test en sciences sociales mesure l'accord entre langues de prompt (κ = .95), pas l'exactitude, sur un seul jeu de données | Rathje et al. 2024 | ✅ |

**Écart repéré.** Aucune étude ne teste l'**exactitude** selon la langue du
prompt, avec des tests d'**équivalence**, pour l'analyse de sentiment dans une
langue bien dotée, avec des modèles actuels.

**Clôture de la revue.** Les deux questions de recherche, puis H1 : *les
prompts en anglais surpassent les prompts en français sur le texte français*,
dérivée de §4, non préenregistrée.

---

## Cohérence avec le reste de l'article

| Section | Ce qu'elle doit reprendre | Références |
|---|---|---|
| Résumé | Les deux « réflexes » (§3, §4) et les deux résultats | — |
| Introduction ¶1 | Domination de l'anglais et avantage supposé du prompt anglais | §4.1, §4.3 |
| Introduction ¶2 | Le postulat testé sur le français ; pourquoi le sentiment | van Atteveldt 2021 ; Young & Soroka 2012 ; Mohammad 2016 |
| Introduction ¶3 | Coût du fine-tuning, promesse des LLM | §1.2, §1.3 |
| Méthodes : annotation | Étalon humain, taille de l'échantillon, accord intercodeurs | van Atteveldt 2021 ; Grimmer & Stewart 2013 ; Song et al. 2020 |
| Méthodes : condition EN→EN | Reproduit la stratégie de traduction | §3.1 |
| Méthodes : modèles | Choix de modèles ouverts, fournisseur fixe | §1.5 |
| Résultats | Comparaison au plafond humain formulée prudemment | Richie et al. 2022 |
| Discussion : traduction | Contredit Etxaniz et Araújo ; explication par les textes natifs | §3.4–3.7 |
| Discussion : langue du prompt | Cohérent avec Kmainasi (écart minime pour le modèle le plus fort) et Nguyen ; dépasse Rathje | §4.4–4.6 |
| Discussion : dictionnaires | Contrepoint de la transparence | §2.6 |
| Discussion : coûts | Les langues ne coûtent pas le même nombre de tokens | Ahia et al. 2023 |
| Conclusion | Retour au biais anglophone | Baden et al. 2022 |

---

## Références actuelles à retirer

Ces références portaient la première grappe de la revue actuelle (performance
du fine-tuning par langue). Ce sont des études sectorielles peu citées, et
cette grappe disparaît du nouveau plan :

Bhowmick & Jana 2021 ; Michailidis 2024 ; Hameed et al. 2023 ; ElJundi et al.
2019 ; Ahmadian et al. 2024 (21 cit.) ; Tela et al. 2024 ; Salahudeen et al.
2023 ; Nazir et al. 2025 ; Kumar & Albuquerque 2021 ; Pan et al. 2023 ;
Kotelnikova et al. 2023 ; Krasitskii et al. 2024 ; Buscemi & Proverbio 2024
(16 cit.) ; Rusnachenko et al. 2024 ; Chalkidis et al. 2020 ; Perron 2024 ;
Ghosh et al. 2023.

**À décider :** Barbieri et al. 2022 (XLM-T, 135 cit.) et Přibáň et al. 2024
(40 cit.) pourraient rester pour représenter le fine-tuning multilingue en une
phrase (§1.3). Strubell et al. 2019 (421 cit.) est le canon du coût
computationnel de l'entraînement : à garder si §1.3 mentionne ce coût, sinon à
retirer.

## Lectures à faire avant rédaction

28 références marquées 📖. Ordre suggéré : d'abord celles qui portent un
résultat contraire (Araújo et al. 2019 ; « advantages of lexicon-based… »
2025), puis celles qui posent la même question que nous (« From dictionaries
to LLMs… German » 2025 ; ParlaSent 2025 ; Widmann & Wich 2023), puis le canon
de cadrage.
