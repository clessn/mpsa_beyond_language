# Cartographie de la littérature — Language Doesn't Matter

Établie le 16 septembre 2026. Citations : OpenAlex (même jour). Les nombres de
citations des articles d'informatique sont **sous-estimés** : OpenAlex compte
souvent séparément la version arXiv et la version publiée (ex. MGSM, 55 cites
pour la seule version arXiv).

**Méthode.** Pour chaque courant : (1) articles-pivots résolus par titre ;
(2) découverte par citations — les travaux les plus cités parmi ceux qui citent
les pivots, filtrés par mots-clés ; (3) recherches par titre triées par
citations. Le but est le portrait du courant, pas la sélection de ce qui nous
appuie : les travaux **contraires** sont marqués ✗.

**Niveau de lecture.** L = lu en entier ; P = passages clés lus ; R = résumé
seulement ; — = identifié, non lu.

Légende : ★ canon (très cité ou fondateur) · ◆ directement sur notre question
(peu cité mais incontournable) · ✗ résultat contraire au nôtre.

---

## C1 — Annotation par LLM en sciences sociales

| | Réf. | Cites | Lu | Apport |
|---|---|---|---|---|
| ★ | Gilardi, Alizadeh & Kubli 2023, *PNAS* | 1086 | P | Pivot du courant ; appelle à tester « performance across multiple languages » |
| ★ | Ziems et al. 2024, *Computational Linguistics* | 503 | — | Évaluation systématique des LLM pour les tâches de CSS |
| ★ | Bail 2024, *PNAS* | 287 | — | Cadrage : l'IA générative peut-elle améliorer les sciences sociales ? |
| ★ | Rathje et al. 2024, *PNAS* | 275 | L | 12 langues ; prompts anglais sur texte non anglais ; un seul test de langue de prompt (accord κ = .95, pas l'exactitude) ; dictionnaires comparés **en anglais seulement** |
| ★ | Törnberg 2025, *SSCR* | 88 | L | Revue ciblée ; 11 pays, prompt anglais non traduit |
| | Heseltine & Clemm von Hohenberg 2024, *Research & Politics* | 84 | L | 4 pays ; légère baisse hors anglais ; forte baisse sur articles longs |
| | Alizadeh et al. 2024, *J. Comput. Soc. Sci.* | 72 | — | Guide des LLM **ouverts** pour l'annotation |
| | Chae & Davidson 2025, *Sociol. Methods & Research* | 45 | — | Du zero-shot à l'instruction-tuning pour la classification |
| | Ornstein et al. 2025, *PSRM* | 43 | — | LLM pour textes politiques |
| | Le Mens & Gallego 2025, *Political Analysis* | 36 | — | Positionner des textes politiques avec des LLM |
| | Pangakis et al. 2023 (arXiv) | 40 | — | « Automated annotation with generative AI requires validation » |
| | Egami et al. 2023 (DSL) | 24 | — | Inférence en aval avec étiquettes imparfaites |
| ◆ | Lin & Zhang 2026, *SSCR* | 8 | P | Revue ciblée ; risques épistémiques ; recommande les modèles ouverts |

## C2 — LLM multilingues et langue du prompt (TAL)

| | Réf. | Cites | Lu | Apport |
|---|---|---|---|---|
| ★ | Bang et al. 2023 (IJCNLP-AACL) | 661 | — | ChatGPT plus faible hors anglais |
| ★ | Muennighoff et al. 2023, ACL | 354 | P | Modèles affinés sur prompts anglais meilleurs avec prompts anglais |
| ★ | Lai et al. 2023 « ChatGPT Beyond English », Findings EMNLP | 186 | — | Évaluation multilingue de référence |
| ★ | Lin et al. 2022 (XGLM), EMNLP | 121 | P | Gabarit anglais corrige les prompts non anglais faibles |
| ★ | Ahuja et al. 2023 (MEGA), EMNLP | 104 | — | Évaluation multilingue de référence |
| ★ | « A survey of multilingual large language models », *Patterns* 2025 (auteurs à vérifier) | 104 | — | Synthèse du courant |
| | Huang et al. 2023 (XLT), Findings EMNLP | 62 | — | Prompting translinguistique |
| | Shi et al. 2023 (MGSM), ICLR | 55* | — | Raisonnement multilingue ; origine du translate-test pour LLM |
| | Wendler et al. 2024, ACL | 40 | — | Langue latente anglaise dans les Llama |
| | Ahia et al. 2023, EMNLP | 33 | — | **Coût** : les langues ne coûtent pas le même nombre de tokens |
| | Qin et al. 2023 « Cross-lingual prompting », EMNLP | 28 | — | |
| ✗ | Etxaniz et al. 2024, NAACL | 23 | L | Traduire en anglais aide — sur données traduites, tâches de raisonnement, modèles de base |
| ✗ | Kmainasi et al. 2025, WISE | 4 | L | Arabe : prompts anglais meilleurs en moyenne ; écarts minimes pour GPT-4o ; pas de sentiment |
| ◆ | Nguyen et al. 2026 (arXiv) | 0 | P | Langue du prompt change la longueur, pas le sens (cos ≈ .83) |

## C3 — Traduction et texte comme données multilingue

| | Réf. | Cites | Lu | Apport |
|---|---|---|---|---|
| ★ | Lucas et al. 2015, *Political Analysis* | 491 | — | Fondateur : traduction automatique pour l'analyse comparée |
| ★ | de Vries, Schoonvelde & Schumacher 2018, *Political Analysis* | 234 | P | Google Translate valide pour bag-of-words |
| ★ | Mohammad, Salameh & Kiritchenko 2016, *JAIR* | 196 | R | La traduction altère le sentiment |
| ★ | Proksch et al. 2019, *LSQ* | 187 | R | Lexicoder traduit dans les langues de l'UE |
| ★ | Baden et al. 2021/2022, *CMM* | 187 | — | « Three gaps » : biais anglophone des méthodes textuelles |
| ✗ | Araújo et al. 2019, *Information Sciences* | 137 | — | Traduire puis analyser en anglais : compétitif pour le sentiment (pré-LLM) |
| | Reber 2018, *CMM* | 72 | — | Traduction automatique et topic models |
| | Maier et al. 2021, *CMM* | 44 | — | Traduction vs dictionnaires multilingues |
| | Licht 2023, *Political Analysis* | 32 | — | Classification translinguistique par plongements multilingues |
| ◆ | Nicholas & Bhatia 2023, CDT (rapport) | 26 | — | « Lost in Translation: LLMs in Non-English Content Analysis » |

## C4 — Validation de l'analyse de sentiment, dictionnaires

| | Réf. | Cites | Lu | Apport |
|---|---|---|---|---|
| ★ | Grimmer & Stewart 2013, *Political Analysis* | 3315 | — | « Validate, validate, validate » |
| ★ | Young & Soroka 2012, *Political Communication* | 618 | R | Lexicoder |
| ★ | van Atteveldt, van der Velden & Boukes 2021, *CMM* | 407 | P | Dictionnaires peu valides ; 100–300 unités pour un étalon |
| ★ | Zhang et al. 2024 « Sentiment analysis in the era of LLMs: a reality check », Findings NAACL | 297 | — | Référence TAL : LLM et sentiment |
| ★ | Grimmer, Roberts & Stewart 2022, *Text as Data* (livre) | — | — | Manuel de référence |
| | Song et al. 2020, *Political Communication* | 127 | — | Étalons humains imparfaits |
| | Widmann & Wich 2023, *Political Analysis* | 94 | — | Dictionnaire vs plongements vs transformers, **allemand** |
| | Wang et al. 2023 « Is ChatGPT a good sentiment analyzer? » | 80 | — | |
| | « Comparing LLMs and human annotators in latent content analysis… », *Scientific Reports* 2025 (auteurs à vérifier) | 68 | — | LLM vs annotateurs : sentiment, orientation, intensité |
| ✗ | « The advantages of lexicon-based sentiment analysis in an age of ML », *PLoS ONE* 2025 | 29 | — | Défense des dictionnaires — contrepoint à citer |
| | Birkenmaier et al. 2023, *CMM* | 25 | — | Pratiques de validation en texte comme données |
| | Duval & Pétry 2016, *RCSP/CJPS* | — | R | Lexicoder français |
| ◆ | « From dictionaries to LLMs … German language data », *CHR* 2025 | 2 | — | **Même question que nous**, en allemand |
| ◆ | « ParlaSent » 2025 (auteurs à vérifier), *Political Research Exchange* | 2 | — | Sentiment politique multilingue par LLM |
| | Richie, Grover & Tsui 2022, BioNLP | 8 | — | L'accord intercodeurs n'est pas un plafond |

## C5 — Modèles ouverts, reproductibilité, coûts

| | Réf. | Cites | Lu | Apport |
|---|---|---|---|---|
| ★ | Spirling 2023, *Nature* | 107 | — | Modèles ouverts : voie éthique pour la science |
| ★ | Ollion et al. 2024, *Nature Machine Intelligence* | 56 | — | Dangers des LLM propriétaires pour la recherche |
| ★ | Palmer, Smith & Spirling 2023, *Nature Computational Science* | 50 | — | Justifier l'usage de modèles propriétaires |
| | Baumann et al. 2025 « LLM hacking » (arXiv) | 0 | — | Choix de configuration → conclusions différentes |

---

## Ce que la carte change

1. **Revue de littérature actuelle : sous-représentative du canon.** Voir le
   tableau des références actuellement citées (bas de page).
2. **Les résultats contraires doivent être traités, pas contournés** : Etxaniz,
   Kmainasi (langue du prompt / traduction) et Araújo et al. 2019 (traduire puis
   analyser fonctionne pour le sentiment, avant les LLM), ainsi que la défense
   des dictionnaires (PLoS ONE 2025).
3. **Deux travaux posent notre question sur une autre langue** (allemand :
   CHR 2025 ; multilingue politique : ParlaSent). Ne pas les citer serait une
   faiblesse évidente.
4. **Cohérence de l'article** : l'introduction, la discussion et la conclusion
   doivent s'appuyer sur la même carte que la revue — mêmes courants, même
   positionnement (Rathje comme point de départ ; traduction comme apport
   principal ; langue du prompt comme résultat d'équivalence).

---

## Annexe — Poids des références actuellement citées (OpenAlex, 16 sept. 2026)

| Cites | Référence | Sort proposé |
|---|---|---|
| 3972 | Touvron et al. 2023 (LLaMA) | Garder (§4.1) |
| 618 | Young & Soroka 2012 | Garder |
| 421 | Strubell et al. 2019 | À décider : canon du coût computationnel |
| 407 | van Atteveldt et al. 2021 | Garder |
| 354 | Muennighoff et al. 2023 | Garder |
| 196 | Mohammad et al. 2016 | Garder |
| 164 | Laurer et al. 2023/2024, *Political Analysis* | Garder (§1.3) |
| 135 | Barbieri et al. 2022 (XLM-T) | À décider |
| 121 | Lin et al. 2022 | Garder |
| 70 | ElJundi et al. 2019 | Retirer |
| 40 | Přibáň et al. 2024 | À décider |
| 31 | Nazir et al. 2025 | Retirer |
| 28 | Venerito et al. 2024 | Garder (§3.8) |
| 23 | Etxaniz et al. 2024 | Garder |
| 21 | Ahmadian et al. 2024 | Retirer |
| 18 | Pan et al. 2023 | Retirer |
| 18 | Chalkidis et al. 2020 (LEGAL-BERT) | Retirer |
| 16 | Buscemi & Proverbio 2024 | Retirer |
| 15 | Michailidis 2024 | Retirer |
| 14 | Rusnachenko et al. 2024 | Retirer |
| 13 | Hameed et al. 2023 | Retirer |
| 10 | Kotelnikova et al. 2023 | Retirer |
| 5 | Perron 2024 | Retirer |
| 5 | Salahudeen et al. 2023 | Retirer |
| 4 | Kmainasi et al. 2025 | Garder (§4.4) |
| 0 | Nguyen et al. 2026 | Garder (§4.5) |
| n.d. | Bhowmick & Jana 2021 ; Tela et al. 2024 ; Kumar & Albuquerque 2021 ; Krasitskii et al. 2024 ; Ghosh et al. 2023 | Retirer |
| n.d. | Duval & Pétry 2016 | Garder (dictionnaire utilisé) |

Sur 36 références citées, 16 ont moins de 25 citations et ne relèvent d'aucun
courant central : ce sont celles que le nouveau plan retire.
