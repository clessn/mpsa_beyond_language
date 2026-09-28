# Données de la passe du 9 septembre 2026 — CORROMPUES, NE PAS UTILISER

## Ce qui s'est passé

Trois instances de `src/40_prompt.R` ont tourné simultanément entre 11:04 et 14:15.
Chacune tenait sa propre copie en mémoire de `df` et écrivait ses propres points
de reprise, s'écrasant mutuellement. Le journal de tokens a reçu des écritures
entrelacées, dont une ligne physiquement fusionnée (deux enregistrements sur une
seule ligne, ligne 2865).

## Ampleur

- 5 515 appels journalisés, 2 563 clés uniques → **54 % d'appels en double**
- Seul `llama321b` était concerné : la passe n'avait pas dépassé le premier modèle
- Points de reprise mutuellement incohérents

## Pourquoi c'est archivé et non supprimé

Ces fichiers documentent l'incident et servent de référence si un écart apparaît
plus tard dans la comptabilité des coûts. Ils ne doivent jamais être remis dans
`data/tmp/` ni servir de point de reprise.

## Correctif appliqué

`src/40_prompt.R` acquiert désormais un verrou mono-instance
(`data/tmp/.rerun.lock`) au démarrage et refuse de démarrer si une autre passe
est active. Le verrou est libéré dans le bloc `finally`, et un verrou périmé
laissé par un processus tué est détecté et retiré automatiquement.
