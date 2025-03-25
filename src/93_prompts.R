###############################################################################
# Prompts for LLM Sentiment Analysis
# 
# This file contains all prompts used for sentiment analysis with various LLMs
# in English and French languages for different text inputs.
#
# Author: Claude & User
# Date: March 2025
###############################################################################

#==============================================================================
# 1. ENGLISH PROMPTS
#==============================================================================

#' English prompt for French text
#' 
#' @param text The French text to analyze
#' @return A complete prompt ready to send to the LLM
en_prompt_fr_text <- function(text) {
  paste0("Please analyze the sentiment of the following French text and provide a single numerical rating according to this scale:
Sentiment Scale:
-1.0: Strong negative sentiment - highly critical, hostile, or pessimistic content
-0.5: Moderate negative sentiment - somewhat negative, disapproving, or concerned content
0.0: Neutral sentiment - factual, balanced, or neither positive nor negative content
0.5: Moderate positive sentiment - somewhat positive, approving, or optimistic content
1.0: Strong positive sentiment - highly supportive, enthusiastic, or optimistic content
You may also use values between these points for intermediate sentiment levels (e.g., -0.7, 0.3).
Important instructions:
1. First, carefully read and understand the text, considering cultural and linguistic nuances in French.
2. Analyze the emotional tone, word choice, and overall message.
3. Respond ONLY with a single numerical value between -1.0 and 1.0 that best represents the sentiment.
4. Do not include ANY explanations, analysis, or additional text in your response.
Here is the text to analyze: ", text)
}

#' English prompt for English text (translated from French)
#' 
#' @param text The English text to analyze
#' @return A complete prompt ready to send to the LLM
en_prompt_en_text <- function(text) {
  paste0("Please analyze the sentiment of the following english text and provide a single numerical rating according to this scale:
Sentiment Scale:
-1.0: Strong negative sentiment - highly critical, hostile, or pessimistic content
-0.5: Moderate negative sentiment - somewhat negative, disapproving, or concerned content
0.0: Neutral sentiment - factual, balanced, or neither positive nor negative content
0.5: Moderate positive sentiment - somewhat positive, approving, or optimistic content
1.0: Strong positive sentiment - highly supportive, enthusiastic, or optimistic content
You may also use values between these points for intermediate sentiment levels (e.g., -0.7, 0.3).
Important instructions:
1. First, carefully read and understand the text, considering cultural and linguistic nuances in English.
2. Analyze the emotional tone, word choice, and overall message.
3. Respond ONLY with a single numerical value between -1.0 and 1.0 that best represents the sentiment.
4. Do not include ANY explanations, analysis, or additional text in your response.
Here is the text to analyze: ", text)
}

#==============================================================================
# 2. FRENCH PROMPTS
#==============================================================================

#' French prompt for French text
#' 
#' @param text The French text to analyze
#' @return A complete prompt ready to send to the LLM
fr_prompt_fr_text <- function(text) {
  paste0("Veuillez analyser le sentiment du texte français suivant et fournir une évaluation numérique unique selon cette échelle :
Échelle de sentiment :
-1.0 : Sentiment négatif fort - contenu très critique, hostile ou pessimiste
-0.5 : Sentiment négatif modéré - contenu plutôt négatif, désapprobateur ou préoccupant
0.0 : Sentiment neutre - contenu factuel, équilibré, ou ni positif ni négatif
0.5 : Sentiment positif modéré - contenu plutôt positif, approbateur ou optimiste
1.0 : Sentiment positif fort - contenu très favorable, enthousiaste ou optimiste
Vous pouvez également utiliser des valeurs intermédiaires entre ces points pour des niveaux de sentiment intermédiaires (par exemple, -0.7, 0.3).
Instructions importantes :
1. D'abord, lisez attentivement et comprenez le texte, en tenant compte des nuances culturelles et linguistiques en français.
2. Analysez le ton émotionnel, le choix des mots et le message global.
3. Répondez UNIQUEMENT avec une valeur numérique unique entre -1.0 et 1.0 qui représente le mieux le sentiment.
4. N'incluez AUCUNE explication, analyse ou texte supplémentaire dans votre réponse.
Voici le texte à analyser : ", text)
}

#==============================================================================
# 3. SYSTEM PROMPT
#==============================================================================

#' System prompt for LLM initialization
#' 
#' @return A string containing the system prompt
get_system_prompt <- function() {
  "You are a helpful assistant that analyzes the sentiment of text. You provide accurate and consistent sentiment ratings according to the specified scale."
}