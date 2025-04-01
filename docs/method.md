# mpsa_beyond_language

Métho:

query:

TEXT= ("logiciel libre" | "logiciels libres" | "open source" | "open-source" | "logiciel open source" | "logiciels open source" | "code source ouvert" | "software libre" | "free software" | "code source libre" | Free Software Foundation | Richard Stallman)

Dates:

1991-01-01 <- Année de création de Linux
2025-01-01 <- Date de fin de la recherche

Sources: Principaux quotidiens (FR)

Devoir, Le
Droit, Le (Ottawa, ON)
Figaro, Le
Journal de Montréal, Le
Journal de Québec, Le
Libération
Monde, Le
Nouvelliste, Le (Trois-Rivières, QC)
Presse, La
Quotidien, Le (Saguenay, QC)
Soleil, Le (Québec, QC)
Tribune, La (Sherbrooke, QC)
Voix de l'Est, La (Granby, QC)

Divison des dates

2013-09-05_2025_01_01 : 999 articles
2005-05-14_2013-09-04 : 997 articles
1991-01-01_2005-05-13 : 705 articles


- What are you doing?
- Why is it important?
- How is the existing literature deficient?
- What are you doing that's better?
- What did you find?


# Prompt

I am evaluating the performance of a handful of large language models to evaluate the sentiment of texts. 

I have written a description of the tast. Your role is to write a R script that does the prompting of a whole dataset with all LLM detailed in the instruction. I work in R and use the ellmer() package to prompt various llms. 

Here is an example of how the package works: 

library(ellmer)

groq <- ellmer::chat_groq(
  system_prompt = "Your role is to answer simple questions",
  model = "llama-3.3-70b-versatile",
  echo = "none"
)

response <- groq$chat("What is the capital of France?")

print(response)

Here are the instructions you need to follow to elaborate the script.


# Required prompts

dataframe name: `df`
dataframe path: "data/tmp/data_manual_ranking.rds"

system prompt: You are a helpful assistant that analyzes the sentiment of text. You provide accurate and consistent sentiment ratings according to the specified scale.

enfr_prompt <- paste0("Please analyze the sentiment of the following French text and provide a single numerical rating according to this scale:
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
Here is the text to analyze: ", df$sentences[i])

enen_prompt <- paste0("Please analyze the sentiment of the following english text and provide a single numerical rating according to this scale:
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
Here is the text to analyze: ", df$sentences[i])


frfr_prompt <- paste0("Veuillez analyser le sentiment du texte français suivant et fournir une évaluation numérique unique selon cette échelle :
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
Voici le texte à analyser : ", df$sentences[i])

ellmer::chat_openai()
    function structure: fireworks <- ellmer::chat_openai(system_prompt = "You are a helpful assistant", base_url = "https://api.fireworks.ai/inference/v1", api_key = Sys.getenv("FIREWORKS_API_KEY"), model = model)
    models:
        - "accounts/fireworks/models/llama-v3p2-3b-instruct"
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama323b_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama323b_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama323b_en_en`
        - "accounts/fireworks/models/qwq-32b"
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$qwq32b_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$qwq32b_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$qwq32b_en_en`
        - "accounts/fireworks/models/deepseek-r1-basic"
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1_en_en`
        - "accounts/fireworks/models/llama-v3p3-70b-instruct"
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama3370b_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama3370b_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama3370b_en_en`
ellmer::chat_groq()
    models:
        - gemma2-9b-it
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemma29b_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemma29b_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemma29b_en_en`
        - llama-3.2-1b-preview
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama321b_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama321b_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$llama321b_en_en`
        - mixtral-8x7b-32768
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$mixtral_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$mixtral_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$mixtral_en_en`
        - deepseek-r1-distill-llama-70b
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1distillllama_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1distillllama_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekr1distillllama_en_en`
ellmer::chat_anthropic()
    model:
        - claude-3-5-haiku-20241022
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$claude35_en_fr` 
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$claude35_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$claude35_en_en`
ellmer::chat_gemini()
    model
        - gemini-2.0-flash
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemini20_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemini20_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gemini20_en_en`
ellmer::chat_deepseek()
    model:
        - deepseek-chat
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekchat_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekchat_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$deepseekchat_en_en`
ellmer::chat_gpt4()
    model:
        - gpt-4o
            - English prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gpt4o_en_fr`
            - French prompt for french text in `df$sentences`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gpt4o_fr_fr`
            - English prompt for english translation of french text in `df$sentences_en`
                - Result is mean() of three runs of the same prompt 
                - Results stored in `df$gpt4o_en_en`

21600 prompts in total

I have made a lot of textual sentiment analysis performance tests for 11 AI models as well as a dictionary method. 
I have 200 sentences in french which I classified manually and which I stored the score I put in the ground_truth column. 
I have the scores of the 11 models and the dictionary method in their respective columns. 
For the dictionary approach I have classified it in df$lsd_fr and lsd_en. 
Each of the 11 models were tested in three different ways. 
model_en_fr, model_fr_fr and model_en_en. 
I want to make regression analysis of all the models and their variations on the ground truth and display the results in a nice regression graph. 
I will provide an example code of a nice regression graph I like.
I want a graph that shows each regression models, not a focused version and I want to see the coefficient and the t-value of each model.


# Creation of focused plot
plot_regression <- ggplot(plot_results, aes(x = estimate, y = reorder(term_with_n, abs(estimate)))) +
  # Points - with different shapes for small sample variables
  geom_point(aes(color = sig_level, shape = is_small_sample), size = 3.5) +
  # Confidence intervals - 99.9% (thinnest)
  geom_linerange(aes(xmin = LowerCI999, xmax = UpperCI999, color = sig_level), linewidth = 0.5) +
  # Confidence intervals - 99% (thin)
  geom_linerange(aes(xmin = LowerCI99, xmax = UpperCI99, color = sig_level), linewidth = 1.2) +
  # Confidence intervals - 95% (thick)
  geom_linerange(aes(xmin = LowerCI95, xmax = UpperCI95, color = sig_level), linewidth = 2.5) +
  # Theme and labels
  clessnize::theme_clean_light() +
  labs(x = "\nRegression Coefficient\n",
       y = "\nVariable\n",
       title = "Main Predictors of Abortion Support",
       subtitle = "Linear regression results with multiple confidence levels",
       caption = paste0(
         "Line thickness indicates confidence level: thick (95%), medium (99%), thin (99.9%)\n",
         "Triangle shape indicates small sample size variable (n<5% of dataset)"
       )) +
  # Scale and reference line
  scale_x_continuous(limits = c(x_min, x_max)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  # T value labels
  geom_text(aes(x = ifelse(estimate > 0, 
                          UpperCI999 + 0.05, 
                          LowerCI999 - 0.05), 
                label = paste("t =", round(t_value, 2), significance)),
            hjust = ifelse(plot_results$estimate > 0, 0, 1),
            size = 4) +
  # Colors for significance levels - different blue shades
  scale_color_manual(values = c("p < 0.001" = "#08519C", # Dark blue
                               "p < 0.01" = "#3182BD",  # Medium blue
                               "p < 0.05" = "#6BAED6",  # Light blue
                               "Not significant" = "grey60"), # Grey
                    name = "Statistical Significance") +
  # Shape for small sample variables
  scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 16),
                    labels = c("TRUE" = "Small sample size", "FALSE" = "Adequate sample size"),
                    name = "Sample Size") +
  # Theme formatting
  theme(plot.caption.position = "plot",
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 20, hjust = 0),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

# Saving the focused plot
cat("Saving focused plot...\n")
ggsave("graph/regression_predictors_focused.png", plot_regression, width = 12, height = 8, dpi = 300)


Here is the names(df) output so you know which variables to deal with. _bin variables are for another analysis. Don't use them.
