# mpsa_beyong_language

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


models:

ollama:
    - llama3.2:3b
    - mistral
    - qwen2.5:3b
    - deepseek-r1:7b
    - gemma3:4b
groq
    - gemma2-9b-it
    - llama-3.3-70b-versatile
    - mixtral-8x7b-32768
    - deepseek-r1-distill-llama-70b
conventional
    - claude-3-5-sonnet-20241022
    - gemini-2.0-flash
    - deepseek-chat
    - gpt-4o

