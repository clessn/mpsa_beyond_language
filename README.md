# Beyond Language Barriers: Evaluating Open-Source LLMs in Non-English Sentiment Analysis

This repository contains the code, data, and analysis for the research article "Beyond Language Barriers: Evaluating the Efficacy of Open-Source LLMs in Analyzing Sentiment in Non-English Textual Data."

## Overview

This research evaluates the cross-linguistic capabilities of general-purpose large language models (LLMs) in sentiment analysis of French texts. We assess whether open-source foundational models can accurately analyze sentiment in non-English content without task-specific fine-tuning, comparing their performance to traditional dictionary-based methods and examining the impact of prompt language on analytical outcomes.

### Research Questions

1. Can general-purpose LLMs accurately evaluate sentiment in non-English texts without task-specific fine-tuning?
2. Can open-source general-purpose foundational LLMs accurately evaluate sentiment in non-English texts without task-specific fine-tuning?
3. How does the language of the prompt affect performance across different languages?

## Data

The corpus consists of 2,683 French-language news articles from 13 media sources collected from the Eureka database, spanning 1991-2025. These articles cover discourse about open-source software with the following search query:

```
TEXT= ("logiciel libre" | "logiciels libres" | "open source" | "open-source" | "logiciel open source" | "logiciels open source" | "code source ouvert" | "software libre" | "free software" | "code source libre" | "Free Software Foundation" | "Richard Stallman")
```

The sources include 9 newspapers from Québec, 1 from Ontario, and 3 from France, ensuring a diverse representation of the francophone media landscape.

## Repository Structure

- `/data/` - Contains all data files
  - `/clean/` - Processed data ready for analysis
  - `/dict/` - Dictionary files for lexicon-based sentiment analysis
  - `/eureka_articles/` - Raw HTML files of collected news articles
  - `/raw/` - Unprocessed data files
  - `/tmp/` - Temporary files and checkpoints
  - `/translation_files/` - Files for the translation process
- `/docs/` - Documentation and manuscript files
  - `/pub/` - Publication-ready files
- `/results/` - Analysis results and outputs
  - `/analysis/` - Statistical analysis results
  - `/graphs/` - Visualizations
  - `/tables/` - Result tables
- `/src/` - Source code for data collection, processing, and analysis

## Methods

### Data Collection

Data was collected using a custom Python web scraper (`src/00_eureka_scraper.py`) that extracts article content from the Eureka database. The scraper uses Selenium WebDriver to navigate the database interface and download articles in HTML format.

HTML documents were preprocessed with custom R functions to extract publication metadata (date, source, title) and full text content.

### Translation

To facilitate cross-linguistic analysis, French texts were translated into English using Google Translate. The process involved:
1. Batching the articles into Word documents using the `officer` R package
2. Adding unique delimiters to maintain document structure
3. Translating the documents using Google Translate
4. Extracting and realigning the translated text with the original French content

### Sentiment Analysis Approaches

#### Manual Annotation
- 200 randomly selected sentences were manually annotated on a scale from -1 to 1
- A custom Shiny application was developed to streamline the annotation process
- This annotated dataset served as the ground truth for evaluation

#### Dictionary-Based Analysis
- French corpus: Analyzed using a French Lexicoder Sentiment Dictionary (frlsd)
- English translations: Analyzed using the standard Lexicoder Sentiment Dictionary

#### LLM-Based Analysis
The research evaluates 11 different LLMs across three linguistic configurations:
1. French text with French prompts (FR→FR)
2. French text with English prompts (EN→FR)
3. English translations with English prompts (EN→EN)

**Models evaluated:**
- Open-source models:
  - Llama 3.2 (1B)
  - Llama 3.2 (3B)
  - Gemma 2 (9B)
  - Mistral Saba (24B)
  - QWQ (32B)
  - Llama 3.3 (70B)
  - DeepSeek R1 Basic (671B)
- Closed-source models:
  - Claude 3.5 Haiku
  - Gemini 2.0 Flash
  - DeepSeek Chat
  - GPT-4o

Each model was prompted three times per sentence to ensure robust results, for a total of 19,800 prompts.

### Evaluation Metrics

Three complementary methodologies were used to assess performance:
1. Correlation analysis: Pearson correlation coefficients between model predictions and ground truth
2. F1 score with 7-category classification: Very negative, negative, somewhat negative, neutral, somewhat positive, positive, very positive
3. F1 score with 3-category classification: Negative, neutral, positive

## Key Findings

1. General-purpose foundation models can effectively analyze sentiment in non-English texts, with the best-performing models achieving correlation coefficients above 0.70 and F1 scores exceeding 0.70 for 3-category classification.

2. Proprietary closed-weight models consistently outperformed their open-weight counterparts, with DeepSeek Chat, GPT-4o, and Gemini 2.0 demonstrating the strongest performance.

3. For correlation metrics, French prompts with French text yielded the strongest performance, closely followed by English prompts with French text. English prompts with English-translated text showed marginally lower performance.

4. For classification metrics, English prompts with French text yielded the highest average performance, slightly outperforming both French-prompted French text and English-prompted English text.

5. All models demonstrated substantially higher performance on the simplified 3-category task compared to the more granular 7-category classification, with models struggling most with "somewhat positive" and "somewhat negative" categories.

## Usage

### Environment Setup

This project uses R for data analysis and Python for web scraping. Dependencies are managed with Poetry.

```bash
# Install Poetry (if not already installed)
curl -sSL https://install.python-poetry.org | python3 -

# Install Python dependencies
poetry install

# Required R packages
# install.packages(c("tidyverse", "quanteda", "caret", "ellmer", "officer", "polyglotR"))
```

### Data Collection

```bash
# Run the Eureka scraper (requires login credentials)
python src/00_eureka_scraper.py --start-date 1991-01-01 --end-date 2025-01-01 --output-dir ./data/eureka_articles
```

### Analysis Pipeline

The analysis can be run step-by-step following the numbered scripts in the `/src/` directory:

1. HTML parsing and data extraction: `src/01_html_parsing.R`
2. Data summarization: `src/02_data_summary.R`
3. Data cleaning: `src/10_dataframe_cleaning.R`
4. Dictionary-based sentiment analysis: `src/20_frlsd.R`, `src/22_lsd_prep.R`, `src/23_lsd.R`
5. Translation: `src/21_translate_to_english.R`
6. Sample creation and manual annotation: `src/30_create_sample.R`, `src/31_validate_manual_anotation.R`
7. LLM prompting: `src/40_prompt.R`, `src/41_prompt_cleaning.R`
8. Performance evaluation: `src/50_cor.R`, `src/51_fscore_7.R`, `src/52_fscore_3.R`
9. Visualization: `src/60_cor_graph.R`, `src/61_mae_graph.R`, `src/62_fscore_graphs.R`
10. Result tables: `src/63_fscore_tables.R`, `src/64_results_summary.R`

### Replicating the Study

To replicate the study with your own API keys:

1. Set up the required API keys as environment variables:
   ```R
   Sys.setenv(FIREWORKS_API_KEY="your_key_here")
   Sys.setenv(OPENAI_API_KEY="your_key_here")
   Sys.setenv(ANTHROPIC_API_KEY="your_key_here")
   Sys.setenv(GOOGLE_API_KEY="your_key_here")
   Sys.setenv(GROQ_API_KEY="your_key_here")
   Sys.setenv(DEEPSEEK_API_KEY="your_key_here")
   ```

2. Run the `src/40_prompt.R` script to perform sentiment analysis with all models.

3. Run the evaluation scripts to analyze the results.

## Citation

If you use this code or data in your research, please cite:

```
Foisy, L.-O. M., Pelletier, C., Proulx, É., Vincent, S.-J., & Dufresne, Y. (2025). 
Beyond Language Barriers: Evaluating the Efficacy of Open-Source LLMs in Analyzing 
Sentiment in Non-English Textual Data.
```

## License

This repository is licensed under the terms included in the LICENSE file.
