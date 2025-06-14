# Manual Sentiment Annotation Instructions for Shiny App

## Overview
This document provides step-by-step instructions for running the manual sentiment annotation process using the Shiny application located in the `src/` directory. This app is designed for coding text sentences with sentiment scores on a scale from -1 (very negative) to +1 (very positive).

## Data Information
- **File to clean**: `data/tmp/data_manual_ranking.rds`
- **Column to code**: `sentence`
- **Annotation scale**: -1.00 (very negative) to +1.00 (very positive)

## Step-by-Step Instructions

### 1. Prerequisites
- Ensure you have R and RStudio installed on your computer
- Required R packages: `shiny`, `shinyjs`, `DT`, `readr`, `tools`
- Make sure all required packages are installed (run `install.packages(c("shiny", "shinyjs", "DT", "readr", "tools"))` if needed)

### 2. Starting the Application
1. Open RStudio
2. Navigate to the project directory
3. Open the file `src/99x_manual_notation_shiny.R`
4. Click the "Run App" button in RStudio (appears at the top of the script editor)
5. The app will open in your web browser or RStudio viewer

### 3. Setting Up Data Source
1. **Choose Data Source**: You'll see two options:
   - "Upload New File": Select this to start fresh with `data/tmp/data_manual_ranking.rds`
   - "Load From Checkpoint": Select this to continue previous work
2. **For New File**:
   - Click "Browse..." and navigate to `data/tmp/data_manual_ranking.rds`
   - Select `sentence` as the column to annotate
   - Keep "manual_sentiment" as the annotation column name
   - If you have English translations, check the translation option
3. **Click "Start Annotation"** to begin the coding process

### 4. Using the Annotation Interface
1. **Reading Sentences**: Each sentence from the `sentence` column will be displayed in a gray box
2. **Sentiment Scale**: Use the sentiment scale reference provided:
   - **-1.00**: Strong negative sentiment (very critical, hostile, pessimistic)
   - **-0.50**: Moderate negative sentiment (somewhat negative, disapproving)
   - **0.00**: Neutral sentiment (factual, balanced, neither positive nor negative)
   - **0.50**: Moderate positive sentiment (somewhat positive, approving)
   - **1.00**: Strong positive sentiment (very supportive, enthusiastic, optimistic)

3. **Assigning Scores**: 
   - The current score is displayed as a large number
   - Use keyboard shortcuts or buttons to set the sentiment score
   - The app starts at 0.00 (neutral) for each new sentence

### 5. Keyboard Shortcuts (Recommended for Efficiency)
- **H** or **Left Arrow**: Decrease score by 0.1
- **L** or **Right Arrow**: Increase score by 0.1  
- **J** or **Down Arrow**: Fine-tune score down by 0.01
- **K** or **Up Arrow**: Fine-tune score up by 0.01
- **0**: Set score to 0 (neutral)
- **-**: Set score to -1 (very negative)
- **=** or **+**: Set score to 1 (very positive)
- **N** or **Page Down**: Move to next sentence
- **P** or **Page Up**: Move to previous sentence
- **S**: Save progress manually

### 6. Navigation and Progress
1. **Progress Display**: Shows current item number, total items, and completion percentage
2. **Navigation**: Use Previous/Next buttons or keyboard shortcuts
3. **Auto-save**: The app automatically saves your progress when moving between sentences

### 7. Saving Your Work
1. **Automatic Checkpoints**: Progress is automatically saved in `data/tmp/` with timestamps
2. **Manual Save**: Use the "Save Progress" button or press **S** for manual saves
3. **Export Data**: Use "Export Data" button to save final results as CSV or RDS

### 8. Quality Control Tips
1. **Consistency**: Try to maintain consistent standards throughout your annotation session
2. **Context**: Consider the full context of each sentence when assigning sentiment
3. **Breaks**: Take regular breaks to avoid fatigue and maintain accuracy
4. **Review**: Use the Previous button to review and adjust previous annotations if needed

### 9. Troubleshooting
- **App Won't Start**: Make sure all required R packages are installed
- **File Not Found**: Ensure `data/tmp/data_manual_ranking.rds` exists in the correct location
- **Lost Progress**: Check the `data/tmp/` folder for checkpoint files with timestamps
- **Keyboard Shortcuts Not Working**: Make sure the annotation panel is active (not in setup mode)

### 10. Completing Your Work
1. **Final Export**: When finished, use "Export Data" to save your completed annotations
2. **File Location**: Choose your preferred location (default is Downloads folder)
3. **File Format**: Choose CSV for general use or RDS to preserve R data types
4. **Backup**: Keep backup copies of your final annotated dataset

## Sentiment Annotation Guidelines
- **Be Consistent**: Apply the same standards throughout your annotation session
- **Context Matters**: Consider the full meaning and context of each sentence
- **Neutral Default**: When in doubt, lean towards neutral (0.00) rather than guessing
- **Take Breaks**: Sentiment annotation requires concentration - take breaks to maintain quality