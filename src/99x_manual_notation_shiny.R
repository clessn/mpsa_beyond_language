###############################################################################
# MANUAL SENTIMENT ANNOTATION SHINY APP
# 
# This script provides a Shiny application for manually annotating sentences
# with sentiment scores. It allows for efficient keyboard-based annotation
# and automatically saves progress as annotations are made.
#
# Author: Ral Zarek
# Date: March 2025
###############################################################################

#==============================================================================
# 1. SETUP AND DEPENDENCIES
#==============================================================================

library(shiny)      # For web application framework
library(shinyjs)    # For enhanced JavaScript functionality

# Load the dataset containing sentences to annotate
df_lsd <- readRDS("data/tmp/data_manual_ranking.rds")

#==============================================================================
# 2. USER INTERFACE DEFINITION
#==============================================================================

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .sentence-container {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        min-height: 100px;
        font-size: 18px;
      }
      .controls-container {
        padding: 15px;
        background-color: #e9ecef;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .navigation-buttons {
        display: flex;
        justify-content: space-between;
        margin-top: 20px;
      }
      .progress-text {
        font-size: 16px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 10px;
      }
      .keyboard-help {
        margin-top: 20px;
        padding: 10px;
        background-color: #f0f0f0;
        border-radius: 5px;
        font-family: monospace;
      }
      .current-value {
        font-size: 24px;
        font-weight: bold;
        text-align: center;
        margin-top: 10px;
      }
      .sentiment-reminder {
        margin-top: 20px;
        padding: 15px;
        background-color: #f0f8ff;
        border-left: 5px solid #4682b4;
        border-radius: 5px;
      }
      .sentiment-reminder h4 {
        color: #4682b4;
        margin-top: 0;
      }
      .sentiment-scale-item {
        display: flex;
        margin-bottom: 8px;
      }
      .sentiment-scale-value {
        font-weight: bold;
        width: 50px;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Global variables to track slider state
        window.currentValue = 0;
        
        // Function to update the slider value
        window.updateSliderValue = function(newValue) {
          // Ensure value is within range and round to 2 decimal places
          newValue = Math.max(-1, Math.min(1, newValue));
          newValue = Math.round(newValue * 100) / 100;
          
          // Update our tracking variable
          window.currentValue = newValue;
          
          // Update the displayed value
          $('#current-value-display').text(newValue.toFixed(2));
          
          // Send the value to Shiny
          Shiny.setInputValue('sentiment_score', newValue);
        };
        
        // Set up key event handler - directly on document to ensure it works
        document.addEventListener('keydown', function(e) {
          console.log('Key pressed:', e.key); // Debug logging
          
          // Get current value
          var value = window.currentValue;
          
          if (e.key === 'h') {
            e.preventDefault();
            window.updateSliderValue(value - 0.1);
            return false;
          }
          else if (e.key === 'l') {
            e.preventDefault();
            window.updateSliderValue(value + 0.1);
            return false;
          }
          else if (e.key === 'j') {
            e.preventDefault();
            window.updateSliderValue(value - 0.01);
            return false;
          }
          else if (e.key === 'k') {
            e.preventDefault();
            window.updateSliderValue(value + 0.01);
            return false;
          }
          else if (e.key === 'n') {
            e.preventDefault();
            $('#next-button').click();
            return false;
          }
          else if (e.key === 'p') {
            e.preventDefault();
            $('#prev-button').click();
            return false;
          }
          else if (e.key === '0') {
            e.preventDefault();
            window.updateSliderValue(0);
            return false;
          }
          else if (e.key === '-') {
            e.preventDefault();
            window.updateSliderValue(-1);
            return false;
          }
          else if (e.key === '=') {
            e.preventDefault();
            window.updateSliderValue(1);
            return false;
          }
          else if (e.key === 's') {
            e.preventDefault();
            $('#save-button').click();
            return false;
          }
        });
      });
    "))
  ),
  
  titlePanel("Sentiment Annotation Tool"),
  
  div(class = "progress-text", textOutput("progress_text")),
  
  div(class = "sentence-container",
      textOutput("sentence_text"),
      conditionalPanel(
        "input.show_translation",
        hr(),
        h4("English Translation:"),
        textOutput("sentence_translation")
      )
  ),
  
  div(class = "controls-container",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          style = "flex: 1;",
          h4("Sentiment Score:"),
          div(class = "current-value", tags$span(id = "current-value-display", "0.00"))
        ),
        div(
          style = "flex: 1; text-align: right;",
          checkboxInput("show_translation", "Show English Translation", FALSE)
        )
      ),
      
      div(class = "navigation-buttons",
          actionButton("prev_button", "Previous", id = "prev-button"),
          actionButton("save_button", "Save Progress", id = "save-button"),
          actionButton("next_button", "Next", id = "next-button")
      )
  ),
  
  div(class = "sentiment-reminder",
      h4("Sentiment Scale Reference:"),
      div(class = "sentiment-scale-item",
          span(class = "sentiment-scale-value", "-1.00:"),
          span("Strong negative sentiment - very critical, hostile, or pessimistic")
      ),
      div(class = "sentiment-scale-item",
          span(class = "sentiment-scale-value", "-0.50:"),
          span("Moderate negative sentiment - somewhat negative, disapproving")
      ),
      div(class = "sentiment-scale-item",
          span(class = "sentiment-scale-value", "0.00:"),
          span("Neutral sentiment - factual, balanced, or neither positive nor negative")
      ),
      div(class = "sentiment-scale-item",
          span(class = "sentiment-scale-value", "0.50:"),
          span("Moderate positive sentiment - somewhat positive, approving")
      ),
      div(class = "sentiment-scale-item",
          span(class = "sentiment-scale-value", "1.00:"),
          span("Strong positive sentiment - very supportive, enthusiastic, or optimistic")
      )
  ),
  
  div(class = "keyboard-help",
      h4("Keyboard Shortcuts:"),
      tags$ul(
        tags$li("H / L: Decrease/Increase score by 0.1"),
        tags$li("J / K: Fine-tune score by 0.01"),
        tags$li("0: Set score to 0 (neutral)"),
        tags$li("-: Set score to -1 (very negative)"),
        tags$li("=: Set score to 1 (very positive)"),
        tags$li("N / P: Next/Previous sentence"),
        tags$li("S: Save progress")
      )
  )
)

#==============================================================================
# 3. SERVER LOGIC
#==============================================================================

server <- function(input, output, session) {
  # Reactive values to track state
  rv <- reactiveValues(
    current_index = 1,
    data = df_lsd,
    unsaved_changes = FALSE
  )
  
  # Initialize with first sentence
  output$sentence_text <- renderText({
    rv$data$sentence[rv$current_index]
  })
  
  output$sentence_translation <- renderText({
    rv$data$sentence_en[rv$current_index]
  })
  
  # Update progress text
  output$progress_text <- renderText({
    completed <- sum(!is.na(rv$data$manual_sentiment))
    total <- nrow(rv$data)
    current <- rv$current_index
    paste0("Annotating sentence ", current, " of ", total, " (", 
           round(completed/total*100), "% complete)")
  })
  
  # Initialize the sentiment value for the current sentence
  observe({
    # If we have a saved value, use it
    if (!is.na(rv$data$manual_sentiment[rv$current_index])) {
      session$sendCustomMessage(
        type = "updateSliderValue",
        message = list(value = rv$data$manual_sentiment[rv$current_index])
      )
      # Use JavaScript to update the display value
      js$updateSliderValue(rv$data$manual_sentiment[rv$current_index])
    } else {
      # Otherwise reset to 0
      js$updateSliderValue(0)
    }
  })
  
  # Handle next button
  observeEvent(input$next_button, {
    # Save current annotation if changed
    if (!is.null(input$sentiment_score)) {
      rv$data$manual_sentiment[rv$current_index] <- input$sentiment_score
      rv$unsaved_changes <- TRUE
    }
    
    # Move to next sentence if not at the end
    if (rv$current_index < nrow(rv$data)) {
      rv$current_index <- rv$current_index + 1
      
      # Update display
      output$sentence_text <- renderText({
        rv$data$sentence[rv$current_index]
      })
      
      output$sentence_translation <- renderText({
        rv$data$sentence_en[rv$current_index]
      })
      
      # If we have a saved value for this sentence, use it
      if (!is.na(rv$data$manual_sentiment[rv$current_index])) {
        js$updateSliderValue(rv$data$manual_sentiment[rv$current_index])
      } else {
        # Otherwise reset to 0
        js$updateSliderValue(0)
      }
    }
  })
  
  # Handle previous button
  observeEvent(input$prev_button, {
    # Save current annotation if changed
    if (!is.null(input$sentiment_score)) {
      rv$data$manual_sentiment[rv$current_index] <- input$sentiment_score
      rv$unsaved_changes <- TRUE
    }
    
    # Move to previous sentence if not at the beginning
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
      
      # Update display
      output$sentence_text <- renderText({
        rv$data$sentence[rv$current_index]
      })
      
      output$sentence_translation <- renderText({
        rv$data$sentence_en[rv$current_index]
      })
      
      # If we have a saved value for this sentence, use it
      if (!is.na(rv$data$manual_sentiment[rv$current_index])) {
        js$updateSliderValue(rv$data$manual_sentiment[rv$current_index])
      } else {
        # Otherwise reset to 0
        js$updateSliderValue(0)
      }
    }
  })
  
  # Automatically save the current sentiment value when it changes
  observeEvent(input$sentiment_score, {
    rv$data$manual_sentiment[rv$current_index] <- input$sentiment_score
    rv$unsaved_changes <- TRUE
  })
  
  # Handle save button
  observeEvent(input$save_button, {
    if (rv$unsaved_changes) {
      # Generate timestamp for the filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("data/tmp/sentence_annotations_", timestamp, ".csv")
      
      # Save the data
      write.csv(rv$data, filename, row.names = FALSE)
      
      # Also save to a standard RDS file
      saveRDS(rv$data, "data/tmp/data_manual_ranking.rds")
      
      # Update state and show message
      rv$unsaved_changes <- FALSE
      showNotification(
        paste("Annotations saved to", filename), 
        type = "message",
        duration = 3
      )
    } else {
      showNotification("No changes to save", type = "message", duration = 2)
    }
  })
  
  # Automatically save every 5 minutes
  autoSaveTimer <- reactiveTimer(300000)  # 5 minutes in milliseconds
  
  observe({
    autoSaveTimer()
    if (rv$unsaved_changes) {
      # Generate timestamp for the filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("data/tmp/sentence_annotations_auto_", timestamp, ".csv")
      
      # Save the data
      write.csv(rv$data, filename, row.names = FALSE)
      
      # Also save to a standard RDS file
      saveRDS(rv$data, "data/tmp/data_manual_ranking.rds")
      
      # Update state and show message
      rv$unsaved_changes <- FALSE
      showNotification(
        paste("Auto-saved to", filename), 
        type = "message",
        duration = 3
      )
    }
  })
  
  # Handle session ending (save data on close)
  session$onSessionEnded(function() {
    if (rv$unsaved_changes) {
      # Generate timestamp for the filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("data/tmp/sentence_annotations_final_", timestamp, ".csv")
      
      # Save the data
      write.csv(rv$data, filename, row.names = FALSE)
      
      # Also save to a standard RDS file
      saveRDS(rv$data, "data/tmp/data_manual_ranking.rds")
    }
  })
}

#==============================================================================
# 4. APP EXECUTION
#==============================================================================

# Run the Shiny app
shinyApp(ui = ui, server = server)