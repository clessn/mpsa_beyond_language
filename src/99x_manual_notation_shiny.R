library(shiny)
library(shinyjs)

# Load your data
df_lsd <- readRDS("data/tmp/data_manual_ranking.rds")

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
            window.updateSliderValue(value - 0.25);
            return false;
          }
          else if (e.key === 'k') {
            e.preventDefault();
            window.updateSliderValue(value + 0.25);
            return false;
          }
          else if (e.key === 'ArrowLeft') {
            e.preventDefault();
            window.updateSliderValue(value - 0.1);
            return false;
          } 
          else if (e.key === 'ArrowRight') {
            e.preventDefault();
            window.updateSliderValue(value + 0.1);
            return false;
          }
          else if (e.key === 'ArrowDown') {
            e.preventDefault();
            window.updateSliderValue(value - 0.25);
            return false;
          }
          else if (e.key === 'ArrowUp') {
            e.preventDefault();
            window.updateSliderValue(value + 0.25);
            return false;
          }
          // Enter or 'n' for next
          else if (e.key === 'Enter' || e.key === 'n') {
            e.preventDefault();
            $('#save_button').click();
            return false;
          }
          // 'p' for previous
          else if (e.key === 'p') {
            e.preventDefault();
            $('#prev_button').click();
            return false;
          }
        }, true);
        
        // Initialize with zero
        window.updateSliderValue(0);
      });
    "))
  ),
  titlePanel("Sentence Sentiment Annotation"),
  
  # Progress information
  div(class = "controls-container",
    div(class = "progress-text", textOutput("progress_text"))
  ),
  
  # Current sentence display
  div(class = "sentence-container",
    htmlOutput("current_sentence")
  ),
  
  # Sentiment score display with custom controls
  div(class = "controls-container",
    h3("Sentiment Score"),
    div(class = "current-value", id = "current-value-display", "0.00"),
    
    # Hidden input that receives values from JavaScript
    numericInput("sentiment_score", label = NULL, value = 0, min = -1, max = 1, step = 0.05),
    tags$style(HTML("#sentiment_score { display: none; }")),
    
    div(class = "navigation-buttons",
      actionButton("prev_button", "< Previous (p)", icon = icon("arrow-left")),
      actionButton("save_button", "Save & Next (Enter/n)", icon = icon("arrow-right"), class = "btn-primary")
    )
  ),
  
  # Keyboard shortcuts help
  div(class = "keyboard-help",
    h4("Keyboard Shortcuts:"),
    tags$ul(
      tags$li(tags$b("h / ←:"), "Decrease score by 0.1"),
      tags$li(tags$b("l / →:"), "Increase score by 0.1"),
      tags$li(tags$b("j / ↓:"), "Decrease score by 0.25"),
      tags$li(tags$b("k / ↑:"), "Increase score by 0.25"),
      tags$li(tags$b("Enter / n:"), "Save and go to next sentence"),
      tags$li(tags$b("p:"), "Go to previous sentence")
    )
  ),
  
  # Download button for annotations
  div(class = "controls-container",
    downloadButton("download_data", "Download Annotations")
  )
)

server <- function(input, output, session) {
  # Reactive values to store state
  rv <- reactiveValues(
    current_index = 1,
    annotations = data.frame(
      doc_id = integer(),
      sentence = character(),
      manual_score = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # Display current sentence
  output$current_sentence <- renderUI({
    if(!exists("df_lsd") || rv$current_index > nrow(df_lsd)) {
      return(HTML("<p>No data loaded or all sentences annotated.</p>"))
    }
    
    # Get current sentence
    sentence <- df_lsd$sentences[rv$current_index]
    HTML(paste("<p>", sentence, "</p>"))
  })
  
  # Update progress information
  output$progress_text <- renderText({
    if(!exists("df_lsd")) return("No data loaded")
    
    paste0("Sentence ", rv$current_index, " of ", nrow(df_lsd), 
           " (", round(100 * rv$current_index / nrow(df_lsd), 1), "%)")
  })
  
  # Handle save and next
  observeEvent(input$save_button, {
    if(!exists("df_lsd") || rv$current_index > nrow(df_lsd)) return()
    
    # Save current annotation
    new_annotation <- data.frame(
      doc_id = df_lsd$doc_id[rv$current_index],
      sentence = df_lsd$sentences[rv$current_index],
      manual_score = input$sentiment_score,
      stringsAsFactors = FALSE
    )
    
    # Add to annotations dataframe
    rv$annotations <- rbind(rv$annotations, new_annotation)
    
    # Move to next sentence
    if(rv$current_index < nrow(df_lsd)) {
      rv$current_index <- rv$current_index + 1
      
      # Reset score to 0
      runjs("window.updateSliderValue(0);")
    } else {
      showNotification("All sentences have been annotated!", type = "message")
    }
  })
  
  # Handle previous button
  observeEvent(input$prev_button, {
    if(rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
      
      # Check if we have a previous annotation for this sentence
      prev_annotation <- rv$annotations[rv$annotations$doc_id == df_lsd$doc_id[rv$current_index], ]
      
      if(nrow(prev_annotation) > 0) {
        # Use the most recent annotation if multiple exist
        latest_annotation <- prev_annotation[nrow(prev_annotation), ]
        
        # Set the value using our JavaScript function
        runjs(sprintf("window.updateSliderValue(%f);", latest_annotation$manual_score))
        
        # Remove the annotation from the dataframe
        rv$annotations <- rv$annotations[!(rv$annotations$doc_id == df_lsd$doc_id[rv$current_index]), ]
      } else {
        # Reset to 0 if no previous annotation
        runjs("window.updateSliderValue(0);")
      }
    }
  })
  
  # Download handler for annotations
  output$download_data <- downloadHandler(
    filename = function() {
      paste("sentence_annotations_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$annotations, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
