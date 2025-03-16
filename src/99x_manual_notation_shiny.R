library(shiny)
library(shinyjs)
library(dplyr)

# Load your data
df_lsd <- readRDS("path/to/your/data.rds")  # Uncomment and replace with your data path
# For demo purposes, if you don't have the data loaded yet
# df_lsd <- data.frame(
#   doc_id = 1:10,
#   sentences = paste("Example sentence", 1:10),
#   stringsAsFactors = FALSE
# )

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
      .slider-container {
        padding-top: 20px;
        padding-bottom: 20px;
      }
      .navigation-buttons {
        display: flex;
        justify-content: space-between;
        margin-top: 20px;
      }
    ")),
    # Add keyboard event handling
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        // 'h' key - decrease slider value
        if(e.key === 'h') {
          var value = $('#sentiment_score').val();
          var newValue = Math.max(-1, parseFloat(value) - 0.05);
          $('#sentiment_score').val(newValue).trigger('change');
        }
        // 'l' key - increase slider value
        else if(e.key === 'l') {
          var value = $('#sentiment_score').val();
          var newValue = Math.min(1, parseFloat(value) + 0.05);
          $('#sentiment_score').val(newValue).trigger('change');
        }
        // Enter key - save and next
        else if(e.key === 'Enter') {
          $('#save_button').click();
        }
      });
    "))
  ),
  titlePanel("Sentence Sentiment Annotation"),
  
  # Progress information
  div(class = "controls-container",
    fluidRow(
      column(4, textOutput("progress_text")),
      column(8, progressBar("progress", value = 0, display_pct = TRUE))
    )
  ),
  
  # Current sentence display
  div(class = "sentence-container",
    htmlOutput("current_sentence")
  ),
  
  # Annotation controls
  div(class = "controls-container",
    div(class = "slider-container",
      sliderInput("sentiment_score", "Sentiment Score",
                min = -1, max = 1, value = 0, step = 0.05,
                width = "100%")
    ),
    div(class = "navigation-buttons",
      actionButton("prev_button", "< Previous"),
      actionButton("save_button", "Save & Next", class = "btn-primary")
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
  
  # Initialize with data once loaded
  observe({
    # This will be called once when the app starts
    # Use the actual doc_ids from your dataset
    if(exists("df_lsd")) {
      rv$doc_ids <- df_lsd$doc_id
    }
  })
  
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
    
    paste0("Sentence ", rv$current_index, " of ", nrow(df_lsd))
  })
  
  # Update progress bar
  observe({
    if(exists("df_lsd")) {
      updateProgressBar(
        session = session,
        id = "progress",
        value = 100 * rv$current_index / nrow(df_lsd)
      )
    }
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
      # Reset slider to 0 for next sentence
      updateSliderInput(session, "sentiment_score", value = 0)
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
        updateSliderInput(session, "sentiment_score", value = latest_annotation$manual_score)
        
        # Remove the annotation from the dataframe
        rv$annotations <- rv$annotations[!(rv$annotations$doc_id == df_lsd$doc_id[rv$current_index]), ]
      } else {
        # Reset to 0 if no previous annotation
        updateSliderInput(session, "sentiment_score", value = 0)
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
