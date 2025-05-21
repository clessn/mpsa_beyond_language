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
library(DT)         # For interactive data tables
library(readr)      # For reading CSV files
library(tools)      # For file extension function

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
      .setup-panel {
        padding: 15px;
        background-color: #f0f8ff;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      /* Panel classes handled via shinyjs */
    ")),
    tags$script(HTML("
      // Global variables to track slider state and keyboard state
      var currentValue = 0;
      window.keyboardShortcutsEnabled = true;
      
      // Function to update the slider value
      function updateSliderValue(newValue) {
        // Ensure value is within range and round to 2 decimal places
        newValue = Math.max(-1, Math.min(1, newValue));
        newValue = Math.round(newValue * 100) / 100;
        
        // Update our tracking variable
        currentValue = newValue;
        
        // Update the displayed value
        $('#current-value-display').text(newValue.toFixed(2));
        
        // Send the value to Shiny
        Shiny.setInputValue('sentiment_score', newValue);
      }
      
      // Make the function globally accessible
      window.updateSliderValue = updateSliderValue;
      
      // Function to show annotation panel and hide setup panel
      function startAnnotation() {
        $('.setup-panel').hide();
        $('.annotation-panel').show();
      }
      
      // Make this function globally accessible
      window.startAnnotation = startAnnotation;
      
      $(document).ready(function() {
        // Set up keyboard shortcuts
        $(document).on('keydown', function(e) {
          // Only process keyboard shortcuts if we're in annotation mode and shortcuts are enabled
          if (!$('.annotation-panel').is(':visible') || window.keyboardShortcutsEnabled === false) {
            return true;
          }
          
          // Log key presses for debugging
          console.log('Key pressed:', e.key);
          
          switch(e.key) {
            // Vim-style and arrow controls for adjusting values
            case 'h':
            case 'ArrowLeft':
              e.preventDefault();
              updateSliderValue(currentValue - 0.1);
              return false;
            case 'l':
            case 'ArrowRight':
              e.preventDefault();
              updateSliderValue(currentValue + 0.1);
              return false;
            case 'j':
            case 'ArrowDown':
              e.preventDefault();
              updateSliderValue(currentValue - 0.01);
              return false;
            case 'k':
            case 'ArrowUp':
              e.preventDefault();
              updateSliderValue(currentValue + 0.01);
              return false;
              
            // Navigation keys
            case 'n':
            case 'PageDown':
              e.preventDefault();
              document.getElementById('next_button').click();
              return false;
            case 'p':
            case 'PageUp':
              e.preventDefault();
              document.getElementById('prev_button').click();
              return false;
              
            // Quick value setting
            case '0':
              e.preventDefault();
              updateSliderValue(0);
              return false;
            case '-':
              e.preventDefault();
              updateSliderValue(-1);
              return false;
            case '=':
            case '+':
              e.preventDefault();
              updateSliderValue(1);
              return false;
              
            // Save shortcut
            case 's':
              e.preventDefault();
              document.getElementById('save_button').click();
              return false;
          }
        });
      });
      
      // Set up a handler for Shiny custom messages to update the slider value
      Shiny.addCustomMessageHandler('updateSliderValue', function(message) {
        updateSliderValue(message.value);
      });
    "))
  ),
  
  titlePanel("Sentiment Annotation Tool"),
  
  # Setup Panel - for file upload and configuration
  div(class = "setup-panel", id = "setup-panel",
      fluidRow(
        column(6,
               radioButtons("data_source", "Data Source:", 
                            choices = c("Upload New File" = "new_file", 
                                       "Load From Checkpoint" = "checkpoint"),
                            selected = "new_file"),
               conditionalPanel(
                 condition = "input.data_source == 'new_file'",
                 fileInput("file_upload", "Upload CSV or RDS file", 
                           accept = c(".csv", ".rds"),
                           buttonLabel = "Browse...",
                           placeholder = "No file selected"),
                 uiOutput("column_selector"),
                 textInput("annotation_column", "Name for annotation column:", 
                           value = "manual_sentiment"),
                 checkboxInput("has_translation", 
                              "Dataset contains English translations?", 
                              value = FALSE),
                 uiOutput("translation_column_selector")
               ),
               conditionalPanel(
                 condition = "input.data_source == 'checkpoint'",
                 fileInput("checkpoint_upload", "Select checkpoint file (.rds)", 
                           accept = c(".rds"),
                           buttonLabel = "Browse...",
                           placeholder = "No checkpoint selected"),
                 uiOutput("checkpoint_selector"),
                 hr(),
                 h4("Column Configuration:"),
                 uiOutput("checkpoint_column_selector"),
                 checkboxInput("cp_has_translation", 
                              "Checkpoint contains English translations?", 
                              value = FALSE),
                 uiOutput("cp_translation_column_selector")
               )
        ),
        column(6,
               h4("File Preview:"),
               DTOutput("file_preview")
        )
      ),
      actionButton("start_annotation", "Start Annotation", class = "btn-primary")
  ),
  
  # Annotation Panel - hidden initially
  div(class = "annotation-panel", id = "annotation-panel", style = "display: none;",
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
              conditionalPanel(
                "input.has_translation == true",
                checkboxInput("show_translation", "Show English Translation", FALSE)
              )
            )
          ),
          
          div(class = "navigation-buttons",
              actionButton("prev_button", "Previous"),
              actionButton("save_button", "Save Progress"),
              actionButton("export_button", "Export Data"),
              actionButton("next_button", "Next")
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
            tags$li("H / L or Left / Right arrows: Decrease/Increase score by 0.1"),
            tags$li("J / K or Down / Up arrows: Fine-tune score by 0.01"),
            tags$li("0: Set score to 0 (neutral)"),
            tags$li("-: Set score to -1 (very negative)"),
            tags$li("= or +: Set score to 1 (very positive)"),
            tags$li("N / P or Page Down / Page Up: Next/Previous sentence"),
            tags$li("S: Save progress")
          )
      )
  ),
  
  # Export Modal is created dynamically in server logic
)

#==============================================================================
# 3. SERVER LOGIC
#==============================================================================

server <- function(input, output, session) {
  # Reactive values to track state
  rv <- reactiveValues(
    current_index = 1,
    data = NULL,
    text_column = NULL,
    trans_column = NULL,
    annotation_column = NULL,
    unsaved_changes = FALSE,
    has_translation = FALSE
  )
  
  # Reactive function to read the uploaded file or checkpoint
  dataset <- reactive({
    # Switch based on data source
    if (input$data_source == "new_file") {
      req(input$file_upload)
      file_ext <- tools::file_ext(input$file_upload$datapath)
      
      if (file_ext == "csv") {
        read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      } else if (file_ext == "rds") {
        readRDS(input$file_upload$datapath)
      } else {
        stop("Unsupported file format. Please upload a CSV or RDS file.")
      }
    } else {
      # Loading from checkpoint - either direct upload or selected from dropdown
      if (!is.null(input$checkpoint_upload) && !is.null(input$checkpoint_upload$datapath)) {
        # User uploaded a checkpoint file directly
        readRDS(input$checkpoint_upload$datapath)
      } else if (!is.null(session$userData$checkpoint_data)) {
        # User selected from dropdown
        session$userData$checkpoint_data
      } else if (!is.null(input$checkpoint_select) && input$checkpoint_select != "") {
        # Fallback to reading selected file path
        readRDS(input$checkpoint_select)
      } else {
        # No checkpoint selected yet
        return(NULL)
      }
    }
  })
  
  # File preview in the setup panel
  output$file_preview <- renderDT({
    req(dataset())
    datatable(head(dataset(), 5), options = list(scrollX = TRUE, dom = 't'))
  })
  
  # Generate UI for selecting the text column
  output$column_selector <- renderUI({
    req(dataset())
    cols <- colnames(dataset())
    selectInput("text_column", "Select column to annotate:", choices = cols)
  })
  
  # Generate UI for selecting the translation column if needed
  output$translation_column_selector <- renderUI({
    req(input$has_translation == TRUE, dataset())
    cols <- colnames(dataset())
    selectInput("translation_column", "Select English translation column:", choices = cols)
  })
  
  # Start annotation
  observeEvent(input$start_annotation, {
    # Check data source type
    if (input$data_source == "new_file") {
      req(dataset(), input$text_column, input$annotation_column)
      
      # Store the dataset in reactive values
      rv$data <- dataset()
      
      # Store column names
      rv$text_column <- input$text_column
      rv$annotation_column <- input$annotation_column
      rv$has_translation <- input$has_translation
      
      if (input$has_translation) {
        req(input$translation_column)
        rv$trans_column <- input$translation_column
      }
      
      # Create annotation column if it doesn't exist
      if (!rv$annotation_column %in% colnames(rv$data)) {
        rv$data[[rv$annotation_column]] <- NA_real_
      }
    } else {
      # Loading from checkpoint file
      req(dataset())
      rv$data <- dataset()
      
      # Use the explicitly selected columns if provided
      if (!is.null(input$cp_text_column) && input$cp_text_column != "") {
        rv$text_column <- input$cp_text_column
      } else {
        # Fall back to auto-detection for text column
        text_cols <- names(rv$data)[sapply(rv$data, is.character)]
        if (length(text_cols) > 0) {
          rv$text_column <- text_cols[1]
        } else {
          showNotification("Could not identify text column in checkpoint", type = "error")
          return()
        }
      }
      
      if (!is.null(input$cp_annotation_column) && input$cp_annotation_column != "") {
        rv$annotation_column <- input$cp_annotation_column
      } else {
        # Fall back to auto-detection for annotation column
        possible_annotation_cols <- names(rv$data)[sapply(rv$data, function(col) {
          is.numeric(col) && any(is.na(col))
        })]
        
        if (length(possible_annotation_cols) > 0) {
          rv$annotation_column <- possible_annotation_cols[1]
        } else {
          numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
          if (length(numeric_cols) > 0) {
            rv$annotation_column <- numeric_cols[1]
          } else {
            rv$annotation_column <- "manual_sentiment"
            rv$data[[rv$annotation_column]] <- NA_real_
          }
        }
      }
      
      # Handle translation column
      rv$has_translation <- input$cp_has_translation
      if (input$cp_has_translation) {
        if (!is.null(input$cp_translation_column) && input$cp_translation_column != "") {
          rv$trans_column <- input$cp_translation_column
        } else {
          # Try to find a second character column
          text_cols <- names(rv$data)[sapply(rv$data, is.character)]
          if (length(text_cols) > 1) {
            rv$trans_column <- text_cols[2]
          } else {
            showNotification("No translation column found despite selection", type = "warning")
            rv$has_translation <- FALSE
          }
        }
      }
      
      # Ensure the annotation column is numeric
      if (!is.numeric(rv$data[[rv$annotation_column]])) {
        showNotification("Converting annotation column to numeric", type = "message")
        rv$data[[rv$annotation_column]] <- as.numeric(rv$data[[rv$annotation_column]])
      }
    }
    
    # Show annotation panel and hide setup panel
    shinyjs::hide("setup-panel")
    shinyjs::show("annotation-panel")
    
    # Find the first unannotated item and start there
    if (input$data_source == "checkpoint") {
      unannotated <- which(is.na(rv$data[[rv$annotation_column]]))
      if (length(unannotated) > 0) {
        rv$current_index <- min(unannotated)
      }
    }
    
    # Initialize with first sentence
    updateTextDisplay()
    updateProgressDisplay()
    updateSentimentDisplay()
  })
  
  # Helper functions to update different parts of the UI display
  updateTextDisplay <- function() {
    # Update sentence text
    output$sentence_text <- renderText({
      rv$data[[rv$text_column]][rv$current_index]
    })
    
    # Update translation if available
    if (rv$has_translation) {
      output$sentence_translation <- renderText({
        rv$data[[rv$trans_column]][rv$current_index]
      })
    }
  }
  
  updateProgressDisplay <- function() {
    # Update progress text
    output$progress_text <- renderText({
      completed <- sum(!is.na(rv$data[[rv$annotation_column]]))
      total <- nrow(rv$data)
      current <- rv$current_index
      paste0("Annotating item ", current, " of ", total, " (", 
             round(completed/total*100), "% complete)")
    })
  }
  
  updateSentimentDisplay <- function() {
    # Update the sentiment display
    current_score <- rv$data[[rv$annotation_column]][rv$current_index]
    if (!is.na(current_score)) {
      runjs(paste0("updateSliderValue(", current_score, ");"))
    } else {
      runjs("updateSliderValue(0);")
    }
  }
  
  # Combined update function
  updateUI <- function() {
    updateTextDisplay()
    updateProgressDisplay()
    updateSentimentDisplay()
  }
  
  # Handle next button
  observeEvent(input$next_button, {
    # Save current annotation if changed
    if (!is.null(input$sentiment_score)) {
      rv$data[[rv$annotation_column]][rv$current_index] <- input$sentiment_score
      # Mark changes for checkpoint
      rv$unsaved_changes <- TRUE
    }
    
    # Always save checkpoint when moving to next item
    saveCheckpoint()
    
    # Move to next sentence if not at the end
    if (rv$current_index < nrow(rv$data)) {
      rv$current_index <- rv$current_index + 1
      # Update displays separately to avoid issues
      updateTextDisplay()
      updateProgressDisplay()
      updateSentimentDisplay()
    } else {
      showNotification("You've reached the last item", type = "warning", duration = 2)
    }
  })
  
  # Handle previous button
  observeEvent(input$prev_button, {
    # Save current annotation if changed
    if (!is.null(input$sentiment_score)) {
      rv$data[[rv$annotation_column]][rv$current_index] <- input$sentiment_score
      # Mark changes for checkpoint
      rv$unsaved_changes <- TRUE
    }
    
    # Always save checkpoint when moving to previous item
    saveCheckpoint()
    
    # Move to previous sentence if not at the beginning
    if (rv$current_index > 1) {
      rv$current_index <- rv$current_index - 1
      # Update displays separately to avoid issues
      updateTextDisplay()
      updateProgressDisplay()
      updateSentimentDisplay()
    } else {
      showNotification("You're at the first item", type = "warning", duration = 2)
    }
  })
  
  # Store the current sentiment value when it changes
  # but don't mark as unsaved to avoid auto-saving
  observeEvent(input$sentiment_score, {
    rv$data[[rv$annotation_column]][rv$current_index] <- input$sentiment_score
  })
  
  # Helper function to save current progress
  saveCheckpoint <- function(notification = TRUE) {
    # Create the tmp directory if it doesn't exist
    if (!dir.exists("data/tmp")) {
      dir.create("data/tmp", recursive = TRUE, showWarnings = FALSE)
    }
    
    # Generate timestamp for the filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("data/tmp/annotations_progress_", timestamp, ".rds")
    
    # Save the data
    saveRDS(rv$data, filename)
    
    # Update state
    rv$unsaved_changes <- FALSE
    
    # Always show a small notification about the checkpoint
    if (notification) {
      showNotification(
        paste("Checkpoint saved:", filename), 
        type = "message",
        duration = 3
      )
    }
    
    return(filename)
  }
  
  # Create the tmp directory at startup if it doesn't exist
  observe({
    if (!dir.exists("data/tmp")) {
      dir.create("data/tmp", recursive = TRUE, showWarnings = FALSE)
    }
  })
  
  # Helper function to list available checkpoint files
  getCheckpointFiles <- function() {
    if (!dir.exists("data/tmp")) {
      return(character(0))
    }
    
    # Get all RDS files in tmp directory
    checkpoint_files <- list.files(
      path = "data/tmp", 
      pattern = "^annotations_.*\\.rds$", 
      full.names = TRUE
    )
    
    # Sort by modification time (newest first)
    file_info <- file.info(checkpoint_files)
    checkpoint_files <- checkpoint_files[order(file_info$mtime, decreasing = TRUE)]
    
    return(checkpoint_files)
  }
  
  # Generate UI for selecting from available checkpoints
  output$checkpoint_selector <- renderUI({
    req(input$data_source == "checkpoint")
    
    checkpoint_files <- getCheckpointFiles()
    
    if (length(checkpoint_files) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        "No checkpoint files found in data/tmp directory"
      ))
    }
    
    # Create human-readable names with timestamps
    checkpoint_names <- sapply(checkpoint_files, function(f) {
      # Extract the timestamp from the filename
      timestamp <- gsub(".*annotations_progress_(.*)\\.[Rr][Dd][Ss]$", "\\1", f)
      # If it's a "final" checkpoint, label it as such
      if (grepl("final", f)) {
        timestamp <- gsub(".*annotations_final_(.*)\\.[Rr][Dd][Ss]$", "\\1", f)
        label <- paste0("Final checkpoint - ", timestamp)
      } else {
        label <- paste0("Checkpoint - ", timestamp)
      }
      return(label)
    })
    
    names(checkpoint_files) <- checkpoint_names
    
    tagList(
      selectInput("checkpoint_select", "Or select from recent checkpoints:", 
                  choices = c("Select a checkpoint" = "", checkpoint_files)),
      actionButton("load_selected_checkpoint", "Load Selected Checkpoint")
    )
  })
  
  # Generate UI for selecting text and annotation columns from checkpoint
  output$checkpoint_column_selector <- renderUI({
    if (input$data_source != "checkpoint") return(NULL)
    
    # Get data from the uploaded or selected checkpoint
    cp_data <- NULL
    if (!is.null(input$checkpoint_upload) && !is.null(input$checkpoint_upload$datapath)) {
      cp_data <- readRDS(input$checkpoint_upload$datapath)
    } else if (!is.null(input$checkpoint_select) && input$checkpoint_select != "") {
      cp_data <- readRDS(input$checkpoint_select)
    } else {
      return(NULL)
    }
    
    # Generate column selection UI
    cols <- colnames(cp_data)
    
    # Try to identify the most likely annotation column (numeric with NA values)
    numeric_cols <- cols[sapply(cp_data, is.numeric)]
    na_cols <- numeric_cols[sapply(numeric_cols, function(col) any(is.na(cp_data[[col]])))]
    
    # Default to first numeric column with NA values, or just first numeric column
    default_annotation <- if(length(na_cols) > 0) na_cols[1] else if(length(numeric_cols) > 0) numeric_cols[1] else ""
    
    # Text columns are likely character columns
    text_cols <- cols[sapply(cp_data, is.character)]
    default_text <- if(length(text_cols) > 0) text_cols[1] else ""
    
    tagList(
      selectInput("cp_text_column", "Select text column:", 
                  choices = cols, selected = default_text),
      selectInput("cp_annotation_column", "Select annotation column:", 
                  choices = cols, selected = default_annotation)
    )
  })
  
  # Generate UI for selecting translation column from checkpoint
  output$cp_translation_column_selector <- renderUI({
    req(input$cp_has_translation == TRUE, input$data_source == "checkpoint")
    
    # Get data from the uploaded or selected checkpoint
    cp_data <- NULL
    if (!is.null(input$checkpoint_upload) && !is.null(input$checkpoint_upload$datapath)) {
      cp_data <- readRDS(input$checkpoint_upload$datapath)
    } else if (!is.null(input$checkpoint_select) && input$checkpoint_select != "") {
      cp_data <- readRDS(input$checkpoint_select)
    } else {
      return(NULL)
    }
    
    # Get character columns for translation selection
    cols <- colnames(cp_data)
    text_cols <- cols[sapply(cp_data, is.character)]
    
    # Default to second text column if available
    default_trans <- if(length(text_cols) > 1) text_cols[2] else if(length(text_cols) > 0) text_cols[1] else ""
    
    selectInput("cp_translation_column", "Select English translation column:", 
                choices = cols, selected = default_trans)
  })
  
  # Handle loading of selected checkpoint
  observeEvent(input$load_selected_checkpoint, {
    req(input$checkpoint_select != "")
    
    # Read the selected checkpoint
    checkpoint_data <- readRDS(input$checkpoint_select)
    
    # Update file preview
    output$file_preview <- renderDT({
      datatable(head(checkpoint_data, 5), options = list(scrollX = TRUE, dom = 't'))
    })
    
    # Simulate a file upload by setting up a reactive value
    # that the dataset() reactive function can use
    session$userData$checkpoint_data <- checkpoint_data
  })
  
  # Handle save button
  observeEvent(input$save_button, {
    if (rv$unsaved_changes) {
      saveCheckpoint()
    } else {
      showNotification("No changes to save", type = "message", duration = 2)
    }
  })
  
  # Show export modal
  observeEvent(input$export_button, {
    # Temporarily disable keyboard shortcuts while modal is open
    runjs("window.keyboardShortcutsEnabled = false;")
    
    # Default download folder path 
    home_dir <- Sys.getenv("HOME")
    download_dir <- file.path(home_dir, "Downloads")
    
    # Create download directory if it doesn't exist
    if (!dir.exists(download_dir)) {
      dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Show modal with download options
    showModal(modalDialog(
      title = "Export Annotated Data",
      textInput("export_filename", "Filename (without extension):", 
               value = paste0("annotated_data_", format(Sys.time(), "%Y%m%d"))),
      textInput("export_directory", "Save to directory:", 
               value = download_dir),
      radioButtons("export_format", "Export Format:",
                  choices = c("CSV" = "csv", "RDS" = "rds"),
                  selected = "csv"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("do_export", "Export", class = "btn-primary")
      )
    ))
  })
  
  # Handle export action
  observeEvent(input$do_export, {
    # Save current annotation if changed
    if (!is.null(input$sentiment_score)) {
      rv$data[[rv$annotation_column]][rv$current_index] <- input$sentiment_score
    }
    
    # Get export directory
    export_dir <- input$export_directory
    
    # Create directory if it doesn't exist
    if (!dir.exists(export_dir)) {
      dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    # Generate filename with path
    base_filename <- input$export_filename
    if (input$export_format == "csv") {
      filename <- file.path(export_dir, paste0(base_filename, ".csv"))
      write.csv(rv$data, filename, row.names = FALSE)
    } else {
      filename <- file.path(export_dir, paste0(base_filename, ".rds"))
      saveRDS(rv$data, filename)
    }
    
    # Show confirmation and close modal
    showNotification(paste("Data exported to", filename), type = "message", duration = 5)
    removeModal()
    
    # Re-enable keyboard shortcuts
    runjs("window.keyboardShortcutsEnabled = true;")
  })
  
  # Re-enable keyboard shortcuts when modal is dismissed
  observeEvent(input$cancel, {
    runjs("window.keyboardShortcutsEnabled = true;")
  })
  
  # Auto-save disabled per user request
  
  # Handle session ending (save data on close)
  session$onSessionEnded(function() {
    if (!is.null(rv$data) && rv$unsaved_changes) {
      # Create the tmp directory if it doesn't exist
      if (!dir.exists("data/tmp")) {
        dir.create("data/tmp", recursive = TRUE, showWarnings = FALSE)
      }
      
      # Generate timestamp for the filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("data/tmp/annotations_final_", timestamp, ".rds")
      
      # Save the data
      saveRDS(rv$data, filename)
    }
  })
}

#==============================================================================
# 4. APP EXECUTION
#==============================================================================

# Run the Shiny app
shinyApp(ui = ui, server = server)
