library(shiny)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("data.table")


library(stringr)
library(DT)
library(data.table)
library(dplyr)

function(input,output,session){
  
  # Render and read summary.txt
  observeEvent(input$summaryfile,{
  output$out <-  renderDT({req(input$summaryfile)
    summary<- read.table(input$summaryfile$datapath,
                         header=TRUE,
                         sep="\t") 
    return(summary)
  }, rownames = FALSE) 
    })
  
  # Control ID saving
  
  ControlID <- reactiveValues(value = character(0))
  observeEvent(input$ControlNameSave, {
    ControlID$value <- input$ControlName
    output$ControlIDOutput <- renderText({"Saved !, make sure only controls contain this name"})
  })
  # Replicate IDs, manage rows and save
  values <- reactiveValues(rows = 4, replicate_names = character(0)) # Create 4 rows to write in
  
  observeEvent(input$add_row, {
    values$rows <- values$rows + 1 
  })  # Add 1 row with button
  
  observeEvent(input$remove_row, {
    if (values$rows > 1) {
      values$rows <- values$rows - 1
    }
  }) # Remove 1 row with button
  
  output$dynamic_replicate <- renderUI({
    lapply(1:values$rows, function(i) {
      textInput(inputId = paste0("replicate_", i), label = paste("Replicate Tag", i))
    })
  }) # Render UI to write replicate names and manage row number
  
  observeEvent(input$save_replicate_names, {
    replicate_names <- sapply(1:values$rows, function(i) {
      input[[paste0("replicate_", i)]]
    }) 
    values$replicate_names <- replicate_names
    output$replicate_names_output <- renderText({"Saved !"})
  }) # Save Replicate names with a button
  
  
  # Convert summary.txt to keys.txt
  
  keys <- reactiveValues(df = NULL) # Create empty dataframe "keys"
  
  observeEvent(input$convert,{
    summary <- read.table(input$summaryfile$datapath, header = TRUE, sep = "\t") # read summary.txt
    replicate_names <- values$replicate_names  # Copy replicate names
    ControlID_name <- ControlID$value # Copy control ID
    
    keys$df<- summary %>% select(Raw.file) %>%  #Copy Raw.file column
      mutate(BioReplicate= summary$Experiment) %>% #Copy Experiment and change column name to Bioreplicate
      mutate(IsotopeLabelType="L") %>%  # Create IsotopeLabelType column with "L" everywhere
      mutate(Run = row_number()) %>%    # Create Run with number same as the row number
      mutate(SAINT = ifelse(
        grepl(ControlID_name, BioReplicate), "C", "T")) # If replicate has the ControlId in its name,
    # Row will have C (control), otherwise T (test) in the column SAINT
    
    keys$df <- keys$df %>%  # Next steps need to be separated to work correctly,copies itself
      mutate(Condition= summary$Experiment) %>%  # Create Condition column from summary
      select(Condition,everything()) # Places as first column
    
    keys$df <- keys$df %>% slice(-n()) # Remove the artefact Total row from summary
    
    
    # Function to remove replicates name in condition
    
    ReplicateNames <- replicate_names # Copy replicate names
    remove_strings <- function(text, patterns) {
      for (pattern in patterns) {
        text <- str_replace_all(text, pattern, "")
      } 
      return(text)
    } # Replace the patterns from replicate names by an empty text
    
    keys$df <- keys$df %>%
      mutate(Condition= remove_strings(Condition, 
                                       ReplicateNames)) # Remove replicate names in Condition column
    output$out <- renderDataTable(keys$df, rownames = FALSE) # Render the keys table
  })
  
  output$keys <- downloadHandler(filename = function(){ # Download keys.txt with button
    "keys.txt"},content= function(file){
      write.table(keys$df,file,row.names = FALSE,sep = "\t")
    },
    contentType = "text/plain" ) 
}