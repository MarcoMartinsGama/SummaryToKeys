library(shiny)
if (!requireNamespace("DT", quietly = TRUE))install.packages("DT")
if (!requireNamespace("dplyr", quietly = TRUE))install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE))install.packages("stringr")
library(DT)
library(dplyr)
library(stringr)

shinyServer(function(input,output,session){
  #_______ summary.txt____________________________
  
    # Output uploaded file
      output$summaryout <- renderDT({req(input$summaryfile)
        
        #Read the uploaded file 
        summary<- read.table(input$summaryfile$datapath,
                             header=TRUE,
                             sep="\t")
        return(summary)
        })
  #_______________________________________________
  #_______ CONTROL NAME___________________________
  ControlID <- reactiveValues(value = character(0))
  observeEvent(input$ControlNameSave, {
    ControlID$value <- input$ControlName
    output$ControlIDOutput <- renderPrint("Saved !, make sure only controls contain this name")
  })
  #_______________________________________________
  #_______________Replicate names input___________
  values <- reactiveValues(rows = 4, replicate_names = character(0))
  
  observeEvent(input$add_row, {
    values$rows <- values$rows + 1
  })
  
  observeEvent(input$remove_row, {
    if (values$rows > 1) {
      values$rows <- values$rows - 1
    }
  })
  
  output$dynamic_replicate <- renderUI({
    lapply(1:values$rows, function(i) {
      textInput(inputId = paste0("replicate_", i), label = paste("Replicate Tag", i))
    })
  })
  
  observeEvent(input$save_replicate_names, {
    replicate_names <- sapply(1:values$rows, function(i) {
      input[[paste0("replicate_", i)]]
    })
    values$replicate_names <- replicate_names
    output$replicate_names_output <- renderPrint("Saved !")
  })
  

  #_______________________________________________
  #_______________convert_________________________
  keys <- reactiveValues(df = NULL)
  observeEvent(input$convert,{
    summary <- read.table(input$summaryfile$datapath, header = TRUE, sep = "\t")
    replicate_names <- values$replicate_names
    ControlID_name <- ControlID$value
    
    keys$df<- summary %>% select(Raw.file) %>%  #Copy Raw.file
      mutate(BioReplicate= summary$Experiment) %>% #Copy Experiment and change column name
      mutate(IsotopeLabelType="L") %>%  # Create IsotopeLabelType column with "L" everywhere
      mutate(Run = row_number()) %>%    # Create Run with number same as the row number
      mutate(SAINT = ifelse(
        grepl(ControlID_name, BioReplicate), "C", "T")) # If replicate has the ControlId in its name,
    # Row will have C (control), otherwise T (test) 
    keys$df <- keys$df %>% 
      mutate(Condition= summary$Experiment) %>%  # Create Condition column from summary
      select(Condition,everything()) # Places it first
    
    keys$df <- keys$df %>% slice(-n()) # Remove the Total row from summary
    
    
    # Function to remove replicates name in condition
    
    ReplicateNames <- replicate_names
    remove_strings <- function(text, patterns) {
      for (pattern in patterns) {
        text <- str_replace_all(text, pattern, "")
      }
      return(text)
    }
    
    keys$df <- keys$df %>%
      mutate(Condition= remove_strings(Condition, 
                                       ReplicateNames)) # Remove replicate names in Condition column
    output$keysout <- renderDT(keys$df)
  })
  
  output$downloadkeys <- downloadHandler(filename = function(){
  "keys.txt"},content= function(file){
    write.table(keys$df,file,row.names = FALSE,sep = "\t")
  })
  #_______________________________________________
  
  
  
  
  
  
  
  
  
  
}) # }function )shinyServer


# Variable 1 "summary" = summary
# Variable 2 "ControlID$value" = ControlID
# variable 3 " values$replicate_names" = ReplicateNames