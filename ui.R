library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("Convert summary.txt to keys.txt"),
  sidebarLayout(
  sidebarPanel(
    width = 6, # Better side panel
    
    # Input summary.txt
    
      fileInput("summaryfile","Input summary.txt",
                accept = c("text/plain",".txt")),
    
    # Input Control name and save
    textInput("ControlName","Name of the control",value = ""),
    actionButton("ControlNameSave","Save"),
    textOutput("ControlIDOutput"), # message confirmation

    
    # Input replicate names 
    uiOutput("dynamic_replicate"),
    actionButton("save_replicate_names","Save"),
    actionButton("add_row","Add row"),
    actionButton("remove_row","Remove row"),
    textOutput("replicate_names_output"),
  
    # Convert to keys.txt and download button compatible with shinylive website
    
    actionButton("convert","Convert to keys.txt"),
    downloadButton("downloadkeys", "Download keys.txt !"),
    fluidRow(style = 'height: 200px; overflow-x: auto',DT::dataTableOutput("keysout"))
  ),
  
  mainPanel(
    width = 5, # Better main panel
    # Output tables
    fluidRow(style = 'overflow-x: auto',DT::dataTableOutput("summaryout"))
   )
  )
  )
  )