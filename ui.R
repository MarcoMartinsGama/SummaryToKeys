library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("Convert summary.txt to keys.txt"),
  sidebarLayout(#sidebarlayout
  sidebarPanel(
    #________ summary.txt input_______________________
      fileInput("summaryfile","Input summary.txt",
                accept = c("text/plain",".txt")),
    
    #________________________________________________
    #_______ _________CONTROL NAME___________________
    #Input Control name and input save
    textInput("ControlName","Name of the control",value = ""),
    actionButton("ControlNameSave","Save"),
    
    # Output the saving 
    verbatimTextOutput("ControlIDOutput"),
    #__________________________________________________
    #_______________Replicate names input______________
    actionButton("add_row","Add row"),
    actionButton("remove_row","Remove row"),
    
    #_________________________________________________
    #_______________Replicate names output______________
    uiOutput("dynamic_replicate"),
    actionButton("save_replicate_names","Save"),
    verbatimTextOutput("replicate_names_output"),
  
    #_________________________________________________
    #________________CONVERT__________________________
    actionButton("convert","Convert to keys.txt"),
    downloadButton("downloadkeys", "Download keys.txt !")
    #________________________________________________
  ),
  
  mainPanel(
    #________ summary.txt output___________________________
    DTOutput("summaryout"),
    DTOutput("keysout")
    
    #______________________________________________________
  
    )#mainPanel
 
   ) #sidebarLayout
  )) #UI et fluidpage