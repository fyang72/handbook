
################################################################################ 
# module_load_ctlModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_update_cppModel_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  tagList(
    fluidRow(
      column(width=12, 
             HTML(colFmt("You may modify the loaded model and then re-assign a name for it.", color="gray")))
    ), 
    
    fluidRow(
      column(width=4,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("model_name"), value=NULL, placeholder ="cppModel-name", label=NULL, width="100%")
      ),
      
      column(2, 
             actionButton(ns("save_model"), label="Save model", style=actionButton_style )
      )
    ),
    fluidRow(
      column(width=12,   
             uiOutput(ns("cppModel_content_container")) 
      )
    ) 
  ) # tagList
}

################################################################################ 
# main function: module_load_ctlModel
################################################################################

module_update_cppModel <- function(input, output, session, ALL, cppModel_name="cppModel_name") {
  
ns <- session$ns 
#values <- reactiveValues()

#--------------------------------------  
# cppModel_content_container
#--------------------------------------
output$cppModel_content_container <- renderUI({
  cppModel =  ALL$cppModel[[cppModel_name]]
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(cppModel, message=FALSE)
  )
    
  fluidRow(
    column(12,
         aceEditor(ns("cppModel_content"), 
                   mode="c_cpp", 
                   value=paste0(see(cppModel,raw=TRUE) , collapse="\n"), 
                   theme = "crimson_editor",   # chrome
                   autoComplete = "enabled",
                   height = "1000px", 
                   fontSize = 15 
         )
    )
  )
})

#--------------------
# event for save_model
#--------------------
observeEvent(input$save_model, {
  
  mymodel =input$cppModel_content
  validate(need(mymodel, message="Empty cppModel..."))
  
  owd <- tempdir()
  on.exit(setwd(owd)) 
  
  ## capture messages and errors to a file.
  zz <- file("all.Rout", open="wt")
  sink(zz, type="message")
  
  # source the function
  cppModel <- NULL
  try(
    eval(parse(text="cppModel=mread('shiny', tempdir(), mymodel")), silent = TRUE 
  )  
  
  ## reset message sink and close the file connection
  sink(type="message")
  close(zz)
  
  ## Display the log file
  error_message <- readLines("all.Rout")
  
  if (length(error_message)>0) {
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }else {
    ALL$cppModel[[input$model_name]]  = cppModel
    showNotification("cppModel saved", type="message")   # "default, "message", "warning", "error"
  }
 
})
  
return(ALL)
}