
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
      ),
      
      column(2, 
             actionButton(ns("delete_model"),label="Delete model", style=actionButton_style)
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
  cppModel =  "  ####" # ALL$cppModel[[cppModel_name()]]
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(cppModel, message=FALSE)
  )
    
  print("in cppModel_content_container")
  script_content <- "FFS" #capture.output(mrgsolve::see(cppModel))
  
  fluidRow(
    column(12,
         aceEditor(ns("cppModel_content"), 
                   mode="c_cpp", 
                   value=paste0(script_content, collapse="\n"), 
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
  
  # evaluate it
  text="cppModel=mread('shiny', tempdir(), mymodel)"
  #text="cppModel=mread(model='cppModel', project=paste0(HOME, '/cpp/'), quiet=TRUE, file=basename(input$which_internal_cppModel))"
  environment(try_eval) <- environment()              # basename
  env = try_eval(text)
  
  if ("cppModel" %in% ls(env)) {
    cppModel = get("cppModel", env)
    ALL$cppModel[[input$model_name]]  = cppModel
    # "default, "message", "warning", "error" 
    showNotification("mread cppModel...done and saved", type="message")  # "default, "message", "warning", "error"  
  }else{
    cppModel = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error" 
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }
   
  
})



#--------------------
# event for delete_model
#--------------------
observeEvent(input$delete_model, {
  
  #mymodel =input$cppModel_content
  #validate(need(mymodel, message="Empty cppModel..."))
   
  print("in deleting:")

  #ALL$cppModel[[cppModel_name()]]  = NULL 
  showNotification("cppModel deleted", type="message")  # "default, "message", "warning", "error"  
  
})
  
return(input)
}