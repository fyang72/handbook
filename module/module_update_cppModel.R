
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

module_update_cppModel_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=12, 
             HTML(colFmt("You may modify the loaded model and then re-assign a name for it.", color="gray")))
    ), 
    
    fluidRow(
      column(6,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("model_name"), value=NULL, 
                       placeholder ="cppModel-name", label=NULL, width="100%")
      ),
      
      column(2, 
             actionButton(ns("save_model"), label="Save model", style=actionButton_style )
      ),
      
      column(2,  
             actionButton(ns("delete_model"),label="Delete model", 
                          style=actionButton_style))
    ),
    
    fluidRow(
      column(width=12,   
             uiOutput(ns("cppModel_content_container")) 
      )
    )
  )
}

module_update_cppModel <- function(input, output, session, ALL, cppModel_name) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  
  ns <- session$ns 
  
  #--------------------------------------  
  # cppModel_content_container
  #--------------------------------------
  output$cppModel_content_container <- renderUI({
    validate(need(cppModel_name(), message=FALSE))
    
    cppModel = ALL$cppModel[[cppModel_name()]]
    validate(need(cppModel, message=FALSE))
    script_content <- capture.output(mrgsolve::see(cppModel))
    script_content <- script_content[3:length(script_content)]
    
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
    
    mymodel = input$cppModel_content
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
   
    validate(need(cppModel_name(), message="delete_model..."))
    ALL$cppModel[[cppModel_name()]]  = NULL 

    showNotification("cppModel deleted", type="message")  # "default, "message", "warning", "error"  
    ALL
  })
  
  return(ALL)  
}
