################################################################################ 
# module_load_cppModel_UI
################################################################################
# A module's UI function should be given a name that is 
# suffixed with Input, Output, or UI; 
module_load_cppModel_UI <- function(id, label = "") {
  
# Create a namespace function using the provided id
ns <- NS(id) 

tagList(  
  fluidRow(column(12,uiOutput(ns("cppModel_source_selector")))),
  fluidRow(column(6, uiOutput(ns("load_external_cppModel_container")))),
  fluidRow(column(6, uiOutput(ns("load_internal_cppModel_container")))), 
  fluidRow(column(6, uiOutput(ns("load_session_cppModel_container")))),
  fluidRow(column(12,uiOutput(ns("update_cppModel_container"))))
  ) 
}

################################################################################ 
# main function: module_load_cppModel
################################################################################

module_load_cppModel <- function(input, output, session, 
                                 ALL, cppModel_name="TEST") {

ns <- session$ns 
values <- reactiveValues()
 
#--------------------------------------  
# cppModel_source_selector
#--------------------------------------
output$cppModel_source_selector <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
  
  fluidRow(
    column(6,  
           radioButtons(ns("cppModel_source"), 
                        label="Select cpp model from:", 
                        choices=c("internal library", 
                                  "within session", 
                                  "external file"), 
                        inline=TRUE, 
                        width="100%",
                        selected="internal library"))
  )
})

#--------------------------------------  
# load_external_cppModel_container
#--------------------------------------  
output$load_external_cppModel_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE),
           need(input$cppModel_source=="external file", message=FALSE)) 
  
  fileInput(ns("which_external_cppModel"), 
            label = "load external cppModel", 
            width="100%" 
            ) # h5
  # accept=c('text/csv/sas7bdat', 
  #          'text/comma-separated-values,text/plain', 
  #          '.xlsx',
  #          '.xls',
  #          '.csv', 
  #          '.sas7bdat', 
  #          '.RData'))
  
})

#--------------------------------------  
# load_internal_cppModel_container
#-------------------------------------- 
output$load_internal_cppModel_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$cppModel_source=="internal library", message=FALSE)) 
  
  dirs_lst=list.files(path = paste0(WORKING_HOME, "/cpp"), 
                       full.names = FALSE, 
                       recursive = FALSE, 
                       pattern=".cpp", 
                       include.dirs=FALSE)  
  dirs_lst = c("", dirs_lst) 
  
  selectizeInput(ns("which_internal_cppModel"), 
                 label    = "load internal cppModel", 
                 choices  = dirs_lst, 
                 multiple = FALSE,
                 width = "100%", 
                 selected = dirs_lst[1]
  ) 
})


#--------------------------------------  
# load_session_cppModel_container
#-------------------------------------- 
output$load_session_cppModel_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$cppModel_source=="within session", message=FALSE)) 
   
  name_lst <- names(isolate({ALL$cppModel})) 
  only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
  dirs_lst = c("", setdiff(name_lst, only_for_internal_use))
  
  selectizeInput(ns("which_session_cppModel"), 
                 label    = "load session cppModel", 
                 choices  = dirs_lst, 
                 multiple = FALSE,
                 width = "100%", 
                 selected = dirs_lst[1]) 
})


#--------------------------------------  
# update_cppModel_container
#--------------------------------------
output$update_cppModel_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE), 
           need(values$cppModel_content, message=FALSE)
  ) 
  
  tagList(
    
    fluidRow(
      column(width=12, 
             HTML(colFmt("You may modify the loaded model and then re-assign a name for it.", 
                         color="gray")))
    ), 
    
    fluidRow(
      column(width=4,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("model_name"), 
                       value=NULL, 
                       placeholder ="cppModel-name", 
                       label=NULL, 
                       width="100%")
      ),
      
      column(2, 
             actionButton(ns("save_model"), 
                          label="Save model", 
                          style=actionButton_style )
      )
    ), 
    
    fluidRow(
      column(12,
             aceEditor(ns("cppModel_content"), 
                       mode="c_cpp", 
                       value=paste0(values$cppModel_content, collapse="\n"), 
                       theme = "crimson_editor",   # chrome
                       autoComplete = "enabled",
                       height = "1000px", 
                       fontSize = 15 
             )
      )
    )
  
  ) # tagList
})

 

#---------------------------------------------------  
# observeEvent of input$which_internal_cppModel 
#---------------------------------------------------  
#observeEvent({input$which_internal_cppModel}, {
load_internal_cppModel <- reactive({
    
  validate(need(input$cppModel_source=="internal library", message=FALSE), 
           need(input$which_internal_cppModel, message=FALSE)
  ) 
  
  # readLines cppModel
  cppModel_file <- paste0(WORKING_HOME, '/cpp/', input$which_internal_cppModel)
  values$cppModel_content <- readLines(cppModel_file)
    
  # create a progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
  progress$set(message = "mread cppModel...please Wait", value = 0)
  
  # mread cppModel
  environment(try_eval) <- environment()              # basename
  text="cppModel=mread(model='cppModel', project=paste0(WORKING_HOME, '/cpp/'), quiet=TRUE, file=basename(input$which_internal_cppModel))"
  env = try_eval(text)
  
  if ("cppModel" %in% ls(env)) {
    cppModel = get("cppModel", env)
    # "default, "message", "warning", "error" 
    showNotification("mread cppModel...done.", type="message")  # "default, "message", "warning", "error"  
  }else{
    cppModel = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error" 
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }
  
  cppModel
})

#---------------------------------------------------  
# observeEvent of input$which_external_cppModel 
#---------------------------------------------------  
#observeEvent({input$which_external_cppModel}, {
load_external_cppModel <- reactive({
  
  validate(need(input$cppModel_source=="external file", message=FALSE) )
  
  inFile = input$which_external_cppModel
  
  # print(inFile)  # readLines cppModel
  # name            size type  datapath
  # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
  # 
  ext <- tools::file_ext(inFile$name)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ext, sep="."))
   
  cppModel_file = paste(inFile$datapath, ext, sep=".")   
  values$cppModel_content <- readLines(cppModel_file)
   
  # create a progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
  progress$set(message = "mread cppModel...please Wait", value = 0)
  
  # mread cppModel
  environment(try_eval) <- environment()
  text="cppModel=mread(model='cppModel',project=dirname(inFile$datapath),quiet=TRUE,file=basename(cppModel_file))"
  env = try_eval(text)
  
  if ("cppModel" %in% ls(env)) {
    cppModel = get("cppModel", env)
    # "default, "message", "warning", "error" 
    showNotification("mread cppModel...done.", type="message")  # "default, "message", "warning", "error"  
  }else{
    cppModel = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error" 
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }
   
  cppModel
})

#---------------------------------------------------  
# observeEvent of input$which_session_cppModel 
#---------------------------------------------------  
#observeEvent({input$which_session_cppModel}, {
load_session_cppModel <- reactive({
  validate(need(input$cppModel_source=="within session", message=FALSE), 
           need(input$which_session_cppModel, message=FALSE) 
  )
  
  cppModel = ALL$cppModel[[input$which_session_cppModel]]
  validate(need(cppModel, message=FALSE))
            
  values$cppModel_content = see(cppModel, raw=TRUE)  
  
  cppModel  
})

#---------------------------------------------------  
# observeEvent of input$YesNoIIV, not used
#--------------------------------------------------- 
observeEvent(input$YesNoIIV, {
  cppModel = values$cppModel
  validate(need(cppModel, message=FALSE))
  
  if (input$YesNoIIV=="No") {
    cppModel = cppModel %>% zero_re 
    values$cppModel  = cppModel 
    message = "Simulation with no inter-individual variability (IIV)"
    # "default, "message", "warning", "error"  
    showNotification(message, type="message")  
  }
  
  if (input$YesNoIIV=="Yes") { 
    message = "Simulation with inter-individual variability (IIV)"
    # "default, "message", "warning", "error"  
    showNotification(message, type="message")  
  }
})
 

#--------------------------------------  
# observeEvent  
#-------------------------------------- 
# https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw
 
# event for values$cppModel 
# default, save the loaded cppModel to 
# ALL$cppModel[[cppModel_name]]
# observeEvent({values$cppModel}, {
#   validate(need(values$cppModel, message=FALSE))
#   
#   ALL$cppModel[[cppModel_name]] = values$cppModel
# })


observeEvent({input$which_internal_cppModel}, {
  validate(need(input$cppModel_source=="internal library", message=FALSE) )
  ALL$cppModel[[cppModel_name]]  = load_internal_cppModel()
})


observeEvent({input$which_external_cppModel}, {
  validate(need(input$cppModel_source=="external file", message=FALSE) )
  ALL$cppModel[[cppModel_name]]  = load_external_cppModel()
})


observeEvent({input$which_session_cppModel}, {
  validate(need(input$cppModel_source=="within session", message=FALSE) )
  ALL$cppModel[[cppModel_name]]  = load_session_cppModel()
})

#-----------------------
# event for save_model
#-----------------------
# or, user manually save the modified cppModel to 
# ALL$cppModel[[cppModel_name]]
observeEvent({input$save_model}, {
  
  if (is.null(input$cppModel_content) | input$cppModel_content=="") {
    message = "There is nothing to save" 
    # "default, "message", "warning", "error"  
    showNotification(message, type="warning")   
  }
  
  if (is.null(input$model_name) | input$model_name=="") {
    message = "Please specify model name" 
    # "default, "message", "warning", "error"  
    showNotification(message, type="warning")    
  }
  
  validate(need(input$cppModel_content, message="No cppModel loaded..."), 
           need(input$model_name, message="Please specify model name")
  ) 
  
  # create a progress object
  progress <- shiny::Progress$new()
  on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
  progress$set(message = "mread cppModel...please Wait", value = 0)
  
  # mread cppModel
  environment(try_eval) <- environment()
  text="cppModel=mread('cppModel', tempdir(), input$cppModel_content, quiet=TRUE)"
  env = try_eval(text)
    
  if ("cppModel" %in% ls(env)) {
    cppModel = get("cppModel", env)
    values$cppModel = cppModel
    ALL$cppModel[[input$model_name]] = cppModel  # visible
    # "default, "message", "warning", "error"  
    showNotification("mread cppModel...done.", type="message")  
  }else{
    values$cppModel = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error"  
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }
   
})

return(ALL)
}
