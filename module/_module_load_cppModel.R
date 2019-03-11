################################################################################ 
# module_load_cppModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_load_cppModel_old_UI <- function(id, label = "") {

ns <- NS(id) # Create a namespace function using the provided id

  tagList(  
    uiOutput(ns("cppModel_source_selector")),
    fluidRow(column(6,uiOutput(ns("load_external_cppModel_container")))),
    fluidRow(column(6,uiOutput(ns("load_internal_cppModel_container")))), 
    fluidRow(column(6,uiOutput(ns("load_session_cppModel_container")))),
    fluidRow(column(12,uiOutput(ns("update_cppModel_container"))))
  )  
}

################################################################################ 
# main function: module_load_cppModel
################################################################################

module_load_cppModel_old <- function(input, output, session, 
                                 ALL, cppModel_name="TEST") {
  
ns <- session$ns 
values <- reactiveValues()

#--------------------------------------  
# cppModel_source_selector
#--------------------------------------
output$cppModel_source_selector <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
  
  fluidRow(
    column(8,  
           radioButtons(ns("cppModel_source"), label="Select cpp model from:", 
                        choices=c("internal library", "within session", "external file"), inline=TRUE, width="100%",
                        selected="internal library")),
    
    column(4, #offset = 1, 
           radioButtons(ns("YesNoIIV"), label = "Want IIV?", inline=TRUE,
                        choices = list("Yes" = "Yes", "No" = "No"), width="100%",
                        selected = "No"))
  )

})
 
#--------------------------------------  
# load_external_cppModel_container
#--------------------------------------  
output$load_external_cppModel_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE),
           need(input$cppModel_source=="external file", message=FALSE)) 
  
  fileInput(ns("which_external_cppModel"), label = "load external cppModel", width="100%" ) # h5
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
  
  dirs_list=list.files(path = paste0(HOME, "/model/cpp"), 
                       full.names = FALSE, recursive = FALSE)  
  dirs_list = c("", dirs_list) 
  
  selectizeInput(ns("which_internal_cppModel"), 
       label    = "load internal cppModel", 
       choices  = dirs_list, 
       multiple = FALSE,
       width="100%", 
       selected = dirs_list[1]
  ) 
})
 
 
#--------------------------------------  
# load_session_cppModel_container
#-------------------------------------- 
output$load_session_cppModel_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$cppModel_source=="within session", message=FALSE)) 
   
  name_lst <- names(ALL$cppModel)
  only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
  dirs_list = c("", setdiff(name_lst, only_for_internal_use))
   
  selectizeInput(ns("which_session_cppModel"), 
                 label    = "load session cppModel", 
                 choices  = dirs_list, 
                 multiple = FALSE,
                 width="100%", 
                 selected = dirs_list[1]) 
})



output$update_cppModel_container <- renderUI({

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
})


#--------------------------------------  
# reactive of load_external_cppModel
#-------------------------------------- 
load_external_cppModel <- reactive({
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_external_cppModel, message = FALSE))
   
  inFile = input$which_external_cppModel
  
  # print(inFile)
  # name            size type  datapath
  # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
  # 
  ext <- tools::file_ext(inFile$name)
  #file.rename(inFile$datapath,
  #            paste(inFile$datapath, ext, sep="."))
  
  #source(paste(inFile$datapath, ext, sep="."))
  #cppModel <- mread("SHINY", tempdir(), mymodel)     # ?mread , ?mcode
  cppModel.file = basename(inFile$datapath)  #paste(inFile$datapath, ext, sep=".")
  
  cppModel <- mread(model ="Shiny", project = dirname(inFile$datapath), file=cppModel.file)     # ?mread , ?mcode
  if(input$YesNoIIV=="No") {cppModel <- cppModel %>% zero_re}
  
  attr(cppModel, 'file_name') <- inFile$name
  attr(cppModel, 'locaton_source') <- "external"
  cppModel
})

 
#--------------------------------------  
# reactive of load_internal_cppModel
#-------------------------------------- 
load_internal_cppModel <- reactive({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_internal_cppModel, message=FALSE))
   
  owd <- tempdir()
  on.exit(setwd(owd)) 
  
  ## capture messages and errors to a file.
  zz <- file("all.Rout", open="wt")
  sink(zz, type="message")
  
  # source the function
  cppModel <- NULL
  cppModel_file = input$which_internal_cppModel 
  text = "cppModel=tryCatch(mread(model='Shiny', project=paste0(HOME, '/model/cpp/'), file=cppModel_file)"
  try(
    eval(parse(text=text)), silent = TRUE 
  )  
  
  ## reset message sink and close the file connection
  sink(type="message")
  close(zz)
  
  ## Display the log file
  error_message <- readLines("all.Rout")
  
  if (length(error_message)>0) {
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }else {
    attr(cppModel, 'file_name') <- cppModel.file
    attr(cppModel, 'locaton_source') <- "internal"
    
    showNotification("mread cppModel sucessfully", type="message")   # "default, "message", "warning", "error"
  }
  
  cppModel
})
 

#--------------------------------------  
# reactive of load_session_cppModel
#-------------------------------------- 
load_session_cppModel <- reactive({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_session_cppModel, message=FALSE))
   
  cppModel = ALL$cppModel[[input$which_session_cppModel]]
  
  attr(cppModel, 'file_name') <- input$which_session_cppModel
  attr(cppModel, 'locaton_source') <- "session"
  cppModel
})  

#--------------------------------------  
# observeEvent  
#-------------------------------------- 
# https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw

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


observeEvent(input$YesNoIIV, {
  cppModel = ALL$cppModel[[cppModel_name]]
  validate(need(cppModel, message=FALSE))
  
  if (input$YesNoIIV=="No") {cppModel = cppModel %>% zero_re }
  ALL$cppModel[[cppModel_name]]  = cppModel  
})
 
return(ALL)
}
