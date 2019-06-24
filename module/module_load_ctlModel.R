
################################################################################ 
# module_load_ctlModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_load_ctlModel_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
 
  fluidRow(
    column(width=12,   
           uiOutput(ns("ctlModel_source_selector")),
           uiOutput(ns("load_external_ctlModel_container")),
           uiOutput(ns("load_internal_ctlModel_container")), 
           uiOutput(ns("load_session_ctlModel_container"))
          )
    ) 
}

################################################################################ 
# main function: module_load_ctlModel
################################################################################

module_load_ctlModel <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {
  
  ns <- session$ns 
  values <- reactiveValues()
   
  #--------------------------------------  
  # ctlModel_source_selector
  #--------------------------------------
  output$ctlModel_source_selector <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    fluidRow(
      column(12,  
             radioButtons(ns("ctlModel_source"), label="Select ctl model from:", 
                          choices=c("internal library", "within session", "external file"), inline=TRUE, width="100%",
                          selected="internal library"))
    )
  })
  
  #--------------------------------------  
  # load_external_ctlModel_container
  #--------------------------------------  
  output$load_external_ctlModel_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE),
             need(input$ctlModel_source=="external file", message=FALSE)) 
    
    fluidRow(
      column(12, 
             fileInput(ns("which_external_ctlModel"), label = "load external ctlModel", width="100%" ) # h5
      )
    )
    # accept=c('text/csv/sas7bdat', 
    #          'text/comma-separated-values,text/plain', 
    #          '.xlsx',
    #          '.xls',
    #          '.csv', 
    #          '.sas7bdat', 
    #          '.RData'))
  })
  
  #--------------------------------------  
  # load_internal_ctlModel_container
  #-------------------------------------- 
  output$load_internal_ctlModel_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$ctlModel_source=="internal library", message=FALSE)) 
    
    dirs_list = c("", list.files(path = paste0(HOME, "/ctl"), full.names = FALSE, recursive = FALSE)) 
    
    fluidRow(
      column(12, 
        selectizeInput(ns("which_internal_ctlModel"), 
                       label    = "load internal ctlModel", 
                       choices  = dirs_list, 
                       multiple = FALSE,
                       width="100%", 
                       selected = dirs_list[1])
      )
    )
  })
  
  
  #--------------------------------------  
  # load_session_ctlModel_container
  #-------------------------------------- 
  output$load_session_ctlModel_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$ctlModel_source=="within session", message=FALSE)) 
     
    name_lst <- names(ALL$ctlModel)
    only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
    dirs_list = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12, 
             selectizeInput(ns("which_session_ctlModel"), 
                            label    = "load session ctlModel", 
                            choices  = dirs_list, 
                            multiple = FALSE,
                            width ="100%", 
                            selected = dirs_list[1])
      )
    ) 
  })
   
  #--------------------------------------  
  # reactive of load_external_ctlModel
  #-------------------------------------- 
  load_external_ctlModel <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_ctlModel, message = FALSE))
    
    inFile = input$which_external_ctlModel
    # print(inFile)
    # name            size  type  datapath
    # cpp.model.cpp   6369        /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
 
    ext <- tools::file_ext(inFile$name) 
    ctlModel = readLines(inFile$datapath)  
    
    attr(ctlModel, 'file_name') <- inFile$name
    attr(ctlModel, 'locaton_source') <- "external"
    ctlModel
  })
  
  
  #--------------------------------------  
  # reactive of load_internal_ctlModel
  #-------------------------------------- 
  load_internal_ctlModel <- reactive({
     
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_internal_ctlModel, message=FALSE))
    
    ctlModel.file = paste0(HOME, "/ctl/", input$which_internal_ctlModel)
    ctlModel = readLines(ctlModel.file)   # "./ctl/control5.ctl")
    
    attr(ctlModel, 'file_name') <- ctlModel.file
    attr(ctlModel, 'locaton_source') <- "internal"
    ctlModel
  })
  
  #--------------------------------------  
  # reactive of load_session_ctlModel
  #-------------------------------------- 
  load_session_ctlModel <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_ctlModel, message=FALSE))
    
    ctlModel = ALL$ctlModel[[input$which_session_ctlModel]]
    
    attr(ctlModel, 'file_name') <- paste0(input$which_session_ctlModel, ".ctl")
    attr(ctlModel, 'locaton_source') <- "session"
    ctlModel
  }) 
   
  #--------------------------------------  
  # observeEvent  
  #-------------------------------------- 
  # https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw
  
  observeEvent({input$which_internal_ctlModel}, {
    validate(need(input$ctlModel_source=="internal library", message=FALSE) )
    ALL$ctlModel[[ctlModel_name]]  = load_internal_ctlModel()
  })
  
  
  observeEvent({input$which_external_ctlModel}, {
    validate(need(input$ctlModel_source=="external file", message=FALSE) )
    ALL$ctlModel[[ctlModel_name]]  = load_external_ctlModel()
  })
  
  
  observeEvent({input$which_session_ctlModel}, {
    validate(need(input$ctlModel_source=="within session", message=FALSE) )
    ALL$ctlModel[[ctlModel_name]]  = load_session_ctlModel()
  })
  
  return(ALL)
}
