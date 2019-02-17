 
################################################################################ 
# module_load_cppModel_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_load_cppModel_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  
    fluidRow(
      column(width=12,   
             uiOutput(ns("cppModel_source_selector")),
             uiOutput(ns("load_external_cppModel_container")),
             uiOutput(ns("load_internal_cppModel_container")), 
             uiOutput(ns("load_session_cppModel_container"))
             )
      )#,
    # fluidRow(
    #   column(width=12, 
    #          uiOutput(ns("cppModelContent_container")))
    # )
 
}

################################################################################ 
# main function: module_load_cppModel
################################################################################

module_load_cppModel <- function(input, output, session, ALL, cppModel_name="TEST") {
    
  ns <- session$ns 
  #values <- reactiveValues()
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  #--------------------------------------  
  # cppModel_source_selector
  #--------------------------------------
  output$cppModel_source_selector <- renderUI({
    
    # need to log in first
    #validate(need(globalVars$login$status, message=FALSE))
    
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
    
    fluidRow(
      column(6,
        fileInput(ns("which_external_cppModel"), label = "load external cppModel", width="100%" ) # h5
        # accept=c('text/csv/sas7bdat', 
        #          'text/comma-separated-values,text/plain', 
        #          '.xlsx',
        #          '.xls',
        #          '.csv', 
        #          '.sas7bdat', 
        #          '.RData'))
      )
    )
  })
   
  #--------------------------------------  
  # load_internal_cppModel_container
  #-------------------------------------- 
  output$load_internal_cppModel_container <- renderUI({
  
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$cppModel_source=="internal library", message=FALSE)) 
    
    dirs.list = c("", list.files(path = paste0("./model/cpp"), full.names = FALSE, recursive = FALSE)) 
    fluidRow(
      column(6,
             selectizeInput(ns("which_internal_cppModel"), 
                   label    = "load internal cppModel", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   width="100%", 
                   selected = dirs.list[1]) 
      )
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
    dirs.list = c("", setdiff(name_lst, only_for_internal_use))
    
    
    fluidRow(
      column(6,
        selectizeInput(ns("which_session_cppModel"), 
                       label    = "load session cppModel", 
                       choices  = dirs.list, 
                       multiple = FALSE,
                       width="100%", 
                       selected = dirs.list[1]) 
      )
    )
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
    
    print("reactive load_external_cppModel")
    
    cppModel <- mread(model ="Shiny", project = dirname(inFile$datapath), file=cppModel.file)     # ?mread , ?mcode
    if(input$YesNoIIV=="No") {cppModel <- cppModel %>% zero_re}
    
    attr(cppModel, 'file.name') <- inFile$name
    attr(cppModel, 'locaton.source') <- "external"
    cppModel
  })
  
   
  #--------------------------------------  
  # reactive of load_internal_cppModel
  #-------------------------------------- 
  load_internal_cppModel <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_internal_cppModel, message=FALSE))
    
    print("reactive load_internal_cppModel")
    
    cppModel.file = input$which_internal_cppModel 
 
    # try to read model after editing 
    cppModel =  tryCatch(mread(model ="Shiny", project = "./model/cpp/", file=cppModel.file),     # ?mread , ?mcode, 
                         error=function(e) {
                           print("mread cppModel not sucessful..."); 
                           return(NULL)
                         } #, finally = {
                         # eval(parse(text=txt)) %>% as.data.frame()
                         #}
    )
    if (is.function(cppModel)) {cppModel=NULL}  # avoid key words such as expand
    validate(need(cppModel, message="Load cppModel not sucefully"))
    
    print("read model sucessfully")
    
    attr(cppModel, 'file.name') <- cppModel.file
    attr(cppModel, 'locaton.source') <- "internal"
    cppModel
  })
   
  
  #--------------------------------------  
  # reactive of load_session_cppModel
  #-------------------------------------- 
  load_session_cppModel <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_cppModel, message=FALSE))
    
    print("reactive load_session_cppModel")
    
    cppModel = ALL$cppModel[[input$which_session_cppModel]]
    
    attr(cppModel, 'file.name') <- input$which_session_cppModel
    attr(cppModel, 'locaton.source') <- "session"
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
