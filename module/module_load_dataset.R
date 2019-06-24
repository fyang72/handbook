
################################################################################ 
# module_load_dataset_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_load_dataset_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(width=12,   
           uiOutput(ns("dataset_source_selector")),
           uiOutput(ns("load_external_dataset_container")),
           uiOutput(ns("load_internal_dataset_container")), 
           uiOutput(ns("load_session_dataset_container"))
    )
  ) 
  
}

################################################################################ 
# main function: module_load_dataset
################################################################################

module_load_dataset <- function(input, output, session, ALL, dataset_name="dataset_name") {
  
  ns <- session$ns 
   
  #--------------------------------------  
  # dataset_source_selector
  #--------------------------------------
  output$dataset_source_selector <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    fluidRow(
      column(12,  
             radioButtons(ns("dataset_source"), 
                          label="Select data from:", 
                          choices=c("internal library", "within session", "external file"), 
                          inline=TRUE, 
                          width="100%",
                          selected="internal library")
             )
    )
    
  })
  
  #--------------------------------------  
  # load_external_dataset_container
  #--------------------------------------  
  output$load_external_dataset_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE),
             need(input$dataset_source=="external file", message=FALSE)) 
    
    fluidRow(
      column(12,
        fileInput(ns("which_external_dataset"), label = "load external dataset", width="100%" ) # h5
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
  # load_internal_dataset_container
  #-------------------------------------- 
  output$load_internal_dataset_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$dataset_source=="internal library", message=FALSE)) 
     
    dirs.list = c("", list.files(path = paste0(HOME, "/data"), full.names = FALSE, recursive = FALSE)) 
    fluidRow(
      column(12,
             selectizeInput(ns("which_internal_dataset"), 
                   label    = "load internal dataset", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   width="100%", 
                   selected = dirs.list[1]) 
      )
    )
  })
  
  
  #--------------------------------------  
  # load_session_dataset_container
  #-------------------------------------- 
  output$load_session_dataset_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$dataset_source=="within session", message=FALSE)) 
    
    name_lst <- names(ALL$DATA)
    only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
    dirs.list = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12,
        selectizeInput(ns("which_session_dataset"), 
                       label    = "load session dataset", 
                       choices  = dirs.list, 
                       multiple = FALSE,
                       width="100%", 
                       selected = dirs.list[1]) 
      )
    )
  }) 
  
  #--------------------------------------  
  # reactive of load_external_dataset
  #-------------------------------------- 
  load_external_dataset <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_dataset, message = FALSE))
     
    inFile = input$which_external_dataset
    tdata = read_datafile(inFile)
    # print(inFile)
    # name            size type  datapath
    # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
    # 
    # ext <- tools::file_ext(inFile$name) 
    # tdata = switch(ext,
    #               "csv" = read_csv(inFile$datapath, col_names=TRUE,  
    #                                col_type=cols(.default=col_character()))  %>% as.data.frame(),
    #               "xlsx"=read_excel(inFile$datapath, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
    #               "xls" = read_excel(inFile$datapath)  %>% as.data.frame(),
    #               "sas7bdat" =  read_sas(inFile$datapath)  %>% as.data.frame(), 
    #               "RData" =  load(inFile$datapath), 
    #               NULL
    # )

    
    message.info = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
    if (is.null(tdata)) {print(message.info)}
    validate(need(tdata, message.info)) 
       
    attr(tdata, 'file_name') <- inFile$name   # only file name, no directory for external file
    attr(tdata, 'locaton_source') <- "external"
    tdata
  })
  
  
  #--------------------------------------  
  # reactive of load_internal_dataset
  #-------------------------------------- 
  load_internal_dataset <- reactive({
     
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_internal_dataset, message=FALSE))
     
    inFile = paste0(HOME, "/data/", input$which_internal_dataset)
    ext <- tools::file_ext(inFile) 
     
    tdata = switch(ext,
                  "csv" = read_csv(inFile, col_names=TRUE,  
                                   col_type=cols(.default=col_character()))  %>% as.data.frame(),
                  "xlsx"=read_excel(inFile, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
                  "xls" = read_excel(inFile)  %>% as.data.frame(),
                  "sas7bdat" =  read_sas(inFile)  %>% as.data.frame(), 
                  "RData" =  load(inFile),   # MUST NAMED AS "adpx"   need some work 
                  NULL
    )
    message.info = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
    if (is.null(tdata)) {print(message.info)}
    validate(need(tdata, message.info)) 
    
    attr(tdata, 'file_name') <- inFile  # with directory
    attr(tdata, 'locaton_source') <- "internal"
    tdata
    
  })
  
  
  #--------------------------------------  
  # reactive of load_session_dataset
  #-------------------------------------- 
  load_session_dataset <- reactive({
    
    dataset = ALL$DATA[[input$which_session_dataset]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_dataset, message=FALSE), 
             need(dataset, message="no data found")
    )
     
    attr(dataset, 'file_name') <- paste0(input$which_session_dataset, ".csv")  # only .csv
    attr(dataset, 'locaton_source') <- "session"
    dataset
  }) 
  
  #--------------------------------------  
  # observeEvent  
  #--------------------------------------   
  observeEvent({input$which_internal_dataset}, {
    validate(need(input$dataset_source=="internal library", message=FALSE) )
    ALL$DATA[[dataset_name]]  = load_internal_dataset()
  })
  
  
  observeEvent({input$which_external_dataset}, {
    validate(need(input$dataset_source=="external file", message=FALSE) )
    ALL$DATA[[dataset_name]]  = load_external_dataset()
  })
  
  
  observeEvent({input$which_session_dataset}, {
    validate(need(input$dataset_source=="within session", message=FALSE) )
    ALL$DATA[[dataset_name]]  = load_session_dataset()
  })
   
  return(ALL)
}
