
################################################################################ 
# module_load_data_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_load_data_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(width=12,   
           uiOutput(ns("data_source_selector")),
           uiOutput(ns("load_external_data_container")),
           uiOutput(ns("load_internal_data_container")), 
           uiOutput(ns("load_session_data_container"))
    )
  ) 
  
}

################################################################################ 
# main function: module_load_data
################################################################################

module_load_data <- function(input, output, session, ALL, data_name="data_name") {
  
  ns <- session$ns 

  #--------------------------------------  
  # data_source_selector
  #--------------------------------------
  output$data_source_selector <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    fluidRow(
      column(12,  
             radioButtons(ns("data_source"), 
                          label="Select data from:", 
                          choices=c("internal library", "within session", "external file"), 
                          inline=TRUE, 
                          width="100%",
                          selected="internal library")
             )
    )
    
  })
  
  #--------------------------------------  
  # load_external_data_container
  #--------------------------------------  
  output$load_external_data_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE),
             need(input$data_source=="external file", message=FALSE)) 
    
    fluidRow(
      column(12,
        fileInput(ns("which_external_data"), label = "load external data", width="100%" ) # h5
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
  # load_internal_data_container
  #-------------------------------------- 
  output$load_internal_data_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$data_source=="internal library", message=FALSE)) 
     
    dirs.list = c("", list.files(path = paste0("./data"), full.names = FALSE, recursive = FALSE)) 
    fluidRow(
      column(12,
             selectizeInput(ns("which_internal_data"), 
                   label    = "load internal data", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   width="100%", 
                   selected = dirs.list[1]) 
      )
    )
  })
  
  
  #--------------------------------------  
  # load_session_data_container
  #-------------------------------------- 
  output$load_session_data_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$data_source=="within session", message=FALSE)) 
    
    name_lst <- names(ALL$DATA)
    only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
    dirs.list = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12,
        selectizeInput(ns("which_session_data"), 
                       label    = "load session data", 
                       choices  = dirs.list, 
                       multiple = FALSE,
                       width="100%", 
                       selected = dirs.list[1]) 
      )
    )
  }) 
  
  #--------------------------------------  
  # reactive of load_external_data
  #-------------------------------------- 
  load_external_data <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_data, message = FALSE))
     
    inFile = input$which_external_data
    
    # print(inFile)
    # name            size type  datapath
    # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
    # 
    ext <- tools::file_ext(inFile$name) 
    tdata = switch(ext,
                  "csv" = read_csv(inFile$datapath, col_names=TRUE,  
                                   col_type=cols(.default=col_character()))  %>% as.data.frame(),
                  "xlsx"=read_excel(inFile$datapath, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
                  "xls" = read_excel(inFile$datapath)  %>% as.data.frame(),
                  "sas7bdat" =  read_sas(inFile$datapath)  %>% as.data.frame(), 
                  "RData" =  load(inFile$datapath), 
                  NULL
    )

    
    message.info = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
    if (is.null(tdata)) {print(message.info)}
    validate(need(tdata, message.info)) 
       
    attr(tdata, 'file.name') <- inFile$name   # only file name, no directory for external file
    attr(tdata, 'locaton.source') <- "external"
    tdata
  })
  
  
  #--------------------------------------  
  # reactive of load_internal_data
  #-------------------------------------- 
  load_internal_data <- reactive({
     
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_internal_data, message=FALSE))
     
    inFile = paste0("./data/", input$which_internal_data)
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
    
    attr(tdata, 'file.name') <- inFile  # with directory
    attr(tdata, 'locaton.source') <- "internal"
    tdata
    
  })
  
  
  #--------------------------------------  
  # reactive of load_session_data
  #-------------------------------------- 
  load_session_data <- reactive({
    
    data = ALL$DATA[[input$which_session_data]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_data, message=FALSE), 
             need(data, message="no data found")
    )
     
    attr(data, 'file.name') <- paste0(input$which_session_data, ".csv")  # only .csv
    attr(data, 'locaton.source') <- "session"
    data
  }) 
  
  #--------------------------------------  
  # observeEvent  
  #-------------------------------------- 
  # https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw
 
  observeEvent({input$which_internal_data}, {
    validate(need(input$data_source=="internal library", message=FALSE) )
    ALL$DATA[[data_name]]  = load_internal_data()
  })
  
  
  observeEvent({input$which_external_data}, {
    validate(need(input$data_source=="external file", message=FALSE) )
    ALL$DATA[[data_name]]  = load_external_data()
  })
  
  
  observeEvent({input$which_session_data}, {
    validate(need(input$data_source=="within session", message=FALSE) )
    ALL$DATA[[data_name]]  = load_session_data()
  })
  
  
  
  # observeEvent({
  #   #input$data_source
  #   input$which_external_data
  #   input$which_internal_data}, {
  #      
  #     validate(need(globalVars$login$status, message=FALSE), 
  #              need(input$data_source, message=FALSE) 
  #     )
  #     
  #     tdata = switch(input$data_source, 
  #                       "internal library"= load_internal_data(), 
  #                       "external file" = load_external_data() 
  #     ) 
  #     
  #     ALL$DATA[[data_name]]  = tdata
  #   })
  
  
  return(ALL)
}
