#######################################################################
# module_build_adex_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_runsim_adex_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width=12,   
           uiOutput(ns("adex_source_selector")),
           uiOutput(ns("load_external_adex_container")),
           uiOutput(ns("load_manual_adex_container")), 
           uiOutput(ns("load_script_adex_container")), 
           uiOutput(ns("load_session_adex_container")),
           uiOutput(ns("adex_tab_container"))
    )
  )
}

########################################################################
# module_runsim_adex
########################################################################

module_runsim_adex <- function(input, output, session, ALL, values)  {
  
  ns <- session$ns 
  
  #--------------------------------------  
  # adex_source_selector
  #--------------------------------------
  output$adex_source_selector <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    fluidRow(
      column(12,  
             radioButtons(ns("adex_source"), 
                          label="Construct adex from:", 
                          choices=c("manual input", "script", "within session","external file"), 
                          inline=TRUE, 
                          width="100%",
                          selected="manual input")
      )
    )
    
  })
  
  #--------------------------------------  
  # load_manual_adex_container
  #-------------------------------------- 
  output$load_manual_adex_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="manual input", message=FALSE)) 
    
    # use manual curation
    output$adex_rHandsontab <- renderRHandsontable({
      
      adex_checkin =  values[["adex_checkin"]]  # 
      if (is.null(adex_checkin)) {return(NULL)}
      
      rhandsontable(adex_checkin, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL)
      
    })
    rHandsontableOutput(ns("adex_rHandsontab"))
    
  })
  
  
  #--------------------------------------  
  # load_script_adex_container
  #-------------------------------------- 
  output$load_script_adex_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="script", message=FALSE)) 
    
    #value = readLines(cppFile)
    value = "expand.grid(USUBJID=1:3, WGTBL=seq(60, 80, by=10)) %>% mutate(ID=1:nrow(.))"
    value = paste0(value, sep="\n")   # sep="<br/>")
    value = paste0(value, collapse="")
    #value = gsub("\n", '<br/>', value, fixed=TRUE)
    
    tagList(
      fluidRow(
        column(width=12, "You may modify the script and then re-assign a name for it.")), 
      fluidRow(
        #column(width=2, "model name"),
        column(width=4,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
               textInput(ns("script_name"), value=NULL, 
                         placeholder ="construct_adex", label=NULL, width="100%")),
        
        column(2, 
               actionButton(ns("save_script"), label="Save script", style=actionButton.style )
        )
      ),
      fluidRow(
        column(12,
               textAreaInput(ns("script_for_adex"), label=NULL, value=value, rows=10,
                             width = '785px',   #400px', or '100%'
                             placeholder= "Your script for adex.") 
        )
      )
    )
  }) 
  
  
  #--------------------------------------  
  # load_session_adex_container
  #-------------------------------------- 
  output$load_session_adex_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="within session", message=FALSE)) 
    
    name_lst <- names(ALL$DATA)
    only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
    dirs.list = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12,
             selectizeInput(ns("which_session_adex"), 
                            label    = "load session adex", 
                            choices  = dirs.list, 
                            multiple = FALSE,
                            width="100%", 
                            selected = dirs.list[1]) 
      )
    )
  }) 
  
  
  #--------------------------------------  
  # load_external_dataset_container
  #--------------------------------------  
  output$load_external_adex_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE),
             need(input$adex_source=="external file", message=FALSE)) 
    
    fluidRow(
      column(12,
             fileInput(ns("which_external_adex"), label = "load external adex", width="100%" ) # h5
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
  # adex_tab_container
  #-------------------------------------- 
  output$adex_tab_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source, message=FALSE)
    )
    
    adex <- switch(input$adex_source, 
                   "manual input" = load_manual_adex(), 
                   "script" = load_script_adex(), 
                   "within session" = load_session_adex(), 
                   "external file" = load_external_adex(), 
                   NULL)
    # 
    # adex = adex %>% mutate(
    #   TIME = time, 
    #   GROUPID = as.integer(GROUPID),
    #   CMT  = as.integer(cmt),
    #   RATE = rate, 
    #   EVID = as.integer(evid), 
    #   NDOSE= as.integer(addl+1), 
    #   cmt  = as.integer(cmt), 
    #   ii   = as.integer(ii), 
    #   addl = as.integer(addl), 
    #   evid = as.integer(evid)
    # )
    # adex = adex %>% select(STUDYID,ARMA,USUBJID,ID,GROUPID,TIME,AMT,UNIT,ROUTE,FREQ,NDOSE,CMT,RATE,WGTBL)
    # # time,cmt,rate,infhr,ii,addl,evid)
    # adex = head(adex, n=100)  # only the first 100 rows show
    # 
    values$adex = adex
    
    
    output$mydatatable <- DT::renderDataTable(                                                                                                                                          
      DT::datatable(data = adex, #load_manual_adex(),  #ifelse(is.null(values$adex),load_manual_adex(), values$adex),                                                                                                                                                       
                    options = list(pageLength = 10, 
                                   lengthChange = FALSE, width="100%", scrollX = TRUE)                                                                   
      ))
    DT::dataTableOutput(ns("mydatatable"))
  }) 
  
  
  
  #--------------------------------------  
  # reactive of load_manual_adex
  #-------------------------------------- 
  
  observeEvent(input$adex_source, {
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="manual input", message=FALSE)) 
    
    ID = 1:10
    Dosing.Regimen = rep("", times=10)
    Dosing.Regimen[1] = "3 mg/kg IV Q2W*12"
    Dosing.Regimen[2] = "350 mg IV Q3W*8"
    
    tdata = data.frame(ID, Dosing.Regimen)
    tdata = tdata %>% mutate(ID=factor(ID), 
                             Dosing.Regimen = as.character(Dosing.Regimen)
    )
    
    values[["adex_checkin"]] <- tdata   # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adex_rHandsontab, {
    values[["adex_checkin"]] = hot_to_r(input$adex_rHandsontab)  # manual alignment
  })
  
  
  load_manual_adex <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE)
    ) 
    
    adex = values[["adex_checkin"]] 
    if (is.null(adex)) {return(NULL)}
    
    adex = adex %>% filter(Dosing.Regimen!="" & !is.na(Dosing.Regimen))
    adex = parseARMA(adex$Dosing.Regimen) 
    
    colnames(adex) = toupper(colnames(adex))
    
    adex
  })
  
  
  #--------------------------------------  
  # reactive of load_script_adex
  #-------------------------------------- 
  load_script_adex <- reactive({
    
    txt = input$script_for_adex
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(txt, message=FALSE)
    )
    
    adex =  tryCatch(eval(parse(text=txt)) %>% as.data.frame(), 
                     error=function(e) {
                       print("not complete expression...."); 
                       return(NULL)
                     } #, finally = {
                     # eval(parse(text=txt)) %>% as.data.frame()
                     #}
    )
    if (is.null(adex)) {return(NULL)}    
    if (is.function(adex)) {return(NULL)}  # avoid key words such as expand
    
    adex = adex %>% as.data.frame()
    if (is.null(adex$ID)) {adex$ID=1}
    if (is.null(adex$WGTBL)) {adex$WGTBL=75} 
    
    std.col.name.lst = c("ID", "WGTBL")
    validate(
      need(all(std.col.name.lst %in% colnames(adex)), 
           message=paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adex)), collapse=", "), " in ", "adex")
      )
    )
    
    adex
  }) 
  
  
  #--------------------------------------  
  # reactive of load_session_dataset
  #-------------------------------------- 
  load_session_adex <- reactive({
    validate(need(input$which_session_adex, message=FALSE)
             )
    
    adex = ALL$DATA[[input$which_session_adex]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_adex, message=FALSE), 
             need(adex, message="no adex found")
    )
    
    attr(adex, 'file.name') <- paste0(input$which_session_adex, ".csv")  # only .csv
    attr(adex, 'locaton.source') <- "session"
    adex
  }) 
  
  #--------------------------------------  
  # reactive of load_external_adex
  #-------------------------------------- 
  load_external_adex <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_adex, message = FALSE))
    
    inFile = input$which_external_adex
    
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
  
  return(values)
}