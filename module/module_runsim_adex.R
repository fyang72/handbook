#######################################################################
# module_runsim_adex_UI
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
           
           fluidRow(column(width=12,uiOutput(ns("load_manual_adex_container")))), 
           fluidRow(column(width=12,uiOutput(ns("load_script_adex_container")))), 
           fluidRow(column(width=6,uiOutput(ns("load_internal_adex_container")))), 
           fluidRow(column(width=6,uiOutput(ns("load_session_adex_container")))),
           fluidRow(column(width=6,uiOutput(ns("load_external_adex_container")))),
           
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
                          choices=c("manual input", 
                                    "script", 
                                    "internal library", 
                                    "within session",
                                    "external file"), 
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
      rhandsontable(adex_checkin, useTypes = TRUE, stretchH = "all", rowHeaders=NULL)
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
    value = 
"
adex <- data.frame(
  ID=1:3, 
  TIME=0, 
  AMT=seq(10,30,by=10),
  WGTBL=seq(60, 80, by=10)
)
"
    value = paste0(value, collapse="\n")
    
    tagList(
      fluidRow(
        column(width=12, "You may modify the script and then re-assign a name for it.")), 
      fluidRow(
        column(width=6,   #status = "primary",  #class = 'rightAlign', #background ="aqua",
               textInput(ns("script_name"), value=NULL, 
                         placeholder ="script_name_for_adex", label=NULL, width="100%")),
        
        column(2, 
               actionButton(ns("save_script"), label="Save script", style=actionButton_style )
        )
      ),
      fluidRow(
        column(12,
               aceEditor(ns("script_for_adex"), 
                         mode="r", 
                         value=value, 
                         theme = "crimson_editor",   # chrome
                         autoComplete = "enabled",
                         height = "200px", 
                         fontSize = 15 
               )
        )
      )
    )  # tagList
  }) 
  
  
  #--------------------------------------  
  # load_internal_adex_container
  #-------------------------------------- 
  output$load_internal_adex_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="internal library", message=FALSE)) 
    
    dirs_lst=list.files(path = paste0(HOME, "/data/"), 
                         full.names = FALSE, 
                         recursive = FALSE, 
                         #pattern=".csv", 
                         include.dirs=FALSE)  
    dirs_lst = c("", dirs_lst) 
    
    selectizeInput(ns("which_internal_adex"), 
                   label    = "load internal adex", 
                   choices  = dirs_lst, 
                   multiple = FALSE,
                   width = "100%", 
                   selected = dirs_lst[1]
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
    dirs_lst = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12,
             selectizeInput(ns("which_session_adex"), 
                            label    = "load session adex", 
                            choices  = dirs_lst, 
                            multiple = FALSE,
                            width = "100%", 
                            selected = dirs_lst[1]) 
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
             need(adex(), message=FALSE)
    )

    ALL = callModule(module_save_data, "adex_table", 
                     ALL,
                     data = adex(),   
                     data_name = "adex"
    )
    
    module_save_data_UI(ns("adex_table"), label = NULL) 
    
  }) 
  
  #--------------------------------------  
  # reactive of load_manual_adex
  #-------------------------------------- 
  
  observeEvent(input$adex_source, {
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adex_source=="manual input", message=FALSE)) 
    
    ndose = 6
    ID = 1:ndose
    Dosing.Regimen = rep("", times=ndose)
    Dosing.Regimen[1] = "3 mg/kg IV Q2W*12"
    Dosing.Regimen[2] = "350 mg IV Q3W*8"
    
    tdata = data.frame(ID=1:ndose, Dosing.Regimen=NA)
    tdata[1, "Dosing.Regimen"] = "3 mg/kg IV Q2W*12"
    tdata[2, "Dosing.Regimen"] = "350 mg IV Q3W*8"
    
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
     
    adex = values[["adex_checkin"]]  
    validate(need(globalVars$login$status, message=FALSE), 
             need(adex, message=FALSE)
    )
    
    adex = adex %>% filter(Dosing.Regimen!="" & !is.na(Dosing.Regimen))
    adex = parseARMA(adex$Dosing.Regimen) 
    
    colnames(adex) = toupper(colnames(adex))
    
    adex
  })
  
  
  #--------------------------------------  
  # reactive of load_script_adex
  #-------------------------------------- 
  load_script_adex <- reactive({
     
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$script_for_adex, message=FALSE)
    ) 
    
    #environment(try_eval) <- environment()
    env = try_eval(text=input$script_for_adex)
    
    adex=NULL; error_message=NULL
    if ("adex" %in% ls(env)) {adex = get("adex", env)}
    if ("message" %in% ls(env)) {error_message = get("message", env)}
    
    if (length(error_message)==0 & !is.null(adex)) {
      adex = adex
    }else{
      adex = NULL
      showNotification(paste0(error_message, collapse="\n"), type="error")
    }
    
    adex = adex %>% as.data.frame()
    if (is.null(adex$ID)) {adex$ID=1}
    if (is.null(adex$WGTBL)) {adex$WGTBL=75} 
    
    std.col.name.lst = c("ID", "WGTBL")
    validate(
      need(all(std.col.name.lst %in% colnames(adex)), 
           message=paste0("Missing columns of ", 
                          paste0(setdiff(std.col.name.lst, colnames(adex)), collapse=", "), " in ", "adex")
      )
    )
    
    adex
  }) 
  
  
  #--------------------------------------  
  # reactive of load_internal_adex
  #-------------------------------------- 
  load_internal_adex <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_internal_adex, message=FALSE))
    
    inFile = paste0(HOME, "/data/", input$which_internal_adex)
    tdata = read_datafile(inFile)

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
  load_session_adex <- reactive({
    validate(need(input$which_session_adex, message=FALSE)
             )
    
    adex = ALL$DATA[[input$which_session_adex]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_adex, message=FALSE), 
             need(adex, message="no adex found")
    )
    
    attr(adex, 'file_name') <- paste0(input$which_session_adex, ".csv")  # only .csv
    attr(adex, 'locaton_source') <- "session"
    adex
  }) 
  
  #--------------------------------------  
  # reactive of load_external_adex
  #-------------------------------------- 
  load_external_adex <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_adex, message = FALSE))
    
    inFile = input$which_external_adex
    tdata = read_datafile(inFile$datapath)
    # print(inFile)
    # name            size type  datapath
    # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
    # 
    # ext <- tools::file_ext(inFile$name) 
    # tdata = switch(ext,
    #                "csv" = read_csv(inFile$datapath, col_names=TRUE,  
    #                                 col_type=cols(.default=col_character()))  %>% as.data.frame(),
    #                "xlsx"=read_excel(inFile$datapath, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
    #                "xls" = read_excel(inFile$datapath)  %>% as.data.frame(),
    #                "sas7bdat" =  read_sas(inFile$datapath)  %>% as.data.frame(), 
    #                "RData" =  load(inFile$datapath), 
    #                NULL
    # )
    
    
    message.info = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
    if (is.null(tdata)) {print(message.info)}
    validate(need(tdata, message.info)) 
    
    attr(tdata, 'file_name') <- inFile$name   # only file name, no directory for external file
    attr(tdata, 'locaton_source') <- "external"
    tdata
  })
  
  #--------------------------------------  
  # reactive of adex()
  #--------------------------------------
  adex <- reactive({
    validate(need(input$adex_source, message=FALSE))
    
    adex <- switch(input$adex_source, 
                   "manual input" = load_manual_adex(), 
                   "script" = load_script_adex(), 
                   "internal library" = load_internal_adex(), 
                   "within session" = load_session_adex(), 
                   "external file" = load_external_adex(), 
                   NULL) %>% capitalize_names()
    
    # must have USUBJID and WGTBL
    col_name_lst = c("ID", "TIME", "AMT")
    all_yes = all(col_name_lst %in% colnames(adex))
    if(all_yes==FALSE) {
      error_message = paste0("Missing column(s) of ", paste0(setdiff(col_name_lst, colnames(adex)), collapse=", "), " in ", "adex")
      showNotification(paste0(error_message, collapse="\n"), type="error")
      adex = NULL
    }
    
    values$adex = adex
  })
  
  return(values)
}