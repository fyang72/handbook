#######################################################################
# module_build_adsl_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_runsim_adsl_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(width=12,   
           uiOutput(ns("adsl_source_selector")),
           uiOutput(ns("load_external_adsl_container")),
           uiOutput(ns("load_manual_adsl_container")), 
           uiOutput(ns("load_script_adsl_container")), 
           uiOutput(ns("load_session_adsl_container")),
           uiOutput(ns("adsl_tab_container"))
    )
  )
}

########################################################################
# module_runsim_adsl
########################################################################

module_runsim_adsl <- function(input, output, session, ALL, values)  {
  
  ns <- session$ns 
  
  #--------------------------------------  
  # adsl_source_selector
  #--------------------------------------
  output$adsl_source_selector <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    fluidRow(
      column(12,  
             radioButtons(ns("adsl_source"), 
                          label="Construct adsl from:", 
                          choices=c("manual input", "script", "within session","external file"), 
                          inline=TRUE, 
                          width="100%",
                          selected="manual input")
      )
    )
    
  })

  #--------------------------------------  
  # load_manual_adsl_container
  #-------------------------------------- 
  output$load_manual_adsl_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adsl_source=="manual input", message=FALSE)) 
    
    tagList(
      numericInput(ns("n_subject"), label = h5("Number of subjects"),
                   value=1, min=1, max=2000,step = 1), 
      
      textInput(ns("pop_WT"), label = h5("Body weight in population"),
                value="75")
    )
    
  })
  
  
  #--------------------------------------  
  # load_script_adsl_container
  #-------------------------------------- 
  output$load_script_adsl_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adsl_source=="script", message=FALSE)) 
       
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
                           placeholder ="construct_adsl", label=NULL, width="100%")),
     
          column(2, 
                 actionButton(ns("save_script"), label="Save script", style=actionButton.style )
          )
        ),
        fluidRow(
          column(12,
                 textAreaInput(ns("script_for_adsl"), label=NULL, value=value, rows=10,
                               width = '785px',   #400px', or '100%'
                               placeholder= "Your script for adsl.") 
          )
        )
      )
  }) 
   
  
  #--------------------------------------  
  # load_session_adsl_container
  #-------------------------------------- 
  output$load_session_adsl_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adsl_source=="within session", message=FALSE)) 
    
    name_lst <- names(ALL$DATA)
    only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
    dirs.list = c("", setdiff(name_lst, only_for_internal_use))
    
    fluidRow(
      column(12,
             selectizeInput(ns("which_session_adsl"), 
                            label    = "load session adsl", 
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
  output$load_external_adsl_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE),
             need(input$adsl_source=="external file", message=FALSE)) 
    
    fluidRow(
      column(12,
             fileInput(ns("which_external_adsl"), label = "load external adsl", width="100%" ) # h5
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
  # adsl_tab_container
  #-------------------------------------- 
  output$adsl_tab_container <- renderUI({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$adsl_source, message=FALSE)
    )
    
    adsl <- switch(input$adsl_source, 
                   "manual input" = load_manual_adsl(), 
                   "script" = load_script_adsl(), 
                   "within session" = load_session_adsl(), 
                   "external file" = load_external_adsl(), 
                     NULL)
    values$adsl = adsl
           
    output$mydatatable <- DT::renderDataTable(                                                                                                                                          
      DT::datatable(data = adsl, #load_manual_adsl(),  #ifelse(is.null(values$adsl),load_manual_adsl(), values$adsl),                                                                                                                                                       
                    options = list(pageLength = 10, 
                                   lengthChange = FALSE, width="100%", scrollX = TRUE)                                                                   
      ))
    DT::dataTableOutput(ns("mydatatable"))
  }) 
  
  
  
  #--------------------------------------  
  # reactive of load_manual_adsl
  #-------------------------------------- 
  load_manual_adsl <- reactive({
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$n_subject, message=FALSE), 
             need(input$pop_WT, message=FALSE)
    ) 
     
    adsl = data.frame(ID=1:input$n_subject, 
                      WGTBL = rep(input$pop_WT, time=input$n_subject)
                      )
    adsl
  })
  
  
  #--------------------------------------  
  # reactive of load_script_adsl
  #-------------------------------------- 
  load_script_adsl <- reactive({
    
    txt = input$script_for_adsl
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(txt, message=FALSE)
    )
     
    adsl =  tryCatch(eval(parse(text=txt)) %>% as.data.frame(), 
                     error=function(e) {
                       print("not complete expression...."); 
                       return(NULL)
                     } #, finally = {
                     # eval(parse(text=txt)) %>% as.data.frame()
                     #}
    )
    if (is.null(adsl)) {return(NULL)}    
    if (is.function(adsl)) {return(NULL)}  # avoid key words such as expand
    
    adsl = adsl %>% as.data.frame()
    if (is.null(adsl$ID)) {adsl$ID=1}
    if (is.null(adsl$WGTBL)) {adsl$WGTBL=75} 
    
    std.col.name.lst = c("ID", "WGTBL")
    validate(
      need(all(std.col.name.lst %in% colnames(adsl)), 
           message=paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adsl)), collapse=", "), " in ", "adsl")
      )
    )
    
    adsl
  }) 
  
  
  #--------------------------------------  
  # reactive of load_session_dataset
  #-------------------------------------- 
  load_session_adsl <- reactive({
    validate(need(input$which_session_adsl, message=FALSE)
    )
    adsl = ALL$DATA[[input$which_session_adsl]]
    
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_session_adsl, message=FALSE), 
             need(adsl, message="no adsl found")
    )
    
    attr(adsl, 'file.name') <- paste0(input$which_session_adsl, ".csv")  # only .csv
    attr(adsl, 'locaton.source') <- "session"
    adsl
  }) 
  
  #--------------------------------------  
  # reactive of load_external_adsl
  #-------------------------------------- 
  load_external_adsl <- reactive({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$which_external_adsl, message = FALSE))
    
    inFile = input$which_external_adsl
    
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