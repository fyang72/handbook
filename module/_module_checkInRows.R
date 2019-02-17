
################################################################################ 
################################################################################
# checkInRowsUI
################################################################################ 
################################################################################

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI 
#-----------------------------------------

checkInRowsUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12,   title="Parse/derive column", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status =  "primary" ,
                 column(width=4,
                        uiOutput(ns("data_selector"))
                 ), 
                 column(width=4
                        #actionButton(ns("runScript"),label = "runScript", style=actionButton.style )   # http://getbootstrap.com/docs/4.0/components/buttons/
                 ),
                 column(width=4,
                        actionButton(ns("checkIn"),label = "checkIn", style=actionButton.style)   # http://getbootstrap.com/docs/4.0/components/buttons/
                 ),
                 
                 # column(width=12,
                 #        uiOutput(ns("col_selector"))
                 # ),
               
                 tabBox(width=12, id = ns("rowParser"), title =NULL, # "Loading Dose",
                         
                        tabPanel(width=12, title="Auto", value = "Auto", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                                  
                                 uiOutput(ns("colAuto_selector")),
                                 
                                 fluidRow( 
                                   column(width=6, uiOutput(ns("cycleInput_container"))),
                                   column(width=6, uiOutput(ns("infhrInput_container")))
                                   ),
                                 
                                 uiOutput(ns("rHandsontabAuto_container"))
                                 
                        ), 
                        
                        tabPanel(width=12, title="Manual", value = "Manual", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE,
                                 
                                 uiOutput(ns("col_selector")),
                                 uiOutput(ns("rHandsontab_container"))
                                 
                        ), 
                        
                        tabPanel(width=12, title="Script", value = "Script", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                                 
                                 #actionButton(ns("runScript"),label = "runScript", style=actionButton.style ) ,
                                 uiOutput(ns("scriptArea_container"))
                        )
                 )
                 
                 
                 
                 
                 
             ) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Aligned data", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,  
                 
                 uiOutput(ns("warnmsg_container")),
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        textInput(ns("data_name4Save"), value="alignedData-", label=NULL,  
                                  placeholder = "Enter data name here:")),
                 
                 column(width=4, #align="left", offset = 0,
                        actionButton(ns("saveData"), label="Save it", style=actionButton.style)), 
                 
                 uiOutput(ns("adpx_container"))
             )
           )
    )
  )
  
}
 

################################################################################ 
################################################################################
# checkInRows
################################################################################ 
################################################################################


checkInRows <- function(input, output, session, DATA) {
  
  ns <- session$ns
  values <- reactiveValues()
   
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # renderUI
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  #----------------  
  # data_selector
  #----------------
  output$data_selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("", unique(names(DATA$mDATA)))
    selectizeInput(ns("data_name4Select"), 
                   label    = NULL, #"Which dataset:" , 
                   choices  = data.lst,   
                   multiple = FALSE, 
                   selected = data.lst[1])    
  })
  
  #----------------
  # col_selector
  #----------------
  output$col_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    #if (mod(input$runScript,2)) {return(NULL)}
    
    col.lst = c("", colnames(data))
    fluidRow( 
        column(width=6,
               selectizeInput(ns("col_name"), 
                       label    = "Parse column:" , 
                       choices  = col.lst,   
                       multiple = FALSE, 
                       selected = col.lst[1])),
        column(width=6,
               textInput(ns("col_name2"), label="To column:", value=NULL, placeholder = "NTIM"))
   )
  })
  
  
  #-----------------------------------------
  # display the adpx_rHandsontab
  #-----------------------------------------
  output$rHandsontab_container <- renderUI({ 
    #if (mod(input$runScript,2)) {return(NULL)}
    
    validate(need(inputData(), message=FALSE), 
             need(input$col_name, message=FALSE), 
             need(input$col_name2, message=FALSE)
    )
    
    # use manual curation
    output$adpx_rHandsontab <- renderRHandsontable({
      
      adpx_checkin =  values[["adpx_checkin"]]  # 
      if (is.null(adpx_checkin)) {return(NULL)}
      
      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all", rowHeaders=NULL)
      
    })
    rHandsontableOutput(ns("adpx_rHandsontab"))
    
  })
  
  
  #-----------------------------------------
  ## after align, display the aligned data
  #-----------------------------------------
  output$adpx_container <- renderUI({
    output$adpxTab <- DT::renderDataTable(
      DT::datatable(data = values[["adpx"]],
                    options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adpxTab"))
    
  })
  
  #------------------------- 
  # scriptArea_container
  #------------------------- 
  output$scriptArea_container <-renderUI({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    textAreaInput(ns("scriptArea"), label=NULL, value=NULL, rows=25, placeholder = "data %>% separate(timept)")
    
  })
  
  
  
  
  
  
  
  #----------------
  # col_selector
  #----------------
  output$colAuto_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    #if (mod(input$runScript,2)) {return(NULL)}
    
    col.lst = c("", colnames(data))
    fluidRow( 
      column(width=6,
             selectizeInput(ns("colAuto_name"), 
                            label    = "Parse column:" , 
                            choices  = col.lst,   
                            multiple = FALSE, 
                            selected = col.lst[1])),
      
      column(width=6,
             selectizeInput(ns("colParserAuto"), label="Select parser:", 
                            choices=c("TIMEPT-parser", "ARMA-parser", "TIME-parse"), 
                            multiple = FALSE,
                            selected ="TIMEPT-parser"))
    )
  })
  
  output$cycleInput_container <- renderUI({
    #data = values[["adpx_checkin_auto"]]
    #if (is.null(data)) {return(NULL)}
    #if (!"CYCLE" %in% colnames(data)) {return(NULL)}
    
    #if (is.null(input$DAY.IN.CYCLE)) {
      numericInput(ns("DAY.IN.CYCLE"), label="# Days in a cycle", value=56)
    #}
  })
  
  output$infhrInput_container <- renderUI({
    #data = values[["adpx_checkin_auto"]]
    #if (is.null(data)) {return(NULL)}
    #if (length(which(c("PRE", "POST") %in% colnames(data)))==0) {return(NULL)}
    
    #if (is.null(input$DAY.IN.CYCLE)) {
      numericInput(ns("INFHR"), label="Infusion hour", value=1)
    #}
  })
  
    
    
  
  
  
  #-----------------------------------------
  # display the adpx_rHandsontab
  #-----------------------------------------
  output$rHandsontabAuto_container <- renderUI({ 
    #if (mod(input$runScript,2)) {return(NULL)}
    
    validate(need(inputData(), message=FALSE), 
             need(input$colAuto_name, message=FALSE), 
             need(input$colParserAuto, message=FALSE)
    )
    
    # use manual curation
    output$adpx_rHandsontab_auto <- renderRHandsontable({
      
      adpx_checkin =  values[["adpx_checkin_auto"]]  # 
      if (is.null(adpx_checkin)) {return(NULL)}
      
      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all", rowHeaders=NULL)
      
    })
    rHandsontableOutput(ns("adpx_rHandsontab_auto"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #---------------- 
  # error message
  #--------------- 
  output$warnmsg_container <- renderUI({
    messageTab = errstatus()  
    validate(need(messageTab, message=FALSE))   
    
    #renderDataTable
    output$warnmsg <- DT::renderDataTable({
      # https://rstudio.github.io/DT/010-style.html
      # https://stackoverflow.com/questions/42569302/format-color-of-shiny-datatable-dt-according-to-values-in-a-different-dataset
      datatable(messageTab)%>%formatStyle(1:2,  #c("type","value"),
                                          #target = 'row',
                                          valueColumns = "type",
                                          backgroundColor=styleEqual(c("warning","error" ),  c("yellow","red")))    
    })
    
    
    box(width=12, title="Warning", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "warning" , 
        DT::dataTableOutput(ns('warnmsg'))
    )
    
  }) 
  
  
  
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # reactive and events
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  
  #-----------------------------------------
  #  inputData
  #-----------------------------------------
  inputData <- reactive({
    
    validate(need(input$data_name4Select, message=FALSE), 
             need(DATA$mDATA, message=FALSE)
    )
    
    # show adpx at the first place 
    data = DATA$mDATA[[input$data_name4Select]]  
    values[["adpx"]] = data  # default
    
    data
  })
  
  
  #-------------------------------------------------------
  # in default, use data_name4Select as data_name4Save
  #-------------------------------------------------------
  observeEvent(inputData(), {
    validate(need(input$data_name4Select, message=FALSE))
    
    updateTextInput(session, "data_name4Save", value=input$data_name4Select)
  })
  
  
  #-----------------------------------------
  #  adpx_checkin and display
  #-----------------------------------------
  default_adpx_checkinTab <- reactive({
    
    data = inputData()
    validate(need(inputData(), message=FALSE), 
             need(input$col_name, message=FALSE), 
             need(input$col_name2, message=FALSE)
    )
              
    # default adpx_checkin
    data = data %>% distinct_(input$col_name, .keep_all=TRUE)
    
    TIMEPT = unique(data[[input$col_name]]) # PCTPT
    if (is.null(TIMEPT)) {return(NULL)}  # you need this, in case inputData changes.
    
    NTIM=0; if (input$col_name2 %in% colnames(data)) {NTIM = data[[input$col_name2]]}
    adpx_checkin = data.frame(TIMEPT=TIMEPT, NTIM=NTIM)
    
    # replace dummy TIMEPT and NTIM
    colnames(adpx_checkin) = gsub("TIMEPT", input$col_name, colnames(adpx_checkin), fix=TRUE)
    colnames(adpx_checkin) = gsub("NTIM", input$col_name2, colnames(adpx_checkin),  fix=TRUE)
      
    # factor and character
    adpx_checkin[, input$col_name] = factor(adpx_checkin[, input$col_name], levels=TIMEPT)
    adpx_checkin[, input$col_name2] = as.character( adpx_checkin[, input$col_name2])
      
    adpx_checkin
    
  })
  
  # update adpx_checkin based on new InputData
  adpx_checkinTab <- reactive({
    adpx_checkin = default_adpx_checkinTab()
    if (is.null(adpx_checkin)) {return(NULL)}
    
    # need pre-processing? or let the user manually do it?
    
    adpx_checkin  
    
  })
  
  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(adpx_checkinTab(), {
    values[["adpx_checkin"]] <- adpx_checkinTab() # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adpx_rHandsontab, {
    values[["adpx_checkin"]] = hot_to_r(input$adpx_rHandsontab)  # manual alignment
  })
  
  
  #-------------------------------
  # if runScript
  #-------------------------------
  rowByManual <- reactive({
    #if (mod(input$runScript,2)) {return(NULL)}
    
    adpx_checkin <- values[["adpx_checkin"]]
    adpx <- values[["adpx"]]  # inputData()
    
    #
    validate(need(adpx, message=FALSE),
             need(adpx_checkin, message=FALSE), 
             need(input$col_name,message=FALSE),
             need(input$col_name2,message=FALSE), 
             need(input$data_name4Select, message=FALSE)
    )
    
    # make the columns compatible 
    adpx = adpx %>% as.data.frame()  # must be data.frame
    adpx[, input$col_name] = as.character(adpx[, input$col_name] )
    adpx_checkin[, input$col_name] = as.character(adpx_checkin[, input$col_name] )
    
    # left_join
    if (input$col_name2 %in% colnames(adpx)) {adpx <- adpx %>% select(-one_of(input$col_name2)) }
    adpx = adpx %>% left_join(adpx_checkin, by=input$col_name)
    
    return(adpx)
    })
    
  
  
  #-------------------------------
  # if by auto
  #-------------------------------
  
  observeEvent({input$colAuto_name
    input$colParserAuto
    input$DAY.IN.CYCLE
    input$INFHR
  }, {
    
    #observe({
    validate(need(inputData(), message=FALSE), 
             need(input$colAuto_name, message=FALSE), 
             need(input$colParserAuto, message=FALSE)
    )
    
    data = inputData()
    
    if (input$colParserAuto=="TIMEPT-parser") {
      TIMEPT = data %>% pull(input$colAuto_name) %>% as.character() %>% unique()
      
      DAY.IN.CYCLE = ifelse(is.null(input$DAY.IN.CYCLE), 0,  input$DAY.IN.CYCLE)
      INFHR = ifelse(is.null(input$INFHR), 0,  input$INFHR)
      
      TIMEPT = parseTIMEPT(TIMEPT,  DAY.IN.CYCLE,  INFHR)
      colnames(TIMEPT)[1] = input$colAuto_name
      
      TIMEPT = TIMEPT %>% arrange(NTIM)
      
      values[["adpx_checkin_auto"]] = TIMEPT  }
    
    
    if (input$colParserAuto=="ARMA-parser") {
      ARMA = data %>% pull(input$colAuto_name) %>% as.character() %>% unique()
      ARMA = parseARMA0(ARMA)
      colnames(ARMA)[1] = input$colAuto_name
      
      
      values[["adpx_checkin_auto"]] = ARMA  }
    
    
    
  })
  
  
  rowByAuto<- reactive({
    #if (mod(input$runScript,2)) {return(NULL)}
    
    adpx_checkin <- values[["adpx_checkin_auto"]]
    adpx <- values[["adpx"]]  # inputData()
    
    #
    validate(need(adpx, message=FALSE),
             need(adpx_checkin, message=FALSE), 
             need(input$colAuto_name,message=FALSE),
             need(input$colParserAuto,message=FALSE), 
             need(input$data_name4Select, message=FALSE)
    )
    
    # make the columns compatible 
    adpx = adpx %>% as.data.frame()  # must be data.frame
    adpx_checkin = adpx_checkin %>% as.data.frame()  # must be data.frame
    adpx[, input$colAuto_name] = as.character(adpx[, input$colAuto_name] )
    adpx_checkin[, input$colAuto_name] = as.character(adpx_checkin[, input$colAuto_name] )
    
    # left_join
    col.lst = setdiff(intersect(colnames(adpx), colnames(adpx_checkin)), input$colAuto_name)
    #col.lst = setdiff(colnames(adpx), input$colAuto_name)
    #if (length(col.lst)>0) {adpx <- adpx %>% select(-one_of(col.lst)) }
    if (length(col.lst)>0) {adpx_checkin <- adpx_checkin %>% select(-one_of(col.lst)) }
    
    adpx = adpx %>% left_join(adpx_checkin, by=input$colAuto_name)
    
    return(adpx)
  })
  
  
  
  
  
  
  #-------------------------------
  # if runScript
  #-------------------------------
  rowByScript <- reactive({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    
    data <- values[["adpx"]]
    txt = input$scriptArea

    
    #validate
    validate(need(data, message=FALSE), 
             need(txt, message=FALSE), 
             need(input$data_name4Select, message=FALSE)
    )
     
    writeLines(txt, "text.R")
    tryCatch(#source("text.R", local=TRUE),    #eval(parse(text=txt ))  , 
             sys.source("text.R", envir=environment()),
                       error=function(e) {
                         print("not complete expression in scriptArea..."); 
                         return(NULL)
                       } #, finally = {
                       # eval(parse(text=txt)) %>% as.data.frame()
                       #}
    )
 
    return(data)
  })
  
  
  #------------------------------------------------- 
  # check in adpx based on adpx_checkin or script
  #------------------------------------------------- 
 
  
  observeEvent(input$checkIn, {
    
    #adpx_checkin <- values[["adpx_checkin"]]
    adpx <- values[["adpx"]]  # inputData()
    
  
    validate(need(input$rowParser, message=FALSE),
             need(adpx, message=FALSE))
    
    
    # if using manually
    if (input$rowParser == "Script") {
        adpx = rowByScript()
    }else if (input$rowParser == "Manual") {
        adpx = rowByManual()
    }else if (input$rowParser == "Auto") {
        adpx = rowByAuto()
    }
    
    key.lst = c("STUDYID","USUBJID","ARMA","TEST","NTIM","TIME","DVOR")
    key.lst = intersect(key.lst, colnames(adpx))
    adpx = adpx[, c(key.lst, setdiff(colnames(adpx), key.lst))]
     
    values[["adpx"]] <- adpx  
    
  })
  
  
  
  #------------------------------------- 
  # errstatus
  #------------------------------------- 
  errstatus <- reactive({ 
    adpx =  values[["adpx"]]
    validate(need(adpx, message= FALSE))
    
    messageTab = NULL
    if (is.null(adpx$STUDYID)) {messageTab = rbind(messageTab, c("warning", "No STUDYID"))}   
    if (is.null(adpx$USUBJID)) {messageTab = rbind(messageTab, c("warning", "No USUBJID"))}   
    if (is.null(adpx$ARMA))    {messageTab = rbind(messageTab, c("warning", "No ARMA"))}    
    if (is.null(adpx$TEST))    {messageTab = rbind(messageTab, c("warning", "No TEST"))}   
    if (is.null(adpx$NTIM))    {messageTab = rbind(messageTab, c("warning", "No NTIM"))}    
    if (is.null(adpx$TIME))    {messageTab = rbind(messageTab, c("warning", "No TIME"))}   
    if (is.null(adpx$DVOR))    {messageTab = rbind(messageTab, c("error", "No DVOR"))}    
    
    if (!is.null(messageTab)) {colnames(messageTab) = c("type", "value")}
    
    messageTab
    
  })
  
  
  #------------------------------------------------- 
  # add data to DATA when action button is pressed
  #-------------------------------------------------
  observeEvent(input$saveData, {
    validate(need(input$data_name4Select, message=FALSE),
             need(input$data_name4Save, message=FALSE), 
             need(values[["adpx"]], message=FALSE),
             #need(is.null(errstatus()), message="Fix the warning before proceed") 
             need(!"error" %in% (errstatus() %>% pull(type)), message="Fix the error before proceed") 
    ) 
    
    newData <- isolate(values[["adpx"]])
    data_name = isolate(input$data_name4Save)
    DATA = addNewData(newData, data_name, DATA)

  })
  
  
  # Return DATA
  return(DATA)
  
}
