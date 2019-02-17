
################################################################################ 
################################################################################
# checkInColsUI
################################################################################ 
################################################################################

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI
#-----------------------------------------

checkInColsUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12,   title="Tell whichCol is which", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status =  "primary" ,
                 column(width=4,
                        uiOutput(ns("data_selector"))
                 ), 
                 column(width=4,
                        actionButton(ns("runScript"),label = "runScript", style=actionButton.style )   # http://getbootstrap.com/docs/4.0/components/buttons/
                       # radioButtons(ns("runScript"), label=NULL, #"Select cpp model from:", 
                       #              choices=c("manual", "script"), inline=TRUE, selected ="manual"),
                 ),
                 column(width=4,
                        actionButton(ns("checkIn"),label = "checkIn", style=actionButton.style)   # http://getbootstrap.com/docs/4.0/components/buttons/
                 ),
                  
                 uiOutput(ns("rHandsontab_container")),
                 
                 uiOutput(ns("scriptArea_container"))
                  
                 
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
# checkInCols
################################################################################
################################################################################

checkInCols <- function(input, output, session, DATA) {
  
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
  
  
  #-----------------------------------------  
  # display the adpx_rHandsontab, alignTab
  #  https://github.com/jrowen/rhandsontable/issues/116
  # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
  #-----------------------------------------
  output$rHandsontab_container <- renderUI({ 
    if (mod(input$runScript,2)) {return(NULL)}
    
    # use manual curation
    output$adpx_rHandsontab <- renderRHandsontable({
      
      adpx_checkin =  values[["adpx_checkin"]]  # 
      if (is.null(adpx_checkin)) {return(NULL)}
      
      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL)
      
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
    if (mod(input$runScript+1,2)) {return(NULL)}
    textAreaInput(ns("scriptArea"), label=NULL, rows=25, placeholder = "data = data %>% mutate(TEST=1)")
    
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
    
    # display warnmsg table
    box(width=12, title="Warning", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "warning" , 
        DT::dataTableOutput(ns('warnmsg'))
    )
    
  })
  

  
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # reactive and events
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  #------------------------- 
  #  inputData
  #------------------------- 
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
  #  adpx_checkinTab and display
  #-----------------------------------------
  default_adpx_checkinTab <- reactive({
    file="./lib/pkmeta.xlsx"
    adpx_checkin = read_excel(file, sheet = "key", col_names = TRUE) %>% as.data.frame()  
    adpx_checkin = adpx_checkin %>% filter(!is.na(StdName))
    adpx_checkin
  })
  
  # update adpx_checkin based on new InputData
  adpx_checkinTab <- reactive({
    adpx_checkin = default_adpx_checkinTab()
    if (is.null(adpx_checkin)) {return(NULL)}
    
    # if new adpx loaded
    adpx = inputData()
    if (is.null(adpx)) {return(adpx_checkin)}
    
    #colnames(adpx) = toupper(colnames(adpx))  #leave it alone
    # based on adpx, update adpx_checkin, note VARS use globalVars$magicTab as background
    adpx_checkin$Column = VARS(adpx, vars.lst=adpx_checkin$StdName, Where="indivProfile", Who="feng.yang") 
     
    adpx_checkin = adpx_checkin %>% 
      mutate(Column=as.character(Column)) %>%
      mutate(Column=ifelse(StdName=="----", "----",  
                           ifelse(is.na(Column), "?", Column))) %>% 
      mutate(Column=factor(Column, levels=c(colnames(adpx), "?",  "----")))
     
    adpx_checkin  
 
  })
 
  
  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(adpx_checkinTab(), {
    values[["adpx_checkin"]] <- adpx_checkinTab()  # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adpx_rHandsontab, {
    values[["adpx_checkin"]] = hot_to_r(input$adpx_rHandsontab)  # manual alignment
  })
   
  
  #-------------------------------
  # if runScript
  #-------------------------------
  colByManual <- reactive({
    if (mod(input$runScript,2)) {return(NULL)}
    
    adpx_checkin <- values[["adpx_checkin"]]
    adpx <- values[["adpx"]]  # inputData()
    
    #
    validate(need(adpx, message=FALSE), 
             need(adpx_checkin, message=FALSE), 
             need(input$data_name4Select, message=FALSE)
    )
    
    #globalVars$magicTab
    globalVars$magicTab = push2MagicTab(globalVars$magicTab,
                                        Domain=as.character(adpx_checkin$StdName),
                                        Alias=as.character(adpx_checkin$Column),
                                        Label="",
                                        Where="",
                                        Who="feng.yang")
    
    #align columns based on adpx_checkin
    tt = adpx_checkin %>% filter(!is.na(Column), !Column %in% c("NA","", "----", "?"))
    if (nrow(tt)>0) { adpx[, tt$StdName] = adpx[, as.character(tt$Column)]}
 
    return(adpx)
  })
  
  #-------------------------------
  # if runScript
  #-------------------------------
  colByScript <- reactive({
    if (mod(input$runScript+1,2)) {return(NULL)}
    
    data <- values[["adpx"]]
    txt = input$scriptArea
    
    #validate
    validate(need(data, message=FALSE), 
             need(txt, message=FALSE),
             need(input$data_name4Select, message=FALSE)
    )
    
    # evaulate it  
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
    
    adpx <- values[["adpx"]]  # inputData()
    
    # update adpx
    if (mod(input$runScript,2)) { 
      adpx = colByScript()   #if using script
    }else { 
      adpx = colByManual()   #if using manually
    }
    
    key.lst = c("STUDYID","USUBJID","ARMA","TEST","NTIM","TIME","DVOR")
    key.lst = intersect(key.lst, colnames(adpx))
    adpx = adpx[, c(key.lst, setdiff(colnames(adpx), key.lst))]
    
    values[["adpx"]] <- adpx  
    
  })
  
  # 
  # observeEvent(input$checkIn999999, {
  # 
  #   adpx_checkin <- values[["adpx_checkin"]]
  #   adpx <- values[["adpx"]]  # inputData()
  #   
  # 
  #   
  #   if (is.null(adpx_checkin)) return(NULL)
  #   if (is.null(adpx)) return(NULL)
  #   
  #   #globalVars$magicTab
  #   globalVars$magicTab = push2MagicTab(globalVars$magicTab,
  #                                       Domain=as.character(adpx_checkin$StdName),
  #                                       Alias=as.character(adpx_checkin$Column),
  #                                       Label="",
  #                                       Where="",
  #                                       Who="feng.yang")
  #   
  #   #secondary columns
  #   
  #   tt = adpx_checkin %>% filter(!is.na(Column), !Column %in% c("NA","", "----", "?"))
  #   
  #   if (nrow(tt)>0) { adpx[, tt$StdName] = adpx[, as.character(tt$Column)]}
  #   
  #   
  #   #adpx[, adpx_checkin[which(is.na(adpx_checkin$Column)), "StdName"]] = "."
  #   data = adpx
  #   #save(data, file="data.RData")
  #   txt = input$scriptArea
  #   if (!is.null(txt)) {
  #     data =  tryCatch(eval(parse(text=txt ))  , 
  #                         error=function(e) {
  #                           print("not complete expression in scriptArea..."); 
  #                           return(NULL)
  #                         } #, finally = {
  #                         # eval(parse(text=txt)) %>% as.data.frame()
  #                         #}
  #     )
  #     if (is.null(data)) {return(NULL)}
  #     adpx = data
  #   }
  #   
  #   
  #   key.lst = c("STUDYID","USUBJID","ARMA","TEST","NTIM","TIME","DVOR")
  #   key.lst = intersect(key.lst, colnames(adpx))
  #   adpx = adpx[, c(key.lst, setdiff(colnames(adpx), key.lst))]
  #   
  #   
  #    ({ values[["adpx"]] <- adpx })
  #   
  #   
  # })
  
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
             need(values[["adpx"]], message=FALSE)
             #need(is.null(errstatus()), message="Fix the warning before proceed") 
             #need(!"error" %in% (errstatus() %>% pull(type)), message="Fix the error before proceed") 
    )
 
    newData <- isolate(values[["adpx"]])
    data_name = isolate(input$data_name4Save)
    DATA = addNewData(newData, data_name, DATA)

  })
  
  # Return DATA
  return(DATA)
  
}
