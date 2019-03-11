 
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, 
#-----------------------------------------

module_run_simulation_saved_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  mg = 1
  mkg = 2
  
  SC = 1
  IV = 2
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
   
  tabBox(width=12, id = ("run_ctlModel"), title =NULL, # "Loading Dose",
         
         # cppModel_container 
         tabPanel(width=12, title="Load cppModel ", value = "load_cppModel", collapsible = TRUE, 
                  collapsed = TRUE, solidHeader = TRUE,
                 #tagList(  
                    fluidRow(  
                          # fluidRow(
                          #   column(6,  
                          #          radioButtons(ns("cppModelLoadingBy"), label="Select cpp model from:", 
                          #                       choices=c("model library", "cpp file"), inline=TRUE)),
                          #   
                          #   column(6,  
                          #          radioButtons(ns("YesNoIIV"), label = "Want IIV?", inline=TRUE,
                          #                       choices = list("Yes" = "Yes", "No" = "No"), 
                          #                       selected = "No"))
                          # ),
                          # 
                          # fluidRow(column(12, uiOutput(ns("cppModelLibrary_container")))), 
                          # 
                          # fluidRow(column(12, uiOutput(ns("cppModelFile_container")))), 
                          
                          fluidRow(column(12, uiOutput(ns("load_cppModel_container")))), 
                          fluidRow(column(12, uiOutput(ns("update_cppModel_container")))), 
                          style='margin-bottom:30px;  border:1px solid; padding: 10px;'
                          
                          #fluidRow(column(12, uiOutput(ns("cppModelParam_container")))), 
                     
                  )
         ),       
         
         # adex_container 
         tabPanel(width=12, title="Dose regimen", value = "load_adex", collapsible = TRUE, 
                  collapsed = TRUE, solidHeader = TRUE,
                  tagList(  
                    radioButtons(ns("adexLoadingBy"), label = "Select adex from:",
                                 choices = list("auto"="auto","manual"="manual", "script"="script", "file"="file"), 
                                 inline = TRUE, selected = "auto"),
                    
                    uiOutput(ns("rHandsontab_container")),
                    
                    uiOutput(ns("adexManual_container")), 
                    
                    uiOutput(ns("adexScript_container")), 
                    
                    uiOutput(ns("adexFile_container")),
                    
                    uiOutput(ns('adexTab_container')) 
                  )
         ),    
         
         # adsl_container 
         tabPanel(width=12, title="Subject population", value = "load_adsl", collapsible = TRUE, 
                  collapsed = TRUE, solidHeader = TRUE,
                  tagList(  
                    radioButtons(ns("adslLoadingBy"), label="Select adsl from:", 
                                 choices=list( "manual"="manual", "script"="script", "file"="file"), 
                                 inline = TRUE, selected = "manual"),
                    
                    uiOutput(ns("adslManual_container")),
                    
                    uiOutput(ns("adslScript_container")), 
                    
                    uiOutput(ns("adslFile_container"))
                  )
         ), 
         
         # sim_setting_container 
         tabPanel(width=12, title="Simulation setting ", value = "simulation_setting", collapsible = TRUE, 
                  collapsed = TRUE, solidHeader = TRUE,
                  tagList(  
                    numericInput(ns("seed"), label = h5("Random seed:"),
                                 value=1234, min=1, max=1000),
                    
                    numericInput(ns("delta"), label = h5("Simulation step (delta, day)"),
                                 value=1, min=0.1, max=28),
                    
                    numericInput(ns("followUpPeriod"), label = h5("Follow-up duration after treatment period(day)"),
                                 value=112, min=1, max=224), 
                    
                    textInput(ns("infhr.lst"), label = h5("Sampling time after treatment (hour)"),
                              value= "0.5, 1, 2")
                  )
         ), 
         
         # runsimulation 
         tabPanel(width=12, title="Run simulation", value = "run_simulation", collapsible = TRUE, 
                  collapsed = TRUE, solidHeader = TRUE,
                  tagList(  
                    fluidRow(
                      column(12,
                             fluidRow(
                               column(width=8,  
                                      actionButton(ns("runSimulation"), label="Run simulation", style=actionButton.style ) 
                               ),
                               
                               column(width=2, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                                      textInput(ns("data_name"), value="simDat-", label=NULL)),
                               
                               column(width=2, #status = "primary",  #class = 'rightAlign',#background ="aqua",
                                      actionButton(ns("saveSimData"),label="Save it", style=actionButton.style))
                             ), 
                             
                             fluidRow(
                               column(width=12, uiOutput(ns("simProfile_container")))
                             )
                      )), 
                    uiOutput(ns('simDataTab_container'))
                  )  # tagList
         )
  )
  
   
  
}



################################################################################ 
################################################################################
# simulation
################################################################################
################################################################################
 

# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

module_run_simulation_saved <- function(input, output, session, ALL)  {
  
  ns <- session$ns
  EPS = 1E-3
  
  globalVars = NULL
  globalVars$login$status = TRUE
  
  mg = 1
  mkg = 2
  
  SC = 1
  IV = 2
  
  values <- reactiveValues(simData=NULL)
  print("in simulation:::")
 
  
  ################################
  # UI for load cpp model
  ################################
  output$load_cppModel_container <- renderUI({ 
    cppModel_name = "TEST"
    ALL =  callModule(module_load_cppModel, "load_cppModel556", ALL, cppModel_name)

    fluidRow(column(12, module_load_cppModel_UI(ns("load_cppModel556"), label = "load_cppModel556")))
  })
  
  output$update_cppModel_container <- renderUI({ 
    cppModel_name = "TEST"
    ALL =  callModule(module_update_cppModel, "update_cppModel556", ALL, cppModel_name)
     
    fluidRow(column(12, module_update_cppModel_UI(ns("update_cppModel556"), label = "update_cppModel556")))
  })
  
  
  #--------------------
  # cppModel 
  #--------------------  
  cppModel <- reactive({
    cppModel_name = "TEST"
    cppModel =  ALL$cppModel[[cppModel_name]]
    
    cppModel
  })
  
   
   
  # dyanmic textInput based on cppModelParam
  # output$cppModelParam_container <- renderUI({
  #   validate(need(globalVars$login$status, message=FALSE))
  #   
  #   if (is.null(input$cppModelByLibrary)) {return(NULL)}
  #   
  #   #conditionalPanel(
  #   #  condition = !is.null(input$cppModelFile)| input$cppModelByLibrary!="NULL", 
  #        
  #   if (input$cppModelLoadingBy=="model library" && input$cppModelByLibrary=="NULL") {
  #     return(NULL)
  #   }else if (input$cppModelLoadingBy=="cpp file" && is.null(input$cppModelFile)) {
  #     return(NULL)
  #   } else {
  #     fluidRow(
  #       column(6, 
  #              lapply(1:round(length(cppModelParam())/2, digits=0), function(i) {
  #                textInput(ns(paste0("param", i)), label = NULL, value = cppModelParam()[i]) #paste0("param-", i))
  #              })), 
  #       column(6, 
  #              lapply((1+round(length(cppModelParam())/2, digits=0)):length(cppModelParam()), function(i) {
  #                textInput(ns(paste0("param", i)), label = NULL, value = cppModelParam()[i]) # paste0("value-", i))
  #              })))
  #   }
  # })
   
    
  
  
   
  
  
  
  ################################
  # UI for load dosing regimen
  ################################
  output$adexManual_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adexLoadingBy!="manual") {return(NULL)}
    tabBox(width=12, id = "loading_dosing", title =NULL, # "Loading Dose",
           tabPanel(width=12, title="EV#1:", collapsible = TRUE, collapsed = FALSE, 
                    textInput(ns("GROUPID"), label = h5("Dose Group ID:"),
                              value="1, 2"),
                    textInput(ns("TIME1"), label = h5("Start Time (day)"),
                              value="0"),
                    textInput(ns("AMT1"), label = h5("Dose Level"),
                              value="250, 450"), 
                    textInput(ns("UNIT1"), label = h5("Dose Unit (mg=1,mkg=2)"),
                              value="1"),   # mg, mg/kg
                    textInput(ns("II1"), label = h5("Dose Freq (day)"),
                              value="28"), 
                    textInput(ns("NDOSE1"), label = h5("Num of Dose"),
                              value="1"), 
                    textInput(ns("ROUTE1"), label = h5("Route (SC=1,IV=2)"),
                              value="1"),   # SC  IV
                    textInput(ns("INFHR1"), label = h5("Infusion Hour"),
                              value="NA"),  
                    textInput(ns("ARMA"), label = h5("Customized Dose Group Name:"),
                              value="NA") 
           ), 
           
           tabPanel(width=12, title="EV#2:", collapsible = TRUE, collapsed = TRUE, 
                    textInput(ns("TIME2"), label = h5("Start Time (day)"),
                              value="14"),
                    textInput(ns("AMT2"), label = h5("Dose Level"),
                              value=c("10, 15")), 
                    textInput(ns("UNIT2"), label = h5("Dose Unit (mg=1,mkg=2)"),
                              value="2"), 
                    textInput(ns("II2"), label = h5("Dose Freq (day)"),
                              value=c("14")), 
                    textInput(ns("NDOSE2"), label = h5("Num of Dose"),
                              value=c("1")), 
                    textInput(ns("ROUTE2"), label = h5("Route (SC=1,IV=2)"),
                              value=c("2")), 
                    textInput(ns("INFHR2"), label = h5("Infusion Hour"),
                              value="1")  
                    
           ), 
           tabPanel(width=12, title="EV#3:", collapsible = TRUE, collapsed = TRUE, 
                    textInput(ns("TIME3"), label = h5("Start Time (day)"),
                              value="84"),
                    textInput(ns("AMT3"), label = h5("Dose Level"),
                              value=c("300, 450")), 
                    textInput(ns("UNIT3"), label = h5("Dose Unit (mg=1,mkg=2)"),
                              value="1"), 
                    textInput(ns("II3"), label = h5("Dose Freq (day)"),
                              value=c("7")), 
                    textInput(ns("NDOSE3"), label = h5("Num of Dose"),
                              value=c("4")), 
                    textInput(ns("ROUTE3"), label = h5("Route (SC=1,IV=2)"),
                              value=c("1")), 
                    textInput(ns("INFHR3"), label = h5("Infusion Hour"),
                              value="NA") 
           )
      )
  })
  
 
  
  output$adexScript_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adexLoadingBy!="script") {return(NULL)}
    textAreaInput(ns("adexScript"), label=NULL, value="", rows=10, 
                  placeholder= "ev(ID=1, time=0, amt=350, ii=21, route=IV, infhr=1, addl=7)")
  })
  
  
  output$adexFile_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adexLoadingBy!="file") {return(NULL)}
    fileInput(ns("adexFile"), label = h5("Load your file"),
               accept=c('text/csv/sas7bdat', 
                        'text/comma-separated-values,text/plain', 
                        '.xlsx','xls',
                        '.csv'))
  })
  
  
  ################################
  # UI for adsl
  ################################
  output$adslManual_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adslLoadingBy!="manual") {return(NULL)}
    tagList(
        numericInput(ns("n_subject"), label = h5("Number of subjects"),
                     value=1, min=1, max=1000,step = 1), 
        
        textInput(ns("pop_WT"), label = h5("Body weight in population"),
                  value="75"), 
        
        textInput(ns("F1"), label = h5("Bioavailability"),
                  value="0.7")
    )
  })
  
  
  output$adslScript_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adslLoadingBy!="script") {return(NULL)}
    textAreaInput(ns("adslScript"), label=NULL, value="", rows=10, 
                  placeholder= "expand.grid(USUBJID=1:3, WGTBL=seq(60, 80, by=10), F1=seq(0.7, 0.8,by=0.1)) %>% mutate(ID=1:nrow(.))")
  })
  
    
  output$adslFile_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    if (input$adslLoadingBy!="file") {return(NULL)}
    fileInput(ns("adslFile"), label = NULL,  #h5("Or load your adsl"),
              accept=c('text/csv/sas7bdat', 
                       '.xlsx','xls',
                       'text/comma-separated-values,text/plain', 
                       '.csv', 
                       '.sas7bdat'))
  })
    
  
  #####################################
  # UI for load simulated time-profile
  #####################################
  output$simProfile_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE))
    
    data = statsTab()
    validate(need(data, message= "no data generated yet"))
    
    output$figLog <- renderPlot({  
      
      cat(file=stderr(), "##############Step: render myPlot #################", "\n")
      #ggplot(out, aes(x=TIME, y=IPRED, group=ID, col=ARMA)) + 
      #  geom_line() + geom_point()
      
      #base_xyplot(out, XVAR="TIME", YVAR="IPRED", group="ID", color_by = "ARMA", type="l")
      
      # data:   XVAR, YVAR, GROUP_BY, COLOR_BY, SHAPE_BY
      # input: 
      #    xscale, yscale, yscale2
      #    xRange, yRange
      #    xlab, ylab
      #    fontSize    facet_by  
      #    xrefline    yrefline  
      #    plotType      
      #input2 = NULL
      input2 = list(xtick = "7, 28",  xscale="nonlog", 
                    ytick = "10, 100",yscale = "log",    # log or nonlog
                    xRange = c(min(data$XVAR, na.rm=TRUE),max(data$XVAR, na.rm=TRUE)),
                    yRange = c(min(data$YVAR, na.rm=TRUE),max(data$YVAR, na.rm=TRUE)), 
                    xlab = "Time (days)",
                    ylab = "Predicted Concentration (mg/L)",
                    fontSize = 12, 
                    facet_by = "", 
                    xrefline = "NA",
                    yrefline = "NA", 
                    plotType = "line", # line  or point
                    errbar_or_CI = "CI",    # errbar_or_CI, or CI
                    CI = "95%", 
                    addline="NULL" )   
      
      figLog = plotIt(data, input2, session)    
      #figLog  = ggplot(data=data, aes(x=NTIM,y=Mean, group=ARMA)) + geom_line()
      figLog
      
    }) 
    
    plotOutput(ns("figLog"), width = "100%", height = "500px")
    
    
    
  })
   
  
  # table for simulated data
  #-----------------------------
  output$simDataTab_container <- renderUI({
    simData = values$simData
    validate(need(simData, message=FALSE))
    
    simData = simData %>% dplyr::mutate_if(is.numeric, function(x) round(x, digits=3)) %>% slice(1:100)
    
    output$simDataTab <- DT::renderDataTable(
      DT::datatable(data = simData,
                    options = list(pageLength = 16, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                    #filter = 'top',    # bottom',
                    #caption = htmltools::tags$caption(
                    # style = 'caption-side: bottom; text-align: center;',
                    # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                    #  )
      ) 
    )
    #rownames = FALSE, 
    # width="100%",
    #autoWidth = FALSE
    # aLengthMenu = c(5, 30, 50), iDisplayLength = 6, bSortClasses = TRUE,
    # bAutoWidth = FALSE,
    # aoColumn = list(list(sWidth = "150px", sWidth = "30px",
    #                      sWidth = "30px", sWidth = "30px"))
    #columnDefs = list(list(width = '200px', targets = "_all"))
    
    DT::dataTableOutput(ns("simDataTab"))
  })
  
  
   
  
  ##################################################
  # control for loading adex 
  ##################################################
  
  #------------------
  # adexByManual
  #------------------  
  adexByManual <- reactive({ 
      
    # if (1==2) {  # debug
    #   event1 = create_event(GROUPID="1:3", TIME=0, AMT="c(100, 200, 300)", UNIT=1, ROUTE=1, INFHR=0, II=14, NDOSE=1)
    #   event2 = create_event(GROUPID="1:3", TIME=14, AMT="c(10, 20, 30)", UNIT=1, ROUTE=1, INFHR=0, II=14, NDOSE=8)  
    #   event3 = create_event(GROUPID="1:3", TIME=84, AMT="c(10, 20, 30)", UNIT=1, ROUTE=1, INFHR=0, II=28, NDOSE=4) 
    #   #adex = rbind(event1, event2, event3) %>% arrange(GROUPID)
    #   
    #   adsl = data.frame(ID=1:2, WGTBL=c(60,70)) #%>% 
    #         #mutate(USUBJID = add_prefix(USUBJID, digits=3))
    # }
    
    # adsl
    adsl = adsl()
    if (is.null(adsl)) {return(NULL)}
    
    event1 = create_event(input$GROUPID, input$TIME1, input$AMT1, input$UNIT1, input$ROUTE1, input$INFHR1, input$II1, input$NDOSE1)
    event2 = create_event(input$GROUPID, input$TIME2, input$AMT2, input$UNIT2, input$ROUTE2, input$INFHR2, input$II2, input$NDOSE2)
    event3 = create_event(input$GROUPID, input$TIME3, input$AMT3, input$UNIT3, input$ROUTE3, input$INFHR3, input$II3, input$NDOSE3)
    if (is.null(event1)) {return(NULL)}
    if (is.null(event2)) {return(NULL)}
    if (is.null(event3)) {return(NULL)}
    event = bind_rows(event1, event2, event3)
    
    adex = event %>% event_add_adsl(adsl) %>%  process_event()  
     
    if (!is.null(adex$F1)) {adex$TVF1 = adex$F1}  #?????
    
    return(adex)
    
  })
  
  #------------------
  # update ARMA
  #------------------ 
   # 
   # #update ARMA using default ARMA
   #  observeEvent(input$ARMA_autoVsmanual, {
   #    #if (is.null(input$ARMA)) {return(NULL)}
   #  
   #    #if (input$ARMA=="NA") {
   #      adex = adexByManual()
   #      value = paste(unique(adex%>%pull(ARMA)), collapse=",")
   #      updateTextInput(session, "ARMA", value= value)  
   #    #}
   #  })
   # 
  #---------------------------------------------------
  # update ARMA if manually input, (not work yet)
  #--------------------------------------------------- 
   adexByManual2 <- reactive({ 
     if (is.null(input$ARMA)) {return(NULL)}
     if (is.null(input$GROUPID)) {return(NULL)}
     
     adex = adexByManual()
     if (input$ARMA=="NA") {return(adex)} 

     # If a customized ARMA provided.
     GROUPID =  tryCatch(eval(parse(text=paste0("c(", input$GROUPID, ")")))  , 
                         error=function(e) {
                           print("not complete expression in GROUPID...."); 
                           return(NULL)
                         } #, finally = {
                         # eval(parse(text=txt)) %>% as.data.frame()
                         #}
     )
     
     arma.lst = strsplit(input$ARMA, ",") %>% unlist()
     
     
     adex = adex %>% select(-ARMA) %>% 
       left_join(data.frame(GROUPID = GROUPID, ARMA = arma.lst), 
                 by = "GROUPID")
   
     
     adex
   })
   
    

  
  
  # update adex based on ARMA
  observeEvent(input$ARMA1888888, {
    if (is.null(input$ARMA) | input$ARMA=="") {return(NULL)}
    if (is.null(input$GROUPID)) {return(NULL)}
     
    if (input$ARMA!="NA") {
      GROUPID = eval(parse(text=paste0("c(", input$GROUPID, ")")))
      arma.lst = input$ARMA
      arma.lst = strsplit(arma.lst, ",") %>% unlist()
       
      adex = adexByManual() 
      if (is.null(adex)) {return(NULL)}
      adex = adex %>% select(-ARMA) %>% 
        left_join(data.frame(GROUPID = GROUPID, ARMA = arma.lst), 
                  by = "GROUPID")
      
      values$adexByManual = adex
    }
  })
   
  
  
  #################################################################################################
  # lazy dosing 
  #################################################################################################
  
  
  #-----------------------------------------
  #  adpx_checkinTab and display
  #-----------------------------------------
  
  
  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(input$adexLoadingBy, {
    if (input$adexLoadingBy != "auto") {return(NULL)}
    
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
  
  
  
  
  #-----------------------------------------  
  # display the adpx_rHandsontab, alignTab
  #-----------------------------------------
  output$rHandsontab_container <- renderUI({ 
    #if (mod(input$runScript,2)) {return(NULL)}
    if (input$adexLoadingBy != "auto") {return(NULL)}
    
    # use manual curation
    output$adex_rHandsontab <- renderRHandsontable({
      
      adex_checkin =  values[["adex_checkin"]]  # 
      if (is.null(adex_checkin)) {return(NULL)}
      
      rhandsontable(adex_checkin, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL)
      
    })
    rHandsontableOutput(ns("adex_rHandsontab"))
    
  })
  
    
  
  adexByAuto <- reactive({ 
    adsl = adsl()
    if (is.null(adsl)) {return(NULL)}
    
    adex = values[["adex_checkin"]] 
    if (is.null(adex)) {return(NULL)}
    
    adex = adex %>% filter(Dosing.Regimen!="" & !is.na(Dosing.Regimen))
    adex = parseARMA(adex$Dosing.Regimen) 
     
    adex = adex %>% event_add_adsl(adsl) %>%  process_event()     
    if (!is.null(adex$F1)) {adex$TVF1 = adex$F1}  #?????
    
    return(adex)
  })
  
  
  #------------------
  # adexByScript
  #------------------  
   
  adexByScript <- reactive({ 
     txt = input$adexScript
     if (is.null(txt)) {return(NULL)}
     if (txt=="") {return(NULL)}
     
     adsl = adsl()
     if (is.null(adsl)) {return(NULL)}
     
      
     event =  tryCatch(eval(parse(text=txt)) %>% as.data.frame(), 
                      error=function(e) {
                        print("not complete expression in adexScript...."); 
                        return(NULL)
                      } #, finally = {
                      # eval(parse(text=txt)) %>% as.data.frame()
                      #}
     )
     if (is.null(event)) {return(NULL)}
     if (is.function(event)) {return(NULL)}  # avoid key words such as expand
    
     # if (1==2) {  # debug
       # expand.ev(ID=1,amt=4:7, time=0, ii = 14, route=1, addl=1) %>% as.ev() + 
      #  expand.ev(ID=2,amt=8:11, time=14, ii = 7, route=1, addl=5) %>% as.ev()
     # }
     
     # adex, ID is for patients
     
     adex = event %>% event_add_adsl(adsl) %>%  process_event()   
     
     adex
      
     
  })
  
  #------------------
  # adexByFile
  #------------------  
  adexFileInput <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$ADEXFile, message = FALSE))
    input$ADEXFile
  })
  
  # The user's data, parsed into a data frame
  adexByFile <- reactive({ 
    
    inFile = adexFileInput()
 
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
 
    library('gdata')
    library('xlsx')      
    
    adex =  switch(ext,
           "csv" = read_csv(paste(inFile$datapath, ext, sep="."), col_names=TRUE )  %>% as.data.frame(),
           "xlsx"= read_excel(paste(inFile$datapath, ext, sep="."), sheet = 1, col_names = TRUE)  %>% as.data.frame(),
           "xls" = read_excel(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame()
    )
     
    adex = adex %>% process_event()   
    
    adex
  })
 
  
  #------------------ 
  # final adex
  #------------------ 
  adex <- reactive({ 
    if (is.null(input$adexLoadingBy)) {return(NULL)}
    
    adex <-switch(input$adexLoadingBy, 
           "auto" = adexByAuto(),
           "manual"= adexByManual2(),  
           "script"= adexByScript(), 
           "file" =  adexByFile()
    ) 
    
    if (is.null(adex)) {return(NULL)}
 
    
    #https://shiny.rstudio.com/articles/validation.html
    std.col.name.lst1 <-c("ID", "time",  "amt",  "unit",  "route", "infhr", "ii",  "addl",  "evid",   "cmt",  "rate")     
    std.col.name.lst2 <-c("STUDYID",   "ARMA",   "GROUPID", "USUBJID",  "AMT",  "UNIT",    "ROUTE",   "NDOSE",   "FREQ",  "WGTBL")     
    std.col.name.lst = c(std.col.name.lst1, std.col.name.lst2)  
    
    validate(
      need(all(std.col.name.lst %in% colnames(adex)), 
           paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adex)), collapse=", "), " in ", "adex")
      )
    )
    
    adex
    
  })
  
  
  #------------------ 
  # adexTab
  #------------------ 
  output$adexTab_container <- renderUI({
  
    output$adexTab <- renderRHandsontable({
        adex = adex()
        if (is.null(adex)) {return(NULL)}
        
        adex = adex %>% mutate(
          TIME = time, 
          GROUPID = as.integer(GROUPID),
          CMT  = as.integer(cmt),
          RATE = rate, 
          EVID = as.integer(evid), 
          NDOSE= as.integer(addl+1), 
          cmt  = as.integer(cmt), 
          ii   = as.integer(ii), 
          addl = as.integer(addl), 
          evid = as.integer(evid)
        )
        adex = adex %>% select(STUDYID,ARMA,USUBJID,ID,GROUPID,TIME,AMT,UNIT,ROUTE,FREQ,NDOSE,CMT,RATE,WGTBL)
                              # time,cmt,rate,infhr,ii,addl,evid)
        adex = head(adex, n=100)  # only the first 100 rows show
        
        rhandsontable(adex,  useTypes = TRUE, stretchH = "all", readOnly=TRUE) %>%  # width = 600, height = 300, 
         hot_col(col=colnames(adex), halign="htCenter")     #htLeft, htCenter, htRight and htJustify  
                                          # "factor_allow", allowInvalid = TRUE)
        
      })
  
    rHandsontableOutput(ns("adexTab"))
  })
  
  
  # ## use renderUI to display table
  #https://rstudio.github.io/DT/
  output$adexTab_container2 <- renderUI({
    adex = adex()
    if (is.null(adex)) {return(NULL)}
    
    adex = adex %>% mutate(
      TIME = time, 
      GROUPID = as.integer(GROUPID),
      CMT  = as.integer(cmt),
      RATE = rate, 
      EVID = as.integer(evid), 
      NDOSE= as.integer(addl+1), 
      cmt  = as.integer(cmt), 
      ii   = as.integer(ii), 
      addl = as.integer(addl), 
      evid = as.integer(evid)
    )
    adex = adex %>% select(STUDYID,ARMA,USUBJID,ID,GROUPID,TIME,AMT,UNIT,ROUTE,FREQ,NDOSE,CMT,RATE,WGTBL)
    # time,cmt,rate,infhr,ii,addl,evid)
    rhandsontable(adex,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
    # hot_col("factor_allow", allowInvalid = TRUE)
    
    output$adexTab <- DT::renderDataTable(
      DT::datatable(data = adex,
                    options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                    #filter = 'top',    # bottom',
                    #caption = htmltools::tags$caption(
                    # style = 'caption-side: bottom; text-align: center;',
                    # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                    #  )
      ) 
    )
    #rownames = FALSE, 
    # width="100%",
    #autoWidth = FALSE
    # aLengthMenu = c(5, 30, 50), iDisplayLength = 6, bSortClasses = TRUE,
    # bAutoWidth = FALSE,
    # aoColumn = list(list(sWidth = "150px", sWidth = "30px",
    #                      sWidth = "30px", sWidth = "30px"))
    #columnDefs = list(list(width = '200px', targets = "_all"))
    
    DT::dataTableOutput(ns("adexTab"))
    
  })
  
  
  
  ################################################################
  # control for loading adsl, Need ID and WGTBL
  ################################################################
  
  #-----------------------------------
  # adslByManual, Need ID and WGTBL
  #-----------------------------------  
  adslByManual <- reactive({ 
    if (is.null(input$n_subject)) {return(NULL)}
     
    adsl = tryCatch(  
      {   
        ID= seq(1, input$n_subject)  #   add_prefix(c(seq(1, input$n_subject)), digits=3)
        F1 =  eval(parse(text=paste0("c(",  input$F1, ")")))  # 75 seq(0.7, 0.7, by=0.1)
        WGTBL=  eval(parse(text=paste0("c(",  input$pop_WT, ")")))  # 75
        
        expand.grid(ID=ID, WGTBL=WGTBL, F1=F1) 
        
      },
      error=function(e) {
        print("Error in constructing adsl by manual"); 
        return(NULL)
      }
    )
    
    
    if (is.null(adsl) | nrow(adsl)==0) {return(NULL)}
 
    adsl = adsl %>% mutate(ID = 1:nrow(adsl)) 
    
    std.col.name.lst = c("ID", "WGTBL")
    validate(
      need(all(std.col.name.lst %in% colnames(adsl)), 
           paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adsl)), collapse=", "), " in ", "adsl")
      )
    )
    
    adsl 
  })
  
  #----------------------------------------------------------------------
  # adsl manually defined a simulated population (or a single subject)
  #----------------------------------------------------------------------    
  adslByManual2 <- reactive({ 
    
    if (as_numeric(input$n_subject)==1) {sdWt = 0}
    if (as_numeric(input$n_subject)>1)  {sdWt = 0.2*as_numeric(input$pop_WT)}
    
    
    # adsl = adslByManual_old(POP=paste0("WT=", input$pop_WT), 
    #                    nsubject=as_numeric(input$n_subject), 
    #                    meanWt=as_numeric(input$pop_WT), 
    #                    sdWt = as_numeric(sdWt),
    #                    seed=1234)
 
    adsl
  })
  
  
  
  #----------------------------------
  # adslByScript, Need ID and WGTBL
  #----------------------------------
  adslByScript <- reactive({ 
    txt = input$adslScript
    if (is.null(txt)) {return(NULL)}
    if (txt=="") {return(NULL)}
    
    #adsl = eval(parse(text=txt)) %>% as.data.frame()
    #adsl = expand.grid(ID=1:10, WGTBL=seq(60, 80, by=10), F1=seq(0.5, 1.0,by=0.1)) %>% as.data.frame()
    
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
           paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adsl)), collapse=", "), " in ", "adsl")
      )
    )
    
    adsl
    
  })
   
  
  # adsl data defined by a flile
  #-------------------------------
  # The selected file, if any
  adslFileInput <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$adslFile, message = FALSE))
    input$adslFile
  })
  
  adslByFile <- reactive({ 
    
    inFile = adslFileInput()
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
 
    switch(ext,
           "csv" = read_csv(paste(inFile$datapath, ext, sep="."), col_names=TRUE )  %>% as.data.frame(),
           "xlsx"=read_excel(paste(inFile$datapath, ext, sep="."), sheet = 1, col_names = TRUE)  %>% as.data.frame(),
           "xls" = read_excel(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame(),
           "sas7bdat" =  read.sas7bdat(paste(inFile$datapath, ext, sep="."))  %>% as.data.frame()
    )
  })
  
  
  
  #--------------------
  # adsl selector
  #--------------------
  adsl <- reactive({ 
    switch(input$adslLoadingBy, 
           "manual" = adslByManual(),  
           "script" = adslByScript(), 
           "file"   =  adslByFile()
    ) 
  })
  
    
      
  ############################################################  
  # control for setting
  ############################################################  
  
  
  #---------------------------------------------------
  # update infhr.lst based on user's input of infr
  #--------------------------------------------------- 
  
  #update infhr.lst  
  observe({
    if (is.null(input$infhr.lst)) {return(NULL)}
    adex = adex();if (is.null(adex)) {return(NULL)}
    
    infhr.in.adex = unique(adex$infhr)
    
    #  infusion hours if IV dose
    infhr.lst =  tryCatch({eval(parse(text=paste0("c(",input$infhr.lst, ")")))}, 
                          error=function(e) {
                            print("not complete expression in infhr.lst...."); 
                            return(c(0.5, 1,2))   # default 
                          } #, finally = {
                          # eval(parse(text=txt)) %>% as.data.frame()
                          #}
    )
    infhr.lst = sort(unique(c(infhr.lst, infhr.in.adex))) 
    infhr.lst = infhr.lst[which(infhr.lst!=0)] %>% 
                as.character() %>% paste0(., collapse=",")
    
    updateTextInput(session, "infhr.lst", value= infhr.lst) 
 
  })
   
  
  
  
  ##############################################################
  # run simulation
  ##############################################################

  observeEvent({
    input$userName
    input$password
  }, {
    
    if (is.null(input$userName)) {return(NULL)}
    if (is.null(input$password)) {return(NULL)}
    if (input$userName=="") {return(NULL)}
    if (input$password=="") {return(NULL)}
    
    result = login(input$userName, input$password)
    values$login  = ifelse(is.null(result), "notok", "ok")     
    
  })
  
  
  
  #-------------------------------------------------------------
  # run simulation
  #-------------------------------------------------------------
 
  #runSimulation <- reactive({
  observeEvent(input$runSimulation, {

    # load 
    cppModel_name = "TEST"
    mod =  ALL$cppModel[[cppModel_name]]   # cppModel() #   values$cppModel #updatecppModel();  
    if (is.null(mod)) {return(NULL)}
    
    adsl = adsl();  if (is.null(adsl)) {return(NULL)}
    adex = adex();  if (is.null(adex)) {return(NULL)}
    adex =  as.data.frame(adex) #%>% mutate(ID=id, F1=f1))
    #save(adex, file="adex.RData")
     
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
    progress$set(message = "Running Simulation...Please Wait", value = 0)
     
    
    # setup parameters for simulation
    # -----------------------------------
    seed = input$seed
    delta = input$delta
    followUpPeriod = input$followUpPeriod
    
    #infhr.lst =   # infusion hours if IV dose
    infhr.lst = input$infhr.lst
    infhr.lst =  tryCatch({eval(parse(text=paste0("c(",input$infhr.lst, ")")))}, 
                      error=function(e) {
                        print("not complete expression in infhr.lst...."); 
                        return(c(0.5,1,2))
                      } #, finally = {
                      # eval(parse(text=txt)) %>% as.data.frame()
                      #}
    )
      
    # set seed to make reproducible simulation (if IIV=1)
    set.seed(seed)
    
    # library("PKPDmisc")
    treat_end = max((adex$addl+1)*adex$ii)  #l24*7    
    sim_end = treat_end + followUpPeriod   # default 112 days                      # note dose by week only
    tgrid = sim_timept(start=0, end=treat_end, delta=delta, dtime=seq(0,treat_end, by=7))
    
    # run simulation
    # -----------------------------------
    out = mod %>% data_set(as.ev(adex)) %>% 
      mrgsim(end=sim_end, delta=delta, add=tgrid, tad=TRUE, carry.out=c("ii", "evid")) %>% 
      as.data.frame() 
    colnames(out) = toupper(colnames(out))
    

      
    # only add ID, STUDYID, USUBJID, ARMA
    out = out %>% left_join(adex %>% as.data.frame() %>% 
                              distinct(ID, .keep_all=TRUE) %>% 
                              select(ID, STUDYID, USUBJID, ARMA),
                            by=c("ID"))  
    
    # nominal time (NTIM)
    out = out %>% mutate(NTIM = TIME)
    
    # fill up II by  Last Observation Carried Forward
    # na.locf https://stackoverflow.com/questions/48392015/r-carry-forward-last-observation-n-times-by-group?rq=1
    out = out %>% mutate(II=ifelse(II==0, NA, II))  %>% group_by(ID) %>% fill(II)
    
    # EVID, DVOR and EXSEQ    
    # -----------------------------------
    simData = out %>% mutate(EVID=ifelse(TAD==0, 1, 0)) %>% slice(2:n()) %>%
      mutate(DVOR = IPRED) %>% 
      group_by(ID)%>%  mutate(EXSEQ=cumsum(EVID)) %>%   # for calculat the last dose interval later
      as.data.frame()
 
    # TEST
    if (is.null(simData$TEST)) {simData$TEST="Unknown"}
    if (is.null(simData$WGTBL)) {simData$WGTBL=75}
    
    # simData is a data.frame of 
    # c("STUDYID", "ARMA", "USUBJID", "ID", "TIME","NTIM", "TAD", "EVID", "II", "TEST","DVOR", "EXSEQ", "WGTBL")
    values$simData = simData
    #save(simData, file="simData.RData")
  
  }) 
  
  
  
  #-------------------------------------------------------------
  # calculate the statistic summary table based on simulated data
  #-------------------------------------------------------------
  statsTab <- reactive({ 
      
      #print("before simulation:")    
      data = values$simData # runSimulation()
      if (is.null(data)) {return(NULL)}
      
      data = data %>% mutate(NTIM=TIME, DVOR=IPRED) %>%   # use IPRED as DVOR
                      mutate(NTIM=as_numeric(NTIM), 
                             DVOR=as_numeric(DVOR))
      
      pkSummary = data %>% select_("ARMA", "USUBJID", "NTIM", "DVOR")  %>% 
                  calc_stats(id="USUBJID", group_by=c("ARMA",  "NTIM"), value="DVOR")  %>%
                  filter(!is.na(NTIM) & !is.na(Mean))
      if (is.null(pkSummary)) {return(NULL)}
      if (nrow(pkSummary)==0) {return(NULL)}
      
      
      data = pkSummary  
      errorbar = "SD"
      if (errorbar == "SE") { 
        data$meanMinus = data$meanMinusSE 
        data$meanPlus = data$meanPlusSE
      }
      
      if (errorbar == "SD") { 
        data$meanMinus = data$meanMinusSD 
        data$meanPlus = data$meanPlusSD  
      } 
      
      EPS= 0.078/2
      data = data %>% mutate(NTIM=NTIM+EPS, Mean=Mean+EPS, meanPlus=meanPlus+EPS, meanMinus=meanMinus+EPS)
 
      # get ready for final plot
      statsTab = data %>% mutate(NTIM = as_numeric(NTIM), 
                             Mean = as_numeric(Mean),
                             meanMinus = as_numeric(meanMinus), 
                             meanPlus = as_numeric(meanPlus)) %>%
        mutate(XVAR=as_numeric(NTIM), 
               YVAR=as_numeric(Mean), 
               GROUP_BY=ARMA, 
               COLOR_BY=ARMA, 
               SHAPE_BY=ARMA)
      
      #save(statsTab, file="statsTab.RData")
      statsTab
  })
  
  
  #-------------------------------------------------------------
  # save simulated data
  #-------------------------------------------------------------
  #add figure to log when action button is pressed
  observeEvent(input$saveSimData, {
    #req(input$addToCart, FIGURE_ALL, input$fig_name)
    
    if(is.null(input$saveSimData))  {return()}
    if(input$saveSimData == 0) {return() }
    cat(file=stderr(), "##############Step: saveSimData #################", "\n")
    
    # data 
    #CurrentLog_mDATA   <- isolate(DATA$mDATA)
    newData <- values$simData
    #newData <- list(isolate(newData))
    data_name <- input$data_name
    #names(newData) = data_name
    
    # meta for data
    # CurrentLog_mTITLES   <- isolate(DATA$mTITLES)
    # newTitle <- list("Simulated")
    # names(newTitle) = data_name
    # 
    # if (!data_name %in% names(CurrentLog_mDATA)) {
    #   DATA$mTITLES[[data_name]] <- newTitle
    #   DATA$mDATA[[data_name]] <- newData[[data_name]]
    #   DATA$NumOfDataset <- isolate(DATA$NumOfDataset) + 1
    #   
    # }else{ 
    #   cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
    #   #validate(
    #   #   need(!fig_name %in% names(CurrentLog_mFIGURES), "fig_name already exist, override will occur! ")
    #   #)
    #   
    #   DATA$mTITLES[[data_name]] <- newTitle
    #   DATA$mDATA[[data_name]] <- newData[[data_name]]
    #   
    # }
    
    #newData = ALL$DATA[[data_name]]
    attr(newData, "data.name") = data_name
    ALL$DATA[[data_name]] <- newData
    
     
    
  })
   
  
  return(ALL)
  
}



