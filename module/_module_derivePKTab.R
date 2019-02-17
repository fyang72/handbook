
 
########################################################### 
########################################################### 
# output$CheckinUI
########################################################### 
########################################################### 

# feng's dataset
#USUBJID, ARMA, NTIM, TIME, TEST, DVOR,LLOQ
# 
# in deli's typical PK dataset
# [1] "STUDYID"           "SUBJECT"           "VISIT"             "TIMEPT"            "NOM_HR"           
# [6] "PKHOUR_C"          "NOM_DAY"           "PKDAY_C"           "CONCCH"            "CONCN"            
# [11] "CONCNI"            "STDUNIT"           "BLQ"               "SAMDTTM"           "ISAMDTTM"         
# [16] "SAMDT"             "SAMTM"             "DRUGDTTM"          "DRUGDT"            "DRUGTM"           
# [21] "IDRDTTM"           "DOSEDTTM"          "DOSESTDT"          "DOSEENDT"          "DOSE"             
# [26] "DOSEKG"            "DOSEPLAN"          "DOSEPLANKG"        "TEST"              "TESTCD"           
# [31] "SCAT"              "SOP"               "DIFFC"             "DIFF"              "LLOQ"             
# [36] "SUBJECTN"          "DOSECH"            "DOSESUBJ"          "ROUTE"             "PCDY"             
# [41] "WT"                "BSA"               "DURATION"          "TESTN"             "USUBJID"          
# [46] "VISITNUM"          "TRT"               "COHORT"            "TRTN"              "TRTSUBJ"          
# [51] "TRTDOSESUBJ"       "DOSEPLAN_LIM"      "PCORRES"           "COMMENT"           "CYCLE"            
# [56] "VISITDY"           "HT"                "SEX"               "BMI"               "ETHNIC"           
# [61] "RACE"              "AGE"               "DINT"              "DINTN"             "FIRSTE"           
# [66] "PKDAY_C2"          "NOM_DAY2"          "GRPSUBJ"           "GRP"               "COUNTRY"          
# [71] "QCQA2"             "QCQA1"             "VERSION2"          "VERSION1"          "GRPN"             
# [76] "RFENDT"            "RFENTM"            "BASELINE_V3_DAY_1"
# 

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

derivePKTabUI <- function(id, label="") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(2,
           uiOutput(ns("data_Selector")),
           uiOutput(ns("test_Selector")),
           
           uiOutput(ns("group_by_selector")),
           uiOutput(ns("xvar_Selector")),
           uiOutput(ns("yvar_Selector")),
            
           uiOutput(ns("define_interval_by")),
           uiOutput(ns("start_time_selector")),
           uiOutput(ns("freq_selector")),           
           uiOutput(ns("end_time_selector"))
           
          # tags$hr(style="border-color: black;")
    ),
      
    column(10, 
           
           #fluidRow(column(width=12), 
           fluidRow(
             column(width=2, "Enter data name here:"),
             column( width = 4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                     textInput(ns("derived_data_name"), value="derivedData-", label=NULL)),
             column( width = 2, #status = "primary",  #class = 'rightAlign',#background ="aqua",
                     actionButton(ns("saveDerivedPK"),h6("Save it"))), 
             column(width=2,  "Datasets currently stored:"), 
             column(width=2,  textOutput(ns("DataCount")))),  
           
           #fluidRow(column(width=12, uiOutput(ns("fig_title_selector")))),
           #fluidRow(column(width=8, textAreaInput(ns("fig_title"), label = NULL,  width='1000px', #'100%',
           #   value = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"))),
           
           
           uiOutput(ns('derivedPK_container'))
           
           #fluidRow(column(width=12, uiOutput(ns("fig_footnote_selector")))),
            
    )
    #)
  )
}



# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function
derivePKTab <- function(input, output, session, DATA) {
  
  ns <- session$ns
  setBookmarkExclude(c("table_search"))
  
  EPS = 1E-3
  
  
  # select which dataset(s)
  inputData <- reactive({
    if (is.null(input$data_name)) {return(NULL)}
    data = DATA$mDATA[[input$data_name]]
    data
  })
  #  
  
  #-----------------------------------------
  # on UI side
  #-----------------------------------------
  
  output$data_Selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = unique(names(DATA$mDATA))
    selectizeInput(ns("data_name"), 
                   label    = "select which dataset:" , 
                   choices  = data.lst,  #unique(inputData()%>%pull(TEST)),    
                   multiple = FALSE, 
                   
                   selected = data.lst[1])  #unique(inputData()%>%pull(TEST))[1])   
  })
   
  output$test_Selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    if (is.null(data$TEST)) {return(NULL)}
  
    
    test.lst = unique(data$TEST)
    
   # print("test_lst")
   # print(test.lst)
    
    selectizeInput(ns("test"), 
                   label    = "select which test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])  
  })
   
  
  output$group_by_selector <- renderUI({
    data = inputData()
    selectizeInput(ns("group_by"), label ="Group by",   #h5()
                choices = c(colnames(data)),  
                multiple = TRUE, 
                selected = c("STUDYID", "ARMA", "USUBJID", "TEST"))})    
  
  
  output$xvar_Selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    TIME = TIME_VARS(data)
    NTIM = NTIM_VARS(data)
    selectizeInput(ns("xvar"), 
                   label    = "select x variable:" , 
                   choices  =  colnames(data), 
                   multiple = FALSE, 
                   selected = ifelse(TIME!="", TIME, 
                                     ifelse(NTIM!="", NTIM, colnames(data)[1]))
    )
  })    
  
  
  
  #### yvar_Selector ####
  output$yvar_Selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    DVOR = DVOR_VARS(data)
    selectizeInput(ns("yvar"), 
                   label    = "select y variable:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="NULL", DVOR, colnames(data)[1]))
  })
  
 
  output$define_interval_by <- renderUI({
    #data = inputData()
    values=c("interval after the last dose", "interval defined by user")
    radioButtons(ns("define_interval_by"),        # radioButtons
                  label ="Define time interval by:", 
                  choices=values, 
                  selected=values[1])
  })
  
  
  output$start_time_selector <- renderUI({
    #data = inputData()
    if (input$define_interval_by!="interval defined by user") {return(NULL)}
    numericInput(ns("start_time"), label ="Start time", value=0, min = 0, max = 24*7, step=7)
    })    
  
  output$freq_selector <- renderUI({
    if (input$define_interval_by!="interval defined by user") {return(NULL)}
    #data = inputData()
    numericInput(ns("freq"), label ="How freqency", value=14, min = 0, max = 8*7, step=7)
  })  
  
  output$end_time_selector <- renderUI({
    if (input$define_interval_by!="interval defined by user") {return(NULL)}
    #data = inputData()
    numericInput(ns("end_time"), label ="End time", value=24*7, min = 0, max = 24*7, step=7)
  })    
  

  
  
  #####Start reactive ##### 
  values <- reactiveValues(simData=NULL)
  
  #construct a reative data object
  derivedPKTab <- reactive({
    data = derivePK(inputData(), input, session, EPS = 1e-3)
    
    #? have bugs in the line
    #data = data  %>% top_n(n=1,wt=EXSEQ) %>% filter(TAD<=LAST_II)
    
 
    cat(file=stderr(), "##############Step: derivedPKTab Data #################", "\n")      

    values$derivedPKTab = data    
    data 
    
  })
  
  
  
  
  # ## use renderUI to display table
  #https://rstudio.github.io/DT/
  output$derivedPK_container <- renderUI({
    
    output$derivedPKTab <- DT::renderDataTable(
      DT::datatable(data = derivedPKTab(),
                    options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE) 
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
    
    DT::dataTableOutput(ns("derivedPKTab"))
    
  })

  
  #add figure to log when action button is pressed
  observeEvent(input$saveDerivedPK, {
    #req(input$addToCart, FIGURE_ALL, input$fig_name)
    
    if(is.null(input$saveDerivedPK))  {return()}
    if(input$saveDerivedPK == 0) return()
    cat(file=stderr(), "##############Step: saveDerivedPK #################", "\n")
    
    # data 
    CurrentLog_mDATA   <- isolate(DATA$mDATA)
    newData <- values$derivedPKTab
    newData <- list(isolate(newData))
    data_name <- isolate(input$derived_data_name)  #### can be name re-used across modules?
    names(newData) = data_name
    
    # meta for data
    CurrentLog_mTITLES   <- isolate(DATA$mTITLES)
    newTitle <- list("DerivedPK")
    names(newTitle) = data_name
    
    if (!data_name %in% names(CurrentLog_mDATA)) {
      DATA$mTITLES[[data_name]] <- newTitle
      DATA$mDATA[[data_name]] <- newData[[data_name]]
      DATA$NumOfDataset <- isolate(DATA$NumOfDataset) + 1
      
      
    }else{ 
      cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
      #validate(
      #   need(!fig_name %in% names(CurrentLog_mFIGURES), "fig_name already exist, override will occur! ")
      #)
      
      DATA$mTITLES[[data_name]] <- newTitle
      DATA$mDATA[[data_name]] <- newData 
      
    }
    
  })
    
  
  #display number of tables stored
  output$DataCount <- renderText(
    
    DATA$NumOfDataset
  )
  
  
  # Return the reactive that yields the data frame
  return(DATA)
}
