 

################################################################################ 
################################################################################
# summaryTab
################################################################################
################################################################################

summaryTabUI <- function(id, label="") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Load data and filter", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("data_selector")),
                        uiOutput(ns("test_selector")),
                        tags$hr(style="border-color: black;"), 
                        
                        uiOutput(ns("dose_group_selector"))
                        
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        uiOutput(ns("summarise_by_selector")), 
                        uiOutput(ns("valueCol_selector")), 
                        tags$hr(style="border-color: black;")
                        
                 )) # box
             
             
              
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Derived data", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        textInput(ns("tab_name"), value="Tab-", label=NULL,  
                                  placeholder = "Enter data name here:")),
                 
                 column(width=4, #align="left", offset = 0,
                        actionButton(ns("addToCart"), label="Add to cart", style=actionButton.style)), 
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        checkboxInput(ns("spreadOut"), label ="?Spreadout", value=FALSE)),
                 
                 uiOutput(ns('summaryTab_container')))
           )
    )
  )
  
  
}



# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):


# Module server function

################################################################################ 
################################################################################
# summaryTab
################################################################################
################################################################################

summaryTab <- function(input, output, session, DATA, TABLE_ALL) {
  
  ns <- session$ns
  EPS = 1E-3
  
  values <- reactiveValues()
  print("in summaryTab::::::")
  
  
  #-----------------------------------------
  # data_selector
  #-----------------------------------------
  output$data_selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("NULL", unique(names(DATA$mDATA)))
    selectizeInput(ns("data_name"), 
                   label    = "select which dataset:" , 
                   choices  = data.lst,  #unique(inputData()%>%pull(TEST)),    
                   multiple = FALSE, 
                   
                   selected = data.lst[1])  #unique(inputData()%>%pull(TEST))[1])   
  })
  
  
  #-----------------------------------------
  # test_selector
  #-----------------------------------------
  output$test_selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    test.lst = c("", unique(data$TEST))
    selectizeInput(ns("test"), 
                   label    = "select which test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])  
  })
  
  #-----------------------------------------
  # summarise_by_selector
  #-----------------------------------------
  output$summarise_by_selector <- renderUI({
    data = inputData()
    
    TEST_VARS = "TEST"
    STUDYID_VARS = "STUDYID"
    ARMA_VARS = "ARMA"
    NTIM_VARS = ifelse(NTIM_VARS(data)!="", NTIM_VARS(data),
                       ifelse(TIME_VARS(data)!="", TIME_VARS(data), ""))
    selectizeInput(ns("summarise_by"), label ="Summarise by",   #h5()
                   choices = c(colnames(data)), 
                   multiple = TRUE,
                   selected = c(STUDYID_VARS, ARMA_VARS, NTIM_VARS, TEST_VARS))
  })
  
  
  #-----------------------------------------
  # valueCol_selector
  #-----------------------------------------
  output$valueCol_selector <- renderUI({
    data = inputData()
    
    DVOR = DVOR_VARS(data)
    selectizeInput(ns("valueCol"), 
                   label    = "select valueCol:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="NULL", DVOR, colnames(data)[1]))
  }) 
  
  #-----------------------------------------
  # group_by_selector
  #-----------------------------------------
  output$group_by_selector <- renderUI({
    data = errbarTabb()
    selectizeInput(ns("group_by"), label ="Group by",   #h5()
                   choices = c(colnames(data)), 
                   multiple = TRUE,
                   selected = c("ARMA"))
  })
  
  
  #-----------------------------------------
  # dose_group_selector
  #-----------------------------------------
  output$dose_group_selector <- renderUI({
    data = inputData()
    if(is.null(data)) {return(NULL)}
    if(is.null(data$ARMA)) {return(NULL)}
    
    checkboxGroupInput(ns("dose_group"), label="Filter by dose groups", 
                       choices = unique(as.character(data$ARMA)), 
                       selected= unique(as.character(data$ARMA))    )})
  
  
  
  #-----------------------------------------
  # tab_title_selector
  #-----------------------------------------
  #value = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day 
  #Following a Single IV Dose of xxxx (Study xxxx-HV-1219"))),
  output$tab_title_selector <- renderUI({
    
    data = inputData()
    patient.name = "Healthy Volunteers"
    dosing.name = paste0("a Single Subcutaneous or Intravenous Dose(s)")
    drug.name = substr(data$STUDYID, 1, 5)
    study.name = unique(data$STUDYID)
    
    
    
    tab.title = paste("Summary of", input$test, "by", paste(input$summarise_by, collapse=", "),
                      "in Study", study.name, sep=" ")
    
    
    #tab.title = "tabure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"
    textAreaInput(ns("tab_title"), label = NULL,  width='1040px', value = tab.title)})
  
  
  
  #-----------------------------------------
  # tab_footnote_selector
  #-----------------------------------------
  #"Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  #footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  output$tab_footnote_selector <- renderUI({
    textAreaInput(ns("tab_footnote"), label = NULL,  width='1040px',
                  value = "Note: QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Concentrations below the lower limit of quantification (LLOQ) are set to zero. ")})
  
  
  
  
  #***************************************** 
  # inputData
  #*****************************************
  # select which dataset(s)
  inputData <- reactive({
    
    if (is.null(input$data_name)) {return(NULL)}
    if (input$data_name=="NULL") {return(NULL)}
    
    data = DATA$mDATA[[input$data_name]]
    
    if (is.null(data)) {return(NULL)}
    if (is.null(data$TEST)) {data$TEST= "xxxx"} 
    
    data
  })
  
  
  #***************************************** 
  # errbarTabb
  #***************************************** 
  
  # select which dataset(s)
  errbarTabb <- reactive({
     
    data = inputData() 
    validate(need(data, message=FALSE), 
             need(input$dose_group, message=FALSE), 
             need(input$test, message=FALSE), 
             need(input$valueCol, message=FALSE)
    )
    
    data <- data %>% filter(ARMA %in% input$dose_group) 
    
    if (is.null(data)) {return(NULL)}
    if (is.null(input$test)) {return(NULL)}    
    
    #data = errbarTab8(data, input, session, EPS = 1e-3)  %>% select_(  "ARMA", "NTIM", "N", "Mean_SD", "Mean_SE", "Median_Range")
    if(!is.null(data$ARMA) & !is.null(input$dose_group)) {data = data %>% filter(ARMA %in% input$dose_group) }
    
    data = data %>% filter(TEST %in% input$test) 
    
    
     
    validate(need(input$summarise_by, message="missing summarise_by"), 
             need(input$valueCol, message="missing valueCol"), 
             need("USUBJID" %in% colnames(data), message= "missing USUBJID in colnames(data)")
    )
    
    pkSummary = data %>% calc_stats(id="USUBJID", group_by=input$summarise_by, value=input$valueCol)  %>% 
                 select(one_of(input$summarise_by), N, Mean, SD, Mean_SD)   # calculate the statistics
    
    
    
    pkSummary
    # print(head(data)%>% as.data.frame())
    # print(colnames(data))
    # 
    # col.lst = c("STUDYID", "ARMA", "NTIM", "N", "Mean_SD", "Mean_SE", "Median_Range")
    # col.lst = intersect(col.lst, colnames(data))
    # print(col.lst)
    # 
    # data %>% select_(col.lst)
    #print(head(data) %>% as.data.frame())
    
  })
  #
  
  
  
  
  # ## use renderUI to display table
  #https://rstudio.github.io/DT/
  output$summaryTab_container <- renderUI({
    pkSummary <- errbarTabb()
    if (input$spreadOut==1) {pkSummary <- pkSummary %>% select(-Mean, -SD) %>% spread(key=TEST, value=Mean_SD) }
    
    output$summaryTab <- DT::renderDataTable(
      DT::datatable(data = pkSummary,
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
    
    DT::dataTableOutput(ns("summaryTab"))
    
  })
  
  
    
  
  #add figure to log when action button is pressed
  observeEvent(input$addToCart, {
    #req(input$addToCart, TABLE_ALL, input$tab_name)
    
    if(is.null(input$addToCart))  {return()}
    if(input$addToCart == 0) return()
    cat(file=stderr(), "##############Step: tab add2Cart #################", "\n")
    
    
    #CurrentLog   <- TABLE_ALL$mTABLES  #####################
    CurrentLog_mTABLES   <- isolate(TABLE_ALL$mTABLES)
    
    
    
    newTab <-  errbarTabb() 
    #newTab <- (isolate(newTab))
    
    col.lst = c("STUDYID", "ARMA", "NTIM", "N", "Mean_SD", "Median_Range")
    col.lst = intersect(col.lst, colnames(newTab))  
    
    newTab <- newTab %>% ungroup() 
    #if (!is.null(newTab$NTIM)) {newTab <- newTab %>% select(STUDYID, ARMA, TEST, NTIM, N, Mean_SD, Median_Range)  }
    #if (is.null(newTab$NTIM)) {newTab <- newTab %>% select(STUDYID, ARMA, TEST, N, Mean_SD, Median_Range)  }
    
    tab_name <- isolate(input$tab_name)
    #names(newTab) = tab_name
    
    CurrentLog_mTITLES   <- isolate(TABLE_ALL$mTITLES)
    newTitle <- (input$tab_title)
    names(newTitle) = tab_name
    
    CurrentLog_mFOOTNOTES  <- isolate(TABLE_ALL$mFOOTNOTES)
    newFootnote <- (input$tab_footnote)
    names(newFootnote) = tab_name
    
    
    if (!tab_name %in% names(CurrentLog_mTABLES)) {
      #TABLE_ALL$mTITLES <- c(CurrentLog_mTITLES,newTitle)
      #TABLE_ALL$mTABLES <- c(CurrentLog_mTABLES,newTab)
      #TABLE_ALL$mFOOTNOTES <- c(CurrentLog_mFOOTNOTES,newFootnote)
      
      TABLE_ALL$mTITLES[[tab_name]] <- newTitle
      TABLE_ALL$mTABLES[[tab_name]] <- newTab
      TABLE_ALL$mFOOTNOTES[[tab_name]] <- newFootnote
      
      TABLE_ALL$NumOfTab <- isolate(TABLE_ALL$NumOfTab) + 1
      
    }else{ 
      
      TABLE_ALL$mTITLES[[tab_name]] <- newTitle
      TABLE_ALL$mTABLES[[tab_name]] <- newTab
      TABLE_ALL$mFOOTNOTES[[tab_name]] <- newFootnote
      
      cat(file=stderr(), "##############Warning: tab_name exist, override will occur! #################", "\n")
      #validate(
      #   need(!tab_name %in% names(CurrentLog_mTABLES), "tab_name already exist, override will occur! ")
      #)
      
    }
    
  })
  
   
  
  # Return the reactive that yields the data frame
  return(TABLE_ALL)
}
