
################################################################################ 
################################################################################
# checkInRowsUI
################################################################################ 
################################################################################

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI 
#-----------------------------------------

deriveDataUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12,   title="Derive data", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status =  "primary" ,
                 
                 column(width=4,
                      uiOutput(ns("data_selector"))
                 ), 
                 
                 # column(width=12,
                 #        uiOutput(ns("col_selector"))
                 # ),
                 
                 tabBox(width=12, id = ns("deriveApproach"), title =NULL, # "Loading Dose",
                        
                        tabPanel(width=12, title="Auto", value = "Auto", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
                                 
                            fluidRow(  
                                 column(6,
                                        # main switch
                                        uiOutput(ns("what2calc_selector"))
                                 ), 
                                 column(6, 
                                        # for calculating statistics
                                        uiOutput(ns("id_selector")),
                                        uiOutput(ns("group_by_selector")),
                                        uiOutput(ns("value_selector")),
                                        uiOutput(ns("which_stats_selector")),
                                        
                                        # for calcualting quantile
                                        uiOutput(ns("quantile_selector")), 
                                        uiOutput(ns("by_quantile_selector")),
                                        uiOutput(ns("by_breaks_selector"))
                                        
                                 )
                            ) 
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
             box(width=12, title="Derived data", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,  
                  
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        textInput(ns("data_name4Save"), value="alignedData-", label=NULL,  
                                  placeholder = "Enter data name here:")),
                 
                 column(width=4, #align="left", offset = 0,
                        actionButton(ns("saveData"), label="Save it", style=actionButton.style)), 
                 
                 uiOutput(ns("derivedTab_container"))
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


deriveData <- function(input, output, session, DATA) {
  
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
                   label    =  "Which dataset" , 
                   choices  = data.lst,   
                   multiple = FALSE, 
                   selected = data.lst[1])    
  })
  
  #------------------------
  # what2calc_selector
  #------------------------  
  output$what2calc_selector <- renderUI({
    radioButtons(ns("what2calc"), label = "What to calculate:",
                 choices = list(#"Cmin/Cmax/AUC" = "exposure" 
                                "meanProfile" = "meanProfile", 
                                "quantile"="quantile"), 
                 selected = "meanProfile")
  })
  
  
  #----------------
  # col_selector
  #----------------
  output$col_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    #if (mod(input$runScript,2)) {return(NULL)}
    
    col.lst = c("", colnames(data))
 
    selectizeInput(ns("col_name"), 
                  label    = "Select column:" , 
                  choices  = col.lst,   
                  multiple = FALSE, 
                  selected = col.lst[1])
   
  })
  
   
  # for calculate statistics
  #---------------------------------
   
  
  # id_selector
  output$id_selector <- renderUI({
    data = inputData()    
    validate(need(data, message=FALSE), 
             need(input$what2calc=="meanProfile", message=FALSE) 
    )
     
    selectizeInput(ns("id"), 
                   label    = "select id variable:" , 
                   choices  = colnames(data), 
                   multiple = FALSE, 
                   selected = ifelse("USUBJID" %in% colnames(data), "USUBJID", colnames(data)[1])
    )
  })    
  
  # group_by_selector
  output$group_by_selector <- renderUI({
    data = inputData()
    validate(need(data, message=FALSE), 
             need(input$what2calc=="meanProfile", message=FALSE) 
    )
    
    col.lst = intersect(c("STUDYID", "ARMA", "TEST", "NTIM"), colnames(data))
    selectizeInput(ns("group_by"), label ="Group by",   #h5()
                   choices = c(colnames(data)),  
                   multiple = TRUE, 
                   selected = col.lst)}) 
  
  # value_selector 
  output$value_selector <- renderUI({
    data = inputData()    
    validate(need(data, message=FALSE), 
             need(input$what2calc=="meanProfile", message=FALSE) 
    )
     
    selectizeInput(ns("value"), 
                   label    = "select value variable:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse("DVOR" %in% colnames(data), "DVOR", colnames(data)[1])
    )
    
  })
  
  
  #--------------------------------------------------
  # which_stats_selector meanProfile
  #--------------------------------------------------
  output$which_stats_selector <- renderUI({
    validate(need(input$what2calc=="meanProfile", message=FALSE) 
             #need(input$define_interval_by=="interval defined by user", message=FALSE) 
    )
    
    stats.lst = c("N",      "Mean_SD" ,    "Mean",      "Median",  "Min",  "Max",  "SD",  "SE" , 
                  "PCT97P5",  "PCT2P5",    "PCT95",  "PCT5",    "PCT90",  "PCT10", 
                  "Mean_SE",  "Mean_CV",   "Median_Range") 
    
    checkboxGroupInput(ns("which_stats"),        # radioButtons
                       label ="Which stats wanted:",
                       choices=stats.lst,
                       selected= c("N",  "Mean", "Median", "SD", "SE", 
                                   "PCT97P5",  "PCT2P5",    "PCT95",  "PCT5",    "PCT90",        "PCT10"))
    
  })
  
  
  #--------------------------------------------------
  # quantile_selector quantile
  #--------------------------------------------------
  output$quantile_selector <- renderUI({
    validate(
      need(input$what2calc=="quantile", message=FALSE)
      #need(input$define_interval_by=="interval defined by user", message=FALSE)
    )  
    
    # print("why not come here?")
    # print(input$what2calc)
    # 
    radioButtons(ns("define_quantile_by"),   inline=TRUE,     # radioButtons
                 label ="Cut interval by:", 
                 choices=c("quantile", "breaks"), 
                 selected="quantile")
    
  })
  
  output$by_quantile_selector <- renderUI({
    # print("what2calc +define_quantile_by1 ")
    # print(input$what2calc)
    # print(input$define_quantile_by)
    # 
    validate(   
      need(input$what2calc=="quantile", message=FALSE),
      need(input$define_quantile_by=="quantile", message=FALSE)
    ) 
    textInput(ns("by_quantile"), value="0, 0.25, 0.5, 0.75, 1.0", label=NULL)
    
    
  })
  
  output$by_breaks_selector <- renderUI({
    # print("what2calc +define_quantile_by ")
    # print(input$what2calc)
    # print(input$define_quantile_by)
    # 
    validate(
      need(input$what2calc=="quantile", message=FALSE),
      need(input$define_quantile_by=="breaks", message=FALSE)
    ) 
    
    textInput(ns("by_breaks"), value="", label=NULL, placeholder = "60, 90, 150")
    
  })
  
  
   
  #------------------------- 
  # scriptArea_container
  #------------------------- 
  output$scriptArea_container <-renderUI({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    textAreaInput(ns("scriptArea"), label=NULL, value=NULL, rows=25, placeholder = "data %>% separate(timept)")
    
  })
  
   
  
  
  
  
  
  # ## use renderUI to display table
  #https://rstudio.github.io/DT/
  output$derivedTab_container <- renderUI({
    
    output$derivedTab <- DT::renderDataTable(
      DT::datatable(data = derivedTab(),
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
    
    DT::dataTableOutput(ns("derivedTab"))
    
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
     
    data
  })
  
  
  #-------------------------------------------------------
  # in default, use data_name4Select as data_name4Save
  #-------------------------------------------------------
  observeEvent(inputData(), {
    validate(need(input$data_name4Select, message=FALSE))
    
    updateTextInput(session, "data_name4Save", value=input$data_name4Select)
  })
  
   
  calcMeanProfile <- reactive({
    data <- inputData() 
    validate(need(data, message=FALSE), 
             need(input$id, message=FALSE), 
             need(input$group_by, message=FALSE), 
             need(input$value, message=FALSE)
    )
    
    data <- data %>% calc_stats(id=input$id, group_by=input$group_by, value=input$value) 
    if (is.null(data)) {return(NULL)}
    col.lst = c(input$group_by, input$which_stats)
    data = data %>% select(one_of(col.lst))
    data
    
  })
  
  calcQuantile <- reactive({
    if (is.null(input$define_quantile_by)) {return(NULL)}
    
    if (input$define_quantile_by=="breaks") {
      by_breaks =  tryCatch(eval(parse(text=paste0("c(", input$by_breaks, ")"))), 
                            error=function(e) {
                              print("not complete expression in quantile_by_breaks...."); 
                              return(NULL)
                            } )
      validate(need(by_breaks, message="Need by_breaks")) 
      
      data = filteredData() %>% group_by_(.dots = input$group_by) %>% 
        assign_Q(value=input$yvar, quartile=NULL, breaks=by_breaks, include.lowest = FALSE, right = TRUE, ordered_result = TRUE) 
    }
    
    
    #data %>% group_by( STUDYID, ARMA) %>% assign_Q(value="DVOR")
    #-----------------------------------------------------------------
    if (input$define_quantile_by=="quantile") {
      by_quantile =  tryCatch(eval(parse(text=paste0("c(", input$by_quantile, ")"))),  
                              error=function(e) {
                                print("not complete expression in quantile_by_quantile...."); 
                                return(NULL)
                              } )
      validate(need(by_quantile, message="Need by_quantile")) 
      
      data = filteredData() %>% group_by_(.dots = input$group_by) %>% 
        assign_Q(value=input$yvar, quartile=by_quantile, breaks=NULL, include.lowest = FALSE, right = TRUE, ordered_result = TRUE) 
    }
    
    data
    
  })
  
  
  #-------------------------------
  # if runScript
  #-------------------------------
  deriveByScript <- reactive({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    
    data <- inputData()  # 
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
  
   
  
  
  #construct a reative data object
  derivedTab <- reactive({ 
    data = NULL
    validate(need(input$deriveApproach, message=FALSE),
             need(input$what2calc, message=FALSE)
             
    )
    
    if(input$deriveApproach == "Auto") {
      if(input$what2calc == "meanProfile") {data = calcMeanProfile()}
      if(input$what2calc == "quantile") {data = calcQuantile()}
    }
    
    if(input$deriveApproach == "Script") {data = deriveByScript()}
    
    data
    
  })
  
    
  
  #------------------------------------------------- 
  # add data to DATA when action button is pressed
  #-------------------------------------------------
  observeEvent(input$saveData, {
    validate(need(input$data_name4Select, message=FALSE),
             need(input$data_name4Save, message=FALSE), 
             need(derivedTab(), message=FALSE)
             #need(is.null(errstatus()), message="Fix the warning before proceed") 
             #need(!"error" %in% (errstatus() %>% pull(type)), message="Fix the error before proceed") 
    ) 
    
    newData <- isolate(derivedTab())
    data_name = isolate(input$data_name4Save)
    DATA = addNewData(newData, data_name, DATA)
    
  })
  
  
  # Return DATA
  return(DATA)
  
}
