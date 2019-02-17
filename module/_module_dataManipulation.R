
# %>% pull(StdName) # select(Name) %>% as.character()
# col.lst = adpx_checkin %>% filter( Column!="",  !is.na(Column)) %>% pull(Column) #
# col.lst = intersect(col.lst, colnames(adpx))
# if (length(col.lst)>0) {adpx[, std.col.lst] = adpx[, col.lst]}
# 
# #if not exist, filled with "."
# adpx[, adpx_checkin[which(is.na(adpx_checkin$Column)), "StdName"]] = "."

#mutate the data type  (need to check)
#adpx = convert_colType(adpx, colnames=adpx_checkin[, "Name"], types=as.character(adpx_checkin[, "Type"]))

#adpx = adpx %>% select_(adpx_checkin[, "Name"])
#adpx = adpx %>% filter(STUDYID!="", USUBJID!="")

# for R3918-HV-1659 only
# if (1==2) {
#   if ("Investigator Name" %in% colnames(adpx)) {
#     if ("Bush, Jim" %in% adpx[, "Investigator Name"]) {
#       adpx = fun.adpx.checkin(rawData$adpx(), 
#                               fun.adsl.checkin(rawData$adsl()))
#     }
#   } 
#   
#   if ("R3918-HV-1659" %in% adpx$STUDYID) {
#     adpx = adpx %>% select(STUDYID, ARMA, USUBJID,SUBJECT,
#                            TEST,STDUNIT,VISIT, TIMEPT,NTIM,TIME,SAMDTTM, BASE,DVOR,CHG,PCHG)
#   }
# }
##############################################################
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
##############################################################

dataManipulationUI <- function(id, label="") {
  # Create a namespace function using the provided id
  # tags$hr(style="border-color: black;")
  
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Load data and filter", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("data_selector")),
                        uiOutput(ns("test_selector")),
                        uiOutput(ns("arm_selector"))  
                       
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        uiOutput(ns("xvar_selector")),
                        uiOutput(ns("yvar_selector")),
                        uiOutput(ns("group_by_selector"))
                       
                 )), # box
                 
                 
             box(width=12, title="Derive what kind of table", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 column(6,
                        # main switch
                        uiOutput(ns("what2calc_selector"))
                 ), 
                 column(6,
                        # for calculating exposure
                      
                        
                        # for calculating statistics
                        uiOutput(ns("which_stats_selector")),
                         
                        # for calcualting quantile
                        uiOutput(ns("quantile_selector")), 
                        uiOutput(ns("by_quantile_selector")),
                        uiOutput(ns("by_breaks_selector"))
                       
                 ))# box
           ) # end of fluidRow
    ),  # end of left column
     
    column(8, 
         fluidRow(
           box(width=12, title="Derived table", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
               
               column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                      textInput(ns("tab_name"), value="Tab-", label=NULL,  
                                 placeholder = "Enter table name here:")),
               
               column(width=2, #align="left", offset = 0,
                      actionButton(ns("addToCart"), label="Add to cart", style=actionButton.style)), 
               
               column(width=2, #align="left", offset = 0,
                      uiOutput(ns("key_spreadOut_selector"))),
               
               column(width=2, #align="left", offset = 0,
                      uiOutput(ns("whichStats_spreadOut_selector"))),
               
               column(width=2, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                      checkboxInput(ns("spreadOut"), label ="?Spreadout", value=FALSE)),
              
               uiOutput(ns('derivedPK_container')))
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

##############################################################
# deriveTab
##############################################################

# Module server function
dataManipulation <- function(input, output, session, DATA, TABLE_ALL) {
  
  ns <- session$ns
  values <- reactiveValues(simData=NULL)
  EPS = 1E-3
   
  #-----------------------------------------
  # on UI side
  #-----------------------------------------
  
  # data_selector
  output$data_selector <- renderUI({
    
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("", unique(names(DATA$mDATA)))
    selectizeInput(ns("data_name"), 
                   label    = "Which dataset:" , 
                   choices  = data.lst,   
                   multiple = FALSE, 
                   selected = data.lst[1])    
  })
  
  # test_selector
  output$test_selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    if (is.null(data$TEST)) {return(NULL)}
    
    test.lst = c("", unique(data$TEST))
    selectizeInput(ns("test"), 
                   label    = "Filter by test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])  
  })
  
  # arm_selector 
  output$arm_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    
    checkboxGroupInput(ns("ARMA"), 
                       label="Filter by dose group:", 
                       choices = unique(data$ARMA),
                       selected =unique(data$ARMA))
    
  })
  
  # group_by_selector
  output$group_by_selector <- renderUI({
    data = filteredData()
    if (is.null(data)) {return(NULL)}
    
    col.lst = intersect(c("STUDYID", "ARMA", "TEST", "NTIM"), colnames(data))
    selectizeInput(ns("group_by"), label ="Group by",   #h5()
                   choices = c(colnames(data)),  
                   multiple = TRUE, 
                   selected = col.lst)})    
  
  # xvar_selector
  output$xvar_selector <- renderUI({
    data = filteredData()    
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
  
  # yvar_selector 
  output$yvar_selector <- renderUI({
    data = filteredData()    
    if (is.null(data)) {return(NULL)}
    
    DVOR = DVOR_VARS(data)
    selectizeInput(ns("yvar"), 
                   label    = "select y variable:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="NULL", DVOR, colnames(data)[1]))
  })
  
   
  
  # what2calc_selector
  #------------------------  
  output$what2calc_selector <- renderUI({
    radioButtons(ns("what2calc"), label = "What to calculate:",
                 choices = list( "meanProfile" = "meanProfile" 
                                #"quantile"="quantile"
                                ), 
                 selected = "meanProfile")
  })

   
  
  
  #--------------------------------------------------
  # which_stats_selector meanProfile
  #--------------------------------------------------
  output$which_stats_selector <- renderUI({
    validate(need(input$what2calc=="meanProfile", message=FALSE) 
             #need(input$define_interval_by=="interval defined by user", message=FALSE) 
   )
    
    stats.lst = c("N",      "Mean_SD" ,     "Mean_SE",  "Mean_CV",   "Median_Range", 
                  "Mean",         "Median",       "Min",          "Max",          "SD",           "SE" , 
                  "PCT97P5",  "PCT2P5",    "PCT95",  "PCT5",    "PCT90",        "PCT10") 
    
    checkboxGroupInput(ns("which_stats"),        # radioButtons
                       label ="Which stats wanted:",
                       choices=stats.lst,
                       selected= c("N",  "Mean_SD", "Mean_SE",  "Mean_CV",  "Median_Range" ))
  
})
  
  
  output$key_spreadOut_selector  <- renderUI({
    
    #data = derivedPKTab()
    validate(#need(data, message=FALSE), 
             need(input$group_by, message=FALSE),
             need(input$which_stats, message=FALSE)
             #need(input$define_interval_by=="interval defined by user", message=FALSE) 
    )
    
    col.lst = input$group_by
    value.lst = ifelse("TEST" %in% col.lst, "TEST", col.lst[1])
    
    selectizeInput(ns("key_spreadOut"), label= NULL, 
                   choices=col.lst, 
                   selected= value.lst,
                   multiple=FALSE)
  })
  
  
  
  output$whichStats_spreadOut_selector  <- renderUI({
  
    #data = derivedPKTab()
    validate(#need(data, message=FALSE), 
             need(input$which_stats, message=FALSE)
             #need(input$define_interval_by=="interval defined by user", message=FALSE) 
    )
  
    col.lst = input$which_stats
    value.lst = ifelse("Mean_SD" %in% col.lst, "Mean_SD", col.lst[1])
    
    selectizeInput(ns("whichStats_spreadOut"), label= NULL, 
                   choices=input$which_stats, 
                   selected=value.lst, 
                   multiple=FALSE)
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
  
  
   
  
  
  #####Start reactive ##### 

  
  #-----------------------------------------
  #  inputData
  #-----------------------------------------
  inputData <- reactive({
    
    validate(need(input$data_name, message=FALSE), 
             need(DATA$mDATA, message=FALSE)
             )

     data = DATA$mDATA[[input$data_name]]
    if (is.null(data)) {return(NULL)}
   
    
     #https://shiny.rstudio.com/articles/validation.html
     std.col.name.lst <-c("STUDYID", "ARMA", "USUBJID", "ID", "TIME","NTIM", "TEST", "DVOR", 
                          "WGTBL",  
                          "TAD", "EVID", "II",  "EXSEQ")
     
     # validate(
     #   need(all(std.col.name.lst %in% colnames(data)), 
     #        paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(data)), collapse=", "), " in ", input$data_name)
     #        )
     # )
    
    data
  })
  
  
  filteredData <- reactive({
    data = inputData()
    validate(need(data, message=FALSE), 
             need(input$ARMA, message=FALSE)
    )
     
    if (input$test!="") {data = data %>% filter(TEST %in% input$test)}
    if (input$ARMA!="") {data = data %>% filter(ARMA %in% input$ARMA)}
     
    
  })
  
  
  
  
  #construct a reative data object
  derivedPKTab <- reactive({
    
    data = filteredData()
   
    validate(need(input$what2calc, message="select what to calcualte"), 
             need(input$group_by, message="missing group_by"), 
             need(input$yvar, message="missing yvar"), 
            
             need(data, message="empty data")
    )
             
    
    if (input$what2calc == "meanProfile") {
      validate(need(input$which_stats, message="missing which_stats"), 
               need("USUBJID" %in% colnames(data), message= "need USUBJID in your dataset")
      )
      
        data <- data %>% calc_stats(id="USUBJID", group_by=input$group_by, value=input$yvar) 
        if (is.null(data)) {return(NULL)}
        col.lst = c(input$group_by, input$which_stats)
        data = data %>% select(one_of(col.lst))
        
        #
        if (input$spreadOut==1 ) {  #& all(c("N", "Mean_SD") %in% input$which_stats)) {
          validate(need(input$key_spreadOut, message=FALSE),
                   need(input$whichStats_spreadOut, message=FALSE)
          )
                   
          
          col.lst = c(input$group_by, c("N", input$whichStats_spreadOut))
          data = data %>% select(one_of(col.lst))
          
          data <- data %>% spread(key=input$key_spreadOut, 
                                  value=input$whichStats_spreadOut ) 
          
          }
        
 
    }
    
    if (input$what2calc == "quantile") {
      if (is.null(input$define_quantile_by)) {return(NULL)}
      validate(need(input$by_breaks, message="missing which_stats"))
      
      if (input$define_quantile_by=="breaks") {
          by_breaks =  tryCatch(eval(parse(text=paste0("c(", input$by_breaks, ")"))), 
                          error=function(e) {
                            print("not complete expression in quantile_by_breaks...."); 
                            return(NULL)
                          } )
          validate(need(by_breaks, message="Need by_breaks")) 
        
          data = data %>% group_by_(.dots = input$group_by) %>% 
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
        
        data = data %>% group_by_(.dots = input$group_by) %>% 
          assign_Q(value=input$yvar, quartile=by_quantile, breaks=NULL, include.lowest = FALSE, right = TRUE, ordered_result = TRUE) 
      }
      
    }
    
    
    cat(file=stderr(), "##############Step: derivedPKTab Data #################", "\n")      
    
    #values$derivedPKTab = data    
    data 
    
  })
  
  
  
  
  # ## use renderUI to display table
  #https://rstudio.github.io/DT/
  output$derivedPK_container <- renderUI({
    
    output$derivedPKTab <- DT::renderDataTable(
      DT::datatable(data = derivedPKTab(),
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
    
    DT::dataTableOutput(ns("derivedPKTab"))
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #add figure to log when action button is pressed
  observeEvent(input$addToCart, {
    #req(input$addToCart, TABLE_ALL, input$tab_name)
    
    if(is.null(input$addToCart))  {return()}
    if(input$addToCart == 0) return()
    cat(file=stderr(), "##############Step: tab add2Cart #################", "\n")
    
    
    #CurrentLog   <- TABLE_ALL$mTABLES  #####################
    CurrentLog_mTABLES   <- isolate(TABLE_ALL$mTABLES)
    
    
    
    newTab <-   derivedPKTab()   # errbarTabb() 
    #newTab <- (isolate(newTab))
    
    col.lst = c("STUDYID", "ARMA", "NTIM", "N", "Mean_SD", "Median_Range")
    col.lst = intersect(col.lst, colnames(newTab))  
    
    newTab <- newTab %>% ungroup() 
    #if (!is.null(newTab$NTIM)) {newTab <- newTab %>% select(STUDYID, ARMA, TEST, NTIM, N, Mean_SD, Median_Range)  }
    #if (is.null(newTab$NTIM)) {newTab <- newTab %>% select(STUDYID, ARMA, TEST, N, Mean_SD, Median_Range)  }
    
    tab_name <- isolate(input$tab_name)
    #names(newTab) = tab_name
    
    # CurrentLog_mTITLES   <- isolate(TABLE_ALL$mTITLES)
    # newTitle <- (input$tab_title)
    # names(newTitle) = tab_name
    # 
    # CurrentLog_mFOOTNOTES  <- isolate(TABLE_ALL$mFOOTNOTES)
    # newFootnote <- (input$tab_footnote)
    # names(newFootnote) = tab_name
    
    
    if (!tab_name %in% names(CurrentLog_mTABLES)) {
      #TABLE_ALL$mTITLES <- c(CurrentLog_mTITLES,newTitle)
      #TABLE_ALL$mTABLES <- c(CurrentLog_mTABLES,newTab)
      #TABLE_ALL$mFOOTNOTES <- c(CurrentLog_mFOOTNOTES,newFootnote)
      
      #TABLE_ALL$mTITLES[[tab_name]] <- newTitle
      TABLE_ALL$mTABLES[[tab_name]] <- newTab
      #TABLE_ALL$mFOOTNOTES[[tab_name]] <- newFootnote
      
      #TABLE_ALL$NumOfTab <- isolate(TABLE_ALL$NumOfTab) + 1
      
    }else{ 
      
      #TABLE_ALL$mTITLES[[tab_name]] <- newTitle
      TABLE_ALL$mTABLES[[tab_name]] <- newTab
      #TABLE_ALL$mFOOTNOTES[[tab_name]] <- newFootnote
      
      cat(file=stderr(), "##############Warning: tab_name exist, override will occur! #################", "\n")
      #validate(
      #   need(!tab_name %in% names(CurrentLog_mTABLES), "tab_name already exist, override will occur! ")
      #)
      
    }
    
  })
  
  
  
  
  
  
  # 
  # #add figure to log when action button is pressed
  # observeEvent(input$addToCart, {
  #   #req(input$addToCart, FIGURE_ALL, input$fig_name)
  #   
  #   if(is.null(input$addToCart))  {return()}
  #   if(input$addToCart == 0) return()
  #   cat(file=stderr(), "##############Step: addToCart #################", "\n")
  #   
  #   # data 
  #   CurrentLog_mDATA   <- isolate(DATA$mDATA)
  #   newData <-  derivedPKTab()  # values$derivedPKTab
  #   newData <- list(isolate(newData))
  #   data_name <- isolate(input$tab_name)  #### can be name re-used across modules?
  #   names(newData) = data_name
  #   
  #   # meta for data
  #   CurrentLog_mTITLES   <- isolate(DATA$mTITLES)
  #   newTitle <- list("DerivedPK")
  #   names(newTitle) = data_name
  #   
  #   if (!data_name %in% names(CurrentLog_mDATA)) {
  #     DATA$mTITLES[[data_name]] <- newTitle
  #     DATA$mDATA[[data_name]] <- newData[[data_name]]
  #     DATA$NumOfDataset <- isolate(DATA$NumOfDataset) + 1
  #     
  #     
  #   }else{ 
  #     cat(file=stderr(), "##############Warning: simDat name exist, override will occur! #################", "\n")
  #     #validate(
  #     #   need(!fig_name %in% names(CurrentLog_mFIGURES), "fig_name already exist, override will occur! ")
  #     #)
  #     
  #     DATA$mTITLES[[data_name]] <- newTitle
  #     DATA$mDATA[[data_name]] <- newData 
  #     
  #   }
  #   
  # })
  # 
   
  
  # output$dataCount_container <- renderValueBox({
  #   valueBox(
  #     DATA$NumOfDataset, "Progress", icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  
 
   
   
  
  # Return the reactive that yields the data frame
  return(DATA)
}
