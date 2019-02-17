
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

deriveTabUI <- function(id, label="") {
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
                 
                 
             box(width=12, title="Derive what metric", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 column(6,
                        # main switch
                        uiOutput(ns("what2calc_selector"))
                 ), 
                 column(6,
                        # for calculating exposure
                        uiOutput(ns("define_interval_by")),
                        uiOutput(ns("start_time_selector")),
                        uiOutput(ns("freq_selector")),           
                        uiOutput(ns("end_time_selector"))
                        
                        # for calculating statistics
                        #uiOutput(ns("which_stats_selector")),
                         
                        # for calcualting quantile
                        #uiOutput(ns("quantile_selector")), 
                        #uiOutput(ns("by_quantile_selector")),
                        #uiOutput(ns("by_breaks_selector"))
                       
                 ))# box
           ) # end of fluidRow
    ),  # end of left column
     
    column(8, 
         fluidRow(
           box(width=12, title="Derived data", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
               
               column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                      textInput(ns("derived_data_name"), value="derivedData-", label=NULL,  
                                 placeholder = "Enter data name here:")),
               
               column(width=4, #align="left", offset = 0,
                      actionButton(ns("saveDerivedPK"), label="Save it", style=actionButton.style)), 
              
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
deriveTab <- function(input, output, session, DATA) {
  
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
    
    test.lst = unique(data$TEST)
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
    
    col.lst = intersect(c("STUDYID", "USUBJID", "ARMA", "TEST"), colnames(data))
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
                 choices = list("Cmin/Cmax/AUC" = "exposure" 
                                #"meanProfile" = "meanProfile", 
                                #"quantile"="quantile"), 
                 ), 
                 selected = "exposure")
  })

  
  #--------------------------------------------------
  # Exposure defined by which
  #--------------------------------------------------
  output$define_interval_by <- renderUI({
    validate(need(input$what2calc=="exposure", message=FALSE))
    
    values=c("interval after the last dose", "interval defined by user")
    radioButtons(ns("define_interval_by"),        # radioButtons
                 label ="Define time interval by:", 
                 choices=values, 
                 selected=values[2])
  })
  
  # Exposure defined by start_time, freq, end_time
  #--------------------------------------------------
  output$start_time_selector <- renderUI({
    validate(need(input$what2calc=="exposure", message=FALSE), 
             need(input$define_interval_by=="interval defined by user", message=FALSE) 
             )
 
    numericInput(ns("start_time"), label ="Start time", value=84, min = 0, max = 48*7, step=7)
  })    
  
  output$freq_selector <- renderUI({
    validate(need(input$what2calc=="exposure", message=FALSE), 
             need(input$define_interval_by=="interval defined by user", message=FALSE) 
    )
     
    numericInput(ns("freq"), label ="How freqency", value=84, min = 0, max = 24*7, step=7)
  })  
  
  output$end_time_selector <- renderUI({
    validate(need(input$what2calc=="exposure", message=FALSE), 
             need(input$define_interval_by=="interval defined by user", message=FALSE) 
    )
     
    numericInput(ns("end_time"), label ="End time", value=24*7, min = 0, max = 48*7, step=7)
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
  
  
  
  derivePK <- function(data, input, session, EPS = 1e-3) { 
    
    # input = NULL
    # input$start_time = 0
    # input$end_time = 112
    # input$freq= 14
    # 
    # input$group_by= c("STUDYID", "ARMA", "USUBJID")
    # input$xvar = "TIME"
    # input$yvar = "DVOR"
    
    # data = data.frame(TIME=seq(0, 112, by=1), 
    #                  DVOR = seq(100, 212, by=1), 
    #                  STUDYID = "xxx", 
    #                  ARMA = "XXX", 
    #                  USUBJID = "xx")
    
    if (is.null(input$xvar)) {return(NULL)}
    if (is.null(input$yvar)) {return(NULL)}  
    if (is.null(input$define_interval_by))  {return(NULL)}
    
    if (!input$xvar %in% colnames(data))  {return(NULL)}
    if (!input$yvar %in% colnames(data))  {return(NULL)}
    
    validate(need(input$group_by, message=FALSE), 
             need(input$start_time, message=FALSE), 
             need(input$end_time, message=FALSE), 
             need(input$freq, message=FALSE), 
             need(input$define_interval_by, message=FALSE) 
             )
    
    data = data %>% mutate_(NTIM=input$xvar, DVOR=input$yvar) %>% 
      mutate(NTIM=as_numeric(NTIM), DVOR=as_numeric(DVOR))  
    
    
    group_by = input$group_by
    if (input$define_interval_by=="interval defined by user") {
      leftover = (input$end_time-input$start_time) %% input$freq
      if (leftover!=0) {print("(end_time - start_time) %% freq should be zero [derivePK.R].")}
      stopifnot(leftover==0)
      
      # assign quartile for each time interval
      data = data %>% filter(NTIM>input$start_time, NTIM<=input$end_time)   # NOTE  > AND <=
      tseq = seq(input$start_time, input$end_time, input$freq)
      data = assign_Q(data, value="NTIM", quartile=NULL, breaks=tseq, include.lowest = FALSE, right = TRUE, ordered_result = TRUE) 
      group_by = c(input$group_by, "Quantile", "Q_LABEL")
      
    }
    
    # filter out the last dosing interval
    if (input$define_interval_by=="interval after the last dose") {
      data = data  %>%  
               group_by_(.dots =c(group_by )) %>%  
               top_n(n=1,wt=EXSEQ) %>% filter(TAD<=II)
      #group_by = c(input$group_by)  # , "Quantile", "Q_LABEL"
      
    }
    
    # calculate the PK parameter for each subject and each time interval
    out = data  %>% 
      group_by_(.dots =c(group_by )) %>%   
      dplyr::summarise(
        #CMIN = round(min(DVOR),digits=3),    # Note the infusion hours, could be 5 min to 2 hr, typically.
        CMIN = round(DVOR[n()],digits=3),   # the last conc at this time interval
        CMAX = round(max(DVOR),digits=3),       
        AUCtau = round(auc_partial(NTIM, DVOR),digits=3)    # , data%>%pull(input$yvar))   #, range=data%>%range_(input$xvar)
      )  
 
      out = gather(out%>% ungroup()  %>% mutate(EXTRT=TEST) %>% select(-TEST), 
                   key=TEST, value=DVOR, -one_of(c(group_by,"EXTRT")))
      
   
    #-CMIN.SS, -CMAX.SS, -AUCtau.SS, -PCT.CMIN, -PCT.CMAX, -PCT.AUCtau) %>% head()
    
    # calcualte the percentage of state getting close to steady-state 
    # out = out %>% left_join(out %>% 
    #                   group_by_(.dots =input$group_by) %>% slice(n()) %>% 
    #                   mutate(CMIN.SS=CMIN, CMAX.SS=CMAX, AUCtau.SS=AUCtau) %>% ungroup() %>%
    #                   select(USUBJID, CMIN.SS, CMAX.SS, AUCtau.SS), 
    #               by="USUBJID")   %>% 
    #        mutate(PCT.CMIN=paste0(round(CMIN/CMIN.SS*100, digits=3), "%"), 
    #               PCT.CMAX=paste0(round(CMAX/CMAX.SS*100, digits=3),"%"),
    #               PCT.AUCtau=paste0(round(AUCtau/AUCtau.SS*100, digits=3), "%")
    #        )
    
    
    return(out)
  } 
  
  
  
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
    if (is.null(data$TEST)) {data$TEST= "Unknown"} 

    
     #https://shiny.rstudio.com/articles/validation.html
     std.col.name.lst <-c("STUDYID", "ARMA", "USUBJID", "ID", "TIME","NTIM", "TEST", "DVOR", 
                          "WGTBL",  
                          "TAD", "EVID", "II",  "EXSEQ")
     
     validate(
       need(all(std.col.name.lst %in% colnames(data)), 
            "Please use simulated data only"
            #paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(data)), collapse=", "), " in ", input$data_name)
            )
     )
    
    data
  })
  
  
  filteredData <- reactive({
    data = inputData()
    validate(need(data, message=FALSE))
     
    
    data = data %>% filter(TEST %in% input$test, 
                           ARMA %in% input$ARMA)
    
  })
  
  
  
  
  #construct a reative data object
  derivedPKTab <- reactive({
 
    data = NULL
    
    if (input$what2calc == "exposure") {
       data = derivePK(filteredData(), input, session, EPS = 1e-3)
    }
    
    if (input$what2calc == "meanProfile") {
        data <- filteredData() %>% calc_stats(id="USUBJID", group_by=input$group_by, value=input$yvar) 
        if (is.null(data)) {return(NULL)}
        col.lst = c(input$group_by, input$which_stats)
        data = data %>% select(one_of(col.lst))
 
    }
    
    if (input$what2calc == "quantile") {
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
      
    }
    
    
    cat(file=stderr(), "##############Step: derivedPKTab Data #################", "\n")      
    
    values$derivedPKTab = data    
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
  
   
  
  # output$dataCount_container <- renderValueBox({
  #   valueBox(
  #     DATA$NumOfDataset, "Progress", icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  
 
   
   
  
  # Return the reactive that yields the data frame
  return(DATA)
}
