
 
########################################################### 
# xyplotUI
########################################################### 
  
xyplotUI <- function(id, label="") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Load data and filter", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        uiOutput(ns("data_selector")),
                        uiOutput(ns("test_selector")),
                        uiOutput(ns("dose_group_selector")),
                        
                        actionButton(
                          inputId= (ns("addFilterModule")),
                          label = "Add More Filter",
                          #class = "btn-primary"   # http://getbootstrap.com/docs/4.0/components/buttons/
                          style = actionButton.style
                        ), 
                        # actionButton(
                        #   inputId= (ns("removeBtn")),
                        #   label = "Remove Filter",
                        #   class = "btn-primary"   # http://getbootstrap.com/docs/4.0/components/buttons/
                        # ), 
                        
                        tags$div(id = 'placeholder')
                 ),
                      #  tags$hr(style="border-color: black;")
                 
                 column(6,     
                        uiOutput(ns("facet_by_selector")),
                        uiOutput(ns("facet_scale_selector")),
                        
                        uiOutput(ns("color_by_selector")),
                        uiOutput(ns("shape_by_selector")),
                        uiOutput(ns("group_by_selector")), 
                 
                        uiOutput(ns("errbar_or_CI_selector")),
                        uiOutput(ns("errbar_selector")),
                        uiOutput(ns("CI_selector")))
                 ),
                        
             box(width=12, title="Visualization", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 
                 
                 tabBox(width=12, id = ns("ggplot"), title =NULL, # "Loading Dose",
                        tabPanel(width=12, title="GUI-ggplot", value="GUI-ggplot", collapsible = TRUE, collapsed = FALSE, 
                                  
                           column(6, 
                                  uiOutput(ns("xvar_selector")),
                                  uiOutput(ns("xlim_selector")),
                                  uiOutput(ns("xtick_selector")),    # ticks
                                  uiOutput(ns("xscale_selector")),   # log, nonlog
                                  uiOutput(ns("xlab_selector")),
                                  uiOutput(ns("xrefline_selector")),
                                  tags$hr(style="border-color: black;"),
                                  
                                  uiOutput(ns("addline_selector"))
                           ),
                           
                           column(6, 
                                  uiOutput(ns("yvar_selector")),
                                  uiOutput(ns("ylim_selector")),
                                  uiOutput(ns("ytick_selector")),   # ticks
                                  uiOutput(ns("yscale_selector")),   # log, nonlog
                                  uiOutput(ns("ylab_selector")), 
                                  uiOutput(ns("yrefline_selector")), 
                                  tags$hr(style="border-color: black;"), 
                                  
                                  uiOutput(ns("plotType_selector")),    
                                  uiOutput(ns("fontSize_selector")), 
                                  
                                  uiOutput(ns("fig_width_selector")),    
                                  uiOutput(ns("fig_height_selector")) 
                           )
                        ), 
                        
                        tabPanel(width=12, title="Script-ggplot", value="Script-ggplot", collapsible = TRUE, collapsed = FALSE, 
                                 uiOutput(ns("scriptArea_container"))
                        )
                 )),
             
             box(width=12, collapsible = TRUE, collapsed = TRUE,  solidHeader = TRUE, 
                 title = "Overlay a secondary dataset", status = "primary",       # "warning",
                 #"Box content here", br(), "More box content",
                 fluidRow(
                   column(6,
                          uiOutput(ns("data2_selector")),
                          uiOutput(ns("test2_selector")),
                          uiOutput(ns("plotType2_selector")), 
                          uiOutput(ns("point.size2_selector")), 
                          uiOutput(ns("line.lwd2_selector")) 
                          
                           ),
                   column(6,
                          uiOutput(ns("xvar2_selector")),
                          uiOutput(ns("yvar2_selector")), 
                          
                          uiOutput(ns("dose_group2_selector")) 
                           
                   )
                 ) 
             )
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="XYPlot:", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,
                 #fluidRow(column(width=12), 
   
                 column(width = 4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                         textInput(ns("fig_name"), value="Fig", label=NULL)),
                 
                 column(width = 4, #status = "primary",  #class = 'rightAlign',#background ="aqua",
                         actionButton(ns("addToCart"),"add to cart", style=actionButton.style)), 
                    
                 fluidRow(column(width=12, uiOutput(ns("fig_title_selector")))),
                 #fluidRow(column(width=8, textAreaInput(ns("fig_title"), label = NULL,  width='1000px', #'100%',
                 #   value = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"))),
                  
                 uiOutput(ns('container_figLog')),
                 uiOutput(ns('addOnInfor_figLog')),
                  
                 fluidRow(column(width=12, uiOutput(ns("fig_footnote_selector"))))
             )
             
             
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

########################################################### 
# xyplot
########################################################### 

# Module server function
xyplot <- function(input, output, session, DATA, FIGURE_ALL) {
  
  ns <- session$ns
  EPS = 1E-3
  
  #--------------------------------------------------
  # inputData
  #--------------------------------------------------
  #### select which dataset(s)  ####
  inputData <- reactive({
    validate(need(input$data_name, message=FALSE))
     
    data = DATA$mDATA[[input$data_name]]
    if (is.null(data)) {return(NULL)}
    if (is.null(data$TEST)) {data$TEST= "Unkown"} 
    
    data
  })
   
  #-----------------------------------------
  # on UI side
  #-----------------------------------------
  
  output$data_selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("", unique(names(DATA$mDATA)))
    selectizeInput(ns("data_name"), 
                   label    = "Which dataset:" , 
                   choices  = data.lst,  #unique(inputData()%>%pull(TEST)),    
                   multiple = FALSE, 
                   
                   selected = data.lst[1])  #unique(inputData()%>%pull(TEST))[1])   
  })
  
  
  output$test_selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    test.lst = c(unique(data$TEST))
    checkboxGroupInput(ns("test"), label="Which test variable", 
                       choices = test.lst, 
                       selected= test.lst[1]   )})
   
  
  output$dose_group_selector <- renderUI({
    data = inputData()
    if(is.null(data)) {return(NULL)}
    if(is.null(data$ARMA)) {return(NULL)}
    
    checkboxGroupInput(ns("dose_group"), label="Which dose groups", 
                       choices = unique(as.character(data$ARMA)), 
                       selected= unique(as.character(data$ARMA))    )})
  
  
  output$facet_scale_selector  <- renderUI({
 
    validate(need(input$facet_by, message=FALSE))
    selectizeInput(ns("facet_scale"), 
                   label    = "Select facet scale:" , 
                   choices  = c("fixed", "free", "free_x", "free_y"), 
                   multiple = FALSE, 
                   selected = "fixed"
    )
  })    
  
  
  
  output$xvar_selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
 
    selectizeInput(ns("xvar"), 
                   label    = "select x variable:" , 
                   choices  =  colnames(data), 
                   multiple = FALSE, 
                   selected = ifelse("TIME" %in% colnames(data), "TIME", 
                                     ifelse("NTIM" %in% colnames(data), "NTIM",  colnames(data)[1]))
    )
  })    
  
  
  
  #### yvar_selector ####
  output$yvar_selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    DVOR = DVOR_VARS(data)
    selectizeInput(ns("yvar"), 
                   label    = "select y variable:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="", DVOR, colnames(data)[1]))
  })
  
  
  
  output$xlim_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    
    validate(#need(input$test, message=FALSE), 
             need(input$xvar, message=FALSE)
    )
    
    cat(file=stderr(), "##############Step: Xrange1 #################", "\n")
    #data = data %>% filter(TEST %in% input$test)  
    
    xvar= data %>% pull(input$xvar) %>% as_numeric()
    xvar = xvar[which(!xvar %in% c(NA, NaN, Inf, -Inf))]
     
    max   <- max(xvar[is.finite(xvar)], na.rm=TRUE)
    min   <- min(xvar[is.finite(xvar)], na.rm=TRUE) 
    tick = (max - min)/20 %>% round(., digits=1) 
     
    
    sliderInput(ns("xRange"), paste("Range of ", input$xvar), 
                min = floor(min), max = ceiling(round(max,digits=3)), value = c(min,max)) # ,  ticks = TRUEstep = tick,
  })
  
  
  output$ylim_selector <- renderUI({
    #req(input$test, input$xvar)
    data = inputData()
    
    validate(need(data, message=FALSE), 
             need(input$yvar, message=FALSE)
    )
    
    cat(file=stderr(), "##############Step: yrange1 #################", "\n")
     
    
    #if (is.null(input$test)) {return(NULL)}
    if (!is.null(input$test)) {
       if (input$test!="") {data = data %>% filter(TEST %in% input$test)}
    }
    
    #print(input$yvar)
    yvar= data %>% pull(input$yvar) %>% as_numeric() %>% round(., digits=3)     #as_numeric(data[,input$yvar])
    
    
    if (input$errbar_or_CI=="errbar" && input$errorbar!="None") {
      meanValue = data %>%pull(input$yvar) %>% as_numeric() %>% round(., digits=3)
      SD_SE = data[, input$errorbar] %>%  as_numeric() %>% round(., digits=3)
      SD_SE = ifelse(is.na(SD_SE), 0, SD_SE)
      yvar=c(meanValue-SD_SE, meanValue+SD_SE)
     }
    
    yvar = yvar[which(!is.na(yvar) & !is.infinite(yvar))]   #  %in% c(NA, NaN, Inf, -Inf))]
    if (input$yscale=="log") {yvar = yvar[which(!is.na(log(yvar)) & !is.infinite(log(yvar)))]  }
    print("range of yvar:")
    print(range(yvar))
    
    max   <- max(yvar[is.finite(yvar)], na.rm=TRUE)
    min   <- min(yvar[is.finite(yvar)], na.rm=TRUE) 
    tick = (max - min)/20 %>% round(., digits=2) 
    
    cat(file=stderr(), "##############Step: yrange2 #################", "\n")
    
    sliderInput(ns("yRange"), paste("Range of ", input$yvar), 
                min = floor(min), max = ceiling(max), value = c(min, max ), step = tick, ticks = TRUE)
  })    
  
  
  
  output$color_by_selector <- renderUI({
    data = inputData()
    ARMA = ARMA_VARS(data)
    selectInput(ns("color_by"), label ="Color by",   #h5()
                choices = colnames(data),  
                selected = ARMA)})
  
  
  output$shape_by_selector <- renderUI({
    data = inputData()
    ARMA = ARMA_VARS(data)
    selectInput(ns("shape_by"), label ="Shape by",   #h5()
                choices = colnames(data),  
                selected = ARMA)})
  
  output$group_by_selector <- renderUI({
    data = inputData()
    USUBJID = USUBJID_VARS(data)
    selectInput(ns("group_by"), label ="Group by",   #h5()
                choices = c(colnames(data)),  
                selected = ifelse("USUBJID" %in% colnames(data), "USUBJID", 
                                  ifelse("ARMA" %in% colnames(data), "ARMA", colnames(data)[1])))})    
  
  output$facet_by_selector <- renderUI({
    data = inputData()
    selectInput(ns("facet_by"), label ="Facet by",   #h5()
                choices = c("",  colnames(data)),  
                selected = "")})
  

  
  
  output$errbar_or_CI_selector <- renderUI({
    
    radioButtons(ns("errbar_or_CI"), label = "Select errbar or CI",
                 choices = list("None"="None", "errbar" = "errbar", "CI" = "CI"), inline = TRUE,
                 selected = "None")
  })
  
  output$errbar_selector <- renderUI({
    if (is.null(input$errbar_or_CI)) {return(NULL)}    
    if (input$errbar_or_CI!="errbar") {return(NULL)}
    radioButtons(ns("errorbar"), label = "Select errorbar",
                 choices = list("SE" = "SE", "SD" = "SD", "None"="None"), inline = TRUE,
                 selected = "SD")
  })
  
  output$CI_selector <- renderUI({
    if (is.null(input$errbar_or_CI)) {return(NULL)}     
    if (input$errbar_or_CI!="CI") {return(NULL)}    
    radioButtons(ns("CI"), label = "Select Confidence Interval",
                 choices = list("95%" = "95%", "90%" = "90%", "80%"="80%"), inline = TRUE,
                 selected = "95%")
  })  
  
  
  
  output$fontSize_selector <- renderUI({
    selectInput(ns("fontSize"), label = "Select font size", 
                choices = seq(2, 50, by=2), 
                selected = 12)})
  
  output$fig_width_selector <- renderUI({
    numericInput(ns("fig_width"), label = "Canvas width", 
                min=1, max=24, value = 6.4)})
  
  output$fig_height_selector <- renderUI({
    numericInput(ns("fig_height"), label = "Canvas height", 
                min=1, max=24, value = 4.8)})
  
  output$plotType_selector <- renderUI({
    selectInput(ns("plotType"), label = "Select plot type",
                choices = list("both" = "both", "point" = "point", "line"="line"), 
                selected = "both")})   
  
  
  output$xtick_selector <- renderUI({
    textInput(ns("xtick"), label = "Minor tick, Major tick [x]", value= "7, 28")})
  
  output$xscale_selector <- renderUI({
    radioButtons(ns("xscale"), label = "Scale for X-axis", inline=TRUE,
                 choices = list("log" = "log" , "nonlog" = "nonlog" ), 
                 selected = "nonlog")})
  
  
  
  # if linear-scale for y-axis, then input minor.tick and major.tick
  output$ytick_selector <- renderUI({
    if (is.null(input$yvar)) {return(NULL)}
    #if (is.null(input$test)) {return(NULL)}    
    
    data = inputData()
    validate(need(data, message=FALSE), 
             need(input$yvar, message=FALSE)
             )
    
    yvar = data %>% pull(input$yvar) %>% as_numeric()
    
    if (!is.null(input$test)) {
      if (input$test!="") {yvar = data %>% filter(TEST %in% input$test) %>% pull(input$yvar) %>% as_numeric()}
    }
    
    mRange <- range(yvar, na.rm=TRUE)
    tick = (mRange[2] - mRange[1])/20
    
    minor.tick = 10^(floor(log10(abs(tick))))
    major.tick = 10^(floor(log10(abs(tick)))) * 10
    
    yscale2.txt = paste0(minor.tick, ", ", major.tick)
    
    textInput(ns("ytick"), label = "Minor tick, Major tick [y]", value=yscale2.txt )
    
  })
  
  
  output$yscale_selector <- renderUI({
    radioButtons(ns("yscale"), label = "Scale for Y-axis",inline=TRUE,
                 choices = list("log" = "log" , "nonlog" = "nonlog" ), 
                 selected = "log")})
  
  
  
  output$xlab_selector <- renderUI({
    textInput(ns("xlab"), label = "X-axis label", value = "Time (day)")})
  
  output$ylab_selector <- renderUI({
    textInput(ns("ylab"), label = "Y-axis label", value = "Concentration (mg/L)")})  
  
  output$xrefline_selector <- renderUI({
    textInput(ns("xrefline"), label = "Horizontal line(s)", value = "NA")})
  
  output$yrefline_selector <- renderUI({
    textInput(ns("yrefline"), label = "Vertical line(s)", value = "NA")})
  
  
  
  output$addline_selector <- renderUI({
    selectInput(ns("addline"), label = "add line",
                choices = list("", "diagonal" = "diagonal" , "loess" = "loess" ), 
                selected = "")})
  
  #value = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day 
  #Following a Single IV Dose of xxxx (Study xxxx-HV-1219"))),
  output$fig_title_selector <- renderUI({
    
    data = inputData()
    patient.name = "Healthy Volunteers"
    dosing.name = paste0("a Single Subcutaneous or Intravenous Dose(s)")
    drug.name = substr(data$STUDYID, 1, 5)
    study.name = unique(data$STUDYID)
    
    if (is.null(input$xscale)) {return(NULL)}    
    if (is.null(input$yscale)) {return(NULL)}
    if (is.null(input$xlab)) {return(NULL)}    
    if (is.null(input$ylab)) {return(NULL)}
    
    key = "Individual"
    
    yscale = ifelse(input$yscale=="log", "Log-scaled", 
                    ifelse(input$yscale=="nonlog", "Linear-scaled", "")) 
    
    x1 = gregexpr("\\(", input$xlab) %>% unlist()
    xlab = trim(substr(input$xlab, 1, x1-1))
    
    y1 = gregexpr("\\(", input$ylab) %>% unlist()
    ylab = trim(substr(input$ylab, 1, y1-1))
    
    fig.title = paste(key, yscale, ylab, "vs.", xlab, 
                      "in", patient.name, "Following", dosing.name, "of", drug.name, "in Study", study.name, sep=" ")
    
    
    #fig.title = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"
    textAreaInput(ns("fig_title"), label = NULL,  width='100%', value = fig.title)})
  
  
  #"Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  #footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  output$fig_footnote_selector <- renderUI({
    textAreaInput(ns("fig_footnote"), label = NULL,  width='100%',
                  value = "Note: QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Concentrations below the lower limit of quantification (LLOQ) are set to zero. ")})
  
  
  #------------------------- 
  # scriptArea_container
  #------------------------- 
  output$scriptArea_container <-renderUI({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    textAreaInput(ns("scriptArea"), label=NULL, value=NULL, rows=25, 
                  placeholder = "fig <- ggplot(data, aes(x=TIME,y=DVOR)) +
                                 geom_point() + geom_line()")
    
  })
  
  
  ######################################################################
  #####Start reactive ##### 
  ######################################################################
  
  # add dynamic filters ----------------------------------------------------------------
  aggregFilters <- reactiveValues()
  #aggregFilters2 <- reactiveValues(data2=inputData())
  
  observeEvent(input$removeBtn, {
    removeUI(
      selector = 'div:has(> #duplicateFilter1#col_selector)'
    )
    
    removeUI(
      selector = 'div:has(> #duplicateFilter1#row_selector)'
    )
    
  })
  
  #Here we add the UI and callModule of the duplicate module
  observeEvent(input$addFilterModule, {
    #session$sendCustomMessage(type = ns("moveModule"), message = "Something")
    duplicateFilterid <- paste0("duplicateFilter", input$addFilterModule)
    
    insertUI(
      selector = '#placeholder',
      where = "beforeEnd",   # afterEnd",
      ui =  FilterUI(ns(duplicateFilterid))
    )
    
    
    # call module, return reactive
    tt = callModule(Filter, duplicateFilterid, data2())
    aggregFilters[[duplicateFilterid]]$col = tt[['colfilterId']]
    aggregFilters[[duplicateFilterid]]$row = tt[['rowfilterId']]
    aggregFilters[[duplicateFilterid]]$remove = tt[['removeFilterId']] 
    
  })
  
  # reduce the colsize after add each filter  
  data2 <- reactive({
    data2 = inputData()  %>% select(-one_of(c("TEST", "ARMA"))) 
    
    for (i in 1:length(names(aggregFilters))) {
      filter = aggregFilters[[names(aggregFilters)[i]]]
      if (is.null(filter$col())) {next}
      
      data2 = data2 %>% select(-one_of(filter$col())) 
    }   
    data2
  })
  
  # render the filtered data
  filterData<- reactive({
    
    data = inputData() 
     
    validate(need(data, message=FALSE), 
             need(input$dose_group, message=FALSE)
    )
    
    
    
    data = data %>% filter(ARMA %in% input$dose_group)
    if(!is.null(input$test)) {
      if(input$test!="")  { data = data %>% filter(TEST %in% input$test)}
    }
 
    #if (is.null(aggregFilters)) {return(NULL)}
    #if (length(names(aggregFilters))==0) {return(NULL)} 
    
    #data= invisible(lapply(aggregFilters, function(filter){
    
    if (length(names(aggregFilters))>0) { 
      
      for (i in 1:length(names(aggregFilters))) {
        which_filter = names(aggregFilters)[i]
        filter = aggregFilters[[which_filter]]
        if (is.null(filter$col())) {next}
        if (is.null(filter$row())) {next}  
        
        col = data[[filter$col()]] 
        data = switch(typeof(col), 
                      "double"= data[which(col>= filter$row()[1], col<= filter$row()[2]), ], 
                      "integer" = data[which((col%in% filter$row())), ], 
                      "character" = data[which((col%in% filter$row())), ]
        ) }
      
    }
    
    # prepare data for plots

    #data = data %>% filter(TEST==input$test)
    data = data %>% mutate_(XVAR=input$xvar,
                            YVAR=input$yvar,
                            GROUP_BY=input$group_by,
                            COLOR_BY=input$color_by,
                            SHAPE_BY=input$shape_by) %>%
      mutate(XVAR=as_numeric(XVAR),
             YVAR=as_numeric(YVAR),
             GROUP_BY = as.factor(GROUP_BY),
             COLOR_BY = as.factor(COLOR_BY),
             SHAPE_BY = as.factor(SHAPE_BY))
    
    #if (!is.null(input$errbar_or_CI) && input$errbar_or_CI %in% "None") { data = data %>%  mutate(meanMinus = Mean,  meanPlus = Mean) } 
    
     if (input$errbar_or_CI=="errbar" && !is.null(input$errorbar) ) {
       if (input$errorbar %in% "None") { data = data %>%  mutate(meanMinus = Mean,  meanPlus = Mean) }  
       if (input$errorbar %in% "SD" && "SD" %in% colnames(data) ) { data = data %>%  mutate(meanMinus = Mean-SD,  meanPlus = Mean+SD) }   
       if (input$errorbar %in% "SE" && "SE" %in% colnames(data) ) { data = data %>%  mutate(meanMinus = Mean-SE,  meanPlus = Mean+SE) }  
     }
    
    
    data
    
  })   
  # #
  # onBookmark(function(state) {
  #   state$values$hash <- digest::digest(input$fig_name, "md5")
  # })
  # 
  # # 
  # onRestore(function(state) {
  #   if (identical(digest::digest(input$fig_name, "md5"), state$values$hash)) {
  #     showNotification(paste0('Module\'s input fig_name "', input$fig_name,
  #                             '" matches hash ', state$values$hash))
  #   } else {
  #     showNotification(paste0('Module\'s input text "', input$fig_name,
  #                             '" does not match fig_name ', state$values$hash))
  #   }
  # })
  
  # observeEvent(input$table_rows_selected, {
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {  onBookmarked() can't be used in a module.
  #   updateQueryString(url)
  # })
  
  
  
  
  figLog <- reactive({

    data = filterData()
    if(is.null(data)) {return(NULL)}
    if(nrow(data)==0) {return(NULL)}  # ("no data after filtered.")}
 
    #validate(need(input$test, message="Please select test variable"))
    
    myFig = plotIt(data, input, session)  
    
    
    # if overlay
    #------------------------
    if (!is.null(input$data2_name)) { 
      if (input$data2_name!="" & 
          !is.null(input$plotType2) & 
          !is.null(input$xvar2) & 
          !is.null(input$yvar2) & 
          !is.null(input$dose_group2) 
          ) {
        
        data2 = DATA$mDATA[[input$data2_name]]
        if (input$test2!="") { data2 = data2 %>% filter(TEST %in% input$test2) }
        if (input$dose_group2!="") { data2 = data2 %>% filter(ARMA %in% input$dose_group2) }
        
        data2 = data2 %>% mutate_(GROUP_BY=input$group_by, 
                                  COLOR_BY=input$color_by, 
                                  SHAPE_BY=input$shape_by)
        
        data2[, input$xvar2] = as_numeric(data2[, input$xvar2])
        data2[, input$yvar2] = as_numeric(data2[, input$yvar2]) + EPS      # in case of log(0)
        data2 = data2 %>% filter_(!input$yvar2 %in% c(NA, Inf, -Inf))
        
        
        if (input$data_name==input$data2_name) { # if input$data_name==input$data2_name
          
          if (input$plotType2=="both")  {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), show_guide=FALSE, size=input$point.size2) +
              geom_line(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), lwd=input$line.lwd2)
          } else if (input$plotType2=="line")  {
            myFig = myFig +
              geom_line(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), lwd=input$line.lwd2)
          } else if (input$plotType2=="point") {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), show_guide=FALSE,size=input$point.size2)
          }    
        } else { # if input$data_name!=input$data2_name
          # # we may need to have these input variables
          # data2 = data2 %>% mutate_(GROUP_BY=input$group_by, 
          #                           COLOR_BY=input$color_by, 
          #                           SHAPE_BY=input$shape_by)
          # 
          if (input$plotType2=="both")  {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), show_guide=FALSE, size=input$point.size2) +
              geom_line(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"),lwd=input$line.lwd2)
          } else if (input$plotType2=="line")  {
            myFig = myFig +
              geom_line(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), lwd=input$line.lwd2)
          } else if (input$plotType2=="point") {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), show_guide=FALSE,size=input$point.size2)
          }
          
        }  # end of input$data_name!=input$data2_name
        
        
        
        
      }} # end of overlay  
    
    
    myFig
  })
  
  
  
  #-------------------------------
  # if runScript
  #-------------------------------
  figByScript <- reactive({
    #if (mod(input$runScript+1,2)) {return(NULL)}
    
    data = filterData()
    txt = input$scriptArea
    
    #validate
    validate(need(data, message=FALSE), 
             need(txt, message=FALSE) )
   
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
    
    return(fig)
  })
  
  
  #-------------------------
  #
  #-------------------------
  
  
  output$container_figLog<- renderUI({
    # output$figLog <- renderPlot({ 
    #   cat(file=stderr(), "##############Step: render myPlot #################", "\n")
    #   #figLog()
    #   
    #   ggplot(mtcars, aes(wt, mpg)) + geom_point()
    # })   
    # plotOutput(ns("figLog"), width = "100%", height = "600px", 
    #            click = clickOpts(id = "figLog_click"), 
    #            brush = brushOpts(id = "figLog_brush")
    #            )
    
    output$figLog <- renderPlot({
       
      
      validate(need(input$ggplot, message=FALSE))
       
      
      if(input$ggplot=="GUI-ggplot")    { fig =figLog()  }
      if(input$ggplot=="Script-ggplot") { fig =figByScript()  }
      
      return(fig)
      
    })
    
    
    plotOutput(ns("figLog"), width = "100%", height = "500px",   # height = 300,
               # Equivalent to: click = clickOpts(id = "figLog_click")
               #click = "figLog_click",
               click = clickOpts(id = ns("figLog_click")),
               brush = brushOpts(id = ns("figLog_brush")) 
    )
    # 
    # dblclick = dblclickOpts(
    #   id = "plot_dblclick",
    #   delay = input$dblclick_delay
    # ),
    # hover = hoverOpts(
    #   id = "plot_hover",
    #   delay = input$hover_delay,
    #   delayType = input$hover_policy,
    #   nullOutside = input$hover_null_outside
    # ),
    # brush = brushOpts(
    #   id = "plot_brush",
    #   delay = input$brush_delay,
    #   delayType = input$brush_policy,
    #   direction = input$brush_dir,
    #   resetOnNew = input$brush_reset
    # )
    # 
    # 
    # output$figLog <- renderPlotly({ 
    #   cat(file=stderr(), "##############Step: render myPlot #################", "\n")
    #   p = figLog()
    #   ggplotly(p)
    # })   
    # 
    # plotlyOutput(ns("figLog"), width = "100%", height = "600px")
    
  })
  
  
  output$addOnInfor_figLog<- renderUI({
    fluidRow(
      column(width = 6,
             h4("Points near click"),
             verbatimTextOutput(ns("click_info"))
      ),
      column(width = 6,
             h4("Brushed points"),
             verbatimTextOutput(ns("brush_info"))
      )
    )
    
    
  })
  
  
  
  output$click_info <- renderPrint({
    #print("input$figLog_click")
    #print(input$figLog_click)
    
    #col.lst = c("USUBJID", "ARMA", "VISIT", "NTIM", "TIME", "TEST", "DVOR", "SOP")
    if (is.null(input$figLog_click)) {return(NULL)}
    nearPoints(filterData(), input$figLog_click, addDist = TRUE)   #%>% 
    #select(USUBJID, ARMA, VISIT, NTIM, TIME, TEST, DVOR, SOP)   #(mpg, cyl, disp, hp, wt)
  })
  
  output$brush_info <- renderPrint({
    #print("input$figLog_brush")
    #print(input$figLog_brush)
    if (is.null(input$figLog_brush)) {return(NULL)}
    
    
    #print("line 811")
    #print(head(filterData(), n=20))
    brushedPoints(filterData(), input$figLog_brush, allRows = FALSE) # %>%  
    #select(USUBJID, ARMA, VISIT, NTIM, TIME, TEST, DVOR, SOP)   #select(mpg, cyl, disp, hp, wt)
  })
  
  
  
  addNewFig <- function(newFig, FIGURE_ALL, fig_name, fig_size, fig_title, fig_footnote) {
    
    
    if (fig_name %in% names(FIGURE_ALL$mFIGURES)) {
      cat(file=stderr(), "##############Warning: fig_name exist, override will occur! #################", "\n")
    }
    
    names(newFig) = input$fig_name
    
    FIGURE_ALL$mFIGURES[[fig_name]] <- newFig     
    FIGURE_ALL$mTITLES[[fig_name]] <- fig_title
    FIGURE_ALL$mFOOTNOTES[[fig_name]] <- fig_footnote
    FIGURE_ALL$mSIZE[[fig_name]] <- fig_size
    
    return(FIGURE_ALL)
  }
   
  #add figure to log when action button is pressed
  observeEvent(input$addToCart, {
    #req(input$addToCart, FIGURE_ALL, input$fig_name)
    
    if(is.null(input$addToCart))  {return()}
    if(input$addToCart == 0) return()
    cat(file=stderr(), "##############Step: Fig add2Cart #################", "\n")
    
    newFig = NULL
    if(input$ggplot=="GUI-ggplot")    { newFig =figLog()  }
    if(input$ggplot=="Script-ggplot") { newFig =figByScript()  }
     
    #CurrentLog   <- FIGURE_ALL$mFIGURES  #####################
    CurrentLog_mFIGURES   <- isolate(FIGURE_ALL$mFIGURES)
 
    newFig <- list(isolate(newFig))
    fig_name <- isolate(input$fig_name)
    names(newFig) = fig_name
    
    CurrentLog_mTITLES   <- isolate(FIGURE_ALL$mTITLES)
    newTitle <- (input$fig_title)
    names(newTitle) = fig_name
    
    CurrentLog_mFOOTNOTES  <- isolate(FIGURE_ALL$mFOOTNOTES)
    newFootnote <- (input$fig_footnote)
    names(newFootnote) = fig_name
    
     
      
      FIGURE_ALL$mTITLES[[fig_name]] <- newTitle
      FIGURE_ALL$mFIGURES[[fig_name]] <- newFig
      FIGURE_ALL$mFOOTNOTES[[fig_name]] <- newFootnote
       
      FIGURE_ALL$mSIZE[[fig_name]] <-  c(input$fig_width, input$fig_height)
      
      
  })
   
  
  
  
  
  
  
  
  
  
  
  output$data2_selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("", unique(names(DATA$mDATA)))
    selectizeInput(ns("data2_name"), 
                   label    = "Which dataset:" , 
                   choices  = data.lst,  #unique(inputData()%>%pull(TEST)),    
                   multiple = FALSE, 
                   selected = data.lst[1])  #unique(inputData()%>%pull(TEST))[1])   
  })
  
  
  output$test2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="") {return(NULL)}
    
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    test.lst = unique(data2$TEST)
    selectizeInput(ns("test2"), 
                   label    = "select which test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])   
  })
  
  
  
  #### xvar2_selector ####
  output$xvar2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="") {return(NULL)}
    
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    TIME = TIME_VARS(data2)
    NTIM = NTIM_VARS(data2)
    selectizeInput(ns("xvar2"), 
                   label    = "select x variable:" , 
                   choices  =  colnames(data2), 
                   multiple = FALSE, 
                   selected = ifelse(TIME!="", TIME, 
                                     ifelse(NTIM!="", NTIM, colnames(data2)[1]))
    )
  }) 
  
  #### yvar2_selector ####
  output$yvar2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    DVOR = DVOR_VARS(data2)
    selectizeInput(ns("yvar2"), 
                   label    = "select y variable:" , 
                   choices  = colnames(data2), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="", DVOR, colnames(data2)[1]))
  })
  
  output$plotType2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="") {return(NULL)}
    radioButtons(ns("plotType2"), label = "Select plot type",
                 choices = list("both" = "both", "point" = "point", "line"="line"), 
                 selected = "both")
    }) 
  
  
  output$dose_group2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="") {return(NULL)}
    
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    if(is.null(data2$ARMA)) {return(NULL)}
    
    checkboxGroupInput(ns("dose_group2"), label="Filter by dose groups", 
                       choices = unique(as.character(data2$ARMA)), 
                       selected= unique(as.character(data2$ARMA))    )
    })
  
  output$point.size2_selector <- renderUI({
    validate(need(input$plotType2, message=FALSE))
    
    if (!input$plotType2 %in% c("both", "point")) {return(NULL)}
    numericInput(ns("point.size2"), label="Point size", min=1,max=20,step=1,value=1) 
    
  })
  
  output$line.lwd2_selector <- renderUI({
    validate(need(input$plotType2, message=FALSE))
    
    if (!input$plotType2 %in% c("both", "line")) {return(NULL)}
    numericInput(ns("line.lwd2"), label="Line width", min=1,max=20,step=1,value=1) 
    
  })
  
  
  
  
  
  # Return the reactive that yields the data frame
  return(FIGURE_ALL)
}
