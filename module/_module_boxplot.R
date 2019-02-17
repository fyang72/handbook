
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

boxplotUI <- function(id, label="") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(2,
           uiOutput(ns("data_Selector")),
           uiOutput(ns("test_Selector")),
           tags$hr(style="border-color: black;"),
           
           uiOutput(ns("facet_by_selector")),
           uiOutput(ns("color_by_selector")),
           uiOutput(ns("shape_by_selector")),
           uiOutput(ns("group_by_selector")),
           uiOutput(ns("dose_group_selector")),
           uiOutput(ns("plotType_selector")),
           uiOutput(ns("fontSize_selector"))
    ),
    
    column(2,
           uiOutput(ns("xvar_Selector")),
           uiOutput(ns("yvar_Selector")),
           uiOutput(ns("xlim_selector")),
           uiOutput(ns("ylim_selector")),
           uiOutput(ns("xscale_selector")),    # ticks
           uiOutput(ns("yscale_selector2")),   # ticks
           uiOutput(ns("yscale_selector")),   # log, nonlog
           uiOutput(ns("xlab_selector")),
           uiOutput(ns("ylab_selector")), 
           uiOutput(ns("xrefline_selector")),
           uiOutput(ns("yrefline_selector"))
    ),
    
    
    column(8, 
           
           #fluidRow(column(width=12), 
           fluidRow(
             column(width=2, "Enter figure name here:"),
             column( width = 2, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                     textInput(ns("fig_name"), value="Fig", label=NULL)),
             column( width = 2, #status = "primary",  #class = 'rightAlign',#background ="aqua",
                     actionButton(ns("addToCart"),h6("add to cart"))), 
             column(width=2, bookmarkButton()), 
             column(width=2,  "Figures currently stored:"), 
             column(width=2,  textOutput(ns("FigCount")))),  
           
           fluidRow(column(width=12, uiOutput(ns("fig_title_selector")))),
           #fluidRow(column(width=8, textAreaInput(ns("fig_title"), label = NULL,  width='1000px', #'100%',
           #   value = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"))),
           
           
           uiOutput(ns('container_figLog')),
           uiOutput(ns('addOnInfor_figLog')),
           
           
           
           fluidRow(column(width=12, uiOutput(ns("fig_footnote_selector")))),
           
           box(width=12, collapsible = TRUE, collapsed = TRUE, 
               title = "Overlay a secondary dataset", status = "primary",       # "warning",
               #"Box content here", br(), "More box content",
               fluidRow(
                 column(width=3, uiOutput(ns("data2_Selector"))),
                 
                 column(width=3, uiOutput(ns("test2_Selector"))),
                 
                 column(width=2, uiOutput(ns("plotType2_selector"))),
                 
                 column(width=2, uiOutput(ns("xvar2_Selector"))),
                 
                 column(width=2, uiOutput(ns("yvar2_Selector")))
               ) 
           )
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
boxplot <- function(input, output, session, DATA, FIGURE_ALL) {
  
  ns <- session$ns
  EPS = 1E-3
  
  
  # select which dataset(s)
  inputData <- reactive({
    if (is.null(input$data_name)) {return(NULL)}
    
    data = DATA$mDATA[[input$data_name]]
    
    if (is.null(data$TEST)) {data$TEST= "xxxx"} 
    
    
    data
  })
  # 
  
  #
  onBookmark(function(state) {
    state$values$hash <- digest::digest(input$fig_name, "md5")
  })
  
  # 
  onRestore(function(state) {
    if (identical(digest::digest(input$fig_name, "md5"), state$values$hash)) {
      showNotification(paste0('Module\'s input fig_name "', input$fig_name,
                              '" matches hash ', state$values$hash))
    } else {
      showNotification(paste0('Module\'s input text "', input$fig_name,
                              '" does not match fig_name ', state$values$hash))
    }
  })
  
  # observeEvent(input$table_rows_selected, {
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {  onBookmarked() can't be used in a module.
  #   updateQueryString(url)
  # })
  
  
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
  
  output$data2_Selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("NONE", unique(names(DATA$mDATA)))
    selectizeInput(ns("data2_name"), 
                   label    = "select which dataset:" , 
                   choices  = data.lst,  #unique(inputData()%>%pull(TEST)),    
                   multiple = FALSE, 
                   selected = data.lst[1])  #unique(inputData()%>%pull(TEST))[1])   
  })
  
  
  output$test_Selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    test.lst = c("", unique(data$TEST))
    selectizeInput(ns("test"), 
                   label    = "select which test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])  
  })
  
  output$test2_Selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="NONE") {return(NULL)}
    
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    test.lst = unique(data2$TEST)
    selectizeInput(ns("test2"), 
                   label    = "select which test variable:" , 
                   choices  =  test.lst,    
                   multiple = FALSE, 
                   selected =  test.lst[1])   
  })
  
  output$xvar_Selector <- renderUI({
    data = inputData()    
    if (is.null(data)) {return(NULL)}
    
    #TIME = TIME_VARS(data)
    ARMA = ARMA_VARS(data)
    selectizeInput(ns("xvar"), 
                   label    = "select x variable:" , 
                   choices  =  colnames(data), 
                   multiple = FALSE, 
                   selected = ifelse(ARMA!="", ARMA, "") 
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
  
  
  #### xvar2_Selector ####
  output$xvar2_Selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="NONE") {return(NULL)}
    
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    TIME = TIME_VARS(data2)
    NTIM = NTIM_VARS(data2)
    selectizeInput(ns("xvar2"), 
                   label    = "select x variable:" , 
                   choices  =  colnames(data2), 
                   multiple = FALSE, 
                   selected = ifelse(TIME!="NULL", TIME, 
                                     ifelse(NTIM!="NULL", NTIM, colnames(data2)[1]))
    )
  }) 
  
  #### yvar2_Selector ####
  output$yvar2_Selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    data2 = DATA$mDATA[[input$data2_name]]  
    if (is.null(data2)) {return(NULL)}
    
    DVOR = DVOR_VARS(data2)
    selectizeInput(ns("yvar2"), 
                   label    = "select y variable:" , 
                   choices  = colnames(data2), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="NULL", DVOR, colnames(data2)[1]))
  })
  
  
  output$xlim_selector <- renderUI({
    data = inputData()
    if (is.null(data)) {return(NULL)}
    
    #req(input$test, input$xvar)
    if (is.null(input$test) | is.null(input$xvar)) {return(NULL)}
    
    cat(file=stderr(), "##############Step: Xrange1 #################", "\n")
    
    #(input$xvar)
    
    data = data %>% filter(TEST %in% input$test)  ######### isolate
    
    xvar= data %>% pull(input$xvar) %>% as_numeric()
    xvar = xvar[which(!xvar %in% c(NA, NaN, Inf, -Inf))]
    
    #(data[,input$xvar])
    #(data[, "NTIM"])
    #print(xvar)      
    mRange <- range(xvar, na.rm=TRUE)
    tick = (mRange[2] - mRange[1])/20
    mMid   <- max(mRange, na.rm=TRUE)
    
    cat(file=stderr(), "##############Step: Xrange2 #################", "\n")
    
    sliderInput(ns("xRange"), paste("Range of ", input$xvar), 
                min = floor(mRange[1]), max = ceiling(mRange[2]), value = c(0, mMid), step = tick, ticks = TRUE)
  })
  
  output$ylim_selector <- renderUI({
    #req(input$test, input$xvar)
    
    if (is.null(input$test) | is.null(input$yvar)) {return(NULL)}
    
    cat(file=stderr(), "##############Step: yrange1 #################", "\n")
    
    data = inputData()
    data = data %>% filter(TEST %in% input$test)
    
    #print(input$yvar)
    yvar= data %>% pull(input$yvar) %>% as_numeric()    #as_numeric(data[,input$yvar])
    
    yvar = yvar[which(!is.na(yvar) & !is.infinite(yvar))]   #  %in% c(NA, NaN, Inf, -Inf))]
    
    
    mRange <- range(yvar, na.rm=TRUE)
    tick = (mRange[2] - mRange[1])/20
    mMid   <- max(mRange, na.rm=TRUE)
    
    cat(file=stderr(), "##############Step: yrange2 #################", "\n")
    
    sliderInput(ns("yRange"), paste("Range of ", input$yvar), 
                min = floor(mRange[1]), max = ceiling(mRange[2]), value = c(0, mMid), step = tick, ticks = TRUE)
  })    
  
  
  
  output$color_by_selector <- renderUI({
    data = inputData()
    selectInput(ns("color_by"), label ="Color by",   #h5()
                choices = colnames(data),  
                selected = "ARMA")})
  
  
  output$shape_by_selector <- renderUI({
    data = inputData()
    selectInput(ns("shape_by"), label ="Shape by",   #h5()
                choices = colnames(data),  
                selected = "ARMA")})
  
  output$group_by_selector <- renderUI({
    data = inputData()
    selectInput(ns("group_by"), label ="Group by",   #h5()
                choices = c(colnames(data)),  
                selected = "USUBJID")})    
  
  output$facet_by_selector <- renderUI({
    data = inputData()
    selectInput(ns("facet_by"), label ="Facet by",   #h5()
                choices = c("NONE",  colnames(data)),  
                selected = "NONE")})
  
  output$dose_group_selector <- renderUI({
    data = inputData()
    checkboxGroupInput(ns("dose_group"), label="Select dose groups", 
                       choices = unique(as.character(data$ARMA)), 
                       selected= unique(as.character(data$ARMA))    )})
  
  
  output$fontSize_selector <- renderUI({
    selectInput(ns("fontSize"), label = "Select font size", 
                choices = list("12" = 12, 
                               "14" = 14,
                               "16" = 16,
                               "18" = 18, 
                               "20" = 20,
                               "22" = 22, 
                               "24" = 24), 
                selected = 12)})
  
  output$plotType_selector <- renderUI({
    radioButtons(ns("plotType"), label = "Select plot type",
                 choices = list("both" = "both", "point" = "point", "line"="line"), 
                 selected = "both")})   
  
  
  output$plotType2_selector <- renderUI({
    if (is.null(input$data2_name)) {return(NULL)}
    if (input$data2_name=="NONE") {return(NULL)}
    radioButtons(ns("plotType2"), label = "Select plot type",
                 choices = list("both" = "both", "point" = "point", "line"="line"), 
                 selected = "both")}) 
  
  
  output$xscale_selector <- renderUI({
    textInput(ns("xscale"), label = "Minor tick, Major tick [x]", value= "7, 28")})
  
  output$yscale_selector <- renderUI({
    radioButtons(ns("yscale"), label = "Scale for Y-axis",
                 choices = list("log" = "log" , "nonlog" = "nonlog" ), 
                 selected = "log")})
  
  # if linear-scale for y-axis, then input minor.tick and major.tick
  output$yscale_selector2 <- renderUI({
    if (is.null(input$yvar)) {return(NULL)}
    if (is.null(input$test)) {return(NULL)}    
    
    data = inputData()
    
    yvar = data %>% filter(TEST %in% input$test) %>% pull(input$yvar) %>% as_numeric()
    mRange <- range(yvar, na.rm=TRUE)
    tick = (mRange[2] - mRange[1])/20
    
    minor.tick = 10^(floor(log10(tick)))
    major.tick = 10^(floor(log10(tick))) * 10
    yscale2.txt = paste0(minor.tick, ", ", major.tick)
    
    textInput(ns("yscale2"), label = "Minor tick, Major tick [y]", value=yscale2.txt )
    
  })
  
  
  output$xlab_selector <- renderUI({
    textInput(ns("xlab"), label = "X-axis label", value = "Time (day)")})
  
  output$ylab_selector <- renderUI({
    textInput(ns("ylab"), label = "Y-axis label", value = "Concentration (mg/L)")})  
  
  output$xrefline_selector <- renderUI({
    textInput(ns("xrefline"), label = "Horizontal reference line", value = "NA")})
  
  output$yrefline_selector <- renderUI({
    textInput(ns("yrefline"), label = "Vertical reference line", value = "NA")})
  
  
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
    textAreaInput(ns("fig_title"), label = NULL,  width='1040px', value = fig.title)})
  
  
  #"Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  #footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  output$fig_footnote_selector <- renderUI({
    textAreaInput(ns("fig_footnote"), label = NULL,  width='1040px',
                  value = "Note: QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Concentrations below the lower limit of quantification (LLOQ) are set to zero. ")})
  
  
  
  
  
  #####Start reactive ##### 
  
  #construct a reative data object
  filterData <- reactive({
    
    # req(input$xvar, input$yvar, 
    #     input$color_by, input$test, 
    #     input$xRange, input$yRange, 
    #     input$dose_group)
    # 
    if (is.null(input$xvar) | is.null(input$yvar)) {return(NULL)}
    
    data = inputData()
    if (is.null(data)) {return(NULL)}
    
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
    
    if (input$test!="") {data = data %>% filter(TEST %in% input$test)}
    if (input$dose_group !="") {data = data %>% filter(ARMA %in% input$dose_group)}
    
    
    
    cat(file=stderr(), "##############Step: filtered Data #################", "\n")      
    data 
    
  })
  
  
  
  
  
  
  figLog <- reactive({
    #plotIt()
    data = filterData()
    if(is.null(data)) {return(NULL)}
    if(nrow(data)==0) {return(NULL)}  # ("no data after filtered.")}
    
    #myFig = plotIt(data, input, session)  
    
    #load("simData.RData")
    theTitle = "xsf"
    outlier.size =1 
    outlier.color = "black"
    ylab.txt = "Y label"
    
    myFig <- ggplot(data = data, aes_string(x = "ARMA", y = "DVOR", fill="ARMA",  col="ARMA")) +
      geom_boxplot(position = position_dodge(width=0.9), outlier.size=outlier.size, outlier.color=outlier.color) +                     #space between boxplots
      ggtitle(theTitle) +                                                      #add title
      scale_y_continuous(ylab.txt ) +       #label y axis and set limtis to allow space to plot summary stats beneath plots
      scale_x_discrete("")                     #label x axis
    #scale_color_manual(name="Population", values=c("black","black"), labels=c("Japanese", "Non-Japanese")) +   
    #scale_fill_manual(name="Population", values=c("cyan","green"), labels=c("Japanese", "Non-Japanese")) + 
    #current_theme(font.size=10, legend.position=legend.position)
    
    myFig = myFig + theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
    
    
    
    
    # if overlay
    #------------------------
    if (!is.null(input$data2_name)) { 
      if (input$data2_name!="NONE" & 
          !is.null(input$plotType2) & 
          !is.null(input$xvar2) & 
          !is.null(input$yvar2) ) {
        
        data2 = DATA$mDATA[[input$data2_name]]
        data2 = data2 %>% filter(TEST %in% input$test2) 
        data2 = data2 %>% mutate_(GROUP_BY=input$group_by, 
                                  COLOR_BY=input$color_by, 
                                  SHAPE_BY=input$shape_by)
        
        data2[, input$xvar2] = as_numeric(data2[, input$xvar2])
        data2[, input$yvar2] = as_numeric(data2[, input$yvar2]) + EPS      # in case of log(0)
        data2 = data2 %>% filter_(!input$yvar2 %in% c(NA, Inf, -Inf))
        
        
        if (input$data_name==input$data2_name) { # if input$data_name==input$data2_name
          
          if (input$plotType2=="both")  {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), show_guide=FALSE, size=4) +
              geom_line(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), lwd=1)
          } else if (input$plotType2=="line")  {
            myFig = myFig +
              geom_line(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), lwd=1)
          } else if (input$plotType2=="point") {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=TRUE, aes_string(x=input$xvar2, y=input$yvar2), show_guide=FALSE,size=4)
          }    
        } else { # if input$data_name!=input$data2_name
          # # we may need to have these input variables
          # data2 = data2 %>% mutate_(GROUP_BY=input$group_by, 
          #                           COLOR_BY=input$color_by, 
          #                           SHAPE_BY=input$shape_by)
          # 
          if (input$plotType2=="both")  {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), show_guide=FALSE, size=4) +
              geom_line(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"),lwd=1)
          } else if (input$plotType2=="line")  {
            myFig = myFig +
              geom_line(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), lwd=1)
          } else if (input$plotType2=="point") {
            myFig = myFig +
              geom_point(data=data2, inherit.aes=FALSE, aes_string(x=input$xvar2, y=input$yvar2, group="USUBJID"), show_guide=FALSE,size=4)
          }
          
        }  # end of input$data_name!=input$data2_name
        
        
        
        
      }} # end of overlay  
    
    
    myFig
  })
  
  
  
  
  #-------------------------
  #
  #-------------------------
  
  output$figLog <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) + geom_point()
  })
  
  
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
      figLog()
      #ggplot(mtcars, aes(wt, mpg)) + geom_point()
    })
    
    plotOutput(ns("figLog"), width = "100%", height = "600px",   # height = 300,
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
    if (is.null(input$figLog_click)) {return(NULL)}
    nearPoints(filterData(), input$figLog_click, addDist = TRUE)   %>% 
      select(USUBJID, ARMA, VISIT, NTIM, TIME, TEST, DVOR, SOP)   #(mpg, cyl, disp, hp, wt)
  })
  
  output$brush_info <- renderPrint({
    #print("input$figLog_brush")
    #print(input$figLog_brush)
    if (is.null(input$figLog_brush)) {return(NULL)}
    brushedPoints(filterData(), input$figLog_brush)  %>%  
      select(USUBJID, ARMA, VISIT, NTIM, TIME, TEST, DVOR, SOP)   #select(mpg, cyl, disp, hp, wt)
  })
  
  
  
  #add figure to log when action button is pressed
  observeEvent(input$addToCart, {
    #req(input$addToCart, FIGURE_ALL, input$fig_name)
    
    if(is.null(input$addToCart))  {return()}
    if(input$addToCart == 0) return()
    cat(file=stderr(), "##############Step: Fig add2Cart #################", "\n")
    
    
    #CurrentLog   <- FIGURE_ALL$mFIGURES  #####################
    CurrentLog_mFIGURES   <- isolate(FIGURE_ALL$mFIGURES)
    newFig <-  figLog()
    newFig <- list(isolate(newFig))
    fig_name <- isolate(input$fig_name)
    names(newFig) = fig_name
    
    CurrentLog_mTITLES   <- isolate(FIGURE_ALL$mTITLES)
    newTitle <- (input$fig_title)
    names(newTitle) = fig_name
    
    CurrentLog_mFOOTNOTES  <- isolate(FIGURE_ALL$mFOOTNOTES)
    newFootnote <- (input$fig_footnote)
    names(newFootnote) = fig_name
    
    
    if (!fig_name %in% names(CurrentLog_mFIGURES)) {
      #FIGURE_ALL$mTITLES <- c(CurrentLog_mTITLES,newTitle)
      #FIGURE_ALL$mFIGURES <- c(CurrentLog_mFIGURES,newFig)
      #FIGURE_ALL$mFOOTNOTES <- c(CurrentLog_mFOOTNOTES,newFootnote)
      
      FIGURE_ALL$mTITLES[[fig_name]] <- newTitle
      FIGURE_ALL$mFIGURES[[fig_name]] <- newFig
      FIGURE_ALL$mFOOTNOTES[[fig_name]] <- newFootnote
      
      FIGURE_ALL$NumOfFig <- isolate(FIGURE_ALL$NumOfFig) + 1
      
    }else{ 
      
      FIGURE_ALL$mTITLES[[fig_name]] <- newTitle
      FIGURE_ALL$mFIGURES[[fig_name]] <- newFig
      FIGURE_ALL$mFOOTNOTES[[fig_name]] <- newFootnote
      
      cat(file=stderr(), "##############Warning: fig_name exist, override will occur! #################", "\n")
      #validate(
      #   need(!fig_name %in% names(CurrentLog_mFIGURES), "fig_name already exist, override will occur! ")
      #)
      
    }
    
  })
  
  
  
  #display number of tables stored
  output$FigCount <- renderText(
    
    FIGURE_ALL$NumOfFig
  )
  
  
  # Return the reactive that yields the data frame
  return(FIGURE_ALL)
}
