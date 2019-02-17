
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

barplotUI <- function(id, label="") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Load data and filter", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("data_Selector")),
                        uiOutput(ns("test_Selector")),
                        tags$hr(style="border-color: black;"),
                        
                        # uiOutput(ns("group_by_selector")), 
                        # uiOutput(ns("facet_by_selector")),
                        # uiOutput(ns("color_by_selector")),
                        # uiOutput(ns("shape_by_selector")),
                        # 
                        uiOutput(ns("dose_group_selector")),
                        textInput(ns("ylab"), label="Y label", value="", placeholder="AUC6wk"),
                        textInput(ns("font.size"), label="Font Size", value="", placeholder="14")
                        
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
             box(width=12, title="Barplot", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        textInput(ns("fig_name"), value="Fig-", label=NULL,  
                                  placeholder = "Enter data name here:")),
                 
                 column(width=4, #align="left", offset = 0,
                        actionButton(ns("addToCartFig"), label="Add to cart", style=actionButton.style)), 
                 
                  
                 uiOutput(ns("fig_title_selector")),
                 uiOutput(ns('container_barplot')),  #########################
                 uiOutput(ns("fig_footnote_selector"))
             ), 
             
             
             box(width=12, title="Table for barplot", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        textInput(ns("tab_name"), value="Tab-", label=NULL,  
                                  placeholder = "Enter data name here:")),
                 
                 column(width=4, #align="left", offset = 0,
                        actionButton(ns("addToCart"), label="Add to cart", style=actionButton.style)), 
                 
                 column(width=4, #status = "primary",  #class = 'rightAlign', #background ="aqua",
                        checkboxInput(ns("spreadOut"), label ="?Spreadout", value=FALSE)),
             
                 uiOutput(ns("tab_title_selector")),
                 uiOutput(ns('summaryTab_container')),
                 uiOutput(ns("tab_footnote_selector"))
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


# Module server function
barplot <- function(input, output, session, DATA, TABLE_ALL, FIGURE_ALL) {
  
  ns <- session$ns
  EPS = 1E-3
    
  #-----------------------------------------
  # on UI side
  #-----------------------------------------
  
  output$data_Selector <- renderUI({
    if (is.null(DATA$mDATA)) {return(NULL)}
    
    data.lst = c("NULL", unique(names(DATA$mDATA)))
    selectizeInput(ns("data_name"), 
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
   
  
  output$summarise_by_selector <- renderUI({
    data = inputData()
     
    # TEST_VARS = "TEST"
    # STUDYID_VARS = "STUDYID"
    # ARMA_VARS = "ARMA"
    # NTIM_VARS = ifelse(NTIM_VARS(data)!="", NTIM_VARS(data),
    #                    ifelse(TIME_VARS(data)!="", TIME_VARS(data), ""))
    selectizeInput(ns("summarise_by"), label ="Summarise by",   #h5()
                   choices = c(colnames(data)), 
                   multiple = TRUE,
                   selected = intersect(c("STUDYID","TEST","ARMA","NTIM"), colnames(data)) 
    )
  })
  
  
  
  
  output$valueCol_selector <- renderUI({
    data = inputData()
     
    
    DVOR = DVOR_VARS(data)
    selectizeInput(ns("valueCol"), 
                   label    = "select valueCol:" , 
                   choices  = colnames(data), 
                   multiple = FALSE,
                   selected = ifelse(DVOR!="NULL", DVOR, colnames(data)[1]))
  }) 
  
  
  output$group_by_selector <- renderUI({
    data = errbarTabb()
    selectizeInput(ns("group_by"), label ="Group by",   #h5()
                   choices = c(colnames(data)), 
                   multiple = TRUE,
                   selected = c("ARMA"))
  })
  
   
  
  output$dose_group_selector <- renderUI({
    data = inputData()
     
    
    if(is.null(data)) {return(NULL)}
    if(is.null(data$ARMA)) {return(NULL)}
    
    checkboxGroupInput(ns("dose_group"), label="Filter by dose groups", 
                       choices = unique(as.character(data$ARMA)), 
                       selected= unique(as.character(data$ARMA))    )})
  
    
   
  
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
    textAreaInput(ns("tab_title"), label = NULL,  width='100%', value = tab.title)})
  
  
  
  
  #"Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  #footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  output$tab_footnote_selector <- renderUI({
    textAreaInput(ns("tab_footnote"), label = NULL,  width='100%',
                  value = "Note: QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Concentrations below the lower limit of quantification (LLOQ) are set to zero. ")})
  
  
  
  
  
  ############################
  # for barplot  only
  ############################
  output$barplot_xvar_selector <- renderUI({
    data =  errbarTabb()
    if (is.null(data)) {return(NULL)}
    
    groupBy.lst = input$summarise_by  # data %>% select(-starts_with("Value")) %>% colnames()
    checkboxGroupInput(ns("barplot_xvar"), 
                       label    = h5("Select x for barplot"),inline = TRUE,   # 
                       choices  =  groupBy.lst,    
                       #multiple = TRUE, 
                       selected =  ifelse("ARMA" %in% groupBy.lst, "ARMA", groupBy.lst[1]))
  })
  
  
  output$barplot_yvar_selector <- renderUI({
    data = errbarTabb()   # myDATA()    updated
    if (is.null(data)) {return(NULL)}
    
    #yvar.lst = data %>% select(ValueCat) %>% unique()
    yvar.lst =  unique(data$TEST)  #data %>% select(contains("_div_")) %>% colnames()    # select the column that contains "_div_"
    
    radioButtons(ns("barplot_yvar"), 
                 label = h5("Select y for barplot"), inline = TRUE,   # 
                 choices =  yvar.lst, 
                 #multiple = FALSE,
                 selected = yvar.lst[1]
    )
  })  
  
  
  
  
   
  
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
  
  
  
  output$container_barplot<- renderUI({
     
    output$barplot <- renderPlot({
      barplot()
      #ggplot(mtcars, aes(wt, mpg)) + geom_point()
    })
    
    plotOutput(ns("barplot"), width = "100%", height = "500px"   
               #click = clickOpts(id = ns("figLog_click")),
               #brush = brushOpts(id = ns("figLog_brush")) 
    )
    #  
  })
  
  
  
  
   
  
  
  # select which dataset(s)
  inputData <- reactive({
    
    if (is.null(input$data_name)) {return(NULL)}
    if (input$data_name=="NULL") {return(NULL)}
    
    data = DATA$mDATA[[input$data_name]]
    
    if (is.null(data)) {return(NULL)}
    if (is.null(data$TEST)) {data$TEST= "xxxx"} 
     
    
    #save(data, file="dataInBarplot.RData")
    
   
    data
  })
  # 
  
  # select which dataset(s)
  errbarTabb <- reactive({
    
    data = inputData() 
   
    
    if (is.null(data) | length(data)==0) {return(NULL)}
    data = data %>% filter(ARMA %in% input$dose_group) 
    
    if (is.null(data)) {return(NULL)}
    if (is.null(input$test)) {return(NULL)}    
    
    #data = errbarTab8(data, input, session, EPS = 1e-3)  %>% select_(  "ARMA", "NTIM", "N", "Mean_SD", "Mean_SE", "Median_Range")
    if(!is.null(data$ARMA) & !is.null(input$dose_group)) {data = data %>% filter(ARMA %in% input$dose_group) }
    
    if(input$test!="") {data = data %>% filter(TEST %in% input$test) }
    
    
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
  #output$summaryTab_container <- renderUI({
  
  barplot <- reactive({
    pkSummary <- errbarTabb()
    if(is.null(pkSummary)) {return(NULL)}
    if(is.null(input$barplot_xvar)) {return(NULL)}
    if(is.null(input$barplot_yvar)) {return(NULL)}
    
    pkSummary <- pkSummary %>% filter(TEST==input$barplot_yvar)
    
    group = input$barplot_xvar[1] # i.e. "Category"
    subgroup = ifelse(length(input$barplot_xvar)==1, input$barplot_xvar[1],  input$barplot_xvar[2]) # i.e "Gender"
    if (group==subgroup) {barplot_title = paste0("Barplot of ", input$barplot_yvar, " by ", group)}
    if (group!=subgroup) {barplot_title = paste0("Barplot of ", input$barplot_yvar, " by ", group, " and ", subgroup)}
    
    # GROUP and SUBGROUP should be as factor
    tdata = pkSummary %>% ungroup() %>% 
      mutate(Mean=as.numeric(Mean),  SD=as.numeric(SD)) #%>% 
    # mutate_(group=group, subgroup=subgroup) %>% 
    # mutate(group=as.factor(as.character(group)), 
    #        subgroup=as.factor(as.character(subgroup)),
    #        STUDYID = as.factor(STUDYID), 
    #        ARMA = as.factor(ARMA), 
    #        TEST = as.factor(TEST)
    # )
    # 
    
    
    
    save(tdata, file="tmp.RData")
    
    # 
    tdata = tdata %>% as.data.frame()
    tdata[, group] = as.factor(tdata[, group])
    tdata[, subgroup] = as.factor(tdata[, subgroup])
    
    font.size = input$font.size
    
    #print(
    # ggplotly(  # make it dynamic, you can remove it. 
    
    
    # look at this link: 
    # https://datascienceplus.com/building-barplots-with-error-bars/  
    # code: https://gist.github.com/faulconbridge/6070ec3bf4acacdcd365  
    
    # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
    ggplot(data = tdata, aes_string(x=group, y="Mean", fill=subgroup)) +    # make it factor(Fields1)#   
      
      geom_bar(stat="identity", position=position_dodge(0.9) ) +        # steelblue,   fill="steelblue"
      geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), 
                    position = position_dodge(0.9), 
                    color="grey40", 
                    width=0.2) + 
      
      # scale_fill_manual(
      #          name=paste0(SUBGROUP, ""),
      #          values = c("#999999", "#E69F00", "#56B4E9", "#999999", "#E69F00", "#56B4E9"),  # c("red", "green", "blue"),
      #          breaks = c("male", "female", "unknown","male", "female", "unknown", "unknown"),
      #          labels = c("Male", "Female", "Unkown","Male", "Female", "Unkown","Male" )) +
      
      
      xlab("") + 
      ylab(ifelse(input$ylab=="", input$barplot_yvar, input$ylab))  + 
      
      theme_bw() + 
      ggtitle(barplot_title)  + 
      
      ggplot2::theme(    
        plot.title = ggplot2::element_text(hjust = 0.5, color="black", size = font.size , face = "bold") ,
        
        legend.text = ggplot2::element_text(size = font.size), 
        legend.title =  ggplot2::element_text(size = font.size),   #element_blank(),  # ggplot2::element_text(size = legend_title),     # Remove only the legend title by set legend.title = element_blank()
        #legend.key =  ggplot2::element_blank() , 
        
        legend.position = "bottom"
      )  + 
      guides(fill=guide_legend(nrow=1,byrow=TRUE))  + 
      
      # 
      #   # fine tune the barplot
      #   #--------------------------
    theme_bw() +  #can be ---> theme_minimal()    + theme_classic()
      ggtitle(barplot_title)  +
      
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, color="black", size = font.size , face = "bold") ,
        
        legend.text = ggplot2::element_text(size = font.size),
        legend.title = ggplot2::element_text(size = font.size),     # Remove only the legend title by set legend.title = element_blank()
        #legend.key =  ggplot2::element_blank() ,
        
        legend.position = "top",   ## left,top, right, bottom. legend.position='none', Remove the plot legend   c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position.
        #legend.justification = c(1, 1),     ??
        legend.box = "horizontal" ,  # Horizontal legend box
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="white"),
        
        
        axis.title   = ggplot2::element_text(size = font.size, face="bold"),
        axis.title.x = ggplot2::element_text(size = font.size, face = "bold"),
        axis.title.y = ggplot2::element_text(size = font.size, face = "bold"),
        
        axis.text   = ggplot2::element_text(color = "black", size = font.size), #axis tick text
        axis.text.x = ggplot2::element_text(color = "black", size = font.size, angle = 30, hjust = 1),
        axis.text.y = ggplot2::element_text(color = "black", size = font.size),
        
        axis.line = ggplot2::element_line(color='black'),
        
        strip.text = ggplot2::element_text(color = "black", size = font.size, face = "bold"),     # strip text in facetted plots.
        strip.text.x = ggplot2::element_text(color = "black", size = font.size, face = "bold"),
        strip.text.y = ggplot2::element_text(color = "black", size = font.size, face = "bold"),
        
        panel.background =  ggplot2::element_blank() ,
        #panel.grid = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major =   element_line(colour = "gray97",size=0.75)
      )  # ggplot2::element_blank()  
    
    # #))
    
    
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
    if (input$spreadOut==1) {newTab <- newTab %>% select(-Mean, -SD) %>% spread(key=TEST, value=Mean_SD) }
    
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
  
   
  #display number of tables stored
  output$TabCount <- renderText(
    
    TABLE_ALL$NumOfTab
  )
  
  
  
  
  
  
  #add figure to log when action button is pressed
  observeEvent(input$addToCartFig, {
    #req(input$addToCart, FIGURE_ALL, input$fig_name)
    
    if(is.null(input$addToCartFig))  {return()}
    if(input$addToCartFig == 0) return()
    cat(file=stderr(), "##############Step: Fig add2Cart #################", "\n")
    
    
    #CurrentLog   <- FIGURE_ALL$mFIGURES  #####################
    CurrentLog_mFIGURES   <- isolate(FIGURE_ALL$mFIGURES)
    newFig <-  barplot()
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
  
  output$fig_title_selector <- renderUI({
        
        data = errbarTabb() 
        patient.name = "Healthy Volunteers"
        dosing.name = paste0("a Single Subcutaneous or Intravenous Dose(s)")
        drug.name = substr(data$STUDYID, 1, 5)
        study.name = unique(data$STUDYID)
        
        
        key = "Individual"
        
        
        
        fig.title =  "Barplot of ....Insert figure title here "
        
        
        #fig.title = "Figure title: Mean (+SE) Log-scaled Concentrations of xxxxx in Serum vs. Nominal Sampling Day Following a Single IV Dose of xxxx (Study xxxx-HV-1219"
        textAreaInput(ns("fig_title"), label = NULL,  width='100%', value = fig.title)
    })
  
  
  #"Note: Concentrations below the lower limit of quantification (LLOQ) are set to zero.", sep="") }
  #footnote = paste("QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous. Note: Concentrations below the lower limit of quantification (LLOQ, horizontal dotted line = ", LLOQ, " mg/L) are imputed as LLOQ/2 = ", round(LLOQ/2, digits=3), " mg/L.", sep="")
  output$fig_footnote_selector <- renderUI({
    textAreaInput(ns("fig_footnote"), label = NULL,  width='100%',
                  value = "Note: QW = Once a week; Q2W = Once every two weeks; Q4W = Once every four weeks; SC = Subcutaneous; IV = Intravenous.  Concentrations below the lower limit of quantification (LLOQ) are set to zero. ")
    })
  
  
  
  
  #display number of tables stored
  output$FigCount <- renderText(
    
    FIGURE_ALL$NumOfFig
  )
  
  
  
  
  
  
  # Return the reactive that yields the data frame
  return(list(TABLE_ALL=TABLE_ALL, FIGURE_ALL=FIGURE_ALL))
}
