

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------

figReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Control panel", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("figSelector")) 
                        
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        tags$hr(style="border-color: black;")
                        
                 )) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Render figure", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 uiOutput(ns("fig_title_selector")),
                 uiOutput(ns('figReview_container')),     #
                 uiOutput(ns("fig_footnote_selector"))
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
figReview <- function(input, output, session,  DATA, FIGURE_ALL) {
  
  ns <- session$ns
  
  #-----------------------------------------
  # on UI side
  #-----------------------------------------
  
  output$fig_title_selector <- renderUI({
    if (is.null(input$selectFig)) {return(NULL)}
    newTitle <- isolate(FIGURE_ALL$mTITLES[[input$selectFig]]) 
    textAreaInput(ns("fig_title"), label = NULL,  width='100%', value = newTitle)})
  
  output$fig_footnote_selector <- renderUI({
    if (is.null(input$selectFig)) {return(NULL)}    
    newFootnote <- isolate(FIGURE_ALL$mFOOTNOTES[[input$selectFig]]) 
    textAreaInput(ns("fig_footnote"), label = NULL,  width='100%', value = newFootnote)})
  
  
  
  output$figSelector <- renderUI({  
    selectInput(ns("selectFig"), label="Select figure:", names(FIGURE_ALL$mFIGURES))
  })
  
  
  ## use renderUI to display table
  output$figReview_container <- renderUI({
    plotOutput(ns("figReview"), width = "100%", height = "600px")
  })
  
  
  output$figReview <- renderPlot({
    #req(input$selectFig)
    if (is.null(input$selectFig)) {return(NULL)}
    
    which_fig = input$selectFig
    newFig <- isolate(FIGURE_ALL$mFIGURES[[which_fig]]) 
    newFig
    
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("figure review accordingly.")
    cat(msg, "\n")
  })
  
  
}




#### Table review ####


tabReviewUI <- function(id, label = "") {
  #---
  #xxxInput/Output, xxxControl, xxxUI, xxx
  
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Control panel", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("tabSelector")) 
                        
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        tags$hr(style="border-color: black;")
                        
                 )) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Render table:", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 uiOutput(ns("tab_title_selector")),
                 uiOutput(ns('tabReview_container')),     #
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
tabReview <- function(input, output, session,  DATA, TABLE_ALL) {
  
  ns <- session$ns
  
  #--------- dgdgdgdgdgssssss --------
  
  
  output$tab_title_selector <- renderUI({
    if (is.null(input$selectTab)) {return(NULL)}
    newTitle <- isolate(TABLE_ALL$mTITLES[[input$selectTab]]) 
    textAreaInput(ns("tab_title"), label = NULL,  width='100%', value = newTitle)})
  
  output$tab_footnote_selector <- renderUI({
    if (is.null(input$selectTab)) {return(NULL)}    
    newFootnote <- isolate(TABLE_ALL$mFOOTNOTES[[input$selectTab]]) 
    textAreaInput(ns("tab_footnote"), label = NULL,  width='100%', value = newFootnote)})
  
  
  
  output$tabSelector <- renderUI({  
    selectInput(ns("selectTab"), label="Select table:", names(TABLE_ALL$mTABLES))
  })
  
  
  ## use renderUI to display table
  output$tabReview_container <- renderUI({
    
    
    DT::dataTableOutput(ns("tabReview"))
    
  })
  
  
  
  output$tabReview <- DT::renderDataTable({
    #req(input$selectTab)
    
    
    if (is.null(input$selectTab)) {return(NULL)}
    
    which_tab = input$selectTab
    
    
    newTab <- isolate(TABLE_ALL$mTABLES[[which_tab]]) 
    print("newTab")
    print(newTab)
    
    DT::datatable(data = newTab,
                  options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                  #filter = 'top',    # bottom',
                  #caption = htmltools::tags$caption(
                  # style = 'caption-side: bottom; text-align: center;',
                  # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                  #  )
    )  
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("table review accordingly.")
    cat(msg, "\n")
  })
  
  
}









#### dataset review ####


datasetReviewUI <- function(id, label = "") {
  #---
  #xxxInput/Output, xxxControl, xxxUI, xxx
  
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Control panel", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("datasetSelector")) 
                        
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        tags$hr(style="border-color: black;")
                        
                 )) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Render dataset:", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 #uiOutput(ns("tab_title_selector")),
                 uiOutput(ns('datasetReview_container'))     #
                 #uiOutput(ns("tab_footnote_selector"))
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
datasetReview <- function(input, output, session,  DATA, TABLE_ALL) {
  
  ns <- session$ns
  
  
  output$datasetSelector <- renderUI({  
    selectInput(ns("selectDataset"), label="Select dataset:", names(DATA$mDATA))
  })
  
  
  ## use renderUI to display table
  output$datasetReview_container <- renderUI({
    
    output$datasetReview <- DT::renderDataTable({
      #req(input$selectTab)
      
      
      if (is.null(input$selectDataset)) {return(NULL)}
      
      which_dataset = input$selectDataset
      
      
      newDataset <- isolate(DATA$mDATA[[which_dataset]]) 
      
      DT::datatable(data = newDataset,
                    options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                    #filter = 'top',    # bottom',
                    #caption = htmltools::tags$caption(
                    # style = 'caption-side: bottom; text-align: center;',
                    # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                    #  )
      )  
    })
    
    DT::dataTableOutput(ns("datasetReview"))
    
  })
  
  
  
}








#### model review ####


modelReviewUI <- function(id, label = "") {
  #---
  #xxxInput/Output, xxxControl, xxxUI, xxx
  
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           fluidRow(
             box(width=12, title="Control panel", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" , 
                 column(6,
                        # data and filter
                        uiOutput(ns("modelSelector")) 
                        
                 ), 
                 column(6,
                        # x-variable, y-variable, and group_by
                        tags$hr(style="border-color: black;")
                        
                 )) # box
           ) # end of fluidRow
    ),  # end of left column
    
    column(8, 
           fluidRow(
             box(width=12, title="Render model:", collapsible = TRUE, collapsed = FALSE,  solidHeader = TRUE, status =  "primary" ,      
                 
                 
                 #uiOutput(ns('modelReview_container'))     #
                 htmlOutput(ns('modelReview_container'))     #
                 
             )
           )
    )
  )
  
} 

# Module server function
modelReview <- function(input, output, session) {
  
  ns <- session$ns
  
  
  output$modelSelector <- renderUI({  
    
    model.lst <-list.files(path = "./cpp", all.files = TRUE,full.names = FALSE, include.dirs = FALSE, recursive =TRUE)
    model.lst <- gsub(".cpp", "", model.lst, fix=TRUE)
    
    selectInput(ns("selectModel"), label = "Select Model:", # h5("Option-1: choose from model library"), 
                choices = c("", model.lst ),
                selected = "")  # ./cpp/Cemiplimab.cpp") 
    
    
  })
  
  
  ## use renderUI to display table
  output$modelReview_container <- renderUI({
    
    validate(need(input$selectModel, message=FALSE))
    
    file.name = paste0("./cpp/", input$selectModel, ".cpp")  
    cppmodel = readLines(file.name)
    cppmodel = paste0(cppmodel, collapse="\n")
    
     
    
    #textAreaInput(ns("scriptArea"), label=NULL, value= gsub("\n", '<br/>', cppmodel, fixed=TRUE), rows=25, placeholder = "add your script here.")
    
    value= gsub("\n", '<br/>', cppmodel, fixed=TRUE)
    HTML(value)
    # output$htmlscript <- renderUI({
    #    i = values[["iScript"]]
    #    value =  scriptTab$VALUE[i]
    #   # textAreaInput(ns("scriptArea"), label=NULL, value= NULL,  rows=25, placeholder = "data %>% mutate(TEST=1)")
    #   value = gsub("\n", '<br/>', value, fixed=TRUE)
    #   
    #   HTML(value)
    # 
    # })
    #htmlOutput(ns("htmlscript"))
    # verbatimTextOutput(ns("textAreascript"))
    #output$textAreascript <- renderUI({
    
    
    
    
    
  })
  
  
  
}

