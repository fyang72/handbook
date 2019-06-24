# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

library(shiny)
library(ggplot2)

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,  
              actionButton(ns("delete_model"),label="Delete model", 
                           style=actionButton_style))
             ),
    
    fluidRow(
      column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
      column(6, plotOutput(ns("plot2"), brush = ns("brush")))
    )
  )
}

linkedScatter <- function(input, output, session, data, left, right, ALL, cppModel_name) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
 
  
  print("in linkedScatter:")
  
  dataWithSelection <- reactive({
    brushedPoints(data(), input$brush, allRows = TRUE)
  })
  
  output$plot1 <- renderPlot({ 
    scatterPlot(dataWithSelection(), left())
  })
  
  output$plot2 <- renderPlot({ 
    scatterPlot(dataWithSelection(), right())
  })
  
  #--------------------
  # event for delete_model
  #--------------------
  observeEvent(input$delete_model, {
  #modify_data <- reactive({
    #brushedPoints(data(), input$brush, allRows = TRUE)
   
    #mymodel =input$cppModel_content
    #validate(need(mymodel, message="Empty cppModel..."))
    
    validate(need(input$delete_model, message="delete_model..."))
    
    print("in deleting:")
    #ALLDATA = ALL
    ALL$cppModel[[isolate({cppModel_name()})]]  = NULL 
    print(names(ALL$cppModel))
    
    showNotification("cppModel deleted", type="message")  # "default, "message", "warning", "error"  
    ALL
  })
  
  return(ALL) # dataWithSelection)
}

scatterPlot <- function(data, cols) {
  
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
    geom_point(aes(color = selected_)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}






ui <- fixedPage(
  h2("Module example"),
   
  selectInput(("select_left"), label = "Select left:",                                                                                                        
              width="100%",    
              multiple = TRUE, 
              choices = colnames(mpg),                                                                                                                                   
              selected = c("cty", "hwy")
  ),
  
  selectInput(("select_right"), label = "Select right:",                                                                                                        
              width="100%",    
              multiple = TRUE, 
              choices = colnames(mpg),                                                                                                                                   
              selected = c("drv", "hwy")
  ),
  
  uiOutput("cppModel_selector"),
  
  linkedScatterUI("scatters"),
  textOutput("summary")
)

server <- function(input, output, session) {
  data(mpg)
   
  cppModel1=mread(model='cppModel',
                 project=paste0(HOME, '/cpp/'),
                 quiet=TRUE,
                 file=basename("LN001.cpp"))
  
  #ALL$cppModel[["LN0011.cpp"]] = cppModel
  
  
  cppModel2=mread(model='cppModel',
                 project=paste0(HOME, '/cpp/'),
                 quiet=TRUE,
                 file=basename("LN001.cpp"))
  
  #ALL$cppModel[["LN0022.cpp"]] = cppModel
  
  #load_cppModel <- reactive({
  #ALL <- list(
 ALL <- reactiveValues(
    DATA   = list(), 
    FIGURE = list(), 
    TABLE = list(), 
    cppModel = list("LN0011.cpp"=cppModel1, 
                    "LN0022.cpp"=cppModel2), 
    ctlModel = list(), 
    script = list()
  )
   

    
    #ALL
  #})
  
  ################################################
  output$cppModel_selector <- renderUI({   
    
    
    
    name_lst <- c("", names(ALL$cppModel)) %>% unique()
    selectInput( ("select_cppModel"), label = "Select cppModel:",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1]
    )
  })
  
  
    ALL <- callModule(linkedScatter, "scatters", 
                   data = reactive(mpg),
                   left = reactive(input$select_left),
                   right = reactive(input$select_right), 
                   
                   (ALL), 
                   cppModel_name = reactive(input$select_cppModel)
  )
  
  output$summary <- renderText({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
    
    sprintf("%d observation(s) selected", length(ALL$cppModel))
    
  })
}

shinyApp(ui, server)