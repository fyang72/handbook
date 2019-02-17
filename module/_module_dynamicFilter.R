#Dynamically adding modules
# https://stackoverflow.com/questions/36841477/shiny-modules-how-to-access-the-input-of-a-looped-shiny-module
## https://stackoverflow.com/questions/36043032/dynamically-adding-modules-in-shiny/36445844#36445844

# https://gallery.shinyapps.io/insertUI-modules/     greatest example of module
# https://shiny.rstudio.com/articles/dynamic-ui.html     insertUI and removeUI


library(shiny)
library(magrittr)
library(dplyr)

#Filter module ------------------------
FilterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, uiOutput(ns('col_Selector'))) 
      #column(12, uiOutput(ns('removeBtn')))
    ),
    fluidRow( 
      column(12, uiOutput(ns('row_Selector')))
    )
  )
  
}

Filter <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$removeBtn, {
    removeUI(
      selector = 'div:has(> #col_Selector)'
    )
    
    removeUI(
      selector = 'div:has(> #row_Selector)'
    )
  })  
  
  
  
  output$removeBtn <- renderUI({ 
    actionButton(
      inputId= ns("removeFilterId"),
      label = "Delete",
      class = "btn-danger"   # http://getbootstrap.com/docs/4.0/components/buttons/
    ) 
  })  
  
  
  
  output$col_Selector <- renderUI({ 
    #data = inputData()
    
    if(is.null(data)) {return(NULL)}
    
    selectInput(ns('colfilterId'), label =NULL, choices = as.list(names(data)), selected = 1)
  })
  
  output$row_Selector <- renderUI({
    if(is.null(data)) {return(NULL)}
    if(is.null(input$colfilterId)) {return(NULL)}   
    
    #data = inputData
    col = data[[(input$colfilterId)]]
    
    
    values = unique(col)  
    min=min(values,na.rm=TRUE)
    max=max(values, na.rm=TRUE)
    
    switch(typeof(col), 
           "double"= sliderInput(ns('rowfilterId'), label = NULL,min, max, value= c(min, max)),
           "integer" = checkboxGroupInput(ns('rowfilterId'), label = NULL,
                                          choices = values, selected = values ), 
           "character" = checkboxGroupInput(ns('rowfilterId'), label = NULL,
                                            choices = values, selected = values )
    )
    
  })
  
  return(list(colfilterId=reactive(input$colfilterId), 
              rowfilterId=reactive(input$rowfilterId) ,
              removeFilterId=reactive(input$removeFilterId)
  ))    
}

# 
# #shiny app ------------------------
# ui <- fixedPage(
#   fixedRow(
#     column(width = 2,  
#            wellPanel(
#              h4("Dynamic Filters"),
#              #p("Click to get more filters"),
#              actionButton(
#                inputId= ("addFilterModule"),
#                label = "Add More Filter",
#                class = "btn-primary"   # http://getbootstrap.com/docs/4.0/components/buttons/
#              ), 
#              
#              hr(),
#              tags$script(HTML('
#                               Shiny.addCustomMessageHandler("moveModule", function(message) {
#                               var source = document.getElementById("addModule").childNodes;
#                               var target = document.getElementById("target");
#                               for (var i = 0; i < source.length; i++) {
#                               target.appendChild(source[i]);
#                               }
#                               })
#                               ')),
#              tags$div(id = "target"),
#              uiOutput("addModule"))), 
#     column(width = 8, wellPanel(tableOutput("data")))
#     
#            )
#   )
# 
# server <- function(input, output, session) {
#   #server code for the original module
#   mtcars <- mtcars %>% mutate(model=c(rep("a", time=16), rep("b", time=16)), 
#                               cyl=as.integer(cyl), 
#                               vs=as.integer(vs), 
#                               am=as.integer(am))
#   
#   aggregFilters <- reactiveValues()
#   aggregFilters2 <- reactiveValues(data2=mtcars)
#   
#   #Here we add the UI and callModule of the duplicate module
#   observeEvent(input$addFilterModule, {
#     session$sendCustomMessage(type = "moveModule", message = "Something")
#     
#     # add FilterUI
#     duplicateFilterid <- paste0("duplicateFilter", input$addFilterModule)
#     output$addModule <- renderUI({
#       FilterUI(duplicateFilterid)
#     })
#     
#     # call module, return reactive
#     tt = callModule(Filter, duplicateFilterid, data2())
#     aggregFilters[[duplicateFilterid]]$col = tt[['colfilterId']]
#     aggregFilters[[duplicateFilterid]]$row = tt[['rowfilterId']]
#     aggregFilters[[duplicateFilterid]]$remove = tt[['removeFilterId']] 
#     
#   })
#   
#   # reduce the colsize after add each filter  
#   data2 <- reactive({
#     data2 = isolate(aggregFilters2$data2) 
#     for (i in 1:length(names(aggregFilters))) {
#       filter = aggregFilters[[names(aggregFilters)[i]]]
#       if (is.null(filter$col())) {next}
#       data2 = data2 %>% select(-one_of(filter$col())) 
#     }   
#     data2
#   })
#   
#   # render the filtered data
#   output$data <- renderTable({
#     data = mtcars  
#     
#     if (is.null(aggregFilters)) {return(NULL)}
#     if (length(names(aggregFilters))==0) {return(NULL)} 
#     
#     #data= invisible(lapply(aggregFilters, function(filter){
#     for (i in 1:length(names(aggregFilters))) {
#       filter = aggregFilters[[names(aggregFilters)[i]]]
#       if (is.null(filter$col())) {next}
#       if (is.null(filter$row())) {next}  
#       
#       col = data[[filter$col()]] 
#       print(filter$row())
#       data = switch(typeof(col), 
#                     "double"= data[which(col>= filter$row()[1], col<= filter$row()[2]), ], 
#                     "integer" = data[which((col%in% filter$row())), ], 
#                     "character" = data[which((col%in% filter$row())), ]
#       )
#       
#     }
#     #})  
#     
#     data
#     
#   })
#   
#   
#   
#   
# }
# 
# shinyApp(ui, server)
# 

