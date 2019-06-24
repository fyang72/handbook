# https://stackoverflow.com/questions/36695577/shiny-module-that-calls-a-reactive-data-set-in-parent-shiny-server

library(shiny)
library(ggplot2)

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
  )
}

linkedScatter <- function(input, output, session, data, left, right) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- reactive({
    
    print("sfsf")
    #print(head(data()))
    print(input$brush)
    
    #validate(need(input$brush, message=FALSE))
    
    if (!is.null(input$brush)) { 
      
      print(head( brushedPoints(data(), input$brush, allRows = TRUE)))
      }
    
    brushedPoints(data(), input$brush, allRows = TRUE)
    
  })
  
  output$plot1 <- renderPlot({
    scatterPlot(dataWithSelection(), left())
  })
  
  output$plot2 <- renderPlot({
    scatterPlot(dataWithSelection(), right())
  })
  
  return(dataWithSelection)
}

scatterPlot <- function(data, cols) {
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
    geom_point(aes(color = selected_)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}

ui <- fixedPage(
  h2("Module example"),
  linkedScatterUI("scatters"),
  textOutput("summary")
)

server <- function(input, output, session) {
  data(mpg)
 
  df <- callModule(linkedScatter, "scatters", 
                   data = reactive(mpg),
                   left = reactive(c("cty", "hwy")),
                   right = reactive(c("drv", "hwy"))
  )
  
  output$summary <- renderText({
    #sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
}

shinyApp(ui, server)