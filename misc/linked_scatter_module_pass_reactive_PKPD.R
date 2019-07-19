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

linkedScatter <- function(input, output, session, data, left, right) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- reactive({
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
  
  linkedScatterUI("scatters"),
  textOutput("summary")
)

server <- function(input, output, session) {
  data(mpg)
  
  setwd("~/handbook/")
  source("~/handbook/global.R")
  
  df <- callModule(linkedScatter, "scatters", 
                   data = reactive(mpg),
                   left = reactive(input$select_left),
                   right = reactive(input$select_right)
  )
  
  output$summary <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
}

shinyApp(ui, server)