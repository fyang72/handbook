

https://stackoverflow.com/questions/54652364/r-shiny-automatically-start-download?rq=1


ui <- fluidPage(
  useShinyjs(),
  conditionalPanel(
    "false", # always hide the download button
    downloadButton("downloadData")
  ),
  actionButton("check", "Download")
)

server <- function(input, output, session){
  
  dat <- mtcars
  
  processData <- function(dat) {
    
    return(dat)
  }
  
  finalData <- reactiveVal() # to store the processed data
  observeEvent(input$check, {
    if(!is.null(df <- processData(dat))){
      finalData(df)
      runjs("$('#downloadData')[0].click();")
    }else{
      print("wrong")
      # something which throws an alert message "invalid data" 
      # (eg with shinyBS::createAlert or shinyWidgets::sendSweetAlert)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      withProgress(message = 'Export data', value = 0, {
        # Number of steps
        n <- 3
        
        incProgress(1/n, detail = "Pre checks and get data")
        
        incProgress(1/n, detail = "Post Proces and check")
       
        incProgress(1/n, detail = "generate flatfile")
        
      write.csv(finalData(), file)
      })
    }
  )
}

shinyApp(ui, server)
