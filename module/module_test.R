




module_test_UI <- function(id, label = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width=12, uiOutput(ns("rHandsontab_container"))
    )
  )
}

 
module_test <- function(input, output, session, default_checkin)  {
  
  ns <- session$ns
 
  
  values = reactiveValues()
  values[["save"]] <- list()
  
  hot2r <- reactive({
    if(!is.null(input$hot)){
      hot_to_r(input$hot)
    }
  })
  
  observeEvent(hot2r(), {
    cat("hot2r has changed\n")
    #toSave <- c(isolate(values[["save"]]), list(hot2r()))
    #values[["save"]] <- toSave
    #saveRDS(toSave, file="aaa.rds")
  })
  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = default_checkin
      else
        DF = values[["DF"]]
    }
    values[["DF"]] = DF
    DF
  })
  
  output$rHandsontab_container <- renderUI({ 
    output$hot <- renderRHandsontable({
      DF = data()
      if (!is.null(DF))
        rhandsontable(DF, stretchH = "all")
    })
    rHandsontableOutput(ns("hot"), width = 350)
  })
  
} 
 



