
 



default_tabOutput_Input <- function(id, label="") {
  
  ns <- NS(id)
   
  fluidPage(
    fluidRow(uiOutput(ns("datatable_container"))), 
    fluidRow( 
      column(10, align="left", offset =7,
             selectInput(ns("pageLength"), label = "Select page length", 
                         choices = seq(1, 100, by=1), 
                         selected = 3)) )
   )
          
}
 




################################################################################ 
# log in and load data (sidebar)
################################################################################

default_tabOutput <- function(input, output, session, mytab) {
  
  ns <- session$ns
  
  # selected_data
  output$pageLength_selector <- renderUI({
    selectInput(ns("pageLength"), label = "Select page length", 
                choices = seq(1, 100, by=1), 
                selected = 3)})
   
  # ggplot_container
  output$datatable_container <- renderUI({
    output$mydatatable <- DT::renderDataTable(
      DT::datatable(data = mytab,
                    options = list(pageLength = input$pageLength, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                    #filter = 'top',    # bottom',
                    #caption = htmltools::tags$caption(
                    # style = 'caption-side: bottom; text-align: center;',
                    # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                    #  )
      ))
    DT::dataTableOutput(ns("mydatatable"))
  })
  
}
