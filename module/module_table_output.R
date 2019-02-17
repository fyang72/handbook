
module_table_output_UI <- function(id, label="") {
  
  ns <- NS(id)
  # https://groups.google.com/forum/#!topic/shiny-discuss/vgeURRSDScA  
  # div(style="display:inline-block",submitButton("Analysis"), style="float:right"),
  # div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")
  
  
  # tags$style(type='text/css', "#search_button { vertical-align: top; }"),
  # if that doesn't work try something like:
  # tags$style(type='text/css', "#search_button { padding-bottom: 35px; }"),
  # and change the number (35) till it looks right.
  # tags$style(type='text/css', "button#searchButton { margin-bottom: 9px; }")
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332; vertical-align: bottom;"
  
  fluidPage(
    fluidRow(
      uiOutput(ns("datatable_container"))
      ), 
     
    fluidRow( 
      
      column(width=1, h5("column:"),align="right"),
      column(width=6,  
             uiOutput(ns("column_selector"))
             ), 
      
      column(width=1, h5("row:"), align="right"),
      column(width=2, #align="left", # offset =7,
             selectInput(ns("pageLength"), label = NULL, # "Select page length", 
                         width="100%",
                         choices = seq(1, 100, by=1), 
                         selected = 3)
             ), 
      
      column(width=2, 
             downloadButton(ns("downloadcsv"),label="download", icon=icon("download"), style=actionButton.style)
            )
      )
    
   )
          
}
 

################################################################################ 
# log in and load data (sidebar)
################################################################################

module_table_output <- function(input, output, session, mytab) {
  
  ns <- session$ns
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332; vertical-align: bottom;"
  
  
  # selected_data
  output$column_selector <- renderUI({
    validate(need(mytab, message=FALSE))
    
    col.lst = c("", colnames(mytab))
    selectizeInput(ns("column_name_lst"), 
                   width="100%",
                   label    = NULL, # "select column" , 
                   choices  = colnames(mytab),  #unique(inputData()%>%pull(TEST)),    
                   multiple = TRUE, 
                   selected = colnames(mytab))  #unique(inputData()%>%pull(TEST))[1])   
  })
  


   
  # ggplot_container
  output$datatable_container <- renderUI({
    tdata = filtered_data() # 
  
    validate(need(tdata, message="no data found"))
    
    output$mydatatable <- DT::renderDataTable(
      DT::datatable(data = tdata,
                    options = list(pageLength = input$pageLength, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                    #filter = 'top',    # bottom',
                    #caption = htmltools::tags$caption(
                    # style = 'caption-side: bottom; text-align: center;',
                    # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                    #  )
      ))
    DT::dataTableOutput(ns("mydatatable"))
  })
  
  
  output$pageLength_selector <- renderUI({
    selectInput(ns("pageLength"), label = "Select page length", 
                choices = seq(1, 100, by=1), 
                selected = 3)
  })
  
  
  output$downloadcsv <- downloadHandler(
    filename = function() {
      # data_name = attr(mytab, "name")
      # filename = ifelse(is.null(data_name), 
      #        paste0("download.csv", sep = ""), 
      #        paste0(data_name, ".csv", sep = "")) 
      #  
      "download.csv" 
    },
    content = function(file) {
      write_csv(filtered_data(), path=file )
    }
  )
  
  filtered_data <- reactive({
    validate(need(mytab, message="no data found"), 
             need(input$column_name_lst, message="no column selected")
             )
    
    mytab %>% select(one_of(input$column_name_lst))
  })
  
  
  
}
