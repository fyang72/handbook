
module_save_data_UI <- function(id, label="") {                                                                                                                                         
  
ns <- NS(id)

fluidRow( 
  fluidRow( 
    column(width=3, 
           uiOutput(ns("data_name_container"))),                                                                                                                             
    
    column(width=2, 
           actionButton(ns("saveit"),label="Save it", style=actionButton.style)),                                                                                            
    
    column(width=2,                                                                                                                                                                   
           downloadButton(ns("downloadcsv"),label="csv", icon=icon("download"), style=actionButton.style)                                                                        
    ), 
    
    column(width=3, 
           h6("Select page length:"), align="right"),  #offset = 2,),    
    
    column(width=2, align="left",  #offset = 2,                                                                                                                                       
           selectInput(ns("pageLength"), label = NULL,                                                                                                        
                       width="100%",                                                                                                                                                  
                       choices = seq(1, 100, by=1),                                                                                                                                   
                       selected = 3)                                                                                                                                                  
    )
  ), 
  
  fluidRow(column(width=12, DT::dataTableOutput(ns("mydatatable")))),
  fluidRow(
    column(width=12,                                                                                                                                                                   
           uiOutput(ns("column_selector"))                                                                                                                                            
    )     
  )
)

}

################################################################################                                                                                                        
# log in and load data (sidebar)                                                                                                                                                        
################################################################################                                                                                                        

module_save_data <- function(input, output, session, ALL, data, data_name) {                                                                                                                 
  
ns <- session$ns 
  
inputData <- reactive({
  tdata = ALL$DATA[[data_name]]
  validate(need(tdata, message="no data found"))      
  tdata
})

output$data_name_container <- renderUI({                                                                                                                                             
  validate(need(globalVars$login$status, message=FALSE))                                                                                                                             
  
  textInput(ns("data_name"), value=NULL, placeholder="data name", width="100%", label=NULL)                                                                                                       
})

output$mydatatable <- DT::renderDataTable(                                                                                                                                          
  DT::datatable(data = inputData(),                                                                                                                                                       
                options = list(pageLength = input$pageLength, 
                               lengthChange = FALSE, width="100%", scrollX = TRUE)                                                                   
  ))

# selected_data                                                                                                                                                                       
output$column_selector <- renderUI({  
  data = inputData()
  validate(need(data, message=FALSE))                                                                                                                                                
  
  col.lst = c("", colnames(data))                                                                                                                                                    
  selectizeInput(ns("column_name_lst"),                                                                                                                                               
                 width="100%",                                                                                                                                                        
                 label    = "select columns",                                                                                                                                 
                 choices  = colnames(data),                                                                                                    
                 multiple = TRUE,                                                                                                                                                     
                 selected = colnames(data))                                                                                                 
})                                                                                                                                                                                    


filtered_data <- reactive({  
  data = inputData()
  validate(need(data, message="no data found"),                                                                                                                                      
           need(input$column_name_lst, message="no column selected")                                                                                                                  
  )      
  data %>% select(one_of(input$column_name_lst))                                                                                                                                     
})

# save data                                                                                                                                                                            
observeEvent(input$saveit, {                                                                                                                                                          
  
  validate(need(input$saveit, message=FALSE), 
           need(ALL$DATA, messag="No data found"),
           need(input$data_name, message="please specify data name")
  )  
  ALL$DATA[[input$data_name]] <- filtered_data()                                                                                                                                                  
  showNotification(paste0("saved to ", input$data_name, " in memory(session)"), type="message")
})

#downloadcsv
output$downloadcsv <- downloadHandler(    

  filename = function() {                                                                                                                                                             
    # data_name = attr(data, "name")                                                                                                                                                 
    ifelse(is.null(input$data_name),                                                                                                                                           
           paste0("download.csv", sep = ""),                                                                                                                                        
           paste0(input$data_name, ".csv", sep = ""))                                                                                                                                     
    
  },                                                                                                                                                                                  
  content = function(file) {      
    write_csv(filtered_data(), path=file )                                                                                                                                            
  }                                                                                                                                                                                   
)                                                                                                                                                                 

return(ALL)

}
