


module_save_data2_UI <- function(id, label="") {                                                                                                                                         
  
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
  

fluidRow( 
  
  fluidRow( 
    
    column(width=3, uiOutput(ns("data_name_container"))),                                                                                                                             
    
    column(width=2, actionButton(ns("saveit"),label="Save it", style=actionButton.style)),                                                                                            
    
    column(width=2,                                                                                                                                                                   
           downloadButton(ns("downloadcsv"),label="csv", icon=icon("download"), style=actionButton.style)                                                                        
    ), 
    
    column(width=3, "Select page length:", align="right"),  #offset = 2,),    
    
    column(width=2, align="left",  #offset = 2,                                                                                                                                       
           selectInput(ns("pageLength"), label = NULL,# "Select page length",                                                                                                        
                       width="100%",                                                                                                                                                  
                       choices = seq(1, 100, by=1),                                                                                                                                   
                       selected = 3)                                                                                                                                                  
    )
  ), 
  
  fluidRow(column(width=12, DT::dataTableOutput(ns("mydatatable")))),
  
  fluidRow( #column(width=1, h5("row:"), align="right"),                                                                                                                                      
    
  ), 
  fluidRow(                                                                                                                                                                           
    #column(width=1, h5("column:"),align="right"),                                                                                                                                    
    column(width=12,                                                                                                                                                                   
           uiOutput(ns("column_selector"))                                                                                                                                            
    )     
  )
)

}

################################################################################                                                                                                        
# log in and load data (sidebar)                                                                                                                                                        
################################################################################                                                                                                        

module_save_data2 <- function(input, output, session, data1) {                                                                                                                 
  
  ns <- session$ns                                                                                                                                                                      
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332; vertical-align: bottom;"                                                               
  
  inputData <- reactive({
    
    # print("teststests12345")
    # print(data_name)
    # print(names(ALL$DATA))
    # print(head(tdata))
    print(head(data1))
    
    tdata = data1 # ALL$DATA[[data_name]]
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
                 label    = "filter column" , # "select column" ,                                                                                                                                 
                 choices  = colnames(data),  #unique(inputData()%>%pull(TEST)),                                                                                                      
                 multiple = TRUE,                                                                                                                                                     
                 selected = colnames(data))  #unique(inputData()%>%pull(TEST))[1])                                                                                                   
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
           need(input$data_name, message="please specify data name")
  )  
  ALL$DATA[[input$data_name]] <- filtered_data()                                                                                                                                                  
  
})


output$downloadcsv <- downloadHandler(                                                                                                                                                
  filename = function() {                                                                                                                                                             
    # data_name = attr(data, "name")                                                                                                                                                 
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



}