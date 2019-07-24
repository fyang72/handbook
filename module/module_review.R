
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_figure_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("fig_selector")))),
         #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("fig_container"))))
  )
  
} 

# Module server function
module_review_figure <- function(input, output, session,  ALL) {
  ns <- session$ns
  
  # fig_selector
  output$fig_selector <- renderUI({  
    name_lst <- names(ALL$FIGURE)
    selectInput(ns("select_figure"), label = "Select Figure(s):",                                                                                                        
                width="100%",    
                multiple = TRUE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  # fig_container
  output$fig_container <- renderUI({ 
    validate(need(input$select_figure, message=FALSE))
    
    lapply(1:length(input$select_figure), function(i) {
      figure_name <- input$select_figure[i]
      
      validate(need(ALL$FIGURE[[figure_name]], message="no figure found"), 
               need(is.ggplot(ALL$FIGURE[[figure_name]]), message="only ggpot object allowed")
      )
      
      # callModule(module_save_figure
      ALL = callModule(module_save_figure, paste0("module_save_figure_", i), 
                       ALL, 
                       figure = ALL$FIGURE[[figure_name]], 
                       figure_index = i, 
                       figure_name = names(ALL$FIGURE[figure_name]), 
                       figure_data = ALL$FIGURE[[figure_name]]$data
      )
      
      module_save_figure_UI(ns(paste0("module_save_figure_", i)), label = NULL) 
    })
    
  })
  
}



#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_table_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("table_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("table_container"))))
  )
  
} 

# Module server function
module_review_table <- function(input, output, session, ALL) {
  ns <- session$ns
  
  # fig_selector
  output$table_selector <- renderUI({  
    name_lst <- names(ALL$TABLE)
    selectInput(ns("select_table"), label = "Select Table(s):",                                                                                                        
                width="100%",    
                multiple = TRUE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  # fig_container
  output$table_container <- renderUI({ 
    validate(need(input$select_table, message=FALSE))
    
    lapply(1:length(input$select_table), function(i) {
      
      table_name <- input$select_table[i]
      validate(need(ALL$TABLE[[table_name]], message="no table found"), 
               need(is.data.frame(ALL$TABLE[[table_name]]), message="only data.frame allowed")
      )
      
      # callModule(module_save_table
      ALL = callModule(module_save_table, paste0("module_save_table_", i), 
                       ALL,
                       table = ALL$TABLE[[table_name]], 
                       table_index = i, 
                       table_name = names(ALL$TABLE[table_name])
      )
      
      module_save_table_UI(ns(paste0("module_save_table_", i)), label = NULL) 
    })
    
  })
  
}





 

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_data_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("data_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("data_container"))))
  )
  
} 

# Module server function
module_review_data <- function(input, output, session, ALL) {
  ns <- session$ns
  
  # data_selector
  output$data_selector <- renderUI({  
    name_lst <- names(ALL$DATA)
    selectInput(ns("select_data"), label = "Select data(s):",                                                                                                        
                width="100%",    
                multiple = TRUE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  # data_container
  output$data_container <- renderUI({ 
    validate(need(input$select_data, message=FALSE))
    
    lapply(1:length(input$select_data), function(i) {
      data_name <- input$select_data[i]
      
      validate(need(ALL$DATA[[data_name]], message="no data found"), 
               need(is.data.frame(ALL$DATA[[data_name]]), message="only data.frame allowed")
      )
      
      # callModule(module_save_data
      ALL = callModule(module_save_data, paste0("module_save_data_", i), 
                       ALL,
                       data = ALL$DATA[[data_name]], 
                       data_index = i, 
                       data_name = names(ALL$DATA[data_name])
      )
      
      module_save_data_UI(ns(paste0("module_save_data_", i)), label = NULL) 
    })
    
  })
  
}

 


