
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
module_review_cppModel_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList( 
    fluidRow(
      column(6, uiOutput(ns("cppModel_selector")))
    ),
    
    fluidRow(
      column(12, 
             module_update_cppModel_UI(ns("cppModel_review"))
      )
    )
    #column(6, tags$hr(style="border-color: black;"))  
  )
  
} 

# Module server function
module_review_cppModel <- function(input, output, session, ALL) {
  ns <- session$ns
  
  # model_selector
  output$cppModel_selector <- renderUI({   
    name_lst <- c("", names(ALL$cppModel)) %>% unique()
    selectInput(ns("select_cppModel"), label = "Select cppModel:",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1]
    )
  }) 
  
  # call module_update_cppModel
  ALL = callModule(module_update_cppModel, "cppModel_review",
             ALL,
             cppModel_name = reactive(input$select_cppModel)
  )
  
  return(ALL)
  
}