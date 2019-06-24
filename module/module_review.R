
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn?t part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
figReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("fig_selector")))),
         #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("fig_container"))))
  )
  
} 

# Module server function
figReview <- function(input, output, session,  ALL) {
  ns <- session$ns
  
  # fig_selector
  output$fig_selector <- renderUI({  
    name_lst <- names(ALL$FIGURE)
    selectInput(ns("selectFig"), label = "Select Figure(s):",                                                                                                        
                width="100%",    
                multiple = TRUE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  # fig_container
  output$fig_container <- renderUI({ 
    validate(need(input$selectFig, message=FALSE))
    
    lapply(1:length(input$selectFig), function(i) {
      validate(need(ALL$FIGURE[[i]], message="no figure found"), 
               need(is.ggplot(ALL$FIGURE[[i]]), message="only ggpot object allowed")
      )
      
      # callModule(module_save_figure
      ALL = callModule(module_save_figure, paste0("module_save_figure_", i), 
                       ALL, 
                       figure = ALL$FIGURE[[i]], 
                       figure_index = i, 
                       figure_name = names(ALL$FIGURE[i]), 
                       figure_data = ALL$FIGURE[[i]]$data
      )
      
      module_save_figure_UI(ns(paste0("module_save_figure_", i)), label = NULL) 
    })
    
  })
  
}



#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
tablReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("table_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("table_container"))))
  )
  
} 

# Module server function
tablReview <- function(input, output, session, ALL) {
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
      validate(need(ALL$TABLE[[i]], message="no table found"), 
               need(is.data.frame(ALL$TABLE[[i]]), message="only data.frame allowed")
      )
      
      # callModule(module_save_table
      ALL = callModule(module_save_table, paste0("module_save_table_", i), 
                       ALL,
                       table = ALL$TABLE[[i]], 
                       table_index = i, 
                       table_name = names(ALL$TABLE[i])
      )
      
      module_save_table_UI(ns(paste0("module_save_table_", i)), label = NULL) 
    })
    
  })
  
}






#-----------------------------------------
#### dataset review ####
#-----------------------------------------

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
dataReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("data_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12,uiOutput(ns("data_container"))))
  )
  
} 

# Module server function
dataReview <- function(input, output, session, ALL) {
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
      validate(need(ALL$DATA[[i]], message="no data found"), 
               need(is.data.frame(ALL$DATA[[i]]), message="only data.frame allowed")
      )
      
      # callModule(module_save_data
      ALL = callModule(module_save_data, paste0("module_save_data_", i), 
                       ALL,
                       data = ALL$DATA[[i]], 
                       data_index = i, 
                       data_name = names(ALL$DATA[i])
      )
      
      module_save_data_UI(ns(paste0("module_save_data_", i)), label = NULL) 
    })
    
  })
  
}

 
#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
cppModelReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList( 
    fluidRow(column(6, uiOutput(ns("cppModel_selector")))),
    fluidRow(column(12, 
                    #uiOutput(ns("update_cppModel_container"))
                    module_update_cppModel_UI(ns("cppModel_review"), label = NULL)
                    ))
    
    
    
    #column(6, tags$hr(style="border-color: black;"))  
  )
  
} 

# Module server function
cppModelReview <- function(input, output, session, ALL) {
  
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

  callModule(module_update_cppModel, "cppModel_review",
             ALL,
             cppModel_name = reactive(input$select_cppModel)
  )
  
  ## use renderUI to display table
  #output$update_cppModel_container <- renderUI({
    
   # validate(need(input$select_cppModel, message=FALSE))
     
    #script_content = readLines(paste0(HOME, "/cpp/", input$select_cppModel))
    #script_content <- capture.output(
    #  mrgsolve::see(ALL$cppModel[[input$select_cppModel]])
    #)
      
    # aceEditor(ns("cppModel_content"),
    #           mode="c_cpp",
    #           value=paste0(script_content, collapse="\n"),
    #           theme = "crimson_editor",   # chrome
    #           autoComplete = "enabled",
    #           height = "1000px",
    #           fontSize = 15
    # )
    
  #      callModule(module_update_cppModel, "cppModel_review",
  #                      ALL,
  #                      cppModel_name = reactive(input$select_cppModel)
  #     )
  # 
  #     module_update_cppModel_UI(ns("cppModel_review"), label = NULL)
  #     
  # })

}


#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI, xxx
#-----------------------------------------
ctlModelReviewUI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidRow( 
    fluidRow(column(12, uiOutput(ns("ctlModel_selector")))),
    #column(6, tags$hr(style="border-color: black;"))  
    fluidRow(column(12, uiOutput(ns("ctlModel_container"))))
  )
  
} 

# Module server function
ctlModelReview <- function(input, output, session, ALL) {
  
  ns <- session$ns
  
  # ctlModel_selector
  output$ctlModel_selector <- renderUI({  
    name_lst=list.files(path = paste0(HOME, "/ctl"), 
                        full.names = FALSE, 
                        recursive = FALSE, 
                        pattern=".ctl", 
                        include.dirs=FALSE
    )  
    
    
    name_lst <- c("", name_lst, names(ALL$ctlModel)) %>% unique()
    selectInput(ns("select_ctlModel"), label = "Select ctlModel(s):",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1])
  })
  
  
  ## use renderUI to display table
  output$ctlModel_container <- renderUI({
    
    validate(need(input$select_ctlModel, message=FALSE))
    
    script_content = readLines(paste0(HOME, "/ctl/", input$select_ctlModel))
    # ALL$ctlModel[[input$select_ctlModel]]
    
    aceEditor(ns("ctlModel_content"), 
              mode="fortran", 
              value=paste0(script_content, collapse="\n"), 
              theme = "crimson_editor",   # chrome
              autoComplete = "enabled",
              height = "1000px", 
              fontSize = 15 
    )
  })
  
}
