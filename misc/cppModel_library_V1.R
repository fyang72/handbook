





ui <- fixedPage(
  h2("Module example"),
  #  
  # selectInput(("select_left"), label = "Select left:",                                                                                                        
  #             width="100%",    
  #             multiple = TRUE, 
  #             choices = colnames(mpg),                                                                                                                                   
  #             selected = c("cty", "hwy")
  # ),
  # 
  # selectInput(("select_right"), label = "Select right:",                                                                                                        
  #             width="100%",    
  #             multiple = TRUE, 
  #             choices = colnames(mpg),                                                                                                                                   
  #             selected = c("drv", "hwy")
  # ),
  
  uiOutput("cppModel_selector"),
  
  module_update_cppModel_UI("scatters") 
)

server <- function(input, output, session) {
  data(mpg)
   
  cppModel1=mread(model='cppModel',
                 project=paste0(HOME, '/cpp/'),
                 quiet=TRUE,
                 file=basename("LN001.cpp"))
 
  cppModel2=mread(model='cppModel',
                 project=paste0(HOME, '/cpp/'),
                 quiet=TRUE,
                 file=basename("LN001.cpp"))
 
 ALL <- reactiveValues(
    DATA   = list(), 
    FIGURE = list(), 
    TABLE = list(), 
    cppModel = list("LN0011.cpp"=cppModel1, 
                    "LN0022.cpp"=cppModel2), 
    ctlModel = list(), 
    script = list()
  )
   

    
    #ALL
  #})
  
  ################################################
  output$cppModel_selector <- renderUI({   

    name_lst <- c("", names(ALL$cppModel)) %>% unique()
    selectInput( ("select_cppModel"), label = "Select cppModel:",                                                                                                        
                width="100%",    
                multiple = FALSE, 
                choices = name_lst,                                                                                                                                   
                selected = name_lst[1]
    )
  })
  
  
    ALL <- callModule(module_update_cppModel, "scatters",  
                   ALL, 
                   cppModel_name = reactive(input$select_cppModel)
  )
  
 
}

shinyApp(ui, server)