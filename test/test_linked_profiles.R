
######################################################################
# setup
######################################################################
library(stringr)
HOME = paste0(normalizePath("."), "/")
if (str_sub(HOME, 1, 6) == "/home/") {HOME=paste0("/home/feng.yang/YANG/")}
if (str_sub(HOME, 1, 9) == "C:\\Users\\") {HOME=paste0("C:\\Users\\feng.yang\\Documents\\handbook/")}

#source(paste0(HOME, "/global.R"))  

source(paste0(HOME, "/module/module_linked_profiles.R"))  
 
######################################################################
# testing
######################################################################
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(uiOutput("figure_container")  )
)
 
server <- function(input, output, session) {

  figure=NULL
  table =NULL
  data = NULL
        
  dataset <- read_datafile(paste0(HOME, "/data/nmdatPKPD.csv")) 
  dataset <- dataset %>% convert_vars_type(nmdat_data_type)
  test_lst <- unique(dataset$TEST) 
  test_lst <- test_lst[which(!is.na(test_lst))]  # 
  
  output$figure_container <- renderUI({
    
    callModule(module_linked_profiles, "sfstest", 
              dataset=dataset,  
              test_name = reactive(test_lst)
              )
    
    module_linked_profiles_UI(("sfstest"))
    
  })
  
  
}
  
shinyApp(ui, server)


