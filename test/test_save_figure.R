
######################################################################
# setup
######################################################################
library(stringr)
HOME = paste0(normalizePath("."), "/")
if (str_sub(HOME, 1, 6) == "/home/") {HOME=paste0("/home/feng.yang/YANG/")}
if (str_sub(HOME, 1, 9) == "C:\\Users\\") {HOME=paste0("C:\\Users\\feng.yang\\Documents\\handbook/")}

#source(paste0(HOME, "/global.R"))  
 
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
        
  library(xpose)
  xpdb <- xpdb_ex_pk %>% 
    update_themes(gg_theme = theme_regn(), #+ base_theme(font.size = 12),
                  xp_theme = list(point_color = 'blue',
                                  line_color  = 'black'))  
   
  #Basic goodness-of-fit plots
  #----------------------------------
  FIGURE = NULL
  
  figure <- xpdb %>% my_dv_vs_pred()
  attr(figure, "title") <- "placeholder index (integer). This is to be used when a placeholder type is not unique in   "
  FIGURE[["dv_vs_pred"]] <- figure
  
  figure <- xpdb %>% dv_vs_ipred(quiet=TRUE)
  attr(figure, "title") <- "sgafsfdsplasceholder index (integer). This is to be used when a placeholder type is not unique in   "
  FIGURE[["dv_vs_ipred"]] <- figure
   
  
  # more plot (plots of the independent variable (IDV)
  FIGURE[["dv_vs_idv"]] <-xpdb %>% dv_vs_idv(quiet=TRUE)
  FIGURE[["ipred_vs_idv"]] <-xpdb %>% ipred_vs_idv(quiet=TRUE)
  FIGURE[["pred_vs_idv"]] <-xpdb %>% pred_vs_idv(quiet=TRUE)
  FIGURE[["dv_preds_vs_idv"]] <-xpdb %>% dv_preds_vs_idv(quiet=TRUE)
   
   
  output$figure_container <- renderUI({
    
    callModule(module_save_multiple_figures, "sfstest", 
               ALL= NULL, figures = FIGURE 
               )
    
    module_save_multiple_figures_UI(("sfstest"))
    
  })
  
  
}
  
shinyApp(ui, server)


