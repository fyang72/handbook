

######################################################################
# setup
######################################################################
library(stringr)
HOME = paste0(normalizePath("."), "/")
if (str_sub(HOME, 1, 22) == "/home/feng.yang/FYANG/") {HOME=paste0("/home/feng.yang/YANG/")}
if (str_sub(HOME, 1, 19) == "C:\\Users\\feng.yang\\") {HOME=paste0("C:\\Users\\feng.yang\\Documents\\handbook/")}

 
library(stringr)
library(officer) 
library(shiny)   
library(DT) 
library(RColorBrewer)
library(xpose) #If you rely on xpose for your work, please keep an eye on our github:
  #https://github.com/UUPharmacometrics/xpose

actionButton_style ="float:left;color: #fff; background-color: #328332; border-color: #328332"

login = NULL
login$status = TRUE
login$user.name.lst = c("training",   "feng.yang"   )
login$user.name = "feng.yang"   # determineCurrentUser(session=session)
login$status = login$user.name %in% login$user.name.lst  
globalVars <- reactiveValues(login=login)


list_files_in_a_folder <- function(folder.loc="./util/", file.extension=c(".r", ".R")) {
  file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
  file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-(nchar(file.extension)-1), nchar(file.lst)) %in% file.extension)]
  file.lst = file.lst[which(!substr(basename(file.lst), 1, 1) %in% "_")]
  file.lst = file.lst[setdiff(1:length(file.lst), grep("_not_used", file.lst, fixed=TRUE))]
  return(file.lst)
}

ihandbook = 0

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/util/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) { print(file.lst[ifile]); source(file=file.lst[ifile]) }     

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/module/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) {source(file=file.lst[ifile])  }     

file.lst <- list_files_in_a_folder(folder.loc=paste0(HOME, "/script/"), file.extension=c(".r", ".R"))
for (ifile in 1:length(file.lst)) {source(file=file.lst[ifile])  }     

 
######################################################################
# Load local R functions
######################################################################

ui <- basicPage( #(id, label = "") {
  
  tagList(
    uiOutput("figure_container")  
  )
  
)


server <- function(input, output, session) {
  
  source(paste0(HOME, "/module/module_save_figure.R"))
  source(paste0(HOME, "/module/module_ggplot_brush.R"))
  
  figure=NULL
  table =NULL
  data = NULL
        
  xpdb <- xpdb_ex_pk %>% 
    update_themes(gg_theme = theme_bw(), #+ base_theme(font.size = 12),
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


