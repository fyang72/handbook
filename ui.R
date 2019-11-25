

#git config â€“global user.name â€œyour GitHub account nameâ€                                                     
#git config â€“global user.email â€œGitHubEmail@something.comâ€

# See simple setup  about GitHub at 
# http://www.molecularecologist.com/2013/11/using-github-with-r-and-rstudio/
# my GitHub worked!!!!!!!  as 08/18/2017


# shinydashboard
#https://rstudio.github.io/shinydashboard/structure.html


# shinyjs
# adsl_checkin %>% slice(rep(1, each=ncol(adsl))) 


# 1) Great resouces for Shiny tips
#-----------------------------------------
# http://deanattali.com/blog/advanced-shiny-tips/
# https://github.com/daattali/advanced-shiny/blob/master/multiple-pages/app.R



# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# if( !require( devtools ) ) install.packages("devtools")
# devtools::install_github('ReporteRsjars', 'davidgohel')
# devtools::install_github('ReporteRs', 'davidgohel')

###
# if( !require( devtools ) ) install.packages("devtools")
# devtools::install_github('ReporteRsjars', 'davidgohel')
# devtools::install_github('ReporteRs', 'davidgohel')
# 

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# why have status 127 when zip
#https://cran.r-project.org/web/packages/openxlsx/vignettes/Introduction.pdf
# shell("PATH")


#rm(list=ls()) 

# https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html    #  Plot interaction - selecting points


#----------------------------------------------------
# loading R library
#----------------------------------------------------

#### Load server detail objects into the global workspace. ####

#----------------------------------------------------
# setup home directory 
#----------------------------------------------------




# loading R package
#----------------------------------------------------  
#source("H:\\FYANG\\aTemplate\\Rpackage\\regnR\\latex\\load_Rpackage.r")


# loading regnR script functions
#----------------------------------------------------
#folder.loc <- "H:\\FYANG\\aTemplate\\rpackage\\regnR\\R"
#file.lst <-list.files(path = folder.loc, pattern = ".r",  all.files = FALSE,full.names = TRUE, include.dirs = FALSE)     
#for (ifile in 1:length(file.lst)) { source(file=file.lst[ifile])  }


#----------------------------------------------------
# loading regnR script functions
#----------------------------------------------------

# don't delete it!
#------------------------------------------------------------------------------------
#https://stackoverflow.com/questions/33299641/source-in-reactive-content-shiny
#------------------------------------------------------------------------------------
# library(shiny)
# shinyServer(function(input, output, session) {
#   # From http://shiny.rstudio.com/articles/scoping.html
#   output$text <- renderText({
#     source('each_call.R', local=TRUE)
#   })
#   
#   # Source in the file.R from the example in the question
#   sys.source('file.R', envir=environment())
# })
# I didn't test it, but you might be able to use:
# 
# sourceRecursive <- function(path, env) {
#     files <- list.files(path = path, pattern = "^.*[Rr]$", recursive = TRUE)
#     for (f in files) sys.source(f, env)
# }
# 
# shinyServer(function(input, output, session) {
#     session.env <- environment()
#     sourceRecursive(path = ".", env = session.env)
# })
# 
# 



################################################################################
#  UI
################################################################################
header <- dashboardHeader(title = "ShinyPMx @REGN",
                          dropdownMenuOutput("notificationMenu"), 
                          dropdownMenuOutput("messageMenu"), 
                          dropdownMenuOutput("taskMenu")
)


sidebar <- dashboardSidebar(
  # # Usage logger iframe.
  # div(
  #   style="font-size:0px;",
  #   source(
  #     file=paste(
  #       "../",
  #       relativePathToRootDirectoryForUsageLoggerViewerInjectionScripts,
  #       "usage_logger_code_injection_for_ui.R",
  #       sep=""
  #     ),
  #     local=TRUE,echo=FALSE
  #   )
  # ),
  
  
  
  
  csvFileInput(id="myinputData", label="Load data")  
  
  # sidebarMenu(
  #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  #   menuItem("Widgets", icon = icon("th"), tabName = "widgets",
  #            badgeLabel = "new", badgeColor = "green")
  # )
  
)


body <- dashboardBody(
  
  navbarPage(id="Shiny_report", title =NULL,  collapsible=TRUE,
             #tabsetPanel(id="Shiny_report", type = "tabs", 
             #navbarPage(id="Shiny_report", type = "tabs", 
             
             navbarMenu(title="Checkin", 
                        
                        tabPanel(title ="alignCols", value="alignCols", #icon = icon("dataset"), 
                                 checkInColsUI(id="mycheckInCols", label = "")   
                        ),  
                        
                        tabPanel(title ="alignRows", value="alignRows", #, #icon = icon("figure"), 
                                 checkInRowsUI(id="mycheckInRows", label = "")   
                        ), 
                        
                        tabPanel(title ="deriveData", value="deriveData", #, #, #icon = icon("figure"), 
                                 deriveDataUI(id="myderiveData", label = "")   
                        )
             ), 
             
             navbarMenu(title="Simulation", 
                        
                        tabPanel(title ="runSimulation", value="runSimulation", icon = icon("dataset"), 
                                 simulationUI(id="mysimulation", label = "")   
                        ),  
                        
                        tabPanel(title ="deriveExposure", value="deriveExposure", #icon = icon("figure"), 
                                 deriveTabUI(id="myderiveTab", label = "")       
                        )
             ), 
             
             # navbarMenu(title="Data Manipulation", 
             #            
             #            tabPanel(title ="dataManipulation", value="dataManipulation", #icon = icon("dataset"),
             #                     dataManipulationUI(id="mydataManipulation", label = "")   
             #            ),  
             #            
             #            tabPanel(title ="placeholder", value="placeholder" #icon = icon("figure"), 
             #                     #deriveTabUI(id="myderiveTab", label = "")       
             #            )
             # ), 
             
             navbarMenu(title="Order Figure",  #value="Order Figure",
                        
                        #tabsetPanel(id="figure", type = "tabs", 
                        tabPanel(title ="xyplot", value="xyplot",  #icon = icon("figure"), 
                                 xyplotUI(id="myxyplot", label = "") 
                        ),  
                        
                        tabPanel(title ="barplot", value="barplot" #, #icon = icon("figure"), 
                                 #barplotUI(id="mybarplot", label = "") 
                        ),
                        
                        tabPanel(title ="boxplot", value="boxplot" #icon = icon("figure"), 
                                 #boxplotUI(id="myboxplot", label = "") 
                        ),
                        
                        tabPanel(title ="histgram", value="histgram" #icon = icon("figure"), 
                                 #xyplotUI(id="myhistgramPlot", label = "") 
                        )#, 
                        
                        # tabPanel(title ="diagnostic", value="diagnostic" #icon = icon("figure"), 
                        #          #xyplotUI(id="myhistgramPlot", label = "") 
                        # ) 
                        
                        #)   
             ), 
             
             navbarMenu(title="Order Table",  #value="Order Table",
                        
                        #tabsetPanel(id="table", type = "tabs", 
                        #tabPanel(title ="summary table", value="summaryTab", #icon = icon("table"), 
                        #         summaryTabUI(id="mysummaryTab", label = "") 
                        #), 
                        
                        tabPanel(title ="summary table", value="summaryTab", #icon = icon("dataset"),
                                 dataManipulationUI(id="mydataManipulation", label = "")   
                        )  
                        
             ),               
             
             navbarMenu(title="Review",  #"", 
                        #tabsetPanel(type = "tabs",   
                        tabPanel(title="Dataset",  icon = icon("database", lib = "font-awesome"),
                                 datasetReviewUI(id="mydatasetReview", label="")
                        ), 
                        
                        tabPanel(title="Figure",  icon = icon("bar-chart-o", lib = "font-awesome"),
                                 figReviewUI(id="myfigReview", label="")
                        ), 
                        
                        tabPanel(title="Table", icon = icon("table", lib = "font-awesome"),
                                 tabReviewUI(id="mytabReview", label="")
                        ), 
                        
                        tabPanel(title="Model", icon = icon("motorcycle", lib = "font-awesome"),
                                 modelReviewUI(id="mymodelReview", label="")
                        )
                        # )
             ), 
             
             tabPanel(title="Checkout", 
                      checkOutUI(id="mycheckOut", label="") 
             ), 
             
             navbarMenu(title="Global option",  #"", 
                        #tabsetPanel(type = "tabs",   
                        tabPanel(title="ColorSetting"#,  #icon = icon("database", lib = "font-awesome"),
                                 #scriptTabUI(id="myscriptTab", label="")  
                        ), 
                        tabPanel(title="themeSetting"  #icon = icon("database", lib = "font-awesome"),
                                 #scriptTabUI(id="myscriptTab", label="")  
                        ) 
             ),
             
             navbarMenu(title="Library",  #"", 
                        #tabsetPanel(type = "tabs",   
                        tabPanel(title="Cheatsheet",  #icon = icon("database", lib = "font-awesome"),
                                 scriptLibUI(id="myscriptLib", label="")  
                        ), 
                        tabPanel(title="UserManual"  #icon = icon("database", lib = "font-awesome"),
                                 #scriptTabUI(id="myscriptTab", label="")  
                        ) 
             )
             
             
  ), 
  tags$style(type = 'text/css', '.navbar { background-color: #90EE90;
             font-family: Arial;
             font-size: 13px;
             color: #FF0000; }',
             
             '.navbar-dropdown { background-color: #90EE90;
                           font-family: Arial;
                           font-size: 13px;
                           color: #FF0000; }',
             
             '.navbar-default .navbar-brand {
                             color: #fff;
                           }'
  )
)


# shinyUI(fluidPage(
# Put them together into a dashboardPage
shinyUI( dashboardPage(
  header, 
  sidebar,
  body,
  skin = 'blue'
)
)

