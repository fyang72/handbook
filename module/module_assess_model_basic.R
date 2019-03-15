
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_assess_model_basic_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to 1) render the source dataset (dataset tab), 
                         2) perform the work based on the source dataset (script tab), 
                         3) render the secondary data (data tab), final table (table tab) and figure (figure tab).", color="gray")))
      ),
    
    tabBox(width=12, id = ns("basic_assess_model"), title =NULL, 
           
           # dataset_container 
           # tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
           #          collapsed = TRUE, solidHeader = TRUE,
           #          tagList(
           #            fluidRow(
           #              column(width=4, uiOutput(ns("select_which_program_container"))), 
           #              
           #              column(width=4, uiOutput(ns("select_which_job_container"))), 
           #              
           #              column(width=4, uiOutput(ns("select_which_runno_container")))
           #              
           #            ),
           #            fluidRow(column(12, uiOutput(ns("dataset_container")))) 
           #          )
           # ),       
           
           # script_container 
           tabPanel(width=12, title="GOF1", value = "GOF1", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    
                    tagList(
                      fluidRow(
                        fluidRow(
                          
                          column(6,
                                 uiOutput(ns("diagnostic.PRED.DVOR"))  
                                 #style='margin-bottom:30px;  border:1px solid; padding: 10px;'   
                          ), 
                          column(6,
                                 uiOutput(ns("diagnostic.IPRED.DVOR")) 
                                 #style='margin-bottom:30px;  border:1px solid; padding: 10px;'  
                          ) 
                        ), 
                        
                        fluidRow(
                          
                          column(6,
                                 uiOutput(ns("diagnostic.PRED.DVOR.LOG"))  
                                 #style='margin-bottom:30px;  border:1px solid; padding: 10px;'   
                          ), 
                          column(6,
                                 uiOutput(ns("diagnostic.IPRED.DVOR.LOG")) 
                                 #style='margin-bottom:30px;  border:1px solid; padding: 10px;'  
                          ) 
                        )
                        
                      )
                    )
           ),     
           
           # data_container
           tabPanel(width=12, title="GOF2", value = "GOF2", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    
                    fluidRow(
                      fluidRow(
                        
                        column(6,
                               uiOutput(ns("diagnostic.CWRES.TIME"))  
                               #style='margin-bottom:30px;  border:1px solid; padding: 10px;'   
                        ), 
                        column(6,
                               uiOutput(ns("diagnostic.CWRES.IPRED")) 
                               #style='margin-bottom:30px;  border:1px solid; padding: 10px;'  
                        ) 
                      ), 
                      
                      fluidRow(
                        
                        column(6,
                               uiOutput(ns("diagnostic.CWRES.ID"))  
                               #style='margin-bottom:30px;  border:1px solid; padding: 10px;'   
                        ), 
                        column(6,
                               uiOutput(ns("diagnostic.CWRES.QUANTILE")) 
                               #style='margin-bottom:30px;  border:1px solid; padding: 10px;'  
                        ) 
                      )
                      
                    )
           ),
           
           # table_container
           tabPanel(width=12, title="GOF3", value = "GOF3", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12,uiOutput(ns("diagnostic.INDIV_PLOT25"))))
           ),
           
           
           # table_container
           tabPanel(width=12, title="ETAvsCOV", value = "ETAvsCOV", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("diagnostic.ETAvsCOV"))))
           ),
           
           #figure_container
           tabPanel(width=12, title="ETAvsETA", value = "ETAvsETA", collapsible = TRUE, 
                    collapsed = TRUE, solidHeader = TRUE,
                    fluidRow(column(12, uiOutput(ns("diagnostic.ETAvsETA"))))   
           )
    ) 
             )  # taglist
}



################################################################################ 
################################################################################
# module_assess_model_basic
################################################################################
################################################################################
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn't part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

module_assess_model_basic <- function(input, output, session, 
                                      ALL, xpdb, values4xpdb
)  {
  
  ns <- session$ns
  values <- reactiveValues(data=NULL, figure=NULL,table = NULL)
  #values4xpdb = reactiveValues(diagnostic=NULL) 
  
  user.name = tolower(Sys.info()["user"])
  
  output$select_which_program_container <- renderUI({
    #validate(need(globalVars$login$status, message=FALSE))
    
    dirs.list = list.files(path = paste0(HOME, "/output/", user.name, "/"), 
                           full.names = FALSE, recursive = FALSE)
    dirs.list = c("", dirs.list)
    selectizeInput(ns("which_program"), 
                   label    = "Select program:", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   selected = dirs.list[1]) 
    
  })
  
  output$select_which_job_container <- renderUI({
    #validate(need(globalVars$login$status, message=FALSE))
    
    dirs.list = list.files(path = paste0(HOME, "/output/", 
                                         user.name, "/", 
                                         input$which_program, "/ctl/"), 
                           full.names = FALSE, recursive = FALSE)
    dirs.list = c("", dirs.list)
    selectizeInput(ns("which_job"), 
                   label    = "Select job:", 
                   choices  = dirs.list, 
                   multiple = FALSE,
                   selected = dirs.list[1]) 
    
  })
  
  output$select_which_runno_container <- renderUI({
    #validate(need(globalVars$login$status, message=FALSE))
    
    # dirs.list = list.files(path = paste0("./output/", user.name, "/", input$which_program, "/"), 
    #                        full.names = FALSE, recursive = FALSE)
    # dirs.list = c("", dirs.list)
    # selectizeInput(("which_runno"), 
    #                label    = "Select runno:", 
    #                choices  = dirs.list, 
    #                multiple = FALSE,
    #                selected = dirs.list[1]) 
    textInput(ns("which_runno"), value="001", label="Which runno:")
    
  })
  
  ################################
  # UI for dataset_container
  ################################
  output$dataset_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))
    
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    callModule(module_table_output, "loaded_dataset", mytab=slot(xpdb, "Data"))
    
    fluidRow(column(12, 
                    module_table_output_UI(ns("loaded_dataset"), label = "loaded_dataset")
    ) 
    )
  }) 
  
  
  ################################
  # UI for script_container
  ################################
  
  output$diagnostic.PRED.DVOR <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$PRED_DVOR   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.PRED.DVOR", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.PRED.DVOR"), 
                           label = "xpdb.diagnostic.PRED.DVOR")
    
  })
  
  
  output$diagnostic.IPRED.DVOR <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$IPRED_DVOR   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.IPRED.DVOR", 
               fig=fig$fig, 
               mydata = fig$data)
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.IPRED.DVOR"), 
                           label = "xpdb.diagnostic.IPRED.DVOR")
    
  })
  
  
  
  
  output$diagnostic.PRED.DVOR.LOG <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$PRED_DVOR_LOG   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.PRED.DVOR.LOG", 
               fig=fig$fig, 
               mydata = fig$data)
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.PRED.DVOR.LOG"), 
                           label = "xpdb.diagnostic.PRED.DVOR.LOG")
    
  })
  
  
  output$diagnostic.IPRED.DVOR.LOG <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$IPRED_DVOR_LOG   
    validate(need(fig$fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.IPRED.DVOR.LOG", 
               fig=fig$fig, 
               mydata = fig$data)
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.IPRED.DVOR.LOG"), 
                           label = "xpdb.diagnostic.IPRED.DVOR.LOG")
    
  })
  
  
  
  
  
  ################################
  # figure_container
  ################################
  
  output$diagnostic.CWRES.TIME <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$CWRES.TIME   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.CWRES.TIME", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.CWRES.TIME"), 
                           label = "xpdb.diagnostic.CWRES.TIME")
    
  })
  
  
  output$diagnostic.CWRES.IPRED <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$CWRES.IPRED   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.CWRES.IPRED", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.CWRES.IPRED"), 
                           label = "xpdb.diagnostic.CWRES.IPRED")
    
  })
  
  
  
  
  output$diagnostic.CWRES.ID <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$CWRES.ID   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.CWRES.ID", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.CWRES.ID"), 
                           label = "xpdb.diagnostic.CWRES.ID")
    
  })
  
  
  output$diagnostic.CWRES.QUANTILE <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$CWRES.QUANTILE   
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.CWRES.QUANTILE", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.CWRES.QUANTILE"), 
                           label = "xpdb.diagnostic.CWRES.QUANTILE")
    
  })
  
  
  
  
  ################################
  # data_container
  ################################
  
  output$diagnostic.INDIV_PLOT25 <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$INDIV_PLOT25  
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.INDIV_PLOT25", 
               fig=fig$fig, 
               mydata = fig$data
    )
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.INDIV_PLOT25"), 
                           label = "xpdb.diagnostic.INDIV_PLOT25")
    
  })
  
  
  
  
  ################################
  # tabl_container
  ################################
  
  output$diagnostic.ETAvsCOV <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$ETAvsCOV[[1]]  
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.ETAvsCOV", 
               fig=fig, 
               mydata = slot(xpdb, "Data")  %>% mutate(xvar="cov_value", yvar="ETA1"),  
               xvar="xvar", yvar="yvar") # cov_value, y = ETA1
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.ETAvsCOV"), 
                           label = "xpdb.diagnostic.ETAvsCOV")
    
  })
  
  
  output$diagnostic.ETAvsETA <- renderUI({ 
    #xpdb = xpdb_inputData()
    validate(need(xpdb, message=FALSE))
    
    fig = values4xpdb$diagnostic$ETAvsETA[[1]]  
    validate(need(fig, message=FALSE))
    
    
    callModule(module_ggplot_brush, "xpdb.diagnostic.ETAvsETA", 
               fig=fig, 
               mydata = slot(xpdb, "Data")  %>% mutate(xvar="ETA_1", yvar="ETA_2"),   
               xvar="xvar", yvar="yvar") # cov_value, y = ETA1
    
    module_ggplot_brush_UI(ns("xpdb.diagnostic.ETAvsETA"), 
                           label = "xpdb.diagnostic.ETAvsETA")
    
  })
  
  ################################
  # run_script  
  ################################  
  xpdb_inputData <- reactive({
    
    validate(need(input$which_program, message=FALSE), 
             need(input$which_job, message=FALSE),
             need(input$which_runno, message=FALSE)
    )  
    #input$which_program = "TEST"
    # input$which_run = "control5_THEOPP"
    #user.name = tolower(Sys.info()["user"])  # determineCurrentUser()
    
    file.dir = paste0(HOME, "/output/", 
                      user.name, "/", 
                      input$which_program, "/ctl/", 
                      input$which_job, "/")
    
    #slotNames(xpdb)
    # "Xvardef"       "Labels"   "Graph.prefs"   "Miss"   "Cat.levels"    "DV.Cat.levels" "Subset"        "Gam.prefs"     "Bootgam.prefs"
    #slotNames(slot(xpdb,"Prefs"))
    
    #slot(slot(xpdb,"Prefs"), "Xvardef")
    #names(slot(slot(xpdb,"Prefs"), "Xvardef"))
    
    xpdb = xpose.data(input$which_runno, directory = file.dir)
    slot(xpdb, "Data") = slot(xpdb, "Data") 
    #xpdb =  slot(xpose4.obj,"Data")
    # runno = slot(xpose4.obj,"Runno")
    
    
    
    xpdb
  })
  
  
  
  ################################
  # run_script  
  ################################ 
  
  observeEvent({xpdb_inputData()}, {
    
    #xpdb = xpdb_inputData() 
    validate(need(xpdb, message=FALSE) 
    )
    
    # all diagnostic model 
    #
    values4xpdb = xpdb_diagnostic_GOF1(xpdb, values4xpdb)
    
    #
    values4xpdb = xpdb_diagnostic_GOF2(xpdb, values4xpdb) 
    
    #
    values4xpdb = xpdb_diagnostic_GOF3(xpdb, values4xpdb, 
                                       n=25, ids=NA)
    
    #
    values4xpdb = xpdb_diagnostic_ETAvsETA(xpdb, values4xpdb, 
                                           eta_name_lst=paste("ETA", 1:3, sep=""))
    
    #$ranpar  "Xvardef"   Prefs
    eta_name_lst = slot(slot(xpdb,"Prefs"), "Xvardef")$ranpar 
    values4xpdb = xpdb_diagnostic_ETAvsCOV(xpdb, values4xpdb, 
                                           cov_name_lst=c("WGTBL" ), 
                                           eta_name_lst=c("ETA1", "ETA2", "ETA3" ))
    
  })
  
  
  #--------------------------------------  
  # observeEvent  
  #-------------------------------------- 
  # https://groups.google.com/forum/#!topic/shiny-discuss/vd_nB-BH8sw
  
  observeEvent({input$save_all_table}, {
    validate(need(length(values$table), message="no table found") )
    isolate({ 
      #lapply(1:length(values$table), function(i) ALL$TABLE[[names(values$table)[i]]] = values$table[[i]])
      
      ALL$TABLE = (c(ALL$TABLE, values$table))
      print("all table have been saved")
      print(names(ALL$TABLE))
    })
    
  })
  
  
  observeEvent({input$save_all_data}, {
    validate(need(length(values$data), message="no data found") )
    #ALL$DATA[[data_name]]  = load_external_data()
    isolate({ 
      #lapply(1:length(values$data), function(i) ALL$DATA[[names(values$data)[i]]] = values$data[[i]])
      ALL$DATA = (c(ALL$DATA, values$data))
      
      print("all data have been saved")
      print(names(ALL$DATA))
    })
  })
  
  
  observeEvent({input$save_all_figure}, {
    validate(need(length(values$figure), message="no figure found") )
    isolate({ 
      #lapply(1:length(values$figure), function(i) ALL$FIGURE[[names(values$figure)[i]]] = values$figure[[i]])
      ALL$FIGURE = (c(ALL$FIGURE, values$figure))
      
      print("all figure have been saved")
      print(names(ALL$FIGURE))
    })
    
  })
  
  
  
  # }) # isolate 
  return(ALL)
}



