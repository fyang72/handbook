

################################################################################ 
# module_fetch_job_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_fetch_job_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(12, 
           uiOutput(ns("server_info_container")), 
           uiOutput(ns("local_info_container")), 
           uiOutput(ns("fetch_control_container")),
           uiOutput(ns("output_container"))#, 
           #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    )
  ) 
  
}




################################################################################ 
# main function: module_load_data
################################################################################

module_fetch_job <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {
  
  ns <- session$ns 
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  values<- reactiveValues()
  
  
  #----------------------------------------------------
  # server_info_container
  #----------------------------------------------------
  
  output$server_info_container <- renderUI({
    
    ctlModel = ALL$ctlModel[[ctlModel_name]]
    nmdat = ALL$DATA[[ctlModel_name]]
    
    if (!is.null(ctlModel) & !is.null(nmdat)) {
      ctlModel.file.name = attributes(ctlModel)$file.name
      ctlModel.locaton.source = attributes(ctlModel)$locaton.source
      
      nmdat.file.name = attributes(nmdat)$file.name
      nmdat.locaton.source = attributes(nmdat)$locaton.source
      
      runno <- gsub(paste0(".", tools::file_ext(ctlModel.file.name)), "", basename(ctlModel.file.name))
      data.name <- gsub(paste0(".", tools::file_ext(nmdat.file.name)),"",  basename(nmdat.file.name))
    }else {
      runno = NULL
      data.name = NULL
    }
    
    
    fluidRow(
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server.model.dir"), 
                       width = '100%',  
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    "TEST/ctl/",
                                    paste(runno, data.name, sep="_" ), "/"), 
                       label="Server directory of the loaded model:")),
      
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server.data.dir"), 
                       width = '100%',
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    "TEST/data/"),
                       label="Server directory of the loaded data:"))
    )
    
    
  })
  
  
  
  
  output$local_info_container <- renderUI({
    
    ctlModel = ALL$ctlModel[[ctlModel_name]]
    nmdat = ALL$DATA[[ctlModel_name]]
    
    if (!is.null(ctlModel) & !is.null(nmdat)) {
      ctlModel.file.name = attributes(ctlModel)$file.name
      ctlModel.locaton.source = attributes(ctlModel)$locaton.source
      
      nmdat.file.name = attributes(nmdat)$file.name
      nmdat.locaton.source = attributes(nmdat)$locaton.source
      
      runno <- gsub(paste0(".", tools::file_ext(ctlModel.file.name)), "", basename(ctlModel.file.name))
      data.name <- gsub(paste0(".", tools::file_ext(nmdat.file.name)),"",  basename(nmdat.file.name))
    }else {
      runno = NULL
      data.name = NULL
    }
    
    fluidRow(
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("local.result.dir"), 
                       width = '100%',  
                       value=paste0("/output/", 
                                    tolower(Sys.info()["user"]), "/",
                                    "TEST/",
                                    paste(runno, data.name, sep="_" ), "/"), 
                       label="Local directory of the fetched result:"))
      
      # column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
      #               textInput("server.data.dir", 
      #                         width = '100%',
      #                         value=paste0("/home/", 
      #                                       tolower(Sys.info()["user"]), "/",
      #                                      "TEST/data/"),
      #                         label="Where to put data on server:"))
    )
    
    
  })
  
  
  output$fetch_control_container <- renderUI({
    
    fluidRow(
      column(width=3, 
             actionButton(ns("fetchResult"), label="fetch result", 
                          style="float:left;color: #fff; background-color: #328332; border-color: #328332" 
             )
      )
    )
  })
  
  
  # output_container
  output$output_container <- renderUI({   #renderPrint  renderText  renderUI
    
    lst.content =  values$run.model$lst.content  # pure text
    lst = values$run.model$lst   # after read.lst data object
    validate(need(lst.content, message=FALSE), 
             need(lst, message=FALSE)
    )
    
    value = lst.content   # see(ctlModel,raw=TRUE)
    value = paste0(value, sep="\n")   # sep="<br/>")
    value = paste0(value, collapse="")
    
    tagList(
   
    fluidRow(
      column(12,
             textAreaInput(ns("lst_content"), label=NULL, value=value, rows=50,
                           width = '780px', #   '800px',   #400px', or '100%'
                           placeholder= "Your output here.") 
      )
    ) 
    )
    
    #https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
    # output = c(
    #   paste0("ofv=", u.signif(lst$ofv, digits=8)), 
    #   paste0("thetas[", 1:length(lst$thetas), "]=",  lst$thetas), 
    #   lst$term) 
    #  
    #  HTML(paste(output, sep='<br/>'))
    # 
    #lst.content
    
  })
  
  
  
  observeEvent({input$fetchResult}, {
    
    ctlModel = ALL$ctlModel[[ctlModel_name]]
    ctlModel.file.name = attributes(ctlModel)$file.name
    ctlModel.locaton.source = attributes(ctlModel)$locaton.source
     
    nmdat = ALL$DATA[[ctlModel_name]]
    nmdat.file.name = attributes(nmdat)$file.name
    nmdat.locaton.source = attributes(nmdat)$locaton.source
    
    validate(
      need(nmdat, message=FALSE), 
      need(nmdat.locaton.source %in% c("internal", "external", "session"), message=FALSE),
      
      need(ctlModel, message=FALSE), 
      need(ctlModel.locaton.source %in% c("internal", "external", "session"), message=FALSE) 
    ) 
    
    server.model.dir = input$server.model.dir   #"/home/feng.yang/test_data/test_nm/"  
    server.data.dir= input$server.data.dir   #"/home/feng.yang/test_data/test_nm/"
    
    
    HOME = "~/handbook/" 
    local.model.name = ctlModel.file.name # ifelse(ctlModel.locaton.source=="internal", ctlModel.file.name, NULL)   
    
    local.data.name = nmdat.file.name # ifelse(nmdat.locaton.source=="internal", nmdat.file.name, NULL)   #"/data/THEOPP.csv"  
    
    
    # create a local directory to hold fetched data
    # runno <- gsub(paste0(".", tools::file_ext(ctlModel.file.name)), "", basename(ctlModel.file.name))
    # data.name <- gsub(paste0(".", tools::file_ext(nmdat.file.name)),"",  basename(nmdat.file.name))
    # local.result.dir = paste0(HOME, "model/output/", 
    #                    tolower(Sys.info()["user"]), "/",
    #                    paste(runno, data.name, sep="_" ), "/")
    
    local.result.dir = paste0(HOME, input$local.result.dir)
    system(command = paste0("mkdir -p ", local.result.dir))
    
    # 
    tt= fetch_job_from_HPC(HOME = "~/FYANG/Template/Handbook/",
                              local.model.name = local.model.name, 
                              local.data.name = local.data.name, 
                              local.result.dir=local.result.dir, 
                              server.model.dir = server.model.dir,
                              server.data.dir = server.data.dir)
    
    
    values$run.model$lst = tt$lst
    values$run.model$lst.content = tt$lst.content 
    if (!is.null(tt$lst)) { print("lst output has been returned. ")}
    
  })
  
   
}