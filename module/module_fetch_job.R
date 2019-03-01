

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
      
      runno <- tools::file_path_sans_ext(basename(ctlModel.file.name))
      data.name <- tools::file_path_sans_ext(basename(nmdat.file.name))
      
      #runno <- gsub(paste0(".", tools::file_ext(ctlModel.file.name)), "", basename(ctlModel.file.name))
      #data.name <- gsub(paste0(".", tools::file_ext(nmdat.file.name)),"",  basename(nmdat.file.name))
    }else {
      runno = NULL
      data.name = NULL
    }
    
    tagList( 
      fluidRow(
        column(width = 6, 
               textInput(ns("server.IP.address"), 
                         width = '100%',  
                         value= "10.244.106.127", 
                         placeholder = "xx.xx.xx.xx.xx", 
                         label="Server IP address:"
               )
        )
      ),
    
    fluidRow(
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server.model.dir"), 
                       width = '100%',  
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    "TEST/ctl/",
                                    paste(runno, data.name, sep="_" ), "/"), 
                       label="Directory of the loaded model on server:")),
      
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server.data.dir"), 
                       width = '100%',
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    "TEST/data/"),
                       label="Directory of the loaded data on server:"))
    )
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
                       label="Directory of the fetched result in local:"))
      
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
    
    server.IP.address = input$server.IP.address
    server.model.dir = input$server.model.dir   
    server.data.dir= input$server.data.dir    
     
    
    local.model.name = ctlModel.file.name # ifelse(ctlModel.locaton.source=="internal", ctlModel.file.name, NULL)   
    model.name = tools::file_path_sans_ext(local.model.name)
    
    local.data.name = nmdat.file.name # ifelse(nmdat.locaton.source=="internal", nmdat.file.name, NULL)   #"/data/THEOPP.csv"  
    data.name = tools::file_path_sans_ext(local.data.name)
    
    # create a local directory to hold fetched data
    # runno <- gsub(paste0(".", tools::file_ext(ctlModel.file.name)), "", basename(ctlModel.file.name))
    # data.name <- gsub(paste0(".", tools::file_ext(nmdat.file.name)),"",  basename(nmdat.file.name))
    # local.result.dir = paste0(HOME, "model/output/", 
    #                    tolower(Sys.info()["user"]), "/",
    #                    paste(runno, data.name, sep="_" ), "/")
    
    local.result.dir = input$local.result.dir
    system(command = paste0("mkdir -p ", local.result.dir), intern = T) 
    
    fetch_job_from_HPC(  
      server.IP.address = server.IP.address,
      server.model.dir = server.model.dir, 
      local.result.dir = local.result.dir,   #"./ctl/LN_BASE_WT/", 
      
      server.data.full.path = paste0(server.data.dir, data.name, ".csv")
    ) 
    

    lst.file = paste0(local.result.dir, model.name, ".lst")
 
    
    values$run.model$lst = read.lst(lst.file)
    values$run.model$lst.content = readLines(lst.file)
    if (!is.null(tt$lst)) {
      showNotification("fetch result sucessfully", type="message")}   # "default, "message", "warning", "error"
    
    
  })
  
   
}