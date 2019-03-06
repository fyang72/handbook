################################################################################ 
# module_submit_job_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_submit_job_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(12, 
           # which_program_container
           fluidRow(
             column(width = 12,
                    uiOutput(ns("which_program_container"))    
             ) 
           ), 
           
           fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
           # server_info_container
           fluidRow(
             column(width = 12, 
                    uiOutput(ns("server_info_container"))
             )
           ),  
           
           
           # actionButton of submit_job
           fluidRow(  
             column(width = 12, 
                    actionButton(ns("submit_job"), 
                                 label="submit job", 
                                 style=actionButton_style
                    )
             )
           ),
           
           fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
           
           # after submission, check the run status
           fluidRow(
             column(width = 12,
                    actionButton(ns("check_status"), 
                                 label="check status", 
                                 style=actionButton_style
                    )
             )
           ),  
           
           # check_status_container
           fluidRow(
            column(width = 12,
              uiOutput(ns("check_status_container")) 
            )
           )
    ) 
  ) 
}

################################################################################ 
# main function: module_load_data
################################################################################

module_submit_job <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {

ns <- session$ns 
values<- reactiveValues()

#--------------------------------------  
# which_program_container
#-------------------------------------- 
output$which_program_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE)) 
  
  # from server?
  # list.of.runs <- list_folder_on_HPC(server.IP.address = "10.244.106.127", 
  #                                    directory.on.server = "/home/feng.yang/R1979/ctl/")
  
  # from local?
  # dirs.list = c(list.files(path = paste0("./output/", 
  #                                            tolower(Sys.info()["user"]), "/"), 
  #                              full.names = FALSE, recursive = FALSE))
  # if (length(dirs.list)==0) {dirs.list = "TEST"}
  
  # just for test
  dirs.list = paste0("TEST")
  
  fluidRow(
    column(3,
           selectizeInput(ns("which_program"), 
                          label    =  "Select program:", 
                          choices  = dirs.list, 
                          multiple = FALSE,
                          width="100%", 
                          selected = dirs.list[1]) 
    )
  )
})


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
    
  }else {
    runno = NULL
    data.name = NULL
  }
  
  tagList( 
    fluidRow(
      column(width = 6, 
             textInput(ns("server.IP.address"), 
                       width = '100%',  
                       value= server.IP.address,  # NULL
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
                                    input$which_program,
                                    "/ctl/",
                                    paste(runno, data.name, sep="_" ), "/"), 
                       label="Directory of the loaded model on server :")),
      
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server.data.dir"), 
                       width = '100%',
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    input$which_program,
                                    "/data/"),
                       label="Directory of the loaded data on server:")
      ) 
    )
  )
  
})



# log_container
output$check_status_container <-  renderUI({  
  
  validate(need(input$server.model.dir, message="empty server.model.dir"), 
           need(input$server.data.dir, message="empty server.data.dir")  
  )
  
  fluidRow(
    column(12,
           textAreaInput(ns("check_status_content"), label=NULL, value=NULL, rows=10,
                         width = '785px',   #400px', or '100%'
                         placeholder= "check your run-status here.")  
    )
  )  
})



###################################################
# submit_job
###################################################

observeEvent({input$submit_job}, {
  
  ctlModel = ALL$ctlModel[[ctlModel_name]]
  nmdat = ALL$DATA[[ctlModel_name]]
  
  attr(ctlModel, "which_program") = input$which_program  
  attr(nmdat, "which_program") = input$which_program  
  ALL$ctlModel[[ctlModel_name]] = ctlModel
  ALL$DATA[[ctlModel_name]] = nmdat
  
  ctlModel.file.name = attributes((ctlModel))$file.name
  ctlModel.locaton.source = attributes((ctlModel))$locaton.source
  
  nmdat.file.name = attributes((nmdat))$file.name
  nmdat.locaton.source = attributes((nmdat))$locaton.source
  
  validate(
    # need(values$run.model$job.submited == FALSE, message=FALSE), #########################
    need(input$server.model.dir, message =FALSE), 
    need(input$server.data.dir, message =FALSE),
    
    need(nmdat, message=FALSE), 
    need(ctlModel, message=FALSE), 
    
    need(ctlModel.locaton.source %in% c("internal", "external", "session"), message=FALSE), 
    need(nmdat.locaton.source %in% c("internal", "external", "session"), message=FALSE)
  ) 
  
  # create a folder locally if not exit
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  owd <- tempdir()
  on.exit(setwd(owd))
  
  if (nmdat.locaton.source %in% c("external", "session"))  {
    write.csv(nmdat, file= paste0(owd,  basename(nmdat.file.name)), 
              row.names = FALSE,
              na = ".", quote=FALSE) 
  }
  
  if (ctlModel.locaton.source %in% c("external", "session"))  {
    writeLines(ctlModel, con = paste0(owd,  basename(ctlModel.file.name)))
  }
  
  server.IP.address = input$server.IP.address 
  local.model.name = ifelse(ctlModel.locaton.source=="internal", ctlModel.file.name, 
                            ifelse(ctlModel.locaton.source%in% c("external", "session"), 
                                   paste0(owd, basename(ctlModel.file.name)), NULL))    
  local.data.name = ifelse(nmdat.locaton.source=="internal", nmdat.file.name, 
                           ifelse(nmdat.locaton.source%in% c("external", "session"), 
                                  paste0(owd, basename(nmdat.file.name)), NULL))  
  
  server.model.dir = input$server.model.dir  
  server.data.dir= input$server.data.dir  
  

  submit_job_to_HPC(server.IP.address = server.IP.address,
                     local.model.name = local.model.name, 
                     local.data.name = local.data.name,  
                     server.model.dir = server.model.dir,
                     server.data.dir = server.data.dir
   )
  showNotification("submit job sucessfully", type="message") # "default, "message", "warning", "error"
  
})


#--------------------------------------------------------
# check status 
#--------------------------------------------------------
observeEvent({input$check_status}, {
  
  validate(need(input$server.model.dir, message="empty server.model.dir"), 
           need(input$server.data.dir, message="empty server.data.dir")  
  )
  
  server.model.dir = (input$server.model.dir)    
  server.data.dir= (input$server.data.dir)    
  
  # create a folder locally if not exit
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  
  system(command = paste0("scp ", input$server.IP.address, ":", 
                          paste0(server.model.dir,  "/output.log  ", "."))) 
  
  
  error.message= "No such file or directory, cannot open the connection"
  value =  tryCatch(readLines(paste0("./output.log")),      
                    error=function(e) {
                      print(error.message); 
                      return(NULL)
                    } #, finally = {
                    # eval(parse(text=txt)) %>% as.data.frame()
                    #}
  )
  
  if (is.null(value)) { 
    updateTextAreaInput(session, "check_status_content", value=error.message)
    showNotification("check status failed", type="error") # "default, "message", "warning", "error"
  }else {
    value = paste0(value, sep="\n")   # sep="<br/>")
    value = paste0(value, collapse="")
    updateTextAreaInput(session, "check_status_content", value=value)
    
    showNotification("check status sucessfully", type="message") # "default, "message", "warning", "error"
  }
})

return(ALL)  
}
