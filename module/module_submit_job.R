

################################################################################ 
# module_submit_job_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_submit_job_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(12, 
           # submit the job
           fluidRow(
             column(width = 12,
                    uiOutput(ns("which_program_container"))    
             ), 
             
             column(width = 12, 
                 
                    actionButton(ns("submit_job"), label="submit job", 
                              style="float:left;color: #fff; background-color: #328332; border-color: #328332"
                              )
                    )
             ),
           uiOutput(ns("server_info_container")),  
           
           # after submission, check the run status
           fluidRow(
             column(width = 12,
                    actionButton(ns("check_status"), label="check status", 
                                 style="float:left;color: #fff; background-color: #328332; border-color: #328332"
                    )
             )
           ),  
           uiOutput(ns("check_status_container")) 
    )
  ) 
  
}




################################################################################ 
# main function: module_load_data
################################################################################

module_submit_job <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {
  
  ns <- session$ns 
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  values<- reactiveValues()
  

#--------------------------------------  
# load_internal_data_container
#-------------------------------------- 
output$which_program_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE)) 
   
         
  dirs.list = c(list.files(path = paste0("./output/", 
                                             tolower(Sys.info()["user"]), "/"), 
                               full.names = FALSE, recursive = FALSE))
  if (length(dirs.list)==0) {dirs.list = "TEST"}
  
  fluidRow(
    column(3,
           selectizeInput(ns("which_program"), 
                          label    = NULL, # "load internal data", 
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
  
  #print("input$submit_job in server_info_container")
  # ((input$submit_job))
  # isolate({ 
  # values$run.model$job.submited = FALSE
  # })
  
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
                                  input$which_program,
                                  "/ctl/",
                                  paste(runno, data.name, sep="_" ), "/"), 
                     label="Server directory of the loaded model:")),
    
    column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
           textInput(ns("server.data.dir"), 
                     width = '100%',
                     value=paste0("/home/", 
                                  tolower(Sys.info()["user"]), "/",
                                  input$which_program,
                                  "/data/"),
                     label="Server directory of the loaded data:")
           )#, 
    
    # column(width=3, 
    #        actionButton(ns("submit_job"), label="run model", 
    #                     style="float:left;color: #fff; background-color: #328332; border-color: #328332"
    #                     ) 
    # )
  )
  
  
})




# output$submit_control_container1 <- renderUI({
#   
#   
#   print("input$submit_job in submit_control_container ")
#   print(isolate(input$submit_job))
#   
#   fluidRow(
#     column(width=3, 
#            actionButton(ns("submit_job"), label="run model", 
#                         style="float:left;color: #fff; background-color: #328332; border-color: #328332") 
#            ) 
#   )
#   
#   
# })


# log_container
output$check_status_container <-  renderUI({  
  #log =   values$run.model$log
  validate(need(input$server.model.dir, message="empty server.model.dir"), 
           need(input$server.data.dir, message="empty server.data.dir")  
  )
    
  fluidRow(
    column(12,
           textAreaInput(ns("check_status_content"), label=NULL, value=NULL, rows=20,
                         width = '785px',   #400px', or '100%'
                         placeholder= "check your run-status here.")  # uiOutput   textOutput  htmlOutput
    )
  )  
})



observeEvent({input$check_status}, {
  
  validate(need(input$server.model.dir, message="empty server.model.dir"), 
           need(input$server.data.dir, message="empty server.data.dir")  
  )
  
  
  server.model.dir = (input$server.model.dir)   #"/home/feng.yang/test_data/test_nm/"  
  server.data.dir= (input$server.data.dir)   #"/home/feng.yang/test_data/test_nm/"
  local.result.dir = "./tmp/"
  system(command = paste0("scp 10.244.106.127:", paste0(server.model.dir,  "/output.log  ", local.result.dir))) 
   
  
  error.message= "No such file or directory, cannot open the connection"
  value =  tryCatch(readLines("./tmp/output.log"),     # ?mread , ?mcode, 
                       error=function(e) {
                         print(error.message); 
                         return(NULL)
                       } #, finally = {
                       # eval(parse(text=txt)) %>% as.data.frame()
                       #}
  )
  
  print("value at 205:")
  print(value)
  
  if (is.null(value)) { 
      updateTextAreaInput(session, "check_status_content", value=error.message)
  }else {
      value = paste0(value, sep="\n")   # sep="<br/>")
      value = paste0(value, collapse="")
      updateTextAreaInput(session, "check_status_content", value=value)
      system(command=paste0("rm ./tmp/output.log"))
  }
  

  
})



 

observeEvent({input$submit_job}, {
  
  # print("input$submit_job in observeEvent ")
  # #print(isolate(input$submit_job))
  # validate(
  #   need(values$run.model$job.submited == FALSE, message=FALSE) #########################
  # )
  
  #isolate({ 
  #print("after observeEvent in submit job")
  
  ctlModel = ALL$ctlModel[[ctlModel_name]]
  nmdat = ALL$DATA[[ctlModel_name]]
  
  
  #ctlModel = ctlModel_run_inputData()
  ctlModel.file.name = attributes((ctlModel))$file.name
  ctlModel.locaton.source = attributes((ctlModel))$locaton.source
  
  #nmdat = nmdat_run_inputData()
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
  
  #print("after validation submit job")
  
  values$run.model$log = "job submited"
  
  if (nmdat.locaton.source %in% c("external", "session"))  {
    owd = "./tmp/"
    #owd <- tempdir()
    #on.exit(setwd(owd))
    write.csv(nmdat, file= paste0(owd,  basename(nmdat.file.name)), 
              row.names = FALSE,
              na = ".", quote=FALSE) 
  }
  
  if (ctlModel.locaton.source %in% c("external", "session"))  {
    owd = "./tmp/"
    #owd <- tempdir()
    #on.exit(setwd(owd)) 
    writeLines(ctlModel, con = paste0(owd,  basename(ctlModel.file.name)))
  }
  
  HOME = "~/FYANG/Template/Handbook/" 
  local.model.name = ifelse(ctlModel.locaton.source=="internal", ctlModel.file.name, 
                            ifelse(ctlModel.locaton.source%in% c("external", "session"), 
                                   paste0(owd, basename(ctlModel.file.name)), NULL))   #/model/ctl/control5copy.ctl"  
  local.data.name = ifelse(nmdat.locaton.source=="internal", nmdat.file.name, 
                           ifelse(nmdat.locaton.source%in% c("external", "session"), 
                                  paste0(owd, basename(nmdat.file.name)), NULL))  
  local.result.dir = NULL    #"/ctl/LN_BASE_WT/"  
  
  server.model.dir = (input$server.model.dir)   #"/home/feng.yang/test_data/test_nm/"  
  server.data.dir= (input$server.data.dir)   #"/home/feng.yang/test_data/test_nm/"
  
  
  #print("just before submit job")
  print("job submited")
  print(server.model.dir)
  print(server.data.dir)
  
  if (1==1) { 
    submit_job_to_HPC(HOME = "~/FYANG/Template/Handbook/",
                     local.model.name = local.model.name, 
                     local.data.name = local.data.name, 
                     local.result.dir=NULL, 
                     server.model.dir = server.model.dir,
                     server.data.dir = server.data.dir
                     )
  
     
  }
  
  #values$run.model$job.submited = TRUE
  #})
})

 
}
