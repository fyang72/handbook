
################################################################################ 
# module_fetch_job_UI
################################################################################
# A module's UI function should be given a name that is suffixed with Input, Output, or UI; 
module_fetch_job_UI <- function(id, label = "") {
  
  ns <- NS(id) # Create a namespace function using the provided id
  
  fluidRow(
    column(12,  
           
           # select program and runs
             fluidRow(
               column(width = 2,
                      radioButtons(ns("want_batch_fetch"), label="Batch fetch", 
                                   inline = TRUE, 
                                   choices = list("No"="No", "Yes"="Yes"),
                                   selected =  "No" 
                                   )
               ),   
               
               column(width = 3,
                      uiOutput(ns("which_program_container"))    
               ), 
               
               # column(width = 3,
               #        uiOutput(ns("which_data_container"))    
               # ),
               
               column(width = 7,
                      uiOutput(ns("which_run_container"))    
               )
             ), 
             
             fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
             
             # server.IP.address
             fluidRow(
               column(width = 6, 
                      textInput(ns("server.IP.address"), 
                                width = '100%',  
                                value= server.IP.address, 
                                placeholder = "xx.xx.xx.xx.xx", 
                                label="Server IP address:"
                      )
               )
             ), 
           
           uiOutput(ns("server_info_container")),  
           
           fluidRow(column(width=12, tags$hr(style="border-color: gray;"))), 
           
           fluidRow(
             column(width=3, 
                    actionButton(ns("fetchResult"), label="fetch result", 
                                 style=actionButton.style
                    )
             )
           ), 
           
           uiOutput(ns("dynamic_output_container"))#, 
           #style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    )
  ) 
  
}

################################################################################ 
# main function: module_fetch_job
################################################################################

module_fetch_job <- function(input, output, session, ALL, ctlModel_name="ctlModel_name") {

ns <- session$ns 
values<- reactiveValues()

#--------------------------------------  
# which_program_container
#-------------------------------------- 
output$which_program_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE)) 
   
  if (input$want_batch_fetch=="No") {
    validate(need(ALL$ctlModel, message=FALSE), 
             need(ctlModel_name, message=FALSE) 
    )
    dirs.list = attributes(isolate(ALL$ctlModel[[ctlModel_name]]))$which.program
  }else if (input$want_batch_fetch=="Yes") {
    dirs.list <- paste0("program", 1:10)
    # list_folder_on_HPC(
    # server.IP.address = input$server.IP.address, 
    # directory.on.server = paste0("/home/", tolower(Sys.info()["user"]), "/")
    # )
    }
   
  fluidRow(
    column(12,
           selectizeInput(ns("which_program"), 
                          label    =  "Select program:", 
                          choices  = dirs.list, 
                          multiple = FALSE,
                          width="100%", 
                          selected = dirs.list[1]) 
    )
  )
}) 


#--------------------------------------  
# which_data_container
#-------------------------------------- 
# output$which_data_container <- renderUI({
#   validate(need(globalVars$login$status, message=FALSE)) 
#   
#   validate(need(input$want_batch_fetch=="Yes", message=FALSE), 
#            need(input$which_program, message=FALSE)
#   )
#   
#   dirs.list = values$list_of_data 
#   
#   fluidRow(
#     column(12,
#            selectizeInput(ns("which_data"), 
#                           label    =  "Select data:", 
#                           choices  = dirs.list, 
#                           multiple = TRUE,
#                           width="100%", 
#                           selected = dirs.list[1]) 
#     )
#   )
# }) 



#--------------------------------------  
# which_run_container
#-------------------------------------- 
output$which_run_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE)) 
  
  validate(need(input$want_batch_fetch=="Yes", message=FALSE), 
           need(input$which_program, message=FALSE)
  )
  
  dirs.list = values$list_of_run 
   
  fluidRow(
    column(12,
           selectizeInput(ns("which_run"), 
                          label    =  "Select run(s):", 
                          choices  = dirs.list, 
                          multiple = TRUE,
                          width="100%", 
                          selected = dirs.list[1]) 
    )
  )
}) 


#----------------------------------------------------
# server_info_container
#----------------------------------------------------
output$server_info_container <- renderUI({
  validate(need(input$want_batch_fetch=="No", message=FALSE))
  
  ctlModel = ALL$ctlModel[[ctlModel_name]]
  nmdat = ALL$DATA[[ctlModel_name]]
  
  if (!is.null(ctlModel) & !is.null(nmdat)) {
    ctlModel.file.name = attributes(ctlModel)$file.name
    ctlModel.locaton.source = attributes(ctlModel)$locaton.source
    
    nmdat.file.name = attributes(nmdat)$file.name
    nmdat.locaton.source = attributes(nmdat)$locaton.source
    
    runno <- tools::file_path_sans_ext(basename(ctlModel.file.name))
    data_name <- tools::file_path_sans_ext(basename(nmdat.file.name))
  }else {
    runno = NULL
    data_name = NULL
  }
  
  fluidRow(
    column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
           textInput(ns("server.model.dir"), 
                     width = '100%',  
                     value=paste0("/home/", 
                                  tolower(Sys.info()["user"]), "/",
                                  paste0(input$which_program, "/ctl/"),
                                  paste(runno, data_name, sep="_" ), "/"), 
                     label="Directory of the loaded model on server:")
           ),
    
    column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
           textInput(ns("server.data.dir"), 
                     width = '100%',
                     value=paste0("/home/", 
                                  tolower(Sys.info()["user"]), "/",
                                  paste0(input$which_program, "/data/")
                                  ),
                     label="Directory of the loaded data on server:")
           )#, 
    
    # column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
    #        textInput(ns("local.result.dir"), 
    #                  width = '100%',  
    #                  value=paste0("/output/", 
    #                               tolower(Sys.info()["user"]), "/",
    #                               paste0(input$which_program, "/"), 
    #                               paste(runno, data_name, sep="_" ), "/"), 
    #                  label="Directory of the fetched result at local:")
    # )
  ) 
  
})
  

#----------------------------------------------------
# output_container
#----------------------------------------------------
 
output$output_container <- renderUI({   #renderPrint  renderText  renderUI
  
  validate(need(input$want_batch_fetch=="No", message=FALSE))
  
  lst.content =  values$run.model$lst.content  # pure text
  lst = values$run.model$lst   # after read.lst data object
  validate(need(lst.content, message=FALSE), 
           need(lst, message=FALSE)
  )
  
  value = lst.content   
  value = paste0(value, sep="\n")   # sep="<br/>")
  value = paste0(value, collapse="")
  
  #https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
 
    fluidRow(
      column(12,
             textAreaInput(ns("lst_content"), label=NULL, value=value, rows=50,
                           width = '780px', #   '800px',   #400px', or '100%'
                           placeholder= "Your output here.") 
      )
    ) 
  
})






# https://github.com/rstudio/shiny/issues/924
output$dynamic_output_container <- renderUI({
  validate(need(values$runno, message=FALSE))
  
  runno_lst = names(values$runno)
  tabs <- lapply(1:length(runno_lst),function(i){
    x = runno_lst[i]
    text = paste0(paste0(values$runno[[x]]$lst.content, sep="\n"), collapse="")
    
    tabPanel(
      title = paste0("Run",i)
      ,h5(paste0('runno: ',x)) 
      ,value=x
      ,fluidRow(
        column(12,
               textAreaInput(paste0('runno_', x), 
                             label=NULL, 
                             value= text, 
                             rows=100,
                             width = '780px', #   '800px',   #400px', or '100%'
                             placeholder= "Your output here.") 
        )
      ) 
    )
  })
  do.call(tabsetPanel,c(tabs,id='selected_runno'))
})

 
#----------------------------------------------------
# look up runs under which program
#----------------------------------------------------

observeEvent({input$which_program}, {
  validate(need(input$want_batch_fetch=="Yes", message=FALSE))
  
    # list_folder_on_HPC(
    # server.IP.address = input$server.IP.address, 
    # directory.on.server = paste0("/home/", 
    #                               tolower(Sys.info()["user"]), "/" 
    # )
    
  # program-1
  if (input$which_program=="program1") {
    values$list_of_run <- paste0("LN", 1:10, "~", "DAT", 1:10)
  }
  
  # if (input$which_program=="program-1") {
  #   values$list_of_data <- paste0("data-", 1:10)
  # } 
  
  # program-2                
  if (input$which_program=="program2") {
    values$list_of_run <- paste0("LN", 11:20, "~", "DAT", 11:20)
  }           
  
  # if (input$which_program=="program-2") {
  #   values$list_of_data <- paste0("data-", "~", "DAT", 1:10 )
  # }
})
 
  
#----------------------------------------------------
# fetchResult
#----------------------------------------------------
observeEvent({input$fetchResult}, {
validate(need(input$want_batch_fetch, message=FALSE))
  
if (input$want_batch_fetch=="No") { 
    ctlModel = ALL$ctlModel[[ctlModel_name]]
    ctlModel.file.name = attributes(ctlModel)$file.name
    ctlModel.locaton.source = attributes(ctlModel)$locaton.source
     
    nmdat = ALL$DATA[[ctlModel_name]]
    nmdat.file.name = attributes(nmdat)$file.name
    nmdat.locaton.source = attributes(nmdat)$locaton.source
    
    validate(
      need(input$which_program, message=FALSE), 
      
      need(nmdat, message=FALSE), 
      need(nmdat.locaton.source %in% c("internal", "external", "session"), message=FALSE),
      
      need(ctlModel, message=FALSE), 
      need(ctlModel.locaton.source %in% c("internal", "external", "session"), message=FALSE) 
    ) 
    
    model_name = tools::file_path_sans_ext(basename(ctlModel.file.name))
    data_name = tools::file_path_sans_ext(basename(nmdat.file.name))
     
    #--------------------------
    # fetch modeling result 
    #--------------------------
    #local.result.dir = input$local.result.dir
    local.result.dir = paste0("/output/", 
                              tolower(Sys.info()["user"]), "/",
                              paste0(input$which_program, "/ctl/"), 
                              paste0(model_name, "~", data_name, "/")
    )
    
    # fetch_job_from_HPC(  
    #    server.IP.address = input$server.IP.address,
    #    server.model.dir = input$server.model.dir, 
    #    local.result.dir = local.result.dir
    #  ) 
    
    #--------------------------
    # fetch nmdat from HPC
    #--------------------------
    server.data.file <- paste0("/home/", 
                               tolower(Sys.info()["user"]), "/",
                               paste0(input$which_program, "/data/", 
                                      basename(nmdat.file.name)
                                      )
    )
    
    local.data.dir <- paste0("/output/", 
                               tolower(Sys.info()["user"]), "/",
                               paste0(input$which_program, "/data/") 
    )
    
    
    # system(command = 
    #          paste0("scp ", input$server.IP.address, ":", 
    #                 server.data.file, "  ", local.data.dir 
    #          )
    # )
    
    #--------------------------
    # read lst file
    #--------------------------
    lst.file = paste0(local.result.dir, model_name, ".lst")
    runno <- paste0(model_name, "_", data_name)
    text1 = paste0("values$runno", "[[", runno, "]]$lst = read.lst(lst.file)")
    text2 = paste0("values$runno", "[[", runno, "]]$lst.content = readLines(lst.file)")
    text = paste0(paste(text1, text2, sep=";"), collpase="")
    error.message <- try_eval(text = text)
     
    if (length(error.message)>0) {
      showNotification(paste0(error.message, collapse="\n"), type="error")
    }
    
    if (length(error.message)==0) { 
      showNotification("fetch result sucessfully", type="message")   # "default, "message", "warning", "error"
    }
}
  
  
######################################
# batch fetch  
######################################
if (input$want_batch_fetch=="Yes") {
  
  validate(need(input$which_run, message=FALSE), 
           need(input$which_program, message=FALSE), 
           need(input$server.IP.address, message=FALSE)
  )
  
  runno_lst = input$which_run
  
  df <- data.frame(do.call(
    "rbind",
    strsplit(runno_lst, "~")
  ))
  df <- cbind(runno_lst, df)
  colnames(df) = c("runno", "ctlModel", "nmdat")
  
  # loop for all runno(s)
  for (i in 1:nrow(df)) {
    model_name = df[i, "ctlModel"] 
    data_name = df[i, "nmdat"] 
    runno <- df[i, "runno"] # paste0(model_name, "_", data_name)
    
    #---------------------------
    # fetch modeling results
    #--------------------------- 
    server.model.dir <- paste0("/home/", 
                               tolower(Sys.info()["user"]), "/",
                               paste0(input$which_program, "/ctl/"),
                               paste0(runno, "/")
    )
    
    local.result.dir <- paste0(HOME, "/output/", 
                               tolower(Sys.info()["user"]), "/",
                               paste0(input$which_program, "/ctl/"), 
                               paste0(runno, "/")
    )
   
    # fetch_job_from_HPC(  
    #   server.IP.address = input$server.IP.address,
    #   server.model.dir = server.model.dir, 
    #   local.result.dir = local.result.dir
    # )
    
    #-------------------
    # fetch dataset  
    #-------------------  
    from <- paste0("/home/", 
                   tolower(Sys.info()["user"]), "/",
                   paste0(input$which_program, "/data/"),
                   paste0(data_name, "/")
    )
    
    to <- paste0("/output/", 
                   tolower(Sys.info()["user"]), "/",
                   paste0(input$which_program, "/data/")
    )
    
    # system(command = 
    #          paste0("scp ", input$server.IP.address, ":", 
    #                 from, "  ", to 
    #          )
    # ) 
  
    #--------------------------
    # read lst file
    #--------------------------
    
    lst.file = paste0(local.result.dir, model_name, ".lst") 
    print(lst.file)
    #print(read.lst(lst.file))
    
    text1 = paste0("values$runno", "[['", runno, "']]$lst = read.lst(lst.file)")
    text2 = paste0("values$runno", "[['", runno, "']]$lst.content = readLines(lst.file)")
    text = paste0(paste(text1, text2, sep=";"), collpase="")
     
    #error.message <- try_eval(text = text)
    try(
      eval(parse(text=text))
    )
     
    error.message = NULL
    # "default, "message", "warning", "error"
    if (length(error.message)>0) {
      showNotification(paste0(runno, ": ", error.message, collapse="\n"), type="error")
    }
    
    if (length(error.message)==0) { 
      showNotification(paste0(runno, ": ", "fetch result sucessfully"), type="message")   # "default, "message", "warning", "error"
    }
  }
  
} # end of batch fetch
  
}) # end of observeEvent
 
 

} 