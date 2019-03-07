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

#derive model_name and data_name
ctlModel = ALL$ctlModel[[ctlModel_name]]
nmdat = ALL$DATA[[ctlModel_name]]

if (!is.null(ctlModel) & !is.null(nmdat)) {
  ctlModel_file_name = attributes(ctlModel)$file_name
  nmdat_file_name = attributes(nmdat)$file_name
  
  # default: .ctl and .csv
  model_name <- tools::file_path_sans_ext(basename(ctlModel_file_name)) # LN001
  data_name <- tools::file_path_sans_ext(basename(nmdat_file_name)) # DAT001
}else {
  model_name = NULL
  data_name = NULL
}


#--------------------------------------  
# which_program_container
#-------------------------------------- 
output$which_program_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE), 
           need(!is.null(input$server_IP_address), message=FALSE)
  )
  
  if (input$server_IP_address=="") {
    list_of_program <- paste0("program", 1:10)
  }else{
    list_of_program <- list_folder_on_HPC(
      server_IP_address = input$server_IP_address, 
      directory_on_server = paste0("/home/", 
                                   tolower(Sys.info()["user"]), "/")
    )
    list_of_program = gsub("/", "", list_of_program, fix=TRUE)
  }

  fluidRow(
    column(3,
           selectizeInput(ns("which_program"), 
                          label    =  "Select program:", 
                          choices  = list_of_program, 
                          multiple = FALSE,
                          width="100%", 
                          selected = list_of_program[1]) 
    )
  )
})


#----------------------------------------------------
# server_info_container
#----------------------------------------------------
output$server_info_container <- renderUI({
  
  tagList( 
    fluidRow(
      column(width = 6, 
             textInput(ns("server_IP_address"), 
                       width = '100%',  
                       value= server_IP_address,  # NULL
                       placeholder = "xx.xx.xx.xx.xx", 
                       label="Server IP address"
             )
      )
    ), 
    
    fluidRow(
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server_model_dir"), 
                       width = '100%',  
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    input$which_program,
                                    "/ctl/",
                                    paste(model_name, data_name, sep="_" ), "/"), 
                       label="Directory of the loaded model on server :")),
      
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server_data_dir"), 
                       width = '100%',
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    input$which_program,
                                    "/data/"),
                       label="Directory of the loaded data on server:")
      ) 
   ), 
   
   # run_command
   fluidRow(
     column(width = 6, 
            textInput(ns("run_command"), 
                      width = '100%',  
                      value= paste0("execute ",  model_name, ".ctl -clean=4 -node=10 "),  #,  # NULL
                      placeholder = "execute LN001.ctl -clean=4 ", 
                      label="command to run"
            )
     )
   )
   
  ) # tagList
  
})



# log_container
output$check_status_container <-  renderUI({  
  
  validate(need(input$server_model_dir, message=FALSE), 
           need(input$server_data_dir, message=FALSE)  
  )
  
  fluidRow(
    column(12,
           textAreaInput(ns("check_status_content"), label=NULL, value=NULL, rows=10,
                         width = '785px',   #400px', or '100%'
                         placeholder= "check your run-status here.")  
    )
  )  
})

# for multiple job submission and check their status
# https://github.com/rstudio/shiny/issues/924
output$dynamic_check_status_container <- renderUI({
  validate(need(values$runno, message=FALSE), 
           need(input$which_program, message = FALSE)
  )
  
  runno_lst = names(values$runno)
  tabs <- lapply(1:length(runno_lst),function(i){
    x = runno_lst[i]
    text = paste0(paste0(values$runno[[x]]$lst_content, sep="\n"), collapse="")
    
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



###################################################
# submit_job
###################################################

observeEvent({input$submit_job}, {
  validate( 
    need(nmdat, message=FALSE), 
    need(ctlModel, message=FALSE), 
    need(input$server_model_dir, message =FALSE), 
    need(input$server_data_dir, message =FALSE)
  )
  
  ctlModel_file_name = attributes((ctlModel))$file_name
  ctlModel_locaton_source = attributes((ctlModel))$locaton_source
  
  nmdat_file_name = attributes((nmdat))$file_name
  nmdat_locaton_source = attributes((nmdat))$locaton_source
  
  # create a folder locally if not exit
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  owd <- tempdir()
  on.exit(setwd(owd))
  
  if (nmdat_locaton_source %in% c("external", "session"))  {
    write.csv(nmdat, file= paste0(owd,  basename(nmdat_file_name)), 
              row.names = FALSE,
              na = ".", quote=FALSE) 
  }
  
  if (ctlModel_locaton_source %in% c("external", "session"))  {
    writeLines(ctlModel, con = paste0(owd,  basename(ctlModel_file_name)))
  }
  
  local_model_name = ifelse(ctlModel_locaton_source=="internal", ctlModel_file_name, 
                            ifelse(ctlModel_locaton_source%in% c("external", "session"), 
                                   paste0(owd, basename(ctlModel_file_name)), NULL))    
  local_data_name = ifelse(nmdat_locaton_source=="internal", nmdat_file_name, 
                           ifelse(nmdat_locaton_source%in% c("external", "session"), 
                                  paste0(owd, basename(nmdat_file_name)), NULL))  
  
  if (!input$server_IP_address == "") {
    submit_job_to_HPC(server_IP_address = input$server_IP_address,
                    local_model_name = local_model_name, 
                    local_data_name = local_data_name,  
                    server_model_dir = input$server_model_dir,
                    server_data_dir = input$server_data_dir, 
                    run_command = input$run_command
   )
  }
  
  showNotification("submit job sucessfully", type="message") # "default, "message", "warning", "error"
  
  # carry it to fetch tab if a single fetch
   attr(ctlModel, "which_program") = input$which_program  
   attr(nmdat, "which_program") = input$which_program  
   ALL$ctlModel[[ctlModel_name]] = ctlModel
   ALL$DATA[[ctlModel_name]] = nmdat
  
})


#--------------------------------------------------------
# check status 
#--------------------------------------------------------
observeEvent({input$check_status}, {
  
  validate(need(!is.null(input$server_IP_address), message=FALSE),
           need(input$server_model_dir, message=FALSE), 
           need(input$server_data_dir, message=FALSE)  
  )
   
  # create a folder locally if not exit
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  
  if (!input$server_IP_address == "") {
  system(command = paste0("scp ", input$server_IP_address, ":", 
                          paste0(input$server_model_dir,  "/output.log  ", "."))) 
  }
  
  error_message= "No such file or directory, cannot open the connection"
  value =  tryCatch(readLines(paste0("./output.log")),      
                    error=function(e) {
                      print(error_message); 
                      return(NULL)
                    } #, finally = {
                    # eval(parse(text=txt)) %>% as.data.frame()
                    #}
  )
  
  if (is.null(value)) { 
    updateTextAreaInput(session, "check_status_content", value=error_message)
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
