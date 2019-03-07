
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
             
             column(width = 7,
                    uiOutput(ns("which_run_container"))    
             )
           ), 
           
           fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
           
           # server_IP_address
           fluidRow(
             column(width = 6, 
                    textInput(ns("server_IP_address"), 
                              width = '100%',  
                              value= server_IP_address, 
                              placeholder = "xx.xx.xx.xx.xx", 
                              label="Server IP address:"
                    )
             )
           ), 
           
           uiOutput(ns("server_info_container")),  
           
           fluidRow(column(width=12, tags$hr(style="border-color: gray;"))), 
           
           fluidRow(
             column(width=3, 
                    actionButton(ns("fetch_result"), label="fetch result", 
                                 style=actionButton_style
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
  # reactive for ctlModel and nmdat
  #--------------------------------------
  ctlModel <- reactive({
    ctlModel = ALL$ctlModel[[ctlModel_name]]
  })
  
  nmdat <- reactive({
    nmdat = ALL$DATA[[ctlModel_name]]
  })
  
  model_name <- reactive({
    ctlModel = ctlModel()
    
    if (!is.null(ctlModel)) {
      ctlModel_file_name = attributes(ctlModel)$file_name
      model_name <- tools::file_path_sans_ext(basename(ctlModel_file_name)) # LN001
    }else {
      model_name = NULL 
    }
  })
  
  data_name <- reactive({
    nmdat = nmdat()
    
    if (!is.null(nmdat)) {
      nmdat_file_name = attributes(nmdat)$file_name
      data_name <- tools::file_path_sans_ext(basename(nmdat_file_name)) # DAT001
    }else {
      data_name = NULL
    }
  })
  
  #--------------------------------------  
  # which_program_container
  #-------------------------------------- 
  output$which_program_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE), 
             need(input$want_batch_fetch, message=FALSE)
    )
    
    # If only a single fetch 
    if (input$want_batch_fetch=="No") {
      validate(need(ALL$ctlModel, message=FALSE), 
               need(ctlModel_name, message=FALSE) 
      )
      
      list_of_program = attributes(isolate(ALL$ctlModel[[ctlModel_name]]))$which_program
      
      # for batch fetch
    }else if (input$want_batch_fetch=="Yes") {
      
      if (input$server_IP_address=="") {
        list_of_program <- paste0("program", 1:10)
      }else{
        list_of_program <- 
          list_folder_on_HPC(server_IP_address = input$server_IP_address, 
                             directory_on_server = paste0("/home/", tolower(Sys.info()["user"]), "/")
          )
        list_of_program = gsub("/", "", list_of_program, fix=TRUE)
      }
    }
    
    fluidRow(
      column(12,
             selectizeInput(ns("which_program"), 
                            label    =  "Select program:", 
                            choices  = list_of_program, 
                            multiple = FALSE,
                            width="100%", 
                            selected = list_of_program[1]) 
      )
    )
  }) 
  
  
  #--------------------------------------  
  # which_run_container
  #-------------------------------------- 
  output$which_run_container <- renderUI({
    validate(need(globalVars$login$status, message=FALSE)) 
    
    validate(need(input$want_batch_fetch=="Yes", message=FALSE), 
             need(input$which_program, message=FALSE)
    )
    
    list_of_run = values$list_of_run 
    
    fluidRow(
      column(12,
             selectizeInput(ns("which_run"), 
                            label    =  "Select run(s):", 
                            choices  = list_of_run, 
                            multiple = TRUE,
                            width="100%", 
                            selected = list_of_run[1]) 
      )
    )
  }) 
  
  
  #----------------------------------------------------
  # server_info_container
  #----------------------------------------------------
  output$server_info_container <- renderUI({
    validate(need(input$want_batch_fetch=="No", message=FALSE))
    
    fluidRow(
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server_model_dir"), 
                       width = '100%',  
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    paste0(input$which_program, "/ctl/"),
                                    paste(model_name(), data_name(), sep="_" ), "/"), 
                       label="Directory of the loaded model on server:")
      ),
      
      column(width = 6, #status = "primary",  #class = 'rightAlign', #background ="aqua",
             textInput(ns("server_data_dir"), 
                       width = '100%',
                       value=paste0("/home/", 
                                    tolower(Sys.info()["user"]), "/",
                                    paste0(input$which_program, "/data/")
                       ),
                       label="Directory of the loaded data on server:")
      )
    ) 
  })
  
  
  # https://github.com/rstudio/shiny/issues/924
  output$dynamic_output_container <- renderUI({
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
  
  
  #----------------------------------------------------
  # look up runs under which program
  #----------------------------------------------------
  
  observeEvent({input$which_program}, {
    validate(need(input$want_batch_fetch=="Yes", message=FALSE))
    
    # reset all runno
    values$runno = NULL
    
    # test
    if (input$server_IP_address=="") {
      if (input$which_program=="program1") {
        values$list_of_run <- paste0("LN", 1:10, "_", "DAT", 1:10)
      }
      
      if (input$which_program=="program2") {
        values$list_of_run <- paste0("LN", 11:20, "_", "DAT", 11:20)
      }  
      
      # fetch from server
    }else{
      values$list_of_run <- gsub("/", "", 
                                 list_folder_on_HPC(
                                   server_IP_address = input$server_IP_address, 
                                   directory_on_server = paste0("/home/", 
                                                                tolower(Sys.info()["user"]), "/", 
                                                                input$which_program, "/ctl/"
                                   )), fix=TRUE)
    }
  })
  
  
  #----------------------------------------------------
  # fetch_result
  #----------------------------------------------------
  observeEvent({input$fetch_result}, {
    validate(need(input$want_batch_fetch, message=FALSE))
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())  # Make sure it closes when we exit this reactive, even if there's an error
    progress$set(message = "fetching...please Wait", value = 0)
    
    if (input$want_batch_fetch=="No") { 
      
      validate(
        need(input$which_program, message=FALSE), 
        need(nmdat(), message=FALSE), 
        need(ctlModel(), message=FALSE)
      ) 
      
      #--------------------------
      # fetch modeling result 
      #--------------------------
      #local_result_dir = input$local_result_dir
      local_result_dir = paste0(HOME, "/output/", 
                                tolower(Sys.info()["user"]), "/",
                                paste0(input$which_program, "/ctl/"), 
                                paste0(model_name(), "_", data_name(), "/")
      )
      
      if (input$server_IP_address==""){
        # nothing
      }else {
        print('it is fetching now....')
        fetch_job_from_HPC(  
          server_IP_address = input$server_IP_address,
          server_model_dir = input$server_model_dir, 
          local_result_dir = local_result_dir
        ) 
      }
      
      
      #--------------------------
      # fetch nmdat from HPC
      #--------------------------
      server_data_file <- paste0("/home/", 
                                 tolower(Sys.info()["user"]), "/",
                                 paste0(input$which_program, "/data/", 
                                        basename(attributes(nmdat())$file_name)
                                 )
      )
      
      local_data_dir <- paste0(HOME, "/output/", 
                               tolower(Sys.info()["user"]), "/",
                               paste0(input$which_program, "/data/") 
      )
      
      if (input$server_IP_address==""){
        # nothing
      }else {
        # create a folder if not exist
        system(command = paste0("mkdir -p ", local_data_dir), intern = T)      
        system(command = 
                 paste0("scp ", input$server_IP_address, ":", 
                        server_data_file, "  ", local_data_dir 
                 )
        )
      }
      
      #--------------------------
      # read lst file
      #--------------------------
      lst_file = paste0(local_result_dir, model_name(), ".lst")
      runno <- paste0(model_name(), "_", data_name())
      text1 = paste0("values$runno", "[['", runno, "']]$lst = read.lst(lst_file)")
      text2 = paste0("values$runno", "[['", runno, "']]$lst_content = readLines(lst_file)")
      text = paste0(paste(text1, text2, sep=";"), collpase="")
      
      #error_message <- try_eval(text = text)
      error_message =  tryCatch(eval(parse(text=text))  , 
                                error=function(e) {
                                  return("fetch_failed")
                                } #, finally = {
                                # eval(parse(text=txt)) %>% as.data.frame()
                                #}
      )
      
      # "default, "message", "warning", "error"
      if (error_message[1]=="fetch_failed") {
        showNotification(paste0(runno, ": ", "Error in file(file, 'r') : cannot open the connection"), type="error")
      }else { 
        showNotification(paste0(runno, ": ", "fetch result sucessfully"), type="message")   # "default, "message", "warning", "error"
      }
    }
    
    
    ######################################
    # batch fetch  
    ######################################
    if (input$want_batch_fetch=="Yes") {
      
      validate(need(input$which_run, message=FALSE), 
               need(input$which_program, message=FALSE)
      )
      
      runno_lst = input$which_run
      
      df <- data.frame(do.call(
        "rbind",
        strsplit(runno_lst, "_")
      ))
      df <- cbind(runno_lst, df)
      colnames(df) = c("runno", "ctlModel", "nmdat")
      
      # loop for all runno(s)
      for (i in 1:nrow(df)) {
        imodel_name = df[i, "ctlModel"] 
        idata_name = df[i, "nmdat"] 
        irunno <- df[i, "runno"] 
        
        #---------------------------
        # fetch modeling results
        #--------------------------- 
        server_model_dir <- paste0("/home/", 
                                   tolower(Sys.info()["user"]), "/",
                                   paste0(input$which_program, "/ctl/"),
                                   paste0(irunno, "/")
        )
        
        local_result_dir <- paste0(HOME, "/output/", 
                                   tolower(Sys.info()["user"]), "/",
                                   paste0(input$which_program, "/ctl/"), 
                                   paste0(irunno, "/")
        )
        
        if (input$server_IP_address==""){
          # nothing
        }else {
          fetch_job_from_HPC(  
            server_IP_address = input$server_IP_address,
            server_model_dir = server_model_dir, 
            local_result_dir = local_result_dir
          )
        }
        
        #-------------------
        # fetch dataset  
        #-------------------  
        from <- paste0("/home/", 
                       tolower(Sys.info()["user"]), "/",
                       paste0(input$which_program, "/data/"),
                       paste0(idata_name, ".csv")
        )
        
        to <- paste0(HOME, "/output/", 
                     tolower(Sys.info()["user"]), "/",
                     paste0(input$which_program, "/data/")
        )
        
        # create a folder if not exist
        system(command = paste0("mkdir -p ", to), intern = T)
        
        if (input$server_IP_address==""){
          # nothing
        }else {
          system(command = 
                   paste0("scp ", input$server_IP_address, ":", 
                          from, "  ", to 
                   )
          ) 
        }
        
        #--------------------------
        # read lst file
        #--------------------------
        
        lst_file = paste0(local_result_dir, imodel_name, ".lst") 
        text1 = paste0("values$runno", "[['", irunno, "']]$lst = read.lst(lst_file)")
        text2 = paste0("values$runno", "[['", irunno, "']]$lst_content = readLines(lst_file)")
        text = paste0(paste(text1, text2, sep=";"), collpase="")
        
        #error_message <- try_eval(text = text)
        error_message =  tryCatch(eval(parse(text=text))  , 
                                  error=function(e) {
                                    return("fetch_failed")
                                  } #, finally = {
                                  # eval(parse(text=txt)) %>% as.data.frame()
                                  #}
        )
        
        # "default, "message", "warning", "error"
        if (error_message[1]=="fetch_failed") {
          showNotification(paste0(irunno, ": ", "Error in file(file, 'r') : cannot open the connection"), type="error")
        }else { 
          showNotification(paste0(irunno, ": ", "fetch result sucessfully"), type="message")   # "default, "message", "warning", "error"
        }
      }
      
    } # end of batch fetch
    
  }) # end of observeEvent
  
  
  
} 