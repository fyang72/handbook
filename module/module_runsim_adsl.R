#######################################################################
# module_build_adsl_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------
module_runsim_adsl_UI <- function(id, label = "") {
# Create a namespace function using the provided id
ns <- NS(id)

fluidRow(
  column(width=12,   
         uiOutput(ns("adsl_source_selector")),
          
         fluidRow(column(width=6, uiOutput(ns("load_manual_adsl_container")))), 
         fluidRow(column(width=12, uiOutput(ns("load_script_adsl_container")))), 
         fluidRow(column(width=6, uiOutput(ns("load_internal_adsl_container")))), 
         fluidRow(column(width=6, uiOutput(ns("load_session_adsl_container")))),
         fluidRow(column(width=6, uiOutput(ns("load_external_adsl_container")))),
         
         uiOutput(ns("adsl_table_container"))
  )
)
}

########################################################################
# module_runsim_adsl
########################################################################

module_runsim_adsl <- function(input, output, session, ALL, values)  {

ns <- session$ns 

#--------------------------------------  
# adsl_source_selector
#--------------------------------------
output$adsl_source_selector <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
  
  fluidRow(
    column(12,  
           radioButtons(ns("adsl_source"), 
                        label="Construct adsl from:", 
                        choices=c("manual input", 
                                  "script", 
                                  "internal library", 
                                  "within session",
                                  "external file"), 
                        inline=TRUE, 
                        width="100%",
                        selected="manual input")
    )
  )
  
})

#--------------------------------------  
# load_manual_adsl_container
#-------------------------------------- 
output$load_manual_adsl_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adsl_source=="manual input", message=FALSE)) 
  
  tagList(
    numericInput(ns("n_subject"), 
                 label = "Number of subjects in each population",
                 value=1, 
                 min=1, max=2000,step = 1
                 ), 
    
    textInput(ns("pop_WT"), 
              label = "Body weight(s) in populations",
              value="75"
              )
  )
  
})


#--------------------------------------  
# load_script_adsl_container
#-------------------------------------- 
output$load_script_adsl_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adsl_source=="script", message=FALSE)) 
      
script = "
#---------------------------------
# Key variables:  USUBJID, WGTBL
#---------------------------------
#Example-1
#adsl = data.frame(USUBJID=1:3, WGTBL=seq(60, 80, by=10))

#Example-2
library(dmutate)
nsubject = 3
seed = 1234

adsl <- 
 data.frame(USUBJID=1:nsubject) %>% 

 # [lower,upper] ~ rnorm(mu,sd))
 mutate_random(WGTBL[50,110] ~ rnorm(75,30)) %>%   

 # '1'=70%, '0'=30%
 mutate_random(SEX ~ rbinomial(0.7))
"
script <- readLines(textConnection(script)) 
script <- paste0(script, collapse="\n")
    
fluidRow(
  column(12, 
         aceEditor(ns("script_for_adsl"), 
                   mode="r", 
                   value=script, 
                   theme = "crimson_editor",   # chrome
                   autoComplete = "enabled",
                   height = "500px", 
                   fontSize = 15 
         ) 
  )
)
  
}) 
 

#--------------------------------------  
# load_internal_adsl_container
#-------------------------------------- 
output$load_internal_adsl_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adsl_source=="internal library", message=FALSE)) 
  
  dirs_lst=list.files(path = paste0(HOME, "/data/"), 
                       full.names = FALSE, 
                       recursive = FALSE, 
                       #pattern=".cpp", 
                       include.dirs=FALSE)  
  dirs_lst = c("", dirs_lst) 
  
  selectizeInput(ns("which_internal_adsl"), 
                 label    = "load internal adsl", 
                 choices  = dirs_lst, 
                 multiple = FALSE,
                 width = "100%", 
                 selected = dirs_lst[1]
  ) 
})


#--------------------------------------  
# load_session_adsl_container
#-------------------------------------- 
output$load_session_adsl_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adsl_source=="within session", message=FALSE)) 
  
  name_lst <- names(ALL$DATA)
  only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
  dirs_lst = c("", setdiff(name_lst, only_for_internal_use))
  
  selectizeInput(ns("which_session_adsl"), 
                label    = "load session adsl", 
                choices  = dirs_lst, 
                multiple = FALSE,
                width="100%", 
                selected = dirs_lst[1]) 
 
}) 


#--------------------------------------  
# load_external_dataset_container
#--------------------------------------  
output$load_external_adsl_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE),
           need(input$adsl_source=="external file", message=FALSE)) 
  
  fluidRow(
    column(12,
           fileInput(ns("which_external_adsl"), label = "load external adsl", width="100%" ) # h5
           # accept=c('text/csv/sas7bdat', 
           #          'text/comma-separated-values,text/plain', 
           #          '.xlsx',
           #          '.xls',
           #          '.csv', 
           #          '.sas7bdat', 
           #          '.RData'))
    )
  )
  
})

#--------------------------------------  
# adsl_table_container
#-------------------------------------- 
output$adsl_table_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(adsl(), message=FALSE)
  )
 
  ALL = callModule(module_save_data, "adsl_table", 
                   ALL,
                   data = adsl(),   
                   data_name = "adsl"
  )
  
  module_save_data_UI(ns("adsl_table"), label = NULL)
  
}) 



#--------------------------------------  
# reactive of load_manual_adsl
#-------------------------------------- 
load_manual_adsl <- reactive({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$n_subject, message=FALSE), 
           need(input$pop_WT, message=FALSE)
  ) 
      
  # mread cppModel
  #environment(try_eval) <- environment()
  text=paste0("pop_WT=c(", input$pop_WT, ")")
  env = try_eval(text)
  
  if ("pop_WT" %in% ls(env)) {
    pop_WT = get("pop_WT", env)
  }else{
    pop_WT = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error" 
    showNotification(paste0(error_message, collapse="\n"), type="error")
  } 
  
  pop_WT = unique(pop_WT)
  if (is.vector(pop_WT)) {
    adsl = data.frame(USUBJID=1:(input$n_subject * length(pop_WT)), 
                      WGTBL = rep(pop_WT, each=input$n_subject)
    )
  }else{
    adsl = NULL
  }
  
  adsl 

})

#--------------------------------------  
# reactive of load_script_adsl
#-------------------------------------- 
load_script_adsl <- reactive({
   
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$script_for_adsl, message=FALSE)
  )
   
  # script_for_adsl
  environment(try_eval) <- environment()
  env = try_eval(text=input$script_for_adsl) # need "adsl= "
  
  if ("adsl" %in% ls(env)) {
    adsl = get("adsl", env)
  }else{
    adsl = NULL
    error_message = get("message", env)
    # "default, "message", "warning", "error" 
    showNotification(paste0(error_message, collapse="\n"), type="error")
  }
   
  if (!is.data.frame(adsl)) {adsl = NULL}    
   
  adsl
}) 


#--------------------------------------  
# reactive of load_session_dataset
#-------------------------------------- 
load_session_adsl <- reactive({
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_session_adsl, message=FALSE), 
           need(ALL$DATA, message="no adsl found")
  )
   
  adsl = ALL$DATA[[input$which_session_adsl]]
  adsl
}) 


#--------------------------------------  
# reactive of load_internal_adsl
#-------------------------------------- 
load_internal_adsl <- reactive({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_internal_adsl, message=FALSE))
  
  inFile = paste0(HOME, "/data/", input$which_internal_adsl)
  adsl = read_datafile(inFile)
  message.info = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
  if (is.null(adsl)) {print(message.info)}
  validate(need(adsl, message.info)) 
  
  attr(adsl, 'file_name') <- inFile  # with directory
  attr(adsl, 'locaton_source') <- "internal"
  adsl
  
})


#--------------------------------------  
# reactive of load_external_adsl
#-------------------------------------- 
load_external_adsl <- reactive({
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_external_adsl, message = FALSE))
  
  inFile = input$which_external_adsl
  adsl = read_datafile(inFile$datapath)
  # print(inFile)
  # name            size type  datapath
  # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
  # 

  error_message = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
  if (is.null(adsl)) {
    showNotification(paste0(error_message, collapse="\n"), type="error")
    }
   
  adsl
})


adsl <- reactive({
  validate(need(input$adsl_source, message=FALSE))
  
  adsl <- switch(input$adsl_source, 
                 "manual input" = load_manual_adsl(), 
                 "script" = load_script_adsl(), 
                 "internal library" = load_internal_adsl(), 
                 "within session" = load_session_adsl(), 
                 "external file" = load_external_adsl(), 
                 NULL) %>% as.data.frame()
  
  # must have USUBJID and WGTBL
  col_name_lst = c("USUBJID", "WGTBL")
  all_yes = all(col_name_lst %in% colnames(adsl))
  if(all_yes==FALSE) {
    error_message = paste0("Missing column(s) of ", paste0(setdiff(col_name_lst, colnames(adsl)), collapse=", "), " in ", "adsl")
    showNotification(paste0(error_message, collapse="\n"), type="error")
    adsl = NULL
  }
  
  values$adsl = adsl
   
  adsl
})

 
return(values)
}