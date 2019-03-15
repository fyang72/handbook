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
                        choices=c("manual input", "script", "within session","external file"), 
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
      
script =   
"    
#---------------------------------
# Key variables:  ID, WGTBL
#---------------------------------
#Example-1
adsl = expand.grid(ID=1:3, WGTBL=seq(60, 80, by=10))

#Example-2
library(dmutate)
nsubject = 3
seed = 1234

adsl <- 
 data.frame(ID=1:nsubject) %>% 

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
# load_session_adsl_container
#-------------------------------------- 
output$load_session_adsl_container <- renderUI({
  
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adsl_source=="within session", message=FALSE)) 
  
  name_lst <- names(ALL$DATA)
  only_for_internal_use <- name_lst[which(substr(name_lst, 1, 6)=="mYtEsT")]
  dirs_list = c("", setdiff(name_lst, only_for_internal_use))
  
  selectizeInput(ns("which_session_adsl"), 
                label    = "load session adsl", 
                choices  = dirs_list, 
                multiple = FALSE,
                width="100%", 
                selected = dirs_list[1]) 
 
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
 
  output$my_adsl_table <- DT::renderDataTable(                                                                                                                                          
    DT::datatable(data = adsl(),                                                                                                                                                     
                  options = list(pageLength = 10, 
                                 lengthChange = FALSE, 
                                 width="100%", 
                                 scrollX = TRUE)                                                                   
    ))
  DT::dataTableOutput(ns("my_adsl_table"))
  
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
  environment(try_eval) <- environment()
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
    adsl = data.frame(ID=1:(input$n_subject * length(pop_WT)), 
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
# reactive of load_external_adsl
#-------------------------------------- 
load_external_adsl <- reactive({
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$which_external_adsl, message = FALSE))
  
  inFile = input$which_external_adsl
  
  # print(inFile)
  # name            size type  datapath
  # 1 cpp.model.cpp 6369      /tmp/RtmprQR1xU/1eb54214311d1970e61c917f/0.cpp
  # 
  ext <- tools::file_ext(inFile$name) 
  adsl = switch(ext,
                 "csv" = read_csv(inFile$datapath, col_names=TRUE,  
                                  col_type=cols(.default=col_character())),
                 "xlsx"=read_excel(inFile$datapath, sheet = 1, col_names = TRUE),
                 "xls" = read_excel(inFile$datapath, sheet = 1, col_names = TRUE),
                 "sas7bdat" =  read_sas(inFile$datapath), 
                 "RData" =  load(inFile$datapath), 
                 NULL
  )
  
  
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
                 "within session" = load_session_adsl(), 
                 "external file" = load_external_adsl(), 
                 NULL) %>% as.data.frame()
  
  # must have ID and WGTBL
  col_name_lst = c("ID", "WGTBL")
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