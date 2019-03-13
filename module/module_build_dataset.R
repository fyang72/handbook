#######################################################################
# module_build_dataset_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_build_dataset_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  tagList(
    fluidRow(column(width=12, tags$hr(style="border-color: gray;"))),
    
    fluidRow(
      column(width=12,  
             HTML(colFmt("Note, the following tabset is used to  <br>
                          1) align the variable (column) against the standard one [column], <br> 
                          2) standardize the input dataset using script [script], <br>
                          3) semi-auto curation and confirmation [curation], <br>
                          4) render the resulting data and save [output].", color="gray")))
      ),
      
    tabBox(width=12, id = ns("run_script_for_dataset_construction"), title =NULL, 
       
        # dataset_container 
        tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
                 collapsed = TRUE, solidHeader = TRUE,
                 fluidRow(column(12, uiOutput(ns("dataset_container")))) 
        ),
       
       # checkInCols_container 
       tabPanel(width=12, title="checkInCols", value = "checkIn_columns", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("checkInCols_container")))) 
       ),        
       
       # runScript_container 
       tabPanel(width=12, title="runScript", value = "run_script", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("runScript_container"))))  
       ),     
           
       # checkInRows_container
       tabPanel(width=12, title="checkInRows", value = "checkIn_rows", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("checkInRows_container"))))
       ), 
       
       # output_container
       tabPanel(width=12, title="output", value = "output", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                fluidRow(column(width=12, uiOutput(ns("output_container"))))
       )
    ) # tabBox
  ) # tagList
}

########################################################################
# module_build_dataset
########################################################################

module_build_dataset <- function(input, output, session, 
                              ALL, dataset_name, script, default_checkin
                              )  {

ns <- session$ns
values <- reactiveValues(data=NULL,figure=NULL,table = NULL)

 


################################
# UI for dataset_container
################################
output$dataset_container <- renderUI({ 
   
  validate(need(ALL$DATA[[dataset_name]], message="no data found yet"))
   
  #0312_2019, NEED to return the data to ALL.  
  ALL = callModule(module_save_data, "dataset_to_be_built", 
                     ALL, 
                     data =ALL$DATA[[dataset_name]], 
                     data_name=dataset_name)
  
  module_save_data_UI(ns("dataset_to_be_built"), label = NULL) 
  
})

  

################################
# UI for checkInCols_container
################################
output$checkInCols_container <- renderUI({ 
  validate(need(globalVars$login$status, message=FALSE))
   
  # call module
  ALL = callModule(module_checkInCols, "checkInCols",  
             ALL, 
             dataset_name=dataset_name, 
             default_checkin = default_checkin 
  )
   
  # UI
  fluidRow(
    column(12, 
           module_checkInCols_UI(ns("checkInCols"), label = NULL)
           )
  )
})  
 
   
################################
# UI for runScript_container
################################
output$runScript_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
   
  tagList(
    fluidRow(column(12, 
                    HTML(colFmt("Note, by running/modifying the following script template, 
                                you can perform any manipulation upon the input data (dataset). 
                                Certain key words should not be changed; certain formatting should be followed. 
                                See instruction carefully.", color="gray"))
                    )
             ), 
    
    fluidRow(
      column(3, 
             actionButton(ns("run_script"), label="Run script", style=actionButton.style )
      )
    ),
    
    fluidRow(
      column(12,
             aceEditor(ns("script_content"), 
                       mode="r", value=script, 
                       theme = "crimson_editor",   # chrome
                       autoComplete = "enabled",
                       height = "1000px", 
                       fontSize = 15 
             )
      )
    )
  )
})

################################
# checkInRows_container
################################
output$checkInRows_container <- renderUI({  
  validate(need(is.list(values$table), message="No table found, or values$table needs to be a list"))
  
  tagList(
    fluidRow(  
      column(2, offset=10,
             actionButton(ns("saveAll"),label="Save all", style=actionButton.style)
      ) 
    ),

    # all curation table   
    lapply(1:length(names(values$table)), function(i) {
      
      validate(need(values$table[[i]], message="no table found"), 
               need(is.data.frame(values$table[[i]]), message="only data.frame allowed"),
               need(values$data, message="no data found")
      )

      values = callModule(module_checkInRows, paste0("module_save_table_", i),  
                          values0 = values,
                          table_index = i
      )   
      
      # UI
      module_checkInRows_UI(ns(paste0("module_save_table_", i)), label = NULL) 
    })
  
  ) # tagList
})
  
################################
# output_container
################################
output$output_container <- renderUI({  
  
  ALL = callModule(module_save_data, paste0("module_save_data_", 1), 
                   ALL,
                   data = ALL$DATA[[dataset_name]] ,   
                   data_name =  dataset_name    #names(values$data[i])
  )
  module_save_data_UI(ns(paste0("module_save_data_", 1)), label = NULL) 
})


################################
# save all curation tables  
################################ 
observeEvent(input$saveAll, {
  validate(need(input$saveAll, message=NULL), 
           need(values$table, message=NULL), 
           need(values$data, message=NULL)
  ) 
    
  tdata_org = ALL$DATA[[dataset_name]] 
  tdata_org = tdata_org %>% 
    rename_at(vars(colnames(tdata_org)),
              ~ paste0(colnames(tdata_org), "_ORG")
    )
  
  tdata = values$data %>% cbind(tdata_org)
  ntabl = length(values$table)
  
  # for all curation tables
  for (i in 1:ntabl) {
    table = values$table[[i]]
    if (is.null(table) ) {next} 
    if (nrow(table)==0) {next} 
    
    # incompatible types (character / logical)
    KEY = attr(table, "key")
    tdata[, KEY] = as.character(tdata[, KEY])
    table[, KEY]  = as.character(table[, KEY])
    
    col.lst =  setdiff(colnames(table), KEY)
    tdata = tdata %>% select(-one_of(intersect(col.lst, colnames(tdata)))) %>% 
      left_join(table, by=KEY)
  }
  
  ALL$DATA[[dataset_name]] <- tdata[, setdiff(colnames(tdata), colnames(tdata_org))]
  showNotification("save all sucessfully", type="message")   # "default, "message", "warning", "error"
  
})

################################
# run_script  
################################ 
observeEvent(input$run_script, {
  validate(need(input$script_content, message="no script loaded yet")
  ) 
    

  dataset = ALL$DATA[[dataset_name]]

  print("sf")
  print(head(dataset))
  # output= NULL
  # # source the function
  # message= tryCatch(eval(parse(text=(input$script_content))), 
  #           error=function(e) {
  #             print("error found in runing script"); 
  #             return(NULL)
  #           } #, finally = {
  #           # eval(parse(text=txt)) %>% as.data.frame()
  #           #}
  #  )  
  # if (!is.null(output$data)) {eval(parse(text="values$data = output$data"))}
  # if (!is.null(output$table)) {eval(parse(text="values$table = output$table"))}
  # 
  # if (is.null(message) ) {
  #   showNotification("error: no data/figure/table generating by runing script", type="error")
  #  }else{
  #   showNotification("run script sucessfully", type="message")   # "default, "message", "warning", "error"
  # }

ihandbook = 1
output= NULL
# setwd(tempdir())
# on.exit(setwd(owd)) 
owd <- tempdir()
on.exit(setwd(owd)) 

## capture messages and errors to a file.
zz <- file("all.Rout", open="wt")
sink(zz, type="message")

# source the function
try(
  eval(parse(text=(input$script_content))) 
)  

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
error_message <- readLines("all.Rout")

if (length(error_message)>0) {
  showNotification(paste0(error_message, collapse="\n"), type="error")
}else {
  if("data" %in% names(output)) {values$data = output$data}
  if("figure" %in% names(output))   {values$figure = output$figure}
  if("table" %in% names(output)) {values$table = output$table}
  
  print(names(output))
  showNotification("run script sucessfully", type="message")   # "default, "message", "warning", "error"
}

})

return(ALL)
}
