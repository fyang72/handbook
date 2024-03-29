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
             HTML(colFmt("Note, the following tabsets are used to 
                          0) display the loaded dataset [dataset],
                          1) align the variable (column) against the standard one [checkInCols],
                          2) standardize the input dataset using script [script], 
                          3) semi-auto curation and confirmation [curation],
                          4) render the resulting data and save [output].  <br> ", color="gray")))
      ),
      
    tabBox(width=12, id = ns("run_script_for_dataset_construction"), title =NULL, 
       
        # dataset_container 
        tabPanel(width=12, title="dataset", value = "dataset", collapsible = TRUE, 
                 collapsed = TRUE, solidHeader = TRUE,
                 tagList(
                    fluidRow(column(12, uiOutput(ns("load_dataset_container")))), 
                    fluidRow(column(12, uiOutput(ns("render_dataset_container"))))
                 )
        ),
       
       # checkInCols_container 
       tabPanel(width=12, title="alignCols", value = "checkIn_columns", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("checkInCols_container")))) 
       ),        
       
       # runScript_container 
       tabPanel(width=12, title="runScript", value = "run_script", collapsible = TRUE, 
                collapsed = TRUE, solidHeader = TRUE,
                  fluidRow(column(12, uiOutput(ns("runScript_container"))))  
       ),     
           
       # checkInRows_container
       tabPanel(width=12, title="curation", value = "checkIn_rows", collapsible = TRUE, 
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
                              ALL, 
                              dataset_name="", 
                              script="", 
                              params=NULL, 
                              default_checkin=NULL 
                              )  {

ns <- session$ns
values <- reactiveValues(data=NULL,figure=NULL,table = NULL)

# script is a list of file names containing the "script"
script <- sapply(script, function(i) paste0(readLines(i), collapse="\n")) %>% unlist()

 
################################
# UI for dataset_container
################################
output$load_dataset_container <- renderUI({ 
   
  #validate(need(ALL$DATA[[dataset_name]], message="no data found yet"))
  #  
  # #0312_2019, NEED to return the data to ALL.  
  # ALL = callModule(module_filtered_dataset, "dataset_to_be_built", 
  #                  ALL,  
  #                  dataset_name=dataset_name 
  #                  )
  #  
  # module_filtered_dataset_UI(ns("dataset_to_be_built"), label = NULL) 
  
  
  
  # callModule 
  ALL = callModule(module_load_dataset, "load_dataset_for_run_script", 
                   ALL, dataset_name=dataset_name)
  
  # UI  
  fluidRow(column(6, 
                  module_load_dataset_UI(id=ns("load_dataset_for_run_script"), label=NULL) 
  ), 
  column(6, 
         HTML(colFmt("internal library: build-in dataset <br>
                        within session: secondary data derived from the original <br> 
                        external file: external file", color="gray")
         )
  )
  )
})



output$render_dataset_container <- renderUI({  
  
  tdata = ALL$DATA[[dataset_name]]
  validate(need(tdata, message="no data found yet")  
  )
  
  callModule(module_save_data, "loaded_dataset", ALL, 
             data=tdata, 
             data_name="")
  
  fluidRow(
    column(12, module_save_data_UI(ns("loaded_dataset"), label = "loaded_dataset"))
  )
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
      column(9,
             selectizeInput(ns("script_selector"),
                            label    = NULL, #"select script:" ,
                            choices  = names(script),
                            multiple = FALSE,
                            selected = names(script)[1]
             )
      ),
      
      column(3, 
             actionButton(ns("run_script"), label="Run script", style=actionButton_style )
      )
    ),
    
    fluidRow(column(12, uiOutput(ns("script_content_container"))))
    
    # fluidRow(
    #   column(12,
    #          aceEditor(ns("script_content"), 
    #                    mode="r", 
    #                    value=script, 
    #                    theme = "crimson_editor",   # chrome
    #                    autoComplete = "enabled",
    #                    height = "1000px", 
    #                    fontSize = 15 
    #          )
    #   )
    # )
  )
})



output$script_content_container <- renderUI({
  validate(need(globalVars$login$status, message=FALSE))
  
  aceEditor(ns("script_content"), 
            mode="r", 
            value=if (is.null(names(script))) {script} else {script[input$script_selector]}, 
            theme = "crimson_editor",   # chrome
            autoComplete = "enabled",
            height = "1000px", 
            fontSize = 15 
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
             actionButton(ns("saveAll"),label="Save all", style=actionButton_style)
      ) 
    ),

    # all curation table   
    lapply(1:length(names(values$table)), function(i) {
      
      validate(need(values$table[[i]], message="no table found"), 
               need(is.data.frame(values$table[[i]]), message="only data.frame allowed")  #,
               #need(values$data, message="no data found")
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
     
  tdata = values$data[[1]]  
  validate(need(length(values$data)==1, 
                message="length(values$data) must be 1 in build_dataset"))
  ntabl = length(values$table)
  
  # for all curation tables
  for (i in 1:ntabl) {
    table = values$table[[i]]
    KEY = attr(table, "key")
    if (is.null(table) ) {next} 
    if (nrow(table)==0) {next} 
    if (is.null(KEY)) {next}  
    
    tdata[, KEY] = as.character(tdata[, KEY]) # incompatible types (character / logical)
    table[, KEY]  = as.character(table[, KEY])# incompatible types (character / logical)
 
    col.lst =  setdiff(colnames(table), KEY)
    tdata = tdata %>% select(-one_of(intersect(col.lst, colnames(tdata)))) %>% 
      left_join(table, by=KEY)
  }
     
  ALL$DATA[[dataset_name]] <- tdata %>% select(-ends_with("_ORG"))
  showNotification("save all sucessfully", type="message")   # "default, "message", "warning", "error"
  
})

################################
# run_script  
################################ 
observeEvent(input$run_script, {
  validate(need(input$script_content, message="no script loaded yet")
  ) 
    

dataset = ALL$DATA[[dataset_name]]

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
  
  showNotification("run script sucessfully", type="message")   # "default, "message", "warning", "error"
}

})

return(ALL)
}
