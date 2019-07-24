################################################################################
# module_build_dataset_wrapper_UI
################################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_build_dataset_wrapper_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
   
  fluidRow(
    column(12, 
           uiOutput(ns("standardize_nmdat_data_selector"))
           ), 
    column(12, 
           uiOutput(ns("standardize_nmdat_container"))
    )
  )
}
 
################################################################################
# module_build_dataset_wrapper
################################################################################ 

module_build_dataset_wrapper <- function(input, output, session, 
                                 ALL, dataset_name, script, default_checkin)  {
  
ns <- session$ns
   
#-----------------------------------
# standardize_nmdat_data_selector
#-----------------------------------
output$standardize_nmdat_data_selector <-renderUI({
  
  # callModule 
  ALL = callModule(module_load_dataset, "load_nmdat_for_standardize_nmat", 
                     ALL, dataset_name=dataset_name)

  # UI  
  fluidRow(column(6, 
                  module_load_dataset_UI(ns("load_nmdat_for_standardize_nmat"), label=NULL)
          ), 
           column(6, 
                   HTML(colFmt("internal library: build-in dataset <br>
                               within session: saved data within session <br> 
                               external file: external file", color="gray")
                        )
           )
   )  
})
 
#----------------------------------------------------
# standardize_nmdat_container    
#----------------------------------------------------
 
# build_nmdat_container
output$standardize_nmdat_container <- renderUI({ 
  validate(need(globalVars$login$status, message=FALSE))
 
  callModule(module_build_dataset, "standardize_nmdat",  
             ALL, 
             dataset_name=dataset_name,   
             script = script,   
             default_checkin =  default_checkin  
  ) 
  
  fluidRow(
    column(12, 
           module_build_dataset_UI(ns("standardize_nmdat"), label = NULL)
    )
  )
}) 

return(ALL)
}
 