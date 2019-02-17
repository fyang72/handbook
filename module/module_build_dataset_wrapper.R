

#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_build_dataset_wrapper_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
   
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
################################################################################
# module_build_dataset
################################################################################
################################################################################
# If a module needs to use a reactive expression, take the reactive expression as a 
# function parameter. If a module wants to return reactive expressions to the calling app, 
# then return a list of reactive expressions from the function.
#
#If a module needs to access an input that isn't part of the module, the containing app 
#should pass the input value wrapped in a reactive expression (i.e. reactive(...)):

module_build_dataset_wrapper <- function(input, output, session, 
                                 ALL, dataset_name, script, default_checkin
)  {
  
  ns <- session$ns
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  #-----------------------------------
# standardize_nmdat_data_selector
  #-----------------------------------
output$standardize_nmdat_data_selector <-renderUI({
  
  # callModule 
  #isolate({ 
    ALL = callModule(module_load_dataset, "load_nmdat_for_standardize_nmat", 
                     ALL, dataset_name=dataset_name)
  #})
  
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
  
  #isolate({ 
  callModule(module_build_dataset, "standardize_nmdat",  
             ALL, 
             dataset_name=dataset_name,  #"mYtEsT_for_standardize_nmat", 
             script = script,  #"", 
             default_checkin =  default_checkin # default_nmdat_checkin()
  )
  #})
  
  fluidRow(
    column(12, 
           module_build_dataset_UI(ns("standardize_nmdat"), label = "standardize_nmdat")
    )
  )
}) 

  return(ALL)
}
 