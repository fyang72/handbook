

push_to_values4nmdat <- function(out, values4nmdat, category)  {
  validate(need(out, message=FALSE), 
           need(values4nmdat, message=FALSE), 
           need(category, message=FALSE)
  )
  
  if(out$status=="orange") {values4nmdat$yellow_alert = rbind(isolate(values4nmdat$yellow_alert), out$info)}
  if(out$status=="red") {values4nmdat$red_alert = rbind(isolate(values4nmdat$red_alert), out$info)}
  
  #values4nmdat$mandatory_check = values4nmdat$mandatory_check & out$status %in% c("green", "yellow")
  values4nmdat$pass_quick_check = values4nmdat$pass_quick_check & out$status %in% c("green", "orange") 
  
  values4nmdat[[category]]$status = out$status
  values4nmdat[[category]]$info = out$info 
  values4nmdat[[category]]$table = out$table
  values4nmdat[[category]]$figure = out$figure 
  
  return(values4nmdat)
}



#######################################################################
# module_check_nmdat_status_UI
#######################################################################
module_check_nmdat_status_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  uiOutput(ns("status_container"))
}        
       
module_check_nmdat_status <- function(input, output, session, values4nmdat, category) {
  
  ns <- session$ns 
  
  status = values4nmdat[[category]]$status  
  info =  values4nmdat[[category]]$info  
  table =  values4nmdat[[category]]$table  
  figure =  values4nmdat[[category]]$figure  
  
  output$status_container <- renderUI(renderText({
    validate(need(status %in% c("green", "red", "orange", "undetermined"), message=FALSE))
    
  switch(status, 
         'green' = colFmt(asis_output("Yes\U2714"),'green'),  
         'red' = colFmt(asis_output("No\U2718"),'red'),
         'orange' = colFmt(asis_output("Warning\U2718"),'orange'), 
         'undetermined' = colFmt(asis_output("Undetermined\U2718"),'lightblue') 
         )
  })() %>% HTML())
  
}






#######################################################################
# module_check_nmdat_info_UI
#######################################################################
module_check_nmdat_info_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  uiOutput(ns("info_container"))
}        

module_check_nmdat_info <- function(input, output, session, values4nmdat, category) {
  
  ns <- session$ns

  status = values4nmdat[[category]]$status  
  info =  values4nmdat[[category]]$info  
  table =  values4nmdat[[category]]$table  
  figure =  values4nmdat[[category]]$figure 
  
  #info_container
  output$info_container <- renderUI(renderText({
    validate(need(info, message=FALSE))
     
    switch(status, 
           'green' = colFmt(info,'green'),  
           'red' = colFmt(info,'red'),
           'orange' = colFmt(info,'orange'), 
           'undetermined' = colFmt(info,'lightblue')
    )
  })() %>% HTML())
 }
  
   
  
  