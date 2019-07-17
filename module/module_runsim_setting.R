#######################################################################
# module_build_setting_UI
#######################################################################
#-----------------------------------------
# xxxUI, xxxInput, xxxOutput, xxxControl 
#-----------------------------------------

module_runsim_setting_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(  
    fluidRow(
      column(width=12, actionButton(ns("confirm"), label="Confirm", style=actionButton_style)
       )
    ),
    
    fluidRow(
      column(width=12, numericInput(ns("seed"), label = h5("Random seed:"),
                 value=1234, min=1, max=1000)
      )
    ),
    
    fluidRow(
    column(width=12, numericInput(ns("delta"), label = h5("Simulation step (delta, day)"),
            value=1, min=0.1, max=28)
          )
    ),
    
    fluidRow(
     column(width=12, numericInput(ns("followup_period"), label = h5("Follow-up duration after treatment period(day)"),
            value=112, min=1, max=224) 
            )
    ),
    
    fluidRow(
     column(width=12, textInput(ns("infusion_hrs_lst"), label = h5("Sampling time after treatment (hour)"),
            value= "0.5, 1, 2")
            )
    )
 )
  
}

########################################################################
# module_runsim_setting
########################################################################

module_runsim_setting <- function(input, output, session, ALL, values)  {
  
  ns <- session$ns 
   
  observeEvent({
    input$seed
    input$delta
    input$followup_period
    input$infusion_hrs_lst
    }, {
      
    validate(need(input$seed, message=FALSE), 
             need(input$delta, message=FALSE), 
             need(input$followup_period, message=FALSE), 
             need(input$infusion_hrs_lst, message=FALSE)
    )
    
    #  infusion hours if IV dose
    output =  tryCatch({eval(parse(text=paste0("c(",input$infusion_hrs_lst, ")")))}, 
                                 error=function(e) {
                                   return(NULL)   # default 
                                 } #, finally = {
                                 # eval(parse(text=txt)) %>% as.data.frame()
                                 #}
    ) 
     
    values$seed = input$seed
    values$delta = input$delta
    values$followup_period = input$followup_period
    values$infusion_hrs_lst = output
    
    if (is.null(output)) {
      showNotification("error in parsing infusion_hrs_lst", type="error")   # "default, "message", "warning", "error"}
    }       
    
  })
  
  return(values)
}