
#---------------------------
# create event from scratch
#---------------------------
create_event <- function(GROUPID, TIME, AMT, UNIT, ROUTE, INFHR, II, NDOSE) {
  
  if (is.null(GROUPID)) {return(NULL)}
  
  tryCatch(  
    data.frame(GROUPID = eval(parse(text=paste0("c(", GROUPID, ")"))),    #input$TIME1, 
               time  = eval(parse(text=paste0("c(", TIME , ")"))),    #input$TIME1, 
               amt   = eval(parse(text=paste0("c(", AMT, ")"))), #input$AMT1, 
               unit  = eval(parse(text=paste0("c(", UNIT, ")"))), #input$UNIT1, 
               route = eval(parse(text=paste0("c(", ROUTE, ")"))), #input$ROUTE1, 
               infhr = eval(parse(text=paste0("c(", INFHR, ")"))), #input$INFHR1, 
               ii    = eval(parse(text=paste0("c(", II, ")"))), #input$II1, 
               addl  = eval(parse(text=paste0("c(", NDOSE, ")-1"))), #input$NDOSE1-1
               evid  = 1 ) ,
    
    error=function(e) {
      print("Error in constructing create_event..."); 
      return(NULL)
    }
  )
}
