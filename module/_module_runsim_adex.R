 
observeEvent(input$adex_source, {
  validate(need(globalVars$login$status, message=FALSE), 
           need(input$adex_source=="manual input", message=FALSE)) 
  
  ID = 1:10
  Dosing.Regimen = rep("", times=10)
  Dosing.Regimen[1] = "3 mg/kg IV Q2W*12"
  Dosing.Regimen[2] = "350 mg IV Q3W*8"
  
  tdata = data.frame(ID, Dosing.Regimen)
  tdata = tdata %>% mutate(ID=factor(ID), 
                           Dosing.Regimen = as.character(Dosing.Regimen)
  )
  
  values[["adex_checkin"]] <- tdata   # initial alignment
})

# if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
observeEvent(input$adex_rHandsontab, {
  values[["adex_checkin"]] = hot_to_r(input$adex_rHandsontab)  # manual alignment
})




#-----------------------------------------  
# display the adpx_rHandsontab, alignTab
#-----------------------------------------
output$rHandsontab_container <- renderUI({ 
  #if (mod(input$runScript,2)) {return(NULL)}
  if (input$adexLoadingBy != "auto") {return(NULL)}
  
  # use manual curation
  output$adex_rHandsontab <- renderRHandsontable({
    
    adex_checkin =  values[["adex_checkin"]]  # 
    if (is.null(adex_checkin)) {return(NULL)}
    
    rhandsontable(adex_checkin, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL)
    
  })
  rHandsontableOutput(ns("adex_rHandsontab"))
  
})



adexByAuto <- reactive({ 
  adsl = adsl()
  if (is.null(adsl)) {return(NULL)}
  
  adex = values[["adex_checkin"]] 
  if (is.null(adex)) {return(NULL)}
  
  adex = adex %>% filter(Dosing.Regimen!="" & !is.na(Dosing.Regimen))
  adex = parseARMA(adex$Dosing.Regimen) 
  
  adex = adex %>% event_add_adsl(adsl) %>%  process_event()     
  if (!is.null(adex$F1)) {adex$TVF1 = adex$F1}  #?????
  
  return(adex)
})



#------------------ 
# final adex
#------------------ 
adex <- reactive({ 
  if (is.null(input$adexLoadingBy)) {return(NULL)}
  
  adex <-switch(input$adexLoadingBy, 
                "auto" = adexByAuto(),  
                "script"= adexByScript(), 
                "file" =  adexByFile()
  ) 
  
  if (is.null(adex)) {return(NULL)}
  
  
  #https://shiny.rstudio.com/articles/validation.html
  std.col.name.lst1 <-c("ID", "time",  "amt",  "unit",  "route", "infhr", "ii",  "addl",  "evid",   "cmt",  "rate")     
  std.col.name.lst2 <-c("STUDYID",   "ARMA",   "GROUPID", "USUBJID",  "AMT",  "UNIT",    "ROUTE",   "NDOSE",   "FREQ",  "WGTBL")     
  std.col.name.lst = c(std.col.name.lst1, std.col.name.lst2)  
  
  validate(
    need(all(std.col.name.lst %in% colnames(adex)), 
         paste0("Missing columns of ", paste0(setdiff(std.col.name.lst, colnames(adex)), collapse=", "), " in ", "adex")
    )
  )
  
  adex
  
})


#------------------ 
# adexTab
#------------------ 
output$adexTab_container <- renderUI({
  
  output$adexTab <- renderRHandsontable({
    adex = adex()
    if (is.null(adex)) {return(NULL)}
    
    adex = adex %>% mutate(
      TIME = time, 
      GROUPID = as.integer(GROUPID),
      CMT  = as.integer(cmt),
      RATE = rate, 
      EVID = as.integer(evid), 
      NDOSE= as.integer(addl+1), 
      cmt  = as.integer(cmt), 
      ii   = as.integer(ii), 
      addl = as.integer(addl), 
      evid = as.integer(evid)
    )
    adex = adex %>% select(STUDYID,ARMA,USUBJID,ID,GROUPID,TIME,AMT,UNIT,ROUTE,FREQ,NDOSE,CMT,RATE,WGTBL)
    # time,cmt,rate,infhr,ii,addl,evid)
    adex = head(adex, n=100)  # only the first 100 rows show
    
    rhandsontable(adex,  useTypes = TRUE, stretchH = "all", readOnly=TRUE) %>%  # width = 600, height = 300, 
      hot_col(col=colnames(adex), halign="htCenter")     #htLeft, htCenter, htRight and htJustify  
    # "factor_allow", allowInvalid = TRUE)
    
  })
  
  rHandsontableOutput(ns("adexTab"))
})


# ## use renderUI to display table
#https://rstudio.github.io/DT/
output$adexTab_container2 <- renderUI({
  adex = adex()
  if (is.null(adex)) {return(NULL)}
  
  adex = adex %>% mutate(
    TIME = time, 
    GROUPID = as.integer(GROUPID),
    CMT  = as.integer(cmt),
    RATE = rate, 
    EVID = as.integer(evid), 
    NDOSE= as.integer(addl+1), 
    cmt  = as.integer(cmt), 
    ii   = as.integer(ii), 
    addl = as.integer(addl), 
    evid = as.integer(evid)
  )
  adex = adex %>% select(STUDYID,ARMA,USUBJID,ID,GROUPID,TIME,AMT,UNIT,ROUTE,FREQ,NDOSE,CMT,RATE,WGTBL)
  # time,cmt,rate,infhr,ii,addl,evid)
  rhandsontable(adex,  useTypes = TRUE, stretchH = "all") #%>%  # width = 600, height = 300, 
  # hot_col("factor_allow", allowInvalid = TRUE)
  
  output$adexTab <- DT::renderDataTable(
    DT::datatable(data = adex,
                  options = list(pageLength = 10, lengthChange = FALSE, width="100%", scrollX = TRUE) 
                  #filter = 'top',    # bottom',
                  #caption = htmltools::tags$caption(
                  # style = 'caption-side: bottom; text-align: center;',
                  # 'Table 2: ', htmltools::em('This is a simple caption for the table.')
                  #  )
    ) 
  )
  #rownames = FALSE, 
  # width="100%",
  #autoWidth = FALSE
  # aLengthMenu = c(5, 30, 50), iDisplayLength = 6, bSortClasses = TRUE,
  # bAutoWidth = FALSE,
  # aoColumn = list(list(sWidth = "150px", sWidth = "30px",
  #                      sWidth = "30px", sWidth = "30px"))
  #columnDefs = list(list(width = '200px', targets = "_all"))
  
  DT::dataTableOutput(ns("adexTab"))
  
})

