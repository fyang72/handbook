
################################################################################ 
################################################################################
# checkInColsUI
################################################################################ 
################################################################################

#-----------------------------------------
#  xxxInput, xxxOutput, xxxControl, xxxUI
#-----------------------------------------

module_checkInCols_UI <- function(id, label = "") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=4,
             actionButton(ns("checkIn"),label = "checkIn", style=actionButton.style)   # http://getbootstrap.com/docs/4.0/components/buttons/
      )
    ), 
    fluidRow(
      column(width=12, uiOutput(ns("rHandsontab_container")))
    )
  )
  
}
 

################################################################################ 
################################################################################
# checkInCols
################################################################################
################################################################################

module_checkInCols  <- function(input, output, session, ALL, dataset, default_checkin) {
  
  ns <- session$ns
  values <- reactiveValues()
  
  
  
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  # renderUI
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------

  
  
  #-----------------------------------------  
  # display the adpx_rHandsontab, alignTab
  #  https://github.com/jrowen/rhandsontable/issues/116
  # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
  #-----------------------------------------
  output$rHandsontab_container <- renderUI({ 
    
    # use manual curation
    output$adpx_rHandsontab <- renderRHandsontable({
      
      adpx_checkin =  values[["adpx_checkin"]] 
      validate(need(adpx_checkin, message=FALSE)
      )
      
      #  https://github.com/jrowen/rhandsontable/issues/116
      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
      
      # define the background color for selected columns, here columns 1 and 3.
      #   library("rhandsontable")
      #   rhandsontable(data.frame(ID=1:5,var1=rnorm(5), var2=letters[1:5])) %>%    
      #     hot_col(c(1,3), 
      #             renderer = "function(instance, td, row, col, prop, value, cellProperties) {
      #   Handsontable.TextCell.renderer.apply(this, arguments);
      #   td.style.background = 'lightblue';
      # }"
      #     )
      
      
      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all", rowHeaders=NULL)
      
      
    })
    rHandsontableOutput(ns("adpx_rHandsontab"))
    
  })
  
  

  
  
  

  #------------------------- 
  #  inputData
  #------------------------- 
  inputData <- reactive({
    
    # show adpx at the first place 
    data =   (ALL$DATA[["mYtEsT_for_standardize_nmdat"]])  #  ALL$DATA[[dataset_name]]
    validate(need(data, message="no data loaded yet"))
     
    #values[["adpx"]] = data  # default
 
    data
  })
  
  #-------------------------------------------------------
  # in default, use data_name4Select as data_name4Save
  #------------------------------------------------------- 
   observeEvent(inputData(), {
     validate(need(input$data_name4Select, message=FALSE))
     
     updateTextInput(session, "data_name4Save", value=input$data_name4Select)
   })
  
   
  #-----------------------------------------
  #  adpx_checkinTab and display
  #-----------------------------------------
  
  # update adpx_checkin based on new InputData
  adpx_checkinTab <- reactive({
    adpx_checkin = default_checkin    # default_adpx_checkinTab()
    if (is.null(adpx_checkin)) {return(NULL)}
    
    # if new adpx loaded
    adpx = inputData()
    if (is.null(adpx)) {return(adpx_checkin)}
     
    print("in initial adpx_checkinTab after load new inputData-1")
    adpx_checkin$which.column = colnames(adpx)[match(adpx_checkin$standard.name, colnames(adpx))]
    
    adpx_checkin = adpx_checkin %>% 
      mutate(which.column=as.character(which.column)) %>%
      mutate(which.column=ifelse(standard.name=="----", "----",  
                                 ifelse(is.na(which.column), "?", which.column))) %>% 
      mutate(which.column=ordered(which.column, levels=c("?", colnames(adpx),"----")))   
    
    print("in initial adpx_checkinTab after load new inputData-2")
    adpx_checkin
  })
 
  
  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(adpx_checkinTab(), {
    print("changed in adpx_checkinTab  " )
    values[["adpx_checkin"]] <- adpx_checkinTab()  # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adpx_rHandsontab, {
    print("changed in adpx_rHandsontab" )
    values[["adpx_checkin"]] = hot_to_r(input$adpx_rHandsontab)  # manual alignment
  })
   
   
 
  #------------------------------------------------- 
  # check in adpx based on adpx_checkin or script
  #------------------------------------------------- 
  observeEvent(input$checkIn, {
    
    adpx <- values[["adpx"]]  # inputData()
    
    # update adpx
    if (mod(input$runScript,2)) { 
      adpx = colByScript()   #if using script
    }else { 
      adpx = colByManual()   #if using manually
    }
    
    key.lst = c("STUDYID","USUBJID","ARMA","TEST","NTIM","TIME","DVOR")
    key.lst = intersect(key.lst, colnames(adpx))
    adpx = adpx[, c(key.lst, setdiff(colnames(adpx), key.lst))]
    
    values[["adpx"]] <- adpx  
    
  })
   
   
  
  #------------------------------------------------- 
  # add data to DATA when action button is pressed
  #-------------------------------------------------
  observeEvent(input$saveData, {
    validate(need(input$data_name4Select, message=FALSE),
             need(input$data_name4Save, message=FALSE), 
             need(values[["adpx"]], message=FALSE)
             #need(is.null(errstatus()), message="Fix the warning before proceed") 
             #need(!"error" %in% (errstatus() %>% pull(type)), message="Fix the error before proceed") 
    )
  

  })
  
  # Return DATA
  return(ALL)
  
}
