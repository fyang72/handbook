
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

module_checkInCols <- function(input, output, session, ALL, dataset_name, default_checkin=NULL) {
  
  ns <- session$ns
  values <- reactiveValues()
  

  #-----------------------------------------  
  # display the adpx_rHandsontab, alignTab
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
      
      myindex = which(adpx_checkin$which.column=="?")
      mycolor = adpx_checkin %>% mutate(
        mycolor = ifelse(which.column=="?" & tier==1, 'red', 
                       ifelse(which.column=="?" & tier==2, 'yellow', 
                              ifelse(which.column=="?" & tier==3, 'lightblue', 'lightblue')
                              )
                       )
      ) %>% filter(which.column=="?") %>% pull(mycolor)

      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL) %>%
        hot_cols(
          renderer = paste0(
            "
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.TextCell.renderer.apply(this, arguments);
                var row_index = ", paste("[", paste(myindex-1, collapse = ","), "]"), ";
                var col_lst = ", paste("[", paste(paste0("'", mycolor,"'"), collapse = ","), "]"), ";
                for (i = 0; i < row_index.length; i++)
                if (row == row_index[i]) {
                  td.style.background = col_lst[i];
                }
              }
            "
          )
        )
      
    })
    rHandsontableOutput(ns("adpx_rHandsontab"))
    
  })
  
  
  #-----------------------------------------
  ## after align, display the aligned data   
  #-----------------------------------------  
  output$adpx_container <- renderUI({
    output$adpxTab <- DT::renderDataTable(
      DT::datatable(data = values[["adpx"]],
                    options = list(pageLength = 6, lengthChange = FALSE, width="100%", scrollX = TRUE)
      )
    )
    DT::dataTableOutput(ns("adpxTab"))
    
  })
   
   
  #------------------------- 
  #  inputData
  #------------------------- 
 inputData <- reactive({
   validate(need(ALL$DATA[[dataset_name]], message=FALSE))
 
   ALL$DATA[[dataset_name]]
 })
  
 
  
  # update adpx_checkin based on new InputData
  adpx_checkinTab <- reactive({
    adpx_checkin = default_checkin 
    if (is.null(adpx_checkin)) {return(NULL)}
    
    # if new adpx loaded
    #isolate({ 
    adpx = inputData()  # ALL$DATA[[dataset_name]]   #inputData()  # ALL$DATA[[data_name]] # ="data_name"  # dataset
   # })
    if (is.null(adpx)) {return(adpx_checkin)}
    
    #colnames(adpx) = toupper(colnames(adpx))  #leave it alone
    # based on adpx, update adpx_checkin, note VARS use globalVars$magicTab as background
    #adpx_checkin$which.column = VARS(adpx, vars.lst=adpx_checkin$standard.name, Where="indivProfile", Who="feng.yang") 
 
    adpx_checkin$which.column = colnames(adpx)[match(adpx_checkin$standard.name, colnames(adpx))]
     
    adpx_checkin = adpx_checkin %>% 
      mutate(which.column=as.character(which.column)) %>%
      mutate(which.column=ifelse(standard.name=="----", "----",  
                           ifelse(is.na(which.column), "?", which.column))) %>% 
      mutate(which.column=ordered(which.column, levels=c("?", colnames(adpx),"----")))  
    
    print("colnames(adpx)888")
    #print(colnames(adpx))
    #print(adpx_checkin$which.column)
    
    adpx_checkin  
 
  })
 
  
  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(adpx_checkinTab(), {
    print("colnames(adpx)888999")
    
    values[["adpx_checkin"]] <- adpx_checkinTab()  # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adpx_rHandsontab, {
    values[["adpx_checkin"]] = hot_to_r(input$adpx_rHandsontab)  # manual alignment
  })
   
   
   
 
  #------------------------------------------------- 
  # check in adpx based on adpx_checkin or script
  #------------------------------------------------- 
  observeEvent(input$checkIn, {
     
    adpx_checkin <- values[["adpx_checkin"]]
    adpx <- values[["adpx"]]  # dataset
    validate(need(adpx_checkin, message=FALSE), 
             need(adpx, message=FALSE)
    )
     
    #align columns based on adpx_checkin
    tt = adpx_checkin %>% filter(!is.na(which.column), !which.column %in% c("NA","", "----", "?"))
    if (nrow(tt)>0) { adpx[, tt$standard.name] = adpx[, as.character(tt$which.column)]}
    
    # key.lst = c("STUDYID","USUBJID","ARMA","TEST","NTIM","TIME","DVOR")
    # key.lst = intersect(key.lst, colnames(adpx))
    # adpx = adpx[, c(key.lst, setdiff(colnames(adpx), key.lst))]
    
    values[["adpx"]] <- adpx  
    
  })
  
  #  
   
  
  #------------------------------------------------- 
  # add data to DATA when action button is pressed
  #-------------------------------------------------
  observeEvent(input$saveData, {
    validate(need(input$data_name4Save, message=FALSE), 
             need(values[["adpx"]], message=FALSE) 
    )
  
    ALL$DATA[[input$data_name4Save]] =  values[["adpx"]] 

  })
  
  # Return DATA
  return(ALL)
  
}
