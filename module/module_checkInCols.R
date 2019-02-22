
################################################################################ 
################################################################################
# module_checkInCols_UI
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

module_checkInCols <- function(input, output, session, ALL, dataset_name, default_checkin) {
  
  ns <- session$ns
  values <- reactiveValues()
  
  #-----------------------------------------  
  # display the adpx_rHandsontab, alignTab
  #-----------------------------------------
  output$rHandsontab_container <- renderUI({ 
 
    # use manual curation
    output$adpx_rHandsontab <- renderRHandsontable({
      
      adpx_checkin =  values[["adpx_checkin"]]  # 
      validate(need(adpx_checkin, message=FALSE)
      )
       
      myindex = which(adpx_checkin$which.column=="?")
      mycolor = adpx_checkin %>% mutate(
        mycolor = ifelse(which.column=="?" & tier==1, 'red', 
                         ifelse(which.column=="?" & tier==2, 'yellow', 
                                ifelse(which.column=="?" & tier==3, 'lightblue', 'lightblue')
                         )
        )
      ) %>% filter(which.column=="?") %>% pull(mycolor)
      
      if (is.null(ALL$DATA[[dataset_name]] )) {
        myindex = 0
        mycolor="black"
       }
      
      # Handsontable.renderers.TextRenderer.apply, new version, 02-15-2019
      # Handsontable.TextCell.renderer.apply  old
      # https://jrowen.github.io/rhandsontable/
      # https://jrowen.github.io/rhandsontable/#custom_renderer
      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
      rhandsontable(adpx_checkin, useTypes = TRUE, stretchH = "all", rowHeaders=NULL) %>%
         hot_cols(
           renderer = paste0(
             "
         function(instance, td, row, col, prop, value, cellProperties) {
         Handsontable.renderers.TextRenderer.apply(this, arguments);
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
  
     
  #------------------------- 
  #  inputData
  #------------------------- 
  inputData <- reactive({
    validate(need(ALL$DATA, message=FALSE), 
             need(dataset_name, message=FALSE)
    )
    ALL$DATA[[dataset_name]]  
  })
  
  
  # update adpx_checkin based on new InputData
  adpx_checkinTab <- reactive({
    adpx_checkin = default_checkin #default_adpx_checkinTab()
    adpx = inputData()
      
    validate(need(adpx_checkin, message=FALSE), 
             need(adpx, message=FALSE)
    )
     
    adpx_checkin$which.column = colnames(adpx)[match(adpx_checkin$standard.name, colnames(adpx))]
    
    adpx_checkin = adpx_checkin %>% 
      mutate(which.column=as.character(which.column)) %>%
      filter(tier !="----") %>% 
      mutate(which.column=ifelse(standard.name=="----", "----",  
                                 ifelse(is.na(which.column), "?", which.column))) %>% 
      mutate(which.column=ordered(which.column, levels=c("?", colnames(adpx))))  
     
    adpx_checkin  
 
  })

  # if new InputData, then update values[["adpx_checkin"]] as initial table
  observeEvent(adpx_checkinTab(), {
    values[["adpx_checkin"]] <- adpx_checkinTab()  # initial alignment
  })
  
  # if user update adpx_rHandsontab, then update values[["adpx_checkin"]]
  observeEvent(input$adpx_rHandsontab, {
    values[["adpx_checkin"]] = hot_to_r(input$adpx_rHandsontab)  # manual alignment
  })
   
  
  #------------------------------------------------- 
  # add data to DATA when action button is pressed
  #------------------------------------------------- 
  observeEvent(input$checkIn, {
    
    adpx_checkin <- values[["adpx_checkin"]]
    adpx <-  inputData()  # dataset
    validate(need(adpx_checkin, message=FALSE), 
             need(adpx, message=FALSE)
    )
    
    #align columns based on adpx_checkin
    tt = adpx_checkin %>% filter(!is.na(which.column), !which.column %in% c("NA","", "----", "?"))
    if (nrow(tt)>0) { adpx[, tt$standard.name] = adpx[, as.character(tt$which.column)]}

    ALL$DATA[[dataset_name]] <- adpx  
    
    # "default, "message", "warning", "error"
    showNotification("check in columns sucessfully", type="message")   
  }) 
  
  return(ALL)
}
