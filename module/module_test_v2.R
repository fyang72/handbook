


# https://github.com/jrowen/rhandsontable/issues/27

module_checkInRows_retired_UI <- function(id, label = "") {
  ns <- NS(id)
  
  
  fluidRow(  
    
    fluidRow(
      column(12, uiOutput(ns("table_title_container")))
    ), 
    
    fluidRow(
      column(width=12,  
             uiOutput(ns("rHandsontab_container"))
      )
    ), 
    
    fluidRow(
      column(width=6, #status = "primary",  #class = 'rightAlign',#background ="aqua",
             uiOutput(ns("table_name_container"))  
      ), 
      
      column(2,  
             actionButton(ns("saveit"),label="Save it", style=actionButton.style)
      ) 
      
    ), 
    style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;")))
    # fluidRow(column(width=12, tags$hr(
    #   style="display: block;
    #   height: 1px;
    #   border: 0;
    #   border-top: 1px solid #ccc;
    #   margin: 1em 0;
    #   padding: 0; "
    # )))
  )
} 
 

 
module_checkInRows_retired <- function(input, output, session, 
                               ALL, dataset_name,
                               table, table_index, table_name)  {
  
  ns <- session$ns
 
  val_ = NULL
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  
  
  output$table_title_container <- renderUI(renderText({
    paste0("Table ", table_index, " ", attributes(table)$title)
  })() %>% HTML()
  
  )
  
  #--------------------------------------  
  # table_name_container
  #-------------------------------------- 
  output$table_name_container <- renderUI({ 
    validate(need(globalVars$login$status, message=FALSE))  
    
    textInput(ns("table_name"), value=table_name, width="100%", label=NULL)
  })
  
  #--------------------------------------  
  # table_container # https://shiny.rstudio.com/articles/render-table.html
  # # https://github.com/jrowen/rhandsontable/issues/27
  #-------------------------------------- 
  output$rHandsontab_container <- renderUI({ 
    
    validate(need(table, message=FALSE), 
             need(nrow(table)>0, message=FALSE)
    )
    
    print("sf")
    print(table)
    
    #print(head(table))
    #print((!is.null(table) & !all(val_==table)))
    
    output$rHandsontab <- renderRHandsontable({
      validate(need(table, message="no table found"))
      
       if (is.null(input$rHandsontab) ||  !isTRUE(all_equal(val_, table))) {
         #if (is.null(input$rHandsontab) || (!is.null(table) & !all(val_==table))) {
          
        val_ <<- table 
        hotdf <- table 
      
      } else if (!is.null(input$rHandsontab)) {
        hotdf = hot_to_r(input$rHandsontab)
      }
      
      # myindex = which(adpx_checkin$which.column=="?")
      # mycolor = adpx_checkin %>% mutate(
      #   mycolor = ifelse(which.column=="?" & tier==1, 'red', 
      #                    ifelse(which.column=="?" & tier==2, 'yellow', 
      #                           ifelse(which.column=="?" & tier==3, 'lightblue', 'lightblue')
      #                    )
      #   )
      # ) %>% filter(which.column=="?") %>% pull(mycolor)
      # 
      # if (is.null(ALL$DATA[[dataset_name]] )) {
      #   myindex = 0
      #   mycolor="black"
      # }
      
      rhandsontable(hotdf, useTypes = TRUE, stretchH = "all",   rowHeaders=NULL) %>%
        hot_table(highlightCol = TRUE,   
                  allowRowEdit=FALSE )
      #   hot_cols(
      #     renderer = paste0(
      #       "
      #       function(instance, td, row, col, prop, value, cellProperties) {
      #       Handsontable.TextCell.renderer.apply(this, arguments);
      #       var row_index = ", paste("[", paste(myindex-1, collapse = ","), "]"), ";
      #       var col_lst = ", paste("[", paste(paste0("'", mycolor,"'"), collapse = ","), "]"), ";
      #       for (i = 0; i < row_index.length; i++)
      #       if (row == row_index[i]) {
      #       td.style.background = col_lst[i];
      #       }
      #       }
      #       "
      #     )
      #     )
      
    })
    rHandsontableOutput(ns("rHandsontab"))
 
   
  })
  
  
  
  #-------------------------------------------------------------
  # save table 
  #------------------------------------------------------------- 
  observeEvent(input$saveit, {
    
    validate(need(input$saveit, message=FALSE),  
             need(table, message=FALSE)
    )
    
    print("save to ALL$DATA sucessful")
    ALL$DATA[["dataset_name"]] <- table 
    
  })
  
} 
 



