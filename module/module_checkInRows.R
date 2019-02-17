


# https://github.com/jrowen/rhandsontable/issues/27

module_checkInRows_UI <- function(id, label = "") {
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
      column(12, uiOutput(ns("table_footnote_container")))
    ), 
    
    fluidRow(  
      column(2,  
             actionButton(ns("saveit"),label="Confirm", style=actionButton.style)
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



module_checkInRows <- function(input, output, session, 
                               ALL, dataset_name,
                               data, 
                               table, table_index, table_name)  {
  
  ns <- session$ns
  values <- reactiveValues(hotdf=NULL)
  val_ = NULL
  
  actionButton.style ="float:left;color: #fff; background-color: #328332; border-color: #328332"
  
  table[is.na(table)] = ""  # so every entries are editable.
  
  
  #--------------------------------------  
  # table_title_container
  #-------------------------------------- 
  output$table_title_container <- renderUI(renderText({
    paste0("Table ", table_index, " ", attributes(table)$title)
    })() %>% HTML()
  
  )
  
  output$table_footnote_container <- renderUI(renderText({
    paste("<font size='", 2,"'>", attributes(table)$footnote, "</size>",sep="")
  })() %>% HTML()
  
  )
  
  #--------------------------------------  
  # table_container # https://shiny.rstudio.com/articles/render-table.html
  # # https://github.com/jrowen/rhandsontable/issues/27
  #-------------------------------------- 
  output$rHandsontab_container <- renderUI({ 
    
    validate(need(table, message=FALSE), 
             need(nrow(table)>0, message=FALSE)
    )
       
    output$rHandsontab <- renderRHandsontable({
      validate(need(table, message="no table found"))
      
      if (is.null(input$rHandsontab) ||  !isTRUE(all_equal(val_, table))) {
        val_ <<- table 
        values$hotdf <- table 
        
      } else if (!is.null(input$rHandsontab)) {
        values$hotdf = hot_to_r(input$rHandsontab)
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
      
      
      col_index = which(colnames(table)==attr(table, "value"))
      
      myindex = which(table[, col_index] %in% c("NA", NA, "", "<NA>"))
      mycolor = rep('lightblue', times=length(myindex))
 
      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
      
      rhandsontable(values$hotdf, useTypes = TRUE, stretchH = "all",  rowHeaders=NULL)  %>%  # stretchH = "all",  
       # hot_table(overflow="hidden",
        #          highlightCol = TRUE,   
         #         allowRowEdit=FALSE ) #%>% 
       
       # hot_col(value.col.index, 
       #         renderer = "function(instance, td, row, col, prop, value, cellProperties) {
       #                     Handsontable.TextCell.renderer.apply(this, arguments);
       #                     td.style.background = 'lightblue';
       #                   }"
       # )  %>% 
        hot_cols(
          renderer = paste0(
           "function(instance, td, row, col, prop, value, cellProperties) {
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
    rHandsontableOutput(ns("rHandsontab"))
    
    
  })
  
  
  
  #-------------------------------------------------------------
  # save table 
  #------------------------------------------------------------- 
  observeEvent(input$saveit, {
    
    validate(need(input$saveit, message=FALSE),  
             need(table, message=FALSE), 
             need(values$hotdf, message=FALSE), 
             need(data, message=FALSE)
    )
    
    tdata = data
    tabl = values$hotdf
    
    # incompatible types (character / logical)
    KEY = attr(table, "key")
    tdata[, KEY] = as.character(tdata[, KEY])
    tabl[, KEY]  = as.character(tabl[, KEY])
 
    col.lst =  setdiff(colnames(tabl), KEY)
    tdata = tdata %>% select(-one_of(intersect(col.lst, colnames(tdata)))) %>% 
      left_join(tabl, by=attr(tabl, "key"))
     
    ALL$DATA[[dataset_name]] <- tdata 
    print("checkInRows sucessful")
    print(tdata %>% select(SEX, SEX_ORG, SEXN) %>% distinct(SEX_ORG, .keep_all=TRUE))
  })
  
  return(ALL)
} 




