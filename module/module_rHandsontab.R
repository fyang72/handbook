################################################################################                                                                                                        
# adapted from module_rHandsontab_UI, 2019-09-17                                                                                                                                          
################################################################################                                                                                                        
################################################################################                                                                                                        
# module_rHandsontab_UI                                                                                                                                          
################################################################################                                                                                                        

module_rHandsontab_UI <- function(id, label = "") {
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
             actionButton(ns("confirm"),label="Confirm", style=actionButton_style)
      ) 
    ),  
    
    style='margin-bottom:30px;  border:1px solid; padding: 10px;'
    #fluidRow(column(width=12, tags$hr(style="border-color: gray;")))
  )
} 


################################################################################                                                                                                        
# module_rHandsontab                                                                                                                                          
################################################################################                                                                                                        

module_rHandsontab <- function(input, output, session, 
                               values0, data_index )  {
  
  ns <- session$ns
  values <- reactiveValues(hotdf=NULL)
  val_ = NULL
  
  # reactive inputTab()
  inputTab <- reactive({
    validate(need(values0$data, FALSE), 
             need(data_index, FALSE)
    )
    
    table = values0$data[[data_index]]
    table[is.na(table)] = ""  # so every entries are editable.
    table 
  })
  
  # table_title_container
  output$table_title_container <- renderUI(renderText({
    table = inputTab()
    paste0("<b>Table ", data_index, ": ", attributes(table)$title, "</b>")
  })() %>% HTML()
  )
  
  # table_footnote_container
  output$table_footnote_container <- renderUI(renderText({
    table = inputTab()
    paste("<font size='", 2,"'>", attributes(table)$footnote, "</size>",sep="")
  })() %>% HTML()
  )
  
  # rHandsontab_container 
  # https://shiny.rstudio.com/articles/render-table.html
  # https://github.com/jrowen/rhandsontable/issues/27
  output$rHandsontab_container <- renderUI({ 
    table = inputTab()
    
    validate(need(table, message=FALSE), 
             need(nrow(table)>0, message=FALSE)
    )
    
    output$rHandsontab <- renderRHandsontable({
      validate(need(table, message="no table found"))
      
      if (is.null(input$rHandsontab) ||  !isTRUE(all_equal(val_, table))) {
        val_ <<- table 
        values$hotdf <- table 
      } else if (!is.null(input$rHandsontab)) {
        newtable = hot_to_r(input$rHandsontab)
        values$hotdf = newtable 
      }
      
      col_index = which(colnames(table)==attr(table, "value"))
      myindex = which(table[, col_index] %in% c("NA", NA, "", "<NA>"))
      mycolor = rep('lightblue', times=length(myindex))
      
      # https://stackoverflow.com/questions/39752455/changing-background-color-of-several-rows-in-rhandsontable
      rhandsontable(values$hotdf, useTypes = TRUE, stretchH = "all", rowHeaders=NULL)  %>%  # stretchH = "all",  
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
  
  # observeEvent input$confirm
  observeEvent(input$confirm, {
    validate(need(values$hotdf, message=FALSE), 
             need(data_index, message=FALSE)
    )
    
    table = values0$data[[data_index]]
    newtable = values$hotdf
    
    mostattributes(newtable) <- attributes(table)
    values0$data[[data_index]] = newtable
    
    showNotification("confirm sucessfully", type="message")   # "default, "message", "warning", "error"
  })
  
  return(values0)
  } 
