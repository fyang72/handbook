
```{r, standardize_nmdat}
#----------------------------------------------------
# standardize_nmdat_data_selector
#----------------------------------------------------
output$standardize_nmdat_data_selector <-renderUI({
  
  # callModule 
  
  ALL = callModule(module_load_dataset, "load_nmdat_for_standardize_nmdat", 
                   ALL, dataset_name="mYtEsT_for_standardize_nmdat"
  )
  
  
  # UI  
  fluidRow(column(6, 
                  module_load_dataset_UI(id="load_nmdat_for_standardize_nmdat", label=NULL)
  ), 
  column(6, 
         HTML(colFmt("internal library: build-in dataset <br>
                     within session: saved data within session <br> 
                     external file: external file", color="gray")
         )
         )
         )  
})
fluidRow(column(12, uiOutput(("standardize_nmdat_data_selector"))))
```




```{r, eval=FALSE }
#----------------------------------------------------
# UI for build_nmdat    
#----------------------------------------------------

# default_nmdat_checkin
default_nmdat_checkin <- reactive({ 
  file="./lib/pkmeta.xlsx"
  default_nmdat_checkin = read_excel(file, sheet = "nmdat", col_names = TRUE) %>% as.data.frame()  
  default_nmdat_checkin = default_nmdat_checkin %>% filter(!is.na(standard.name))
})

# build_nmdat_container
output$standardize_nmdat_container <- renderUI({ 
  validate(need(globalVars$login$status, message=FALSE),   
           need(ALL$DATA[["mYtEsT_for_standardize_nmdat"]], message="no data loaded yet")
  )
  # ALL, dataset_name, default_checkin
  isolate({ 
    print(head(ALL$DATA[["mYtEsT_for_standardize_nmdat"]]))
    
    callModule(module_checkInCols, "standardize_nmdat",  
               ALL, 
               #dataset_name="mYtEsT_for_standardize_nmdat", 
               dataset = ALL$DATA[["mYtEsT_for_standardize_nmdat"]],
               #script = "", 
               default_checkin =  read_excel("./lib/pkmeta.xlsx", sheet = "nmdat", col_names = TRUE) %>% 
                 as.data.frame()  %>% filter(!is.na(standard.name))
    )
  })
  
  fluidRow(
    column(12, 
           module_checkInCols_UI( ("standardize_nmdat"), label = "standardize_nmdat")
    )
  )
}) 

uiOutput(("standardize_nmdat_container"))

```






test2222222
```{r, test}

#----------------------------------------------------
# test2222222
#----------------------------------------------------
output$test2222222 <-renderUI({
  validate(need(globalVars$login$status, message=FALSE)#,   
           #need(ALL$DATA[["mYtEsT_for_standardize_nmdat"]], message="no data loaded yet")
  )
  
  # callModule 
  ALL = callModule(checkInCols, "test2222222", 
                   ALL, 
                   dataset_name="mYtEsT_for_standardize_nmdat",
                   default_checkin =  read_excel("./lib/pkmeta.xlsx", sheet = "nmdat", col_names = TRUE) %>% 
                     as.data.frame()  %>% filter(!is.na(standard.name))
  )
  
  
  # UI  
  fluidRow(column(12, 
                  checkInColsUI(id="test2222222", label=NULL)
  ) 
  )  
})
fluidRow(column(12, uiOutput(("test2222222"))))
```

