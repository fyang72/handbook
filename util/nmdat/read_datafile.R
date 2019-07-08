

read_datafile <- function(inFile=NULL) {
  ext <- tools::file_ext(inFile) %>% unique()
  ext <- ext[which(ext!="")]
  ext <- ext[1]
   
  tdata <- NULL
  tdata <- switch(ext,
         "csv" = read_csv(inFile, col_names=TRUE,  
                          col_type=cols(.default=col_character()))  %>% as.data.frame(),
         "xlsx"=read_excel(inFile, sheet = 1, col_names = TRUE)  %>% as.data.frame(),
         "xls" = read_excel(inFile)  %>% as.data.frame(),
         "sas7bdat" =  read_sas(inFile)  %>% as.data.frame(), 
         "RData" =  load(inFile),   # MUST NAMED AS "adpx"   need some work 
         NULL
  )
  
  error_message = "read data not sucessful. Only .csv, .xlsx, .xls, .sas7bdat, .RData can be read"
  if (is.null(tdata)) {print(error_message)}
  showNotification(paste0(error_message, collapse="\n"), type="error")
  
  #validate(need(tdata, error_message)) 
  
  tdata
}



