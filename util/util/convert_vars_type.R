
# 
# DF <- data.frame("a" = as.character(0:5),
#                  "b" = paste(0:5, ".1", sep = ""),
#                  "c" = letters[1:6],
#                  stringsAsFactors = FALSE)
# 
# 
# 
#  data_type <- c("character", "integer", "numeric")
#  names(data_type) = c("STUDYID", "SEXN", "WGTBL")
# #  
#  tdata = adsl %>% select(STUDYID, SEXN, WGTBL) %>% as.data.frame()
# 
#  t1 = convert_vars_type(tdata, data_type)
#  
 
 
convert_vars_type <- function(tdata, data_type) {
   
  tdata <- tdata %>% as.data.frame(stringsAsFactors=FALSE)
  
  vars_name <- names(data_type)
  data_type <- fuzzy_match(data_type, c("character", "numeric", "integer"))
  names(data_type) <- vars_name
  
# Check columns classes
#sapply(tdata, class)
 
  for (itype in data_type) { 
    if (itype=="integer") {
      col_lst <- intersect(colnames(tdata), 
                           names(data_type)[which(data_type==itype)])
      tdata[,col_lst] = sapply(tdata[,col_lst], as.integer)  %>% 
        as.data.frame(stringsAsFactors=FALSE)
    }
    
    if (itype=="character") {
      col_lst <- intersect(colnames(tdata), 
                           names(data_type)[which(data_type==itype)])
      tdata[col_lst] = sapply(tdata[col_lst], as.character)%>% 
        as.data.frame(stringsAsFactors=FALSE)
    }
    
    if (itype=="numeric") {
      col_lst <- intersect(colnames(tdata), 
                           names(data_type)[which(data_type==itype)])
      tdata[col_lst] = sapply(tdata[col_lst], as_numeric)%>% 
        as.data.frame(stringsAsFactors=FALSE)
    }
    
  }
     
#sapply(tdata, class)

return(tdata)
}
