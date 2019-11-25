

batch_body_replace_all_text <- function(x, old_value, new_value,
                                        only_at_cursor = FALSE,
                                        ignore.case=TRUE, 
                                        warn = TRUE, ...) {
  
  assert(length(old_value) == length(new_value), 
         msg="Number of old_value is not equal to the number of new_value in batch_body_replace_all_text"
  )
  
  for (i in 1:length(old_value)) { 
    #itext <- 
    #ivalue <- 
    
    # founded <- tryCatch({
    #   x %>% cursor_bookmark(itext)
    #   TRUE
    # },
    # error=function(cond="No such text found!") {
    #   message(paste0("text not found in docx: ", itext))
    #   # Choose a return value in case of error
    #   return(FALSE)
    # }
    # )
    # 
    # if (founded) { 
    
    #print(paste0("printing old_value: ", old_value[i]))
    
      x <- x %>% body_replace_all_text(
        old_value[i], new_value[i], 
        only_at_cursor = only_at_cursor, 
        ignore.case=ignore.case, 
        warn = warn)
   # }
  }
  
  return(x)
}