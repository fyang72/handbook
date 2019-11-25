batch_body_replace_text_at_bkm <- function(x, bookmark="", value="") {
  
  assert(length(bookmark) == length(value), 
         msg="Number of bookmark is not equal to the number of value in batch_body_replace_text_at_bkm"
  )
  
  for (i in 1:length(bookmark)) { 
    ibookmark <- bookmark[i]
    ivalue <- value[i]
    
    founded <- tryCatch({
      x %>% cursor_bookmark(ibookmark)
      TRUE
    },
    error=function(cond="No such bookmark found!") {
      message(paste0("Bookmark not found in docx: ", ibookmark))
      # Choose a return value in case of error
      return(FALSE)
    }
    )
    
    if (founded) { 
      x <- x %>% cursor_bookmark(ibookmark) %>% 
        body_replace_text_at_bkm(ibookmark, ivalue)
    }
  }
  
  return(x)
}