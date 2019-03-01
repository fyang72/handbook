
read_lst <- function(lst.file) {

# read the results  
# -------------------------------


#lst = read.lst(lst.file)
lst <- tryCatch(#source("text.R", local=TRUE),    #eval(parse(text=txt ))  , 
  read.lst(lst.file),
  error=function(e) {
    print("no result found yet"); 
    return("no result found yet")
  } #, finally = {
  # eval(parse(text=txt)) %>% as.data.frame()
  #}
)


lst.content = tryCatch(#source("text.R", local=TRUE),    #eval(parse(text=txt ))  , 
  readLines(lst.file),
  error=function(e) {
    print("nonmem job not finish yet ..."); 
    return("nonmem job not finish yet ...")
  } #, finally = {
  # eval(parse(text=txt)) %>% as.data.frame()
  #}
) 


return(list(lst=lst, lst.content=lst.content))
}
