
merge_all <- function(mydflist) {
  
  # put data.frames into list (dfs named df1, df2, df3, etc)
  #mydflist <- mget(ls(pattern="df\\d+"))
  
  # get all variable names
  allNms <- unique(unlist(lapply(mydflist, names)))
  
  # https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
  # put em all together
  out =  do.call(rbind,
                 lapply(mydflist,
                        function(x) {
                          if (is.null(x)) {
                            NULL
                          }else{
                            data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                   function(y) NA)))
                          }
                        }
                 )
  )  
  
  return(out)
}

