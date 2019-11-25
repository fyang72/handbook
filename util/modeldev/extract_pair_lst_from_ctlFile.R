
extract_pair_lst_from_ctlFile <- function(base_ctl, key_word="_ON_") {
  # Purpose: Auto-extract the potential pair_lst(such as WGTBL_ON_CLQ) from the ctlFile
  #
  # Input: base_ctl:  input model
  #        key_word:  an identifier for such covariate variables
  #
  # Output: a vector of strings, such as c("WGTBL_ON_CLQ", "WGTBL_ON_VSS") 
  #
  
  start <- which(str_detect(base_ctl, "^\\$THETA\\s+"))
  end <- which(str_detect(base_ctl, "^\\$OMEGA\\s+"))
  ids <- intersect(start:end, which(!str_detect(base_ctl, "^\\s*;")))
 
  # if error
  assert(length(ids)>0,  msg=paste0("No covariate found in your THETA block"))
  
  pair_lst <- str_extract_all(
    base_ctl[ids], 
    paste0("[a-zA-Z0-9_.-]*", key_word, "[a-zA-Z0-9_.-]*"), 
    simplify = FALSE
  )  %>% unlist() %>% unique()
  
  # if error
  assert(length(pair_lst)>0,  msg=paste0("No covariate found in your THETA block"))
  
  return(pair_lst)
}
