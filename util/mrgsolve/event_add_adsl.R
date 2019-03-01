
#---------------------------
# combined adsl into event
#---------------------------
event_add_adsl <- function(event, adsl) {
  
  # adex, ID is for patients
  n.subject = length(unique(adsl$ID))
  if (n.subject==0) {return(NULL)}
  if (is.null(event)) {return(NULL)}
  
  event = event %>% as.data.frame()
  if (is.null(event$ID)) {event = event %>% mutate(ID = 1)}
  if (is.null(event$GROUPID)) {event = event %>% mutate(GROUPID = ID)}
  
  library(dplyr)
  
  adex = bind_rows(replicate(n.subject, event, simplify = FALSE))
  adex = adex %>% mutate(ID= rep(1:n.subject, each=nrow(event))) 
  adex = adex %>% left_join(adsl, by="ID")   # ID, WGTBL, and others
  
  adex = adex %>% mutate(USUBJID=paste0(add_prefix(ID, digits=3), "-", 
                                        add_prefix(GROUPID, digits=3))) 
  
  # ID is unique here
  adex = adex %>% mutate(ID=as.integer(as.factor(USUBJID)))
  return(adex)
}
