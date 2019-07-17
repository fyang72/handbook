
#-----------------------------------------------
# remove CWRES>6 outliers
#-----------------------------------------------

remove_CWRES6 <- function(xpdb, adpx, CWRES_threshold=6) {
  
  #xpdb %>% filter(ID=="1423061") %>% mutate(ABS_CWRES=abs(CWRES)) %>% 
  #                          select(ID, TIME, DV, IPRED, PRED, CWRES, ABS_CWRES)  
  
  # flag those outliers 
  tdata = adpx %>% left_join(xpdb[, c("ROWID", setdiff(colnames(xpdb), colnames(adpx)))], by="ROWID")
  
  tdata = tdata %>% mutate(HIGHCWRES = ifelse(abs(CWRES) > CWRES_threshold | 
                                                abs(WRES) > CWRES_threshold |
                                                abs(IWRES) > CWRES_threshold, 1, 0))  
  #paste("Outliers: ", nrow(tdata), " out of ", nrow(xpdb %>% filter(MDV==0) ), sep="")
  
  #tdata %>% top_n(20) %>% select(ROWID, ID, TIME, DV, IPRED, PRED, WRES, CWRES, IWRES) %>% arrange(ID, TIME)  %>% kable(digits = 2)
  
  # comment it out in the dataset
  tdata$CFLAG=as.character(tdata$CFLAG)
  tdata$C=as.character(tdata$C)
  tdata[which(tdata$HIGHCWRES==1 & tdata$MDV==0 & tdata$C=="."), "CFLAG"] = "Outliers" 
  tdata[which(tdata$HIGHCWRES==1 & tdata$MDV==0 & tdata$C=="."), "C"] = "C" 
  
  
  #unique(adpx$CFLAG)
  #adpx = adpx %>% mutate(C = ifelse(CFLAG!=".", "C", ""))
  
  # check it
  tt = tdata %>% filter(CFLAG=="Outliers", !is.na(IPRED)) %>% 
    select(C, CFLAG, ROWID, ID, TAD, TIME, DV, DVOR, IPRED, PRED, MDV, EVID, MDV, WRES, CWRES, IWRES)
  print(tt%>% as.data.frame())
  print(paste0(nrow(tt), " samples have removed, since were considered to be outliers"))
  #tt = adpx %>% filter(CFLAG!=".") %>% select( C, CFLAG, ROWID, ID) %>% as.data.frame()  
  #t(t(as.matrix(table(tt$CFLAG))))
  
  # output
  #DATA_INPUT = ifelse(!colnames(adpx) %in% key.col.lst, paste0(colnames(adpx),"=DROP"), colnames(adpx))
  #paste0("$INPUT ", paste0(DATA_INPUT, sep=" ", collapse=" "))
  
  return(tdata[, colnames(adpx)])
}


