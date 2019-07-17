
build_nmdat  <- function(adpx) {
  
  adpx = as.data.frame(adpx)
  
  if (!"ROWID" %in% colnames(adpx)) {adpx = cbind(ROWID=1:nrow(adpx), adpx)}
  if (!"CFLAG" %in% colnames(adpx)) {adpx = cbind(CFLAG="", adpx)}
  if (!"C" %in% colnames(adpx)) {adpx = cbind(C="", adpx)}
  
  adpx = adpx %>% mutate(NTIM=as_numeric(NTIM), 
                         TIME=as_numeric(TIME), 
                         DVOR=as_numeric(DVOR), 
                         AMT =as_numeric(AMT)
                         )    #, as.numeric) %>% 
     
  
  adpx$AMT[which(is.na(adpx$AMT))] = "."
  adpx$DV[which(is.na(adpx$DV))] = "."
  
  adpx[is.na(adpx)] = "."
  adpx[which(adpx$DV=="."), "MDV"] = 1
  
  # remove NA entries
  nacols <- function(df) { 
    colnames(df)[unlist(lapply(df, function(x) any(is.na(x)|is.nan(x)|x=="NA"|x=="NAN"|x=="NaN")))] }
  if (length(nacols(adpx))>0) {
    print(nacols(adpx))
    print(anyna(adpx))
  }
  
  
  # adpx$EVID = as_numeric(adpx$EVID) 
  # adex = adpx[which(adpx$EVID!=0), ]; t.adex = paste(adex$USUBJID, adex$TIME, sep="")
  # adpc = adpx[which(adpx$EVID==0), ]; t.adpc = paste(adpc$USUBJID, adpc$TIME, sep="")
  # adpc = adpc[which(!t.adpc %in% t.adex), ]
  # adpx = rbind(adex, adpc)
  # adpx = adpx %>% arrange(USUBJID, TIME, -EVID) 
  #adpx = adpx[order(adpx$STUDYID, adpx$USUBJID, adpx$TIME, -adpx$EVID), ]  # no ARMA, Since it becomes complicated for multilple doses per subject
  
  
  nmdat = adpx
  colnames(nmdat) <- gsub(".", "_", colnames(nmdat), fixed=TRUE)
  colnames(nmdat) <- gsub(" ", "_", colnames(nmdat), fixed=TRUE)
  
  # remove "," and " " in some columns 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(" ", "-", nmdat[, icol])} 
  for (icol in 1:ncol(nmdat)) {nmdat[, icol] = gsub(",", "-", nmdat[, icol])} 
  
  
  
  
  # add header
  for (i in colnames(nmdat)) {
    nmdat[,i] <- as.character(nmdat[,i]) 
    nmdat[,i] <- gsub(" ", "_", nmdat[,i], fixed=TRUE)
  }
  
  return(nmdat) 
  
  #nmdat <- rbind("C"=colnames(nmdat),  nmdat) 
  
  #nmdat["C1", 1] <- descp  # in order to use PDxPop
  #rownames(nmdat)[2:nrow(nmdat)] <- 1:(nrow(nmdat)-1)
  
  print( paste(colnames(nmdat), collapse=" ")) 
  #nonmem.file.name <- paste(".\\NONMEM\\R1193_HV_1219_simulation_01", ".csv", sep="")
  #write_table(nmdat, path, sep=",", col.names = FALSE, row.names = FALSE, quote = FALSE)
  
  
}
