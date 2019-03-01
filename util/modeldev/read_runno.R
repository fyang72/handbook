
read_runno  <- function(MODEL.HOME, subdir="ctl/", runno="LN001", postfix="")  {
  
  #postfix = ".nm7/"
  # need both sdtab001 and patab001,  "IPRED", +  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  library(xpose4) #read in the xopse library 
  
  #xpdb <- xpose.data("001", tab.suffix="", directory=paste(PsN_HOME, subdir, runno, postfix, "\\", sep="")) # run number of xpose table files
  #xpdb <- xpose.data("001", tab.suffix="", directory="H:\\FYANG\\R2810_PD1\\MODEL\\PsN\\LN001\\")
  
  
  directory=paste0(MODEL.HOME, subdir, runno, postfix, "/")
  
  sdtab = read.table(file=paste0(directory, "sdtab001"), skip=1, sep="", header=TRUE)
  patab = read.table(file=paste0(directory, "patab001"), skip=1, sep="", header=TRUE)

  lst = read.lst(paste0(directory, runno, ".lst")  )
  
  
  xpdb = left_join(sdtab, 
                   patab[, c("ID", setdiff(colnames(patab), colnames(sdtab)))] %>% distinct(ID, .keep_all=TRUE),
                   by="ID")
  
  
  xpdb$DV <- exp(as_numeric(xpdb$DV))
  xpdb$IPRED <- exp(as_numeric(xpdb$IPRED))
  xpdb$PRED <- exp(as_numeric(xpdb$PRED))
  
  
  #-----------------------------------------------------------------------------
  # attach adpx information (from nonmem datafile)
  #----------------------------------------------------------------------------- 
  
  base_ctl = readLines(paste0(directory, runno, ".ctl"),warn=FALSE) 
  
  library("readr")
  library("dplyr")
  
  # need to remove all ; comments
  # need to add ROWID in the dataset (adpx)
  
  tt = unlist(strsplit(base_ctl[which(regexpr('$DATA', base_ctl, fix=TRUE)>0)], " "))     
  tt = gsub("..", "", tt[which(regexpr('.csv', tt, fix=TRUE)>0)], fix=TRUE)
  tt = gsub(",", "", tt, fix=TRUE)
  
  # not use read_csv,  it auto convert double to integer
  adpx <- read.csv(file = paste0(MODEL.HOME, tt), skip=0,  stringsAsFactors=FALSE)
  #adpx <- read_csv(file = paste0(MODEL_HOME, tt), skip=0)
  #adpx %>% filter(ID=="1423390") %>% pull(ALBBL)
  
  
  head(adpx)
  
  return(list(xpdb=xpdb, adpx=adpx, ctl=base_ctl, lst = lst))
  
}

