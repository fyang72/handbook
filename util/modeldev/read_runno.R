

read_runno <- function(local.model.home.dir="./KRM/output/", list.of.runs, runno="001", tab.suffix = "") {
  library("readr")
  library("dplyr")
  library(xpose4) #read in the xopse library 
  
  for (i in 1:length(list.of.runs)) {
   
    runno = gsub("/", "", list.of.runs[i], fix=TRUE)
    local.result.dir = paste0(local.model.home.dir, list.of.runs[i])
    
    file.lst <-list.files(path = local.result.dir, all.files = FALSE,
                          full.names = TRUE, include.dirs = TRUE, recursive =TRUE)   
    
    # .lst
    lst.file.name <- file.lst[which(tools::file_ext(file.lst) == "lst")]
    if (length(lst.file.name)==1) {
      lst <- read.lst(lst.file.name)
      lst.content <- readLines(lst.file.name)
    }  
    
    # xpdb.obj
    xpdb.obj = xpose.data(runno=runno, 
                          tab.suffix = tab.suffix, 
                          directory = local.result.dir
    )
    # .ctl
    ctl.file.name <- file.lst[which(tools::file_ext(file.lst) == "ctl")]
    if (length(ctl.file.name)==1) {
      ctl = readLines(ctl.file.name,warn=FALSE) 
    } 
    
    # .csv
    csv.file.name <- file.lst[which(tools::file_ext(file.lst) == "csv")]
    if (length(csv.file.name)==1) {
      adpx <- read.csv(file = csv.file.name, skip=0,  stringsAsFactors=FALSE)
    } 
    
    values = NULL
    values[[runno]]$lst = lst
    values[[runno]]$lst.content = lst.content
    values[[runno]]$xpdb.obj = xpdb.obj
    values[[runno]]$ctl = ctl
    values[[runno]]$adpx = adpx 
    
  }
  return(values)
  
}






read_runno_old  <- function(MODEL.HOME, subdir="ctl/", runno="LN001", postfix="")  {
  
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

