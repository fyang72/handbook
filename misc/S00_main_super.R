

rm(list = ls())

HOME = "~/FYANG/R1979_CD20_CD3/"  
setwd(HOME)

source(paste0(dirname(HOME), "/global.R") )


ihandbook = 0
folder.loc <- "~/handbook/script/"
file.lst <-list.files(path = folder.loc, all.files = FALSE,full.names = TRUE, include.dirs = TRUE, recursive =TRUE)     
file.lst = file.lst[which(substr(file.lst, nchar(file.lst)-1, nchar(file.lst)) %in% c(".r", ".R"))]

file.lst = file.lst[which(!substr(gsub(folder.loc, "", file.lst, fix=TRUE), 1,1) %in% c("_"))]

for (ifile in 1:length(file.lst)) { 
  print(file.lst[ifile]);  
  source(file=file.lst[ifile])  
}     #sys.source('file.R', envir=environment())



##########################################################################################
# load nmdat for all subsequent analysis 
##########################################################################################
 #nmdat = read_csv("./KRM/data/NM.csv", 
  #                col_type=cols(.default=col_character()))     # read as character as defualt


nmdat = read_sas("./KRM/data/nm2.sas7bdat")     # read as character as defualt

tdata = nmdat %>% rename(ARMA=ACTARM, 
                         ARMAN=ACTARMN, 
                         TESTN = TESTCATN
                         ) %>% 
  select(C, CFLAG, ROWID, STDY, ID, USUBJID, ARMA, ARMAN, COHORT, COHORTN, 
         PCTPT, TAD,  TIME, NTIM, DV,  DVOR, DVORU,  TESTCAT, TESTN,  TEST,  EVID,  CMT, BLQ,
         EXDUR, EXDOSE, AMT, EXTRT, EXROUTE, EXROUTN,  RATE,    
         AGE, AGEU, RACE, RACEN, SEX, SEXN, ETHNIC, ETHNICN, WGTBL, HGTBL, BMIBL)   %>% 
  mutate(MDV = ifelse(is.na(as_numeric(DV)), 1, 0),  
         EVID = as.integer(EVID)
         ) %>% 
  as.data.frame() %>%  # must be data.frame
  
  filter(TEST %in% c("Total R1979 Concentration", ""))

# predose and predose blq
tdata = tdata %>% mutate(CFLAG =NA)
tdata = tdata %>% mutate(CFLAG = ifelse(TIME<=0&EVID==0, "Pre-dose", CFLAG), 
                         CFLAG = ifelse(TIME<=0&EVID==0&as_numeric(DVOR)>0, "Pre-dose concentration > LLOQ", CFLAG)
                         ) 

tdata$C[which(!tdata$CFLAG %in% c("", NA))] = "C"

tdata %>% filter(ID==87) %>% select(ID, TIME, DVOR, EXSTDTC, EXENDTC, ADTM, ADT)
 #tdata$MDV %>% unique()
tdata %>% filter(USUBJID == "R1979-HM-1333-840206-143") %>% pull(ID) %>% unique()
 
col.lst = names(which(sapply(tdata, class)=="character"))
tdata = tdata %>% as.data.frame()
 for (i in 1:length(col.lst))  {
   tdata[, col.lst[i]] = gsub(" ", "_",tdata[, col.lst[i]], fix=TRUE )
 }
 tdata[is.na(tdata)] = "."
tdata = tdata %>% rename_at(vars(setdiff(col.lst, "C")), ~ paste0(setdiff(col.lst, "C"), "=", "DROP"))


#--------------------------------------------------------
# save the nonmem data  
#--------------------------------------------------------

paste0(colnames(tdata), sep=" ", collapse="")

program.name = "R1979"
model.name = "MM0226_v5"   # default  .ctl
data.name = "nmdat_0226_2019"   # default  .csv

server.IP.address = "10.244.106.127"
server.home.directory = "/home/feng.yang/"

write_csv(tdata, path=paste0("./KRM/data/", data.name, ".csv"))

#--------------------------------------------------------
# derive all varaibles for HPC
#--------------------------------------------------------
local.model.name = paste0("./KRM/ctl/", model.name, ".ctl")
local.data.name  = paste0("./KRM/data/", data.name, ".csv")   
local.result.dir = paste0("./KRM/output/", paste0(model.name, "_", data.name), "/")

server.model.dir = paste0(server.home.directory, 
                      program.name, "/ctl/", 
                      paste0(model.name, "_", data.name, "/")
                    )
 
server.data.dir= paste0(server.home.directory, 
                        program.name, "/data/"
)


#--------------------------------------------------------
# submit job to HPC
#--------------------------------------------------------
submit_job_to_HPC(
  server.IP.address = server.IP.address,  
  local.model.name = local.model.name, 
  local.data.name = local.data.name, 
  server.model.dir = server.model.dir,
  server.data.dir= server.data.dir
)
  
#--------------------------------------------------------
# check status 
#--------------------------------------------------------
# create a folder locally if not exit
system(command = paste0("mkdir -p ", "./KRM/"), intern = T)

system(command = paste0("scp ", server.IP.address, ":", 
                        paste0(server.model.dir,  "output.log  ", 
                               paste0("./KRM/")))) 
readLines("./KRM/output.log")



#--------------------------------------------------------
# fetch:  
#--------------------------------------------------------
system(command = paste0("mkdir -p ", local.result.dir), intern = T)

fetch_job_from_HPC(  
  server.IP.address = server.IP.address,
  server.model.dir = server.model.dir, 
  local.result.dir = local.result.dir,   #"./ctl/LN_BASE_WT/", 
  
  server.data.full.path = paste0(server.data.dir, data.name, ".csv")
)
   


########################################################
# 
########################################################
model.name <- "MM0226_v4"
local.result.dir = "./KRM/output/MM0226_v4_nmdat_0226_2019/"

lst.file = paste0(local.result.dir, model.name, ".lst")
values <- read_lst(lst.file)
values$lst.content




list.of.runs <- list_folder_on_HPC(server.IP.address = "10.244.106.127", 
                   directory.on.server = "/home/feng.yang/R1979/ctl/")




local.result.dir = "./KRM/output/MM0226_v3_nmdat_0226_2019/"
#--------------------------------------------------------
# read runno using xpose
#--------------------------------------------------------
xpdb.obj = xpose.data(runno="001", 
                      tab.suffix = "", 
                      directory = local.result.dir)

slot(xpdb.obj, "Data") <- slot(xpdb.obj, "Data") %>% 
  filter(MDV==0) %>% 
  mutate(IPRED = exp(IPRED), 
         PRED = ifelse(as_numeric(PRED)!=0, exp(PRED), PRED),
         DV = ifelse(as_numeric(DV)!=0, exp(DV), DV)
         ) %>% 
  filter(IPRED <70, PRED<70, DV<75) 

xpdb  = slot(xpdb.obj, "Data") 
head(xpdb)

#xpdb =  slot(xpdb.obj,"Data")
# runno = slot(xpdb.obj,"Runno")
 
#--------------------------------------------------------
# diagnostic plot
#--------------------------------------------------------
tt = xpdb_diagnostic_GOF1(xpdb.obj, values4xpdb=NULL) 

tt$diagnostic$PRED_DVOR

tt$diagnostic$PRED_DVOR_LOG

tt$diagnostic$IPRED_DVOR

tt$diagnostic$IPRED_DVOR_LOG

xpdb_diagnostic_GOF2(xpdb.obj, values4xpdb=NULL) 


tt = xpdb_diagnostic_GOF3(xpdb.obj, values4xpdb=NULL, n=25, ids=NA)
   
tt$diagnostic$INDIV_PLOT25 + facet_wrap(~ID, scale="free")



#--------------------------------------------------------
# read lst file  
#--------------------------------------------------------
values$lst
values$lst.content
 

# read the results  
# -------------------------------
lst.file = paste0(local.result.dir, model.runno, ".lst")

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


# $ofv
# [1] -675


