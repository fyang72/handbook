
fetch_job_from_HPC <- function(  
  server.IP.address = "xx.xx.xx.xx.xx",
  server.model.dir = NULL, 
  local.result.dir = NULL #,   #"./ctl/LN_BASE_WT/", 
  
  #server.data.full.path = NULL   
)  {
  # derive model and data information
  server.model.dir = paste0(server.model.dir, "/")
  local.result.dir = paste0(local.result.dir, "/")
  
  #if (is.null(local.result.dir)) {local.result.dir = paste0(HOME, dirname(local.model.name), "/")}
  
  # fetch the results
  #scp user@someRemoteHost.com:'/folder/*.{jpg,png}' .
  #https://unix.stackexchange.com/questions/417428/copying-files-with-certain-extensions-with-scp
  #system(command = paste0("scp 10.244.106.127:", paste0(server.data.full.path, "   ", local.result.dir)))
  
  system(command = paste0("mkdir -p ", local.result.dir), intern = T) 
  
  # all .ext, .ctl, .phi, .coi files
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "*.ctl  "), local.result.dir))
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "sdtab*  ", local.result.dir)))
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "patab*  ", local.result.dir)))
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "catab*  ", local.result.dir)))
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "cotab*  ", local.result.dir)))
  system(command = paste0("scp ", server.IP.address, ":", paste0(server.model.dir,  "mytab*  ", local.result.dir))) 
  
  # showNotification("download sucessfully")
  
   print("download sucessfully")
  # 
  # # read the results  
  # # -------------------------------
  # lst.file = paste0(local.result.dir, model.runno, ".lst")
  # 
  # #lst = read.lst(lst.file)
  # lst <- tryCatch(#source("text.R", local=TRUE),    #eval(parse(text=txt ))  , 
  #   read.lst(lst.file),
  #   error=function(e) {
  #     print("no result found yet"); 
  #     return("no result found yet")
  #   } #, finally = {
  #   # eval(parse(text=txt)) %>% as.data.frame()
  #   #}
  # )
  # 
  # 
  # lst.content = tryCatch(#source("text.R", local=TRUE),    #eval(parse(text=txt ))  , 
  #   readLines(lst.file),
  #   error=function(e) {
  #     print("nonmem job not finish yet ..."); 
  #     return("nonmem job not finish yet ...")
  #   } #, finally = {
  #   # eval(parse(text=txt)) %>% as.data.frame()
  #   #}
  # ) 
  # 
  # return(list(lst=lst, lst.content=lst.content))
}
