
fetch_job_from_HPC <- function(  
  server_IP_address = "xx.xx.xx.xx.xx",
  server_model_dir = NULL, 
  local_result_dir = NULL #,   #"./ctl/LN_BASE_WT/", 
  
  #server.data.full.path = NULL   
)  {
  # derive model and data information
  server_model_dir = paste0(server_model_dir, "/")
  local_result_dir = paste0(local_result_dir, "/")
  
  #if (is.null(local_result_dir)) {local_result_dir = paste0(HOME, dirname(local.model.name), "/")}
  
  # fetch the results
  #scp user@someRemoteHost.com:'/folder/*.{jpg,png}' .
  #https://unix.stackexchange.com/questions/417428/copying-files-with-certain-extensions-with-scp
  #system(command = paste0("scp 10.244.106.127:", paste0(server.data.full.path, "   ", local_result_dir)))
  
  system(command = paste0("mkdir -p ", local_result_dir), intern = T) 
  
  # all .ext, .ctl, .phi, .coi files
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.ctl  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.lst  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.coi  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.cor  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.cov  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.ext  "), local_result_dir))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "*.phi  "), local_result_dir))
  
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "sdtab*  ", local_result_dir)))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "patab*  ", local_result_dir)))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "catab*  ", local_result_dir)))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "cotab*  ", local_result_dir)))
  system(command = paste0("scp ", server_IP_address, ":", paste0(server_model_dir,  "mytab*  ", local_result_dir))) 
 
  # 
  # # read the results  
  # # -------------------------------
  # lst.file = paste0(local_result_dir, model.runno, ".lst")
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
