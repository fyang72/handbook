
#-------------------------------
# fetch the result when done
#-------------------------------
batch_fetch_job_from_HPC <- function(server_IP_address, program_name, runno, local_home="./KRM") {
  
  runno_df = cbind(runno, str_split_fixed(runno, pattern="_", n=2)) %>% as.data.frame()
  colnames(runno_df) <- c("runno", "ctl", "dat")
  runno_df = runno_df %>% mutate(runno=as.character(runno))
  
  #
  for (i in 1:nrow(runno_df)) {
    runno = runno_df[i, "runno"]
    print(paste0("fetching", runno))
    
    main_directory <- paste0("/home/", tolower(Sys.info()["user"]), "/", program_name, "/")
    server_model_dir = paste0(main_directory, "ctl/", runno, "/")
    local_result_dir = paste0(local_home, "/output/ctl/", runno, "/")
    
    fetch_job_from_HPC(  
      server_IP_address = server_IP_address,
      server_model_dir = server_model_dir, 
      local_result_dir = local_result_dir  #,   #"./ctl/LN_BASE_WT/", 
    )
    
  }
  
}


fetch_job_from_HPC <- function(  
  server_IP_address = "xx.xx.xx.xx.xx",
  server_model_dir = NULL, 
  local_result_dir = NULL #,   #"./ctl/LN_BASE_WT/", 
  
  #server.data.full.path = NULL   
)  {
  # derive model and data information
  tt = strsplit(basename(server_model_dir), "_") %>% unlist()
  model_name = tt[1]
  data_name = tt[2]
  
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
 
  
  tt = list_folder_on_HPC(server_IP_address, server_model_dir)  
  modelfit_directory <- sort(tt, decreasing=TRUE)[1]  # the most recent one
  
  for (i in 1:length(modelfit_directory)) {
    modelfit_dir <- modelfit_directory[i]
    file_full_path_name = paste0(
        server_model_dir, modelfit_dir,
        paste0("raw_results_", model_name, ".csv") ############################
    )
  
    system(command = paste0("scp ", server_IP_address, ":", 
                          file_full_path_name, " ",  local_result_dir))
  
    file.rename(paste0(local_result_dir, "raw_results_", model_name, ".csv"), 
                paste0(local_result_dir, "fit", i, "_", model_name, ".csv")
    )
  
  }
  
   
  
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
