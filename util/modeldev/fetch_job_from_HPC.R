
#-------------------------------
# fetch the result when done
#-------------------------------
batch_fetch_job_from_HPC <- function(
  server_IP_address = "10.244.64.97",
  program_name="TEST", 
  runno ="",
  
  server_home = "/home/",   # /data/feng.yang/     /home/feng.yang/
  server_ctl_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/ctl/"),
  server_data_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/data/"), 
  
  local_home = "./",
  local_ctl_dir = paste0(local_home, "ctl/"),    # local directory that holds all ctl files
  local_data_dir = paste0(local_home, "data/"),   # local directory that holds all data files
  local_output_dir = paste0(local_home, "output/")  # local directory that holds all nonmem output files from server
  
) {
  
  runno_df = cbind(runno, str_split_fixed(runno, pattern="_", n=2)) %>% as.data.frame()
  colnames(runno_df) <- c("runno", "ctl", "dat")
  runno_df = runno_df %>% mutate(runno=as.character(runno))
  
  #
  for (i in 1:nrow(runno_df)) {
    runno = runno_df[i, "runno"]
    print(paste0("fetching ", runno))
    
    fetch_job_from_HPC(  
      server_IP_address = server_IP_address,
      server_model_dir = paste0(server_ctl_dir, runno),
      local_result_dir = paste0(local_output_dir, "/ctl/", runno)
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
 
  # order the modelfit_directory by their IDs
  #tt = list_folder_on_HPC(server_IP_address, server_model_dir)  
  tt = list_subdir_or_file_on_HPC(server_IP_address, 
    directory_on_server = server_model_dir, 
    option="-d */")
    
  tt = bind_cols(modelfit_directory=tt, 
        ID=tt %>% extractExpr(regexpr="([0-9]*\\.?[0-9]+)" ) %>% as.numeric()
        ) %>% 
    arrange(ID)
  modelfit_directory <- tt%>% pull(modelfit_directory) # , decreasing=TRUE)  
  
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
  #     print("nonmem runno not finish yet ..."); 
  #     return("nonmem runno not finish yet ...")
  #   } #, finally = {
  #   # eval(parse(text=txt)) %>% as.data.frame()
  #   #}
  # ) 
  # 
  # return(list(lst=lst, lst.content=lst.content))
}
