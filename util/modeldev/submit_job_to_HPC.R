

batch_submit_job_to_HPC <- function(
  server_IP_address,
  program_name, 
  runno_lst, 
  
  server_home = "/home/",   # /data/feng.yang/     /home/feng.yang/
  server_ctl_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/ctl/"),
  server_data_dir = paste0(server_home, tolower(Sys.info()["user"]), "/", program_name, "/data/"), 
  
  local_home = "./",
  local_ctl_dir = paste0(local_home, "/ctl/"),    # local directory that holds all ctl files
  local_data_dir = paste0(local_home, "/data/"),   # local directory that holds all data files
  local_output_dir = paste0(local_home, "/output/"),  # local directory that holds all nonmem output files from server
  
  execute_option = " -clean=4 -nodes=50", 
  want2_verbose_print = TRUE
) {
  
  
  runno_df = cbind(runno_lst, str_split_fixed(runno_lst, pattern="_", n=2)) %>% as.data.frame()
  colnames(runno_df) <- c("runno", "ctl", "dat")
  runno_df = runno_df %>% mutate(runno=as.character(runno))
  
  for (i in 1:nrow(runno_df)) {
    local_model_name = paste0(local_ctl_dir, runno_df[i, "ctl"], ".ctl")
    local_data_name = paste0(local_data_dir, runno_df[i, "dat"], ".csv") # 
    
    runno = runno_df[i, "runno"]
    
    #---------------------------------------------------
    # specify where to put the model and data on server
    #--------------------------------------------------- 
    server_model_dir = paste0(server_ctl_dir, "/", runno, "/") 
    server_data_dir= paste0(server_data_dir, "/")
    run_command = paste0("execute ", basename(local_model_name), " ", execute_option)   
    
    #-------------------------------
    # submit the model and data
    #-------------------------------
    submit_job_to_HPC(
      server_IP_address = server_IP_address,
      
      local_model_name = local_model_name, 
      local_data_name = local_data_name, 
      
      server_model_dir = server_model_dir, 
      server_data_dir= server_data_dir, 
      
      run_command = run_command
    )
    
    if (want2_verbose_print) {
      cat(paste0(runno, " submitted"), sep="\n") 
      cat("\n")
    }
  }
  
}





submit_job_to_HPC <- function(
  server_IP_address = "xx.xx.xx.xx.xx",
  
  local_model_name = NULL, 
  local_data_name = NULL, 
  
  server_model_dir = NULL, 
  server_data_dir= NULL, 
  
  run_command = ""
)  {
  
  # derive model and data information
  model_name = basename(local_model_name)  
  model_runno = tools::file_path_sans_ext(basename(local_model_name)) # LN001
  data_name = basename(local_data_name)  
  
  # create a server directory to hold data and model
  system(command = paste0("ssh ", server_IP_address, " '", paste0("mkdir -p ", server_model_dir), "'"), intern = T)
  system(command = paste0("ssh ", server_IP_address, " '", paste0("mkdir -p ", server_data_dir), "'"), intern = T)
  
  # upload model and dataset to server
  system(command = paste0("scp  ", local_model_name, "  ", server_IP_address, ":", server_model_dir), intern = T)
  system(command = paste0("scp  ", local_data_name,  "  ", server_IP_address, ":", server_data_dir), intern = T)
  
  # run the commands
  command1 = paste0('cd ',  server_model_dir)         
  command2 = paste0('setsid ', run_command, ' > output.log 2>&1 & ') 
  command = paste(command1, command2, sep="; ")
  
  # print(paste0("ssh ", server_IP_address, " \"", command, "\""))
  # system(command = paste0("ssh ", server_IP_address, " '", command, "'"), intern = T)
  system(command = paste0("ssh ", server_IP_address, " \"", command, "\""), intern = T)
    
}

 