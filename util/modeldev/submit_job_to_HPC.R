
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
  system(command = paste0("ssh ", server_IP_address, " '", command, "'"), intern = T)
    
}

 