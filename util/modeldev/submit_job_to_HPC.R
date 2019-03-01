
submit_job_to_HPC <- function(
  server.IP.address = "xx.xx.xx.xx.xx",
  local.model.name = NULL, 
  local.data.name = NULL, 
  server.model.dir = NULL, 
  server.data.dir= NULL
)  {
  
  # derive model and data information
  model.name = basename(local.model.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  model.runno = gsub(paste0(".", tools::file_ext(model.name)), "", model.name,  fix=TRUE)
  data.name = basename(local.data.name)  # "/home/feng.yang/R3918/ctl/LN_BASE_WT/LN_BASE_WT.ctl"
  
  # create a server directory to hold data and model
  system(command = paste0("ssh ", server.IP.address, " '", paste0("mkdir -p ", server.model.dir), "'"), intern = T)
  system(command = paste0("ssh ", server.IP.address, " '", paste0("mkdir -p ", server.data.dir), "'"), intern = T)
  
  # upload model and dataset to server
  system(command = paste0("scp  ", local.model.name, "  ", server.IP.address, ":", server.model.dir), intern = T)
  system(command = paste0("scp  ", local.data.name,  "  ", server.IP.address, ":", server.data.dir), intern = T)
  
  # run the commands
  command1 = paste0('cd ',  server.model.dir)            #'cd /home/feng.yang/R3918/ctl/LN_BASE_WT/;'
  command2 = paste0('setsid execute ',  model.name, ' -clean=4 ', '> output.log 2>&1 & ') 
  command = paste(command1, command2, sep="; ")
  system(command = paste0("ssh ", server.IP.address, " '", command, "'"), intern = T)
   
  # showNotification("download sucessfully")
  
  print("job submited sucessfully")
}

 