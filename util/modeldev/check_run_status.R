check_run_status <- function(server_IP_address, 
                             file_full_path_name
) {
  
  owd <- setwd(tempdir())
  on.exit(setwd(owd))  
  
  system(command = paste0("scp ", server_IP_address, ":", file_full_path_name, "./"))
  readLines("./output.log")
  
  
}