
check_lst_file_exist <- function(server_IP_address, runno_full_path_lst)  {
  # Purpose: check whether a lst file exists on the specified directory on server
  #
  # Input: server_IP_address:  server_IP_address
  #        runno_full_path_lst:  a list of runno_full_path
  #
  # Output: a vector of boolean 
  #
  found <- lapply(runno_full_path_lst, function(runno_directory) {
    any(str_detect(list_subdir_or_file_on_HPC(
      server_IP_address, 
      directory_on_server = runno_directory, 
      option="-p | grep -v / "), ".lst"))
  })  %>% unlist()
  return(found)
}

