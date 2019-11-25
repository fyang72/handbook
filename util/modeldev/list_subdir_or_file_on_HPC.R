
 
list_subdir_or_file_on_HPC <- function(
  server_IP_address = "xx.xx.xx.xx.xx", 
  directory_on_server, 
  option="-d */") {
  
  # Purpose: return the file names or subfolder names under a specified directory on server
  #
  # Input: server_IP_address:  server_IP_address
  #        option:  "-d */" for subdirectory; 
  #                 "-f" for file name
  #                 can be any other ls option
  #
  # Output: a vector of string 
  #  
  
  directory_on_server = paste0(directory_on_server, "/")
  
  command1 = paste0('cd ',  directory_on_server)     
  command2 = paste0('ls ', option,   ' > list.of.dir.txt 2>&1 & ') 
  command = paste(command1, command2, sep="; ")
  system(command = paste0("ssh ", server_IP_address, " '", command, "'"), intern = T)
  
  # create a folder locally if not exit
  owd <- setwd(tempdir())
  on.exit(setwd(owd)) 
  
  system(command = paste0("scp ", server_IP_address, ":", 
                          paste0(directory_on_server,  "list.of.dir.txt  ", 
                                 paste0(".")))
  ) 
  txt = readLines("./list.of.dir.txt")
  return(txt)
}



list_folder_on_HPC <- function(server_IP_address = "xx.xx.xx.xx.xx", directory_on_server, option="-d  */ ") {
  directory_on_server = paste0(directory_on_server, "/")
  
  command1 = paste0('cd ',  directory_on_server)    
  command2 = paste0('ls ', option,  '> list.of.dir.txt 2>&1 & ') 
  command = paste(command1, command2, sep="; ")
  system(command = paste0("ssh ", server_IP_address, " '", command, "'"), intern = T)
  
  # create a folder locally if not exit
  owd <- setwd(tempdir())
  on.exit(setwd(owd)) 
  
  system(command = paste0("scp ", server_IP_address, ":", 
                          paste0(directory_on_server,  "list.of.dir.txt  ", 
                                 paste0(".")))
  ) 
  txt = readLines("./list.of.dir.txt")
  return(txt)
}


