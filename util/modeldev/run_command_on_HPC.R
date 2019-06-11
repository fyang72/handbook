

run_command_on_HPC <- function(server_IP_address = "xx.xx.xx.xx.xx", command="ls")  {
  # create a folder locally if not exit
  owd <- setwd(tempdir())
  on.exit(setwd(owd)) 
  
  tmpfile_name="./tmp.txt"
  command = paste0(command, '> ~/', tmpfile_name, ' 2>&1 ') 
  system(command = paste0("ssh ", server_IP_address, " '", command, "'"), intern = T)
  system(command = paste0("scp ", server_IP_address, ":", tmpfile_name, " ."))
  
  txt = readLines(tmpfile_name)
  return(txt)
}


# directory_on_server = "R2810"
# directory_on_server = paste0(directory_on_server, "/")
# command1 = paste0('cd ',  directory_on_server)    
# command2 = paste0('ls -d */ ') 
# 
# command = paste(command1, command2, sep="; ")
# 
# command = "ls"
# run_command_on_HPC(server_IP_address = server_IP_address, 
#                    command)
# 
# 
