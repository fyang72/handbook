
# run the commands
list_folder_on_HPC <- function(server.IP.address = "xx.xx.xx.xx.xx", directory.on.server) {
  directory.on.server = paste0(directory.on.server, "/")
  
  #directory.on.server = "/home/feng.yang/"
  command1 = paste0('cd ',  directory.on.server)            #'cd /home/feng.yang/R3918/ctl/LN_BASE_WT/;'
  command2 = paste0('ls -d */ ', '> list.of.dir.txt 2>&1 & ') 
  command = paste(command1, command2, sep="; ")
  system(command = paste0("ssh ", server.IP.address, " '", command, "'"), intern = T)
  
  # create a folder locally if not exit
  system(command = paste0("mkdir -p ", "./KRM/"), intern = T)
  
  system(command = paste0("scp ", server.IP.address, ":", 
                          paste0(directory.on.server,  "list.of.dir.txt  ", 
                                 paste0("./KRM/")))
  ) 
  txt = readLines("./KRM/list.of.dir.txt")
  return(txt)
}
